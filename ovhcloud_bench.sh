#!/usr/bin/env bash
#
# ovhcloud_bench.sh - provision an OVHcloud Metal Instance (Public Cloud bare
# metal, e.g. bm-s1), set it up and run the benchmark on it inside tmux, so the
# run survives disconnects.
#
# Commands (default: run):
#   run           create the instance (if needed), set it up, start the benchmark
#   attach        ssh in and attach to the tmux session ("bench")
#   status        one-shot: tmux state, bench.done, last log lines
#   logs          tail -f the benchmark log
#   wait          block until the run finishes and print the pull request
#   list          list the Metal Instances in the project (to spot leftovers)
#   destroy       delete the instance and forget it locally
#
# Options:
#   --flavor <name>       default bm-s1 (Metal Instance: 4c/8t, 32GB, 2x960GB)
#   --region <name>       default: the first region where the flavor is available
#   --image <id>          default: newest Ubuntu LTS available in that region
#   --project <id>        Public Cloud project; default OVH_CLOUD_PROJECT_SERVICE
#                         / OS_TENANT_ID, or the only project in the account
#   --name <name>         instance name (default related-post-gen-bench)
#   --repo <url>          default: the origin of this checkout
#   --branch <name>       default main
#   --device <label>      machine label for the PR + readme (derived by default)
#   --ssh-key <path>      private key, default ~/.ssh/id_rsa; its .pub is uploaded
#   --update-args "<a b>" extra arguments for update_results.sh, eg.
#                         "--tests go" or "--tests go,go_ho" or "--skip-existing"
#   --no-label-sync       do not touch the readme's "VM Specs" line
#   --timeout <minutes>   delivery + install + ssh wait timeout (default 60)
#   --wait                stay until the benchmark finishes
#   --destroy-when-done   with --wait: delete the instance afterwards
#   --force               recreate/restart even if one is already tracked/running
#   --dry-run             print the create command and stop
#
# Requirements: ovhcloud (authenticated with a token that grants the /cloud
# routes - `ovhcloud login`), jq and ssh locally, and an SSH key.
#
# Examples:
#   ./ovhcloud_bench.sh                                   # all tests
#   ./ovhcloud_bench.sh --update-args "--tests go"        # quick subset
#   ./ovhcloud_bench.sh status                            # how is it going?
#   ./ovhcloud_bench.sh --wait --destroy-when-done        # wait for the PR, clean up
#
# Metal Instances are billed hourly (bm-s1 is ~EUR 0.50/h) while the instance
# exists, so run "destroy" (or use --destroy-when-done) once the PR is open.

set -euo pipefail

FLAVOR="bm-s1"
REGION=""
IMAGE_ID=""
PROJECT=""
NAME="related-post-gen-bench"
REPO_URL=""
BRANCH="main"
DEVICE_LABEL=""
DEVICE_LABEL_ARG=""
SSH_KEY="$HOME/.ssh/id_rsa"
UPDATE_ARGS=""
LABEL_SYNC=1
WAIT=0
DESTROY_WHEN_DONE=0
TIMEOUT_MIN=60
FORCE=0
DRY_RUN=0
CMD="run"

STATE_DIR="${XDG_CACHE_HOME:-$HOME/.cache}/related-post-gen"
STATE_FILE="$STATE_DIR/ovhcloud-bench.env"

# where the benchmark lives on the instance (absolute, so it does not depend on
# which user we end up logging in as)
REMOTE_BASE="/opt/related-post-gen-bench"
REMOTE_REPO="$REMOTE_BASE/repo"
REMOTE_SCRIPT="$REMOTE_BASE/run-bench.sh"
REMOTE_LOG="$REMOTE_BASE/bench.log"
REMOTE_DONE="$REMOTE_BASE/bench.done"

# ---------------------------------------------------------------------------
# helpers
# ---------------------------------------------------------------------------
die() {
    printf 'Error: %s\n' "$*" >&2
    exit 1
}
warn() { printf 'Warning: %s\n' "$*" >&2; }
info() { printf '%s\n' "$*"; }
step() { printf '\n=== %s ===\n' "$*"; }
need() { command -v "$1" >/dev/null 2>&1 || die "$1 is required but not installed"; }

usage() {
    # print the header comment block (everything after the shebang)
    awk 'NR > 2 && /^#/ { sub(/^# ?/, ""); print; next } NR > 2 { exit }' "$0"
}

# ---------------------------------------------------------------------------
# ovhcloud
#
# Every call goes through "ovhcloud ... -o json". The CLI prints API errors on
# *stdout* as {"message": "...", "error": true} and exits 1, so success is
# checked on both the exit code and the payload.
# ---------------------------------------------------------------------------
ovh() { # ovh <args...> -> JSON on stdout, dies on error
    local out rc msg
    if out="$(ovhcloud "$@" -o json)"; then rc=0; else rc=$?; fi
    msg="$(jq -r 'if type == "object" and (.error == true) then .message else empty end' <<<"${out:-}" 2>/dev/null || true)"
    if [ "$rc" -ne 0 ] || [ -n "$msg" ]; then
        printf 'Error: %s\n' "${msg:-ovhcloud $* failed (exit $rc)}" >&2
        return 1
    fi
    printf '%s\n' "$out"
}

# Same, but usable when the caller has to inspect the error (creates): runs in
# this shell, so $LAST_OUT and $OVH_ERROR survive. stdout carries the JSON (the
# CLI prints API errors there too), stderr is left alone so progress shows.
ovh_try() {
    local rc
    if LAST_OUT="$(ovhcloud "$@" -o json)"; then rc=0; else rc=$?; fi
    OVH_ERROR="$(jq -r 'if type == "object" and (.error == true) then .message else empty end' <<<"${LAST_OUT:-}" 2>/dev/null || true)"
    if [ -z "$OVH_ERROR" ] && [ "$rc" -ne 0 ]; then
        OVH_ERROR="ovhcloud $* failed (exit $rc)"
    fi
    [ -z "$OVH_ERROR" ] && return 0
    LAST_OUT=""
    return 1
}

# project flag array, empty when the CLI should resolve the project itself
PC=()
set_project_flag() {
    PC=()
    [ -n "$PROJECT" ] && PC=(--cloud-project "$PROJECT")
    return 0
}

# ---------------------------------------------------------------------------
# ssh
# ---------------------------------------------------------------------------
SSH_OPTS=(-o StrictHostKeyChecking=accept-new -o UserKnownHostsFile="$HOME/.ssh/known_hosts"
    -o ConnectTimeout=10 -o ServerAliveInterval=30 -o BatchMode=yes)

ssh_run() { # ssh_run <user> <ip> <command>
    local user="$1" ip="$2" cmd="$3"
    if [ "$REMOTE_SUDO" = "1" ]; then
        ssh "${SSH_OPTS[@]}" -i "$SSH_KEY" "$user@$ip" "sudo -n bash -c $(printf '%q' "$cmd")"
    else
        ssh "${SSH_OPTS[@]}" -i "$SSH_KEY" "$user@$ip" "$cmd"
    fi
}

ssh_script() { # ssh_script <user> <ip> < script
    local user="$1" ip="$2"
    if [ "$REMOTE_SUDO" = "1" ]; then
        ssh "${SSH_OPTS[@]}" -i "$SSH_KEY" "$user@$ip" 'sudo -n bash -s'
    else
        ssh "${SSH_OPTS[@]}" -i "$SSH_KEY" "$user@$ip" 'bash -s'
    fi
}

ssh_probe() { # ssh_probe <user> <ip> -> true when we can log in
    local user="$1" ip="$2"
    timeout 20 ssh "${SSH_OPTS[@]}" -o ConnectTimeout=8 -i "$SSH_KEY" "$user@$ip" true >/dev/null 2>&1
}

# ---------------------------------------------------------------------------
# local state (so status/attach/destroy know what we started)
# ---------------------------------------------------------------------------
load_state() {
    [ -f "$STATE_FILE" ] && . "$STATE_FILE"
    return 0
}

save_state() {
    mkdir -p "$STATE_DIR"
    cat >"$STATE_FILE" <<EOF
INSTANCE_ID=$INSTANCE_ID
INSTANCE_IP=$INSTANCE_IP
INSTANCE_PROJECT=$(printf '%q' "$PROJECT")
INSTANCE_REGION=$(printf '%q' "$REGION")
INSTANCE_FLAVOR=$(printf '%q' "$FLAVOR")
SSH_USER=$(printf '%q' "$SSH_USER")
REMOTE_SUDO=$REMOTE_SUDO
DEVICE_LABEL=$(printf '%q' "$DEVICE_LABEL")
NAME=$(printf '%q' "$NAME")
EOF
}

ensure_address() { # make sure we know how to reach the tracked instance
    [ -n "${INSTANCE_ID:-}" ] || die "no instance in $STATE_FILE"
    [ -n "${INSTANCE_IP:-}" ] || refresh_instance_ip
}

# ---------------------------------------------------------------------------
# Public Cloud lookups
# ---------------------------------------------------------------------------
resolve_project() {
    set_project_flag
    [ -n "$PROJECT" ] && return 0

    # the CLI also accepts OVH_CLOUD_PROJECT_SERVICE / OS_TENANT_ID
    if [ -n "${OVH_CLOUD_PROJECT_SERVICE:-}" ] || [ -n "${OS_TENANT_ID:-}" ]; then
        return 0
    fi

    local list count
    list="$(ovh cloud project list)" ||
        die "cannot list Public Cloud projects.
  Check that the CLI is authenticated and its token grants the /cloud routes
  (run 'ovhcloud login'): ./$SCRIPT_NAME --help mentions the requirements"

    count="$(jq 'length' <<<"${list:-null}")"
    case "$count" in
    0) die "no Public Cloud project in this account.

  Metal Instances (bm-*) only exist inside a Public Cloud project, and OVHcloud
  has no API/CLI command to create the first one. Create it in the Control
  Panel (Public Cloud > create a project), then re-run this script:
    https://www.ovh.com/manager/#/public-cloud
  If the account has no payment method the project starts in 'discovery' mode
  (read-only) - activate it first." ;;
    1)
        PROJECT="$(jq -r '.[0].id' <<<"$list")"
        set_project_flag
        info "project:  $PROJECT ($(jq -r '.[0].currentState.name // "-"' <<<"$list"))"
        ;;
    *)
        # several projects: keep whatever the CLI config already selects
        if ovh cloud region list >/dev/null 2>&1; then
            info "project:  (default from the ovhcloud configuration)"
        else
            die "several Public Cloud projects - pick one with --project <id>:
$(jq -r '.[] | "  \(.id)  \(.currentState.name // "-")"' <<<"$list")"
        fi
        ;;
    esac
}

flavor_candidates() { # -> "<id>\t<region>\t<vcpus>\t<ram GB>\t<disk>\t<osType>"
    ovh cloud instance flavor list "${PC[@]}" | jq -r --arg f "$FLAVOR" '
        [ .[]? | select(.name == $f) | select(.available == null or .available == true)
          | {id, region: (.region // ""), vcpus: (.vcpus // .vcpusCount // 0),
             ram: (.ram // 0), disk: (.disk // 0), osType: (.osType // "")} ]
        | (group_by(.region) | map(.[0]))[]
        | "\(.id)\t\(.region)\t\(.vcpus)\t\(.ram)\t\(.disk)\t\(.osType)"'
}

# Note: the OVHcloud flavor API reports "ram" in GB (bm-s1 -> 32), not MB.
# Images are named "Ubuntu 24.04" or, for metal, "Baremetal - Ubuntu 24.04";
# their state field is "active" (older references said "available").
IMG_FILTER='
    [ .[]?
      | select((.status // "active") | test("^(active|available)$"))
      | select((.name // "") | test("(^| )Ubuntu [0-9]+\\.[0-9]+"))
      | . + {ver: (.name | capture("Ubuntu (?<v>[0-9]+\\.[0-9]+)").v)} ]
    | sort_by(.ver | split(".") | map(tonumber))
    | reverse
    | .[]
    | "\(.id)\t\(.name)"'

ubuntu_images() { # ubuntu_images <region> [os-type] -> "<image id>\t<name>", newest first
    local region="$1" os_type="${2:-}" out
    # a metal flavor needs a "baremetal-linux" image; fall back to every image
    if [ -n "$os_type" ]; then
        out="$(ovh cloud instance image list -r "$region" --os-type "$os_type" "${PC[@]}" 2>/dev/null |
            jq -r "$IMG_FILTER" 2>/dev/null)" || out=""
        if [ -n "$out" ]; then
            printf '%s\n' "$out"
            return 0
        fi
    fi
    ovh cloud instance image list -r "$region" "${PC[@]}" | jq -r "$IMG_FILTER"
}

good_regions() { # regions whose quota profile is not a discovery/limited stub
    ovh cloud quota get "${PC[@]}" 2>/dev/null | jq -r '
        (.currentState.regions // [])[]
        | select((.profile // "") != "" and ((.profile | test("^(0vm|limited)$")) | not))
        | (.location.region // .region // empty)' 2>/dev/null || true
}

resolve_flavor_and_image() {
    step "Looking up $FLAVOR"
    set_project_flag

    local cands cand
    cands="$(flavor_candidates)"
    if [ -n "$REGION" ]; then
        cand="$(awk -F'\t' -v r="$REGION" '$2 == r' <<<"$cands")"
        [ -n "$cand" ] ||
            die "$FLAVOR is not available in $REGION (see: ovhcloud cloud instance flavor list -r $REGION)
  pick another --region or leave it out to use the first available one"
    else
        # prefer a region with a real compute quota: the discovery default
        # ("limited") often cannot host a whole Metal Instance
        [ -n "$cands" ] ||
            die "no region offers $FLAVOR right now (see: ovhcloud cloud instance flavor list)
  it may be out of stock, or the flavor name is wrong - try --flavor"
        local r
        while IFS= read -r r; do
            [ -n "$r" ] || continue
            cand="$(awk -F'\t' -v r="$r" '$2 == r' <<<"$cands" | head -1)"
            [ -n "$cand" ] && break
        done < <(good_regions)
        [ -n "$cand" ] || cand="$(head -1 <<<"$cands")"
    fi

    FLAVOR_ID="${cand%%$'\t'*}"
    local rest="${cand#*$'\t'}"
    REGION="${rest%%$'\t'*}"
    rest="${rest#*$'\t'}"
    CPU="${rest%%$'\t'*}"
    rest="${rest#*$'\t'}"
    RAM_GB="${rest%%$'\t'*}"
    rest="${rest#*$'\t'}"
    local disk="${rest%%$'\t'*}"
    OS_TYPE="${rest#*$'\t'}"

    info "flavor:   $FLAVOR (${CPU}vCPU, ${RAM_GB}GB RAM, ${disk}GB disk) in $REGION"

    local images=() id nm
    if [ -n "$IMAGE_ID" ]; then
        images=("$IMAGE_ID"___"Ubuntu (image id $IMAGE_ID)")
    else
        while IFS=$'\t' read -r id nm; do
            [ -n "$id" ] && images+=("$id"___"$nm")
        done < <(ubuntu_images "$REGION" "$OS_TYPE")
        [ ${#images[@]} -gt 0 ] ||
            die "no Ubuntu LTS image available in $REGION
  list them with: ovhcloud cloud instance image list -r $REGION"
    fi
    info "image:    ${images[0]#*___}"
}

# The OVHcloud instance API (v1) resolves --ssh-key.name against the v1
# "sshkey" store, which is *separate* from the v2 one the CLI's
# "cloud ssh-key" commands read and write (a key created through those is not
# visible to instance creation). So the key is created inline with
# --ssh-key.create.* and referenced by name once it exists.
SSH_KEY_NAME=""
init_ssh_key_name() {
    [ -f "$SSH_KEY.pub" ] || die "$SSH_KEY.pub not found - create one with: ssh-keygen -t ed25519"
    SSH_KEY_NAME="related-post-gen-$(basename "$SSH_KEY")"
}

try_create() { # try_create <image id> <create|name> -> $LAST_OUT / $OVH_ERROR
    local image="$1" keymode="$2"
    local -a cmd=(cloud instance create "$REGION"
        --name "$NAME" --flavor "$FLAVOR_ID" --boot-from.image "$image"
        --network.public --billing-period hourly --wait)
    if [ "$keymode" = name ]; then
        cmd+=(--ssh-key.name "$SSH_KEY_NAME")
    else
        cmd+=(--ssh-key.create.name "$SSH_KEY_NAME"
            --ssh-key.create.public-key "$(cat "$SSH_KEY.pub")")
    fi
    ovh_try "${cmd[@]}" "${PC[@]}"
}

instance_id_by_name() { # instance_id_by_name <name> -> id or empty
    ovh cloud instance list "${PC[@]}" 2>/dev/null |
        jq -r --arg n "$1" '[ .[]? | select(.targetSpec.name == $n) ] | (.[0].id // empty)' 2>/dev/null ||
        true
}

instance_public_ip() { # instance_public_ip <instance json> -> IPv4 or empty
    jq -r '
        ([ (.currentState.networks // [])[]
           | (.addresses // [])[]
           | select(((.version // "") | tostring) == "4")
           | select((.type // "public") != "private")
           | .ip ] | .[0])
        // ([ .. | objects | select(has("ip")) | .ip
              | select(test("^[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+$")) ] | .[0])
        // ""' <<<"$1" 2>/dev/null || echo ""
}

# ---------------------------------------------------------------------------
# steps
# ---------------------------------------------------------------------------
create_instance() {
    step "Creating $FLAVOR ($REGION)"

    init_ssh_key_name

    local images=() cand attempt out id partial
    # candidates: "image id___image name"
    if [ -n "$IMAGE_ID" ]; then
        images=("$IMAGE_ID"___"Ubuntu (image id $IMAGE_ID)")
    else
        while IFS=$'\t' read -r id nm; do
            [ -n "$id" ] && images+=("$id"___"$nm")
        done < <(ubuntu_images "$REGION" "${OS_TYPE:-}")
    fi

    attempt=0
    for cand in "${images[@]}"; do
        attempt=$((attempt + 1))
        id="${cand%%___*}"
        local nm="${cand#*___}"
        out=""
        if [ "$attempt" = 1 ]; then
            info "image:    $nm"
        else
            info "image:    $nm  [fallback]"
        fi

        if [ -n "$DEVICE_LABEL_ARG" ]; then
            DEVICE_LABEL="$DEVICE_LABEL_ARG"
        else
            DEVICE_LABEL="OVHcloud $FLAVOR-${CPU}vCPU-${RAM_GB}GB-RAM-$(sed -e 's/^Baremetal - //' -e 's/ LTS$//' <<<"$nm")"
        fi
        info "label:    $DEVICE_LABEL"

        if [ "$DRY_RUN" = 1 ]; then
            info "[dry-run] ovhcloud cloud instance create $REGION --name $NAME --flavor $FLAVOR_ID --boot-from.image $id --network.public --ssh-key.create.name $SSH_KEY_NAME --ssh-key.create.public-key <$SSH_KEY.pub> --billing-period hourly --wait ${PC[*]} -o json"
            if [ ${#images[@]} -gt 1 ]; then
                info "[dry-run] if the API rejects that image it retries with the next Ubuntu LTS:"
                for cand in "${images[@]:1}"; do info "[dry-run]   ${cand%%___*}  ${cand#*___}"; done
            fi
            exit 0
        fi

        info "waiting for OVHcloud to deliver the server (this takes ~15-30 min)..."
        # create the key inline; if it is already registered, reference it by name
        if try_create "$id" create; then
            out="$LAST_OUT"
        elif grep -qi 'already exist' <<<"$OVH_ERROR"; then
            warn "ssh key '$SSH_KEY_NAME' is already registered - reusing it"
            try_create "$id" name && out="$LAST_OUT"
        fi

        if [ -n "$out" ]; then
            INSTANCE_ID="$(jq -r '.details.id // empty' <<<"$out" 2>/dev/null || true)"
            break
        fi

        # A create that reports failure - or that is interrupted while the
        # instance is being delivered - can still leave a partial instance
        # behind, and that holds the project's RAM quota. Track it so
        # 'destroy' can reach it.
        partial="$(instance_id_by_name "$NAME")"
        if [ -n "$partial" ]; then
            INSTANCE_ID="$partial"
            SSH_USER="ubuntu"
            REMOTE_SUDO=1
            save_state
            warn "the failed create left instance $partial behind; tracking it (./$SCRIPT_NAME destroy removes it)"
        fi

        case "$OVH_ERROR" in
        *"failed to wait"* | *"error fetching operation"* | *"Internal server error"* | *"InternalServerError"*)
            # The request was accepted and the instance exists; only the CLI's
            # "--wait" poll failed (a transient 5xx on the operation endpoint).
            # Continue and watch the instance itself.
            if [ -n "$partial" ]; then
                warn "creation could not be confirmed through the API operation (transient error);"
                warn "watching instance $partial directly instead"
                break
            fi
            die "creating the instance failed: $OVH_ERROR" ;;
        *"quota"*)
            die "not enough project quota for $FLAVOR in $REGION (${OVH_ERROR})
  OVHcloud RAM/cores are counted per project, so a leftover instance blocks
  this one. Look for it with:
    ovhcloud cloud instance list${PROJECT:+ --cloud-project $PROJECT}
  and remove it with:
    ovhcloud cloud instance delete <id>${PROJECT:+ --cloud-project $PROJECT}" ;;
        *"forbidden"* | *"Forbidden"*)
            die "the project refused to create the instance (${OVH_ERROR})
  a project in discovery mode cannot create resources: activate it (add a
  payment method) in the OVHcloud Control Panel first" ;;
        *"stock"* | *"available"*)
            die "$FLAVOR is not available in $REGION (${OVH_ERROR})
  check availability with: ovhcloud cloud instance flavor list -r $REGION" ;;
        *os* | *"image"* | *"install"* | *boot*)
            if [ "$attempt" -lt "${#images[@]}" ]; then
                warn "$nm cannot be used on $FLAVOR: trying the next Ubuntu LTS"
                continue
            fi
            die "no Ubuntu LTS image could be installed on $FLAVOR in $REGION (last error: $OVH_ERROR)" ;;
        *)
            die "creating the instance failed: $OVH_ERROR" ;;
        esac
    done
    [ -n "${out:-}" ] || die "creating the instance failed"

    # the create output sometimes carries no id: fall back to the name lookup
    [ -n "$INSTANCE_ID" ] || INSTANCE_ID="$(instance_id_by_name "$NAME")"
    [ -n "$INSTANCE_ID" ] ||
        die "the instance was created but no id came back - check the OVHcloud console (name: $NAME)"

    SSH_USER="ubuntu"
    REMOTE_SUDO=1
    save_state
    info "instance $INSTANCE_ID created"
}

wait_for_instance() {
    step "Waiting for the instance to be ready"
    local deadline=$((SECONDS + TIMEOUT_MIN * 60)) raw status power ip
    while :; do
        raw="$(ovh cloud instance get "$INSTANCE_ID" "${PC[@]}" 2>/dev/null || true)"
        status="$(jq -r '.resourceStatus // "unknown"' <<<"${raw:-null}" 2>/dev/null || echo unknown)"
        power="$(jq -r '.targetSpec.powerState // "unknown"' <<<"${raw:-null}" 2>/dev/null || echo unknown)"
        ip="$(instance_public_ip "${raw:-null}")"

        if [ "$status" = "ERROR" ]; then
            die "instance $INSTANCE_ID is in state ERROR - check the OVHcloud console"
        fi
        if [ "$power" = "ACTIVE" ] && [ -n "$ip" ]; then
            INSTANCE_IP="$ip"
            break
        fi

        [ "$SECONDS" -lt "$deadline" ] ||
            die "timed out after ${TIMEOUT_MIN}min waiting for $INSTANCE_ID (status: $status, power: $power)"
        printf '  %s status=%s power=%s ip=%s\n' "$(date +%H:%M:%S)" "$status" "$power" "${ip:-none}"
        sleep 15
    done
    save_state
    info "instance is up at $INSTANCE_IP"
}

refresh_instance_ip() {
    local raw
    raw="$(ovh cloud instance get "$INSTANCE_ID" "${PC[@]}" 2>/dev/null || true)"
    [ -n "$raw" ] || die "cannot read instance $INSTANCE_ID${PROJECT:+ in project $PROJECT}"
    local ip
    ip="$(instance_public_ip "$raw")"
    [ -n "$ip" ] || die "instance $INSTANCE_ID has no public IPv4 address"
    if [ "$ip" != "${INSTANCE_IP:-}" ]; then
        INSTANCE_IP="$ip"
        save_state
        info "instance address: $INSTANCE_IP"
    fi
}

wait_for_ssh() {
    step "Waiting for ssh"
    local deadline=$((SECONDS + TIMEOUT_MIN * 60)) user
    while :; do
        for user in ubuntu debian root; do
            if ssh_probe "$user" "$INSTANCE_IP"; then
                SSH_USER="$user"
                [ "$user" = "root" ] && REMOTE_SUDO=0 || REMOTE_SUDO=1
                save_state
                info "ssh works as $user"
                return 0
            fi
        done
        [ "$SECONDS" -lt "$deadline" ] ||
            die "timed out after ${TIMEOUT_MIN}min waiting for ssh on $INSTANCE_IP"
        printf '  %s waiting for ssh...\n' "$(date +%H:%M:%S)"
        sleep 15
    done
}

run_setup() {
    step "Installing docker + gh and cloning the repository"

    local token update_cmd branch_q repo_q
    if [ -z "${GH_TOKEN:-}" ]; then
        GH_TOKEN="$(gh auth token 2>/dev/null)" ||
            die "no GitHub token: set GH_TOKEN or run 'gh auth login' first"
    fi
    token="$(printf '%q' "$GH_TOKEN")"
    branch_q="$(printf '%q' "$BRANCH")"
    repo_q="$(printf '%q' "$REPO_URL")"

    update_cmd="./update_results.sh --device $(printf '%q' "$DEVICE_LABEL")"
    if [ -n "$UPDATE_ARGS" ]; then
        local extra a
        # shellcheck disable=SC2206
        extra=($UPDATE_ARGS)
        for a in "${extra[@]}"; do
            update_cmd+=" $(printf '%q' "$a")"
        done
    fi

    ssh_script "$SSH_USER" "$INSTANCE_IP" <<SETUP
set -euo pipefail
echo "=== benchmark setup: \$(date -u) ==="
export DEBIAN_FRONTEND=noninteractive

if [ "\$(id -u)" != 0 ] && ! sudo -n true 2>/dev/null; then
    echo "!!! user \$(id -un) has no passwordless sudo - cannot install anything" >&2
    exit 1
fi

echo "--- package jobs ---"
# A fresh server runs apt-daily/unattended-upgrades on first boot. They hold
# the dpkg lock now and would also steal CPU/IO during the benchmark, so turn
# the timers off and wait for the run that is already in flight to finish.
sudo systemctl disable apt-daily.timer apt-daily-upgrade.timer >/dev/null 2>&1 || true
sudo systemctl mask unattended-upgrades.service >/dev/null 2>&1 || true
for _ in \$(seq 1 180); do
    pgrep -f 'unattended-upgrade( |\$)|apt\.systemd\.daily' >/dev/null 2>&1 || break
    echo "  waiting for the boot-time package job to finish..."
    sleep 10
done

echo "--- packages ---"
# backstop: wait for the lock instead of failing if something else takes it
echo 'DPkg::Lock::Timeout "1800";' | sudo tee /etc/apt/apt.conf.d/99-related-post-gen-lock >/dev/null
sudo apt-get update -y
sudo apt-get install -y --no-install-recommends ca-certificates curl gnupg git tmux jq unzip

if ! command -v docker >/dev/null 2>&1; then
    echo "--- docker ---"
    curl -fsSL https://get.docker.com | sudo sh
fi
sudo systemctl enable --now docker
sudo docker info >/dev/null

if ! command -v gh >/dev/null 2>&1; then
    echo "--- gh ---"
    sudo mkdir -p -m 755 /etc/apt/keyrings
    curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg \\
        | sudo tee /etc/apt/keyrings/githubcli-archive-keyring.gpg >/dev/null
    sudo chmod go+r /etc/apt/keyrings/githubcli-archive-keyring.gpg
    echo "deb [arch=\$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" \\
        | sudo tee /etc/apt/sources.list.d/github-cli.list >/dev/null
    sudo apt-get update -y
    sudo apt-get install -y gh
fi

echo "--- gh auth ---"
printf '%s\\n' $token | gh auth login --with-token
gh auth setup-git
gh auth status >/dev/null && echo "gh authenticated"

echo "--- repository ---"
sudo mkdir -p $REMOTE_BASE
sudo chown "\$(id -u):\$(id -g)" $REMOTE_BASE
if [ ! -d "$REMOTE_REPO/.git" ]; then
    git clone --branch $branch_q $repo_q "$REMOTE_REPO"
fi
cd "$REMOTE_REPO"
# The clone may predate a branch used by a later campaign (e.g. add-nax after
# an add-fortran run), so refresh refs before checking it out.
git fetch --prune --quiet origin
git checkout $branch_q
if ! git pull --ff-only --quiet; then
    # untracked leftovers in the clone (eg. a script that has since been added to
    # the repo) block a fast-forward; only drop the ones the new revision brings
    echo "--- pull blocked, clearing untracked files added by the new revision ---"
    git fetch --quiet origin $branch_q
    for f in \$(git diff --name-only --diff-filter=A HEAD "origin/$branch_q"); do
        if [ -e "\$f" ] && ! git ls-files --error-unmatch "\$f" >/dev/null 2>&1; then
            echo "  removing untracked \$f"
            rm -f "\$f"
        fi
    done
    git pull --ff-only --quiet
fi

if [ "$LABEL_SYNC" = 1 ] && grep -q '^##### VM Specs' readme.md; then
    sed -i "s|^##### VM Specs.*|##### VM Specs ( $DEVICE_LABEL )|" readme.md
    echo "readme machine label: $DEVICE_LABEL"
fi

cat >$REMOTE_SCRIPT <<'RUNBENCH'
#!/usr/bin/env bash
# started by ovhcloud_bench.sh in the tmux session "bench"
cd $REMOTE_REPO || exit 1
rm -f $REMOTE_DONE
$update_cmd 2>&1 | tee -a $REMOTE_LOG
status=\${PIPESTATUS[0]}
pr="\$(grep -oE 'https://github\\.com/[^ ]+/pull/[0-9]+' $REMOTE_LOG | tail -1 || true)"
{
    echo "status: \$status"
    echo "pull request: \${pr:-none}"
    echo "finished: \$(date -u)"
} >$REMOTE_DONE
echo "benchmark finished with status \$status"
RUNBENCH
chmod +x "$REMOTE_SCRIPT"
bash -n "$REMOTE_SCRIPT" && echo "run-bench.sh ready"
SETUP

    info "setup done"
}

start_tmux() {
    step "Starting the benchmark in tmux"

    # a run is in progress when the session exists but has not written bench.done
    if [ "$FORCE" != "1" ] &&
        ssh_run "$SSH_USER" "$INSTANCE_IP" "tmux has-session -t bench 2>/dev/null && [ ! -f $REMOTE_DONE ]"; then
        warn "a benchmark is already running in tmux session 'bench'"
        warn "watch it with './$SCRIPT_NAME attach', or restart it with --force"
        # not an error: --wait on an already-running benchmark must keep going
        return 0
    fi

    # the pane runs the benchmark inside a shell and stays interactive afterwards,
    # so attaching when it is finished gives you a prompt instead of a dead pane
    ssh_run "$SSH_USER" "$INSTANCE_IP" "
        tmux kill-session -t bench 2>/dev/null || true
        tmux new-session -d -s bench \"bash -lc '$REMOTE_SCRIPT; exec bash'\"
        tmux set-option -t bench remain-on-exit on
        tmux list-sessions
    "
}

wait_for_run() {
    step "Waiting for the benchmark to finish (this can take hours)"
    local deadline=$((SECONDS + 24 * 3600)) done_file last
    while :; do
        done_file="$(ssh_run "$SSH_USER" "$INSTANCE_IP" "cat $REMOTE_DONE 2>/dev/null || true")"
        if [ -n "$done_file" ]; then
            info ""
            info "$done_file"
            return 0
        fi
        [ "$SECONDS" -lt "$deadline" ] || die "still running after 24h, giving up (the tmux session keeps going)"
        last="$(ssh_run "$SSH_USER" "$INSTANCE_IP" "tail -1 $REMOTE_LOG 2>/dev/null || true")"
        printf '  %s %s\n' "$(date +%H:%M:%S)" "${last:-waiting for output...}"
        sleep 60
    done
}

destroy_instance() {
    local id="${INSTANCE_ID:-}"
    load_state
    id="${INSTANCE_ID:-$id}"
    [ -n "$id" ] || die "no instance in $STATE_FILE (delete it in the OVHcloud console)"
    set_project_flag
    step "Deleting $id"
    ovh cloud instance delete "$id" "${PC[@]}" >/dev/null

    # OVHcloud deletes asynchronously and a delete can stick (the instance stays
    # OUT_OF_SYNC and keeps holding the project's RAM quota). Wait for it to
    # really disappear, and keep the local state if it does not, so a later run
    # cannot create a second instance next to the orphan.
    local deadline=$((SECONDS + 300))
    while [ "$SECONDS" -lt "$deadline" ]; do
        if ! ovh cloud instance get "$id" "${PC[@]}" >/dev/null 2>&1; then
            rm -f "$STATE_FILE"
            info "deleted $id"
            return 0
        fi
        printf '  %s waiting for the instance to be deleted...\n' "$(date +%H:%M:%S)"
        sleep 10
    done
    warn "instance $id is still listed after 5min - keeping $STATE_FILE"
    warn "re-run './$SCRIPT_NAME destroy' to retry (it is still billed)"
    return 1
}

# ---------------------------------------------------------------------------
# main
# ---------------------------------------------------------------------------
while [ $# -gt 0 ]; do
    case "$1" in
    run | create | setup | attach | status | logs | wait | destroy | list)
        CMD="$1"
        shift
        ;;
    --flavor)
        FLAVOR="$2"
        shift 2
        ;;
    --region)
        REGION="$2"
        shift 2
        ;;
    --image)
        IMAGE_ID="$2"
        shift 2
        ;;
    --project)
        PROJECT="$2"
        shift 2
        ;;
    --name)
        NAME="$2"
        shift 2
        ;;
    --repo)
        REPO_URL="$2"
        shift 2
        ;;
    --branch)
        BRANCH="$2"
        shift 2
        ;;
    --device)
        DEVICE_LABEL_ARG="$2"
        shift 2
        ;;
    --ssh-key)
        SSH_KEY="$2"
        shift 2
        ;;
    --update-args)
        UPDATE_ARGS="$2"
        shift 2
        ;;
    --no-label-sync)
        LABEL_SYNC=0
        shift
        ;;
    --timeout)
        TIMEOUT_MIN="$2"
        shift 2
        ;;
    --wait)
        WAIT=1
        shift
        ;;
    --destroy-when-done)
        WAIT=1
        DESTROY_WHEN_DONE=1
        shift
        ;;
    --force)
        FORCE=1
        shift
        ;;
    --dry-run)
        DRY_RUN=1
        shift
        ;;
    -h | --help)
        usage
        exit 0
        ;;
    *) die "unknown argument '$1' (try --help)" ;;
    esac
done

need ovhcloud
need jq
need ssh

SCRIPT_NAME="$(basename "$0")"

# the repo defaults to the origin of this checkout so the instance benchmarks
# the same project you are in
if [ -z "$REPO_URL" ]; then
    REPO_URL="$(git -C "$(dirname "$0")" config --get remote.origin.url 2>/dev/null || true)"
    [ -n "$REPO_URL" ] || die "no --repo given and no origin remote in this checkout"
    # the instance has no ssh agent, so benchmark over https
    case "$REPO_URL" in
    git@*:*) REPO_URL="https://$(sed -e 's/^git@//' -e 's/:/\//' <<<"$REPO_URL")" ;;
    ssh://git@*) REPO_URL="https://$(sed -e 's|^ssh://git@||' <<<"$REPO_URL")" ;;
    esac
fi

INSTANCE_ID=""
INSTANCE_IP=""
SSH_USER="ubuntu"
REMOTE_SUDO=1
load_state
FLAVOR="${INSTANCE_FLAVOR:-$FLAVOR}"
REGION="${INSTANCE_REGION:-$REGION}"
PROJECT="${INSTANCE_PROJECT:-$PROJECT}"
[ -n "$DEVICE_LABEL_ARG" ] && DEVICE_LABEL="$DEVICE_LABEL_ARG"

case "$CMD" in
attach)
    ensure_address
    if [ "$REMOTE_SUDO" = "1" ]; then
        exec ssh "${SSH_OPTS[@]}" -t -i "$SSH_KEY" "$SSH_USER@$INSTANCE_IP" 'sudo -n tmux attach -t bench'
    else
        exec ssh "${SSH_OPTS[@]}" -t -i "$SSH_KEY" "$SSH_USER@$INSTANCE_IP" 'tmux attach -t bench'
    fi
    ;;
status)
    ensure_address
    info "instance $INSTANCE_ID ($INSTANCE_IP, $SSH_USER, $INSTANCE_FLAVOR in ${INSTANCE_REGION:-$REGION})  $DEVICE_LABEL"
    out="$(ssh_run "$SSH_USER" "$INSTANCE_IP" "
        echo '--- tmux ---'
        tmux list-sessions 2>/dev/null || echo 'no tmux session'
        echo '--- bench.done ---'
        cat $REMOTE_DONE 2>/dev/null || echo 'still running'
        echo '--- last log lines ---'
        tail -15 $REMOTE_LOG 2>/dev/null || echo 'no log yet'
    " 2>&1)" || {
        warn "cannot reach $INSTANCE_IP over ssh - is the instance still there?"
        info "$out"
        info "check with: ./$SCRIPT_NAME list   (delete leftovers with: ./$SCRIPT_NAME destroy)"
        exit 1
    }
    printf '%s\n' "$out"
    ;;
logs)
    ensure_address
    if [ "$REMOTE_SUDO" = "1" ]; then
        exec ssh "${SSH_OPTS[@]}" -t -i "$SSH_KEY" "$SSH_USER@$INSTANCE_IP" "sudo -n tail -f $REMOTE_LOG"
    else
        exec ssh "${SSH_OPTS[@]}" -t -i "$SSH_KEY" "$SSH_USER@$INSTANCE_IP" "tail -f $REMOTE_LOG"
    fi
    ;;
destroy)
    destroy_instance
    ;;
list)
    resolve_project
    set_project_flag
    step "Metal Instances"
    ovh cloud instance list "${PC[@]}" | jq -r '
        .[]? | "  \(.id)  \(.targetSpec.name // "-")  \((.targetSpec.location.region) // "-")  \(.resourceStatus // "-")  \(.currentState.powerState // .targetSpec.powerState // "-")"'
    ;;
wait)
    ensure_address
    wait_for_run
    ;;
run) ;;
esac

case "$CMD" in
attach | logs | status | wait | destroy | list) exit 0 ;;
esac

if [ -n "${INSTANCE_ID:-}" ] && [ "$FORCE" != "1" ]; then
    step "Reusing instance $INSTANCE_ID"
    set_project_flag
    refresh_instance_ip
    wait_for_ssh
else
    resolve_project
    resolve_flavor_and_image
    # A previous run can leave an instance behind - a delete that stuck, or a
    # state file removed while the instance lived on. Creating a second one
    # trips the project's RAM quota, so adopt the leftover instead.
    leftover="$(instance_id_by_name "$NAME")"
    if [ -n "$leftover" ]; then
        warn "an instance named '$NAME' already exists ($leftover) and is not tracked locally"
        warn "reusing it; if it is stale, delete it and re-run:"
        warn "  ovhcloud cloud instance delete $leftover${PROJECT:+ --cloud-project $PROJECT}"
        INSTANCE_ID="$leftover"
        SSH_USER="ubuntu"
        REMOTE_SUDO=1
        save_state
    else
        create_instance
    fi
    wait_for_instance
    wait_for_ssh
fi

run_setup
start_tmux

step "Benchmark running in tmux"
cat <<EOF

Instance:   $INSTANCE_ID ($INSTANCE_IP, $FLAVOR in $REGION)
Device:     $DEVICE_LABEL
Watch it:   ./$SCRIPT_NAME attach          (tmux attach)
            ./$SCRIPT_NAME status          (one shot)
            ./$SCRIPT_NAME logs            (tail -f)
Wait for it: ./$SCRIPT_NAME --wait
Clean up:   ./$SCRIPT_NAME destroy         (billed hourly until then)
EOF

if [ "$WAIT" = 1 ]; then
    wait_for_run
    if [ "$DESTROY_WHEN_DONE" = 1 ]; then
        destroy_instance
    fi
fi
