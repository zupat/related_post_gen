# Self-contained image: Debian 12 + the pinned MoonBit toolchain.
#
# MoonBit publishes rolling channels (latest/nightly/dev) rather than versioned
# URLs, so the toolchain is pinned through the community moonbit-overlay
# release mirrors, which repackage the exact same archives under a version tag.
# Both archives are checked against their sha256 before use.
#
# The toolchain archive carries the compiler and runtime libraries but not the
# core library, which ships separately and must be bundled after unpacking
# (mirrors the upstream install/unix.sh steps).
FROM debian:12-slim

# benchmark harness (+ gcc/libc headers, which the native backend invokes as
# the C compiler for the moonc-generated sources)
RUN apt-get update && apt-get install -y --no-install-recommends \
    git python3 python3-pip time which curl ca-certificates build-essential && \
    rm -rf /var/lib/apt/lists/* && \
    pip3 install --break-system-packages --no-cache-dir faker nanoid

ARG MOONBIT_VERSION=v0.10.19+bee578715+ea07287
ARG MOONBIT_TOOLCHAIN_SHA256=dd1d6d793c9675405ef32b39bce66d40041e2052a9c3f2f403e0a9da98bd94fd
ARG MOONBIT_CORE_SHA256=b5b33afc759b8ae332346f357f42235d4c8d54afd91bb891b22ead3d746fa74f

ENV MOON_HOME=/opt/moon
ENV PATH=/opt/moon/bin:$PATH

RUN set -eux; \
    base="https://github.com/moonbit-community/moonbit-overlay/releases/download/v0.10.19%2Bbee578715%2Bea07287"; \
    curl -fsSL -o /tmp/moonbit.tar.gz "$base/moonbit-linux-x86_64.tar.gz"; \
    echo "$MOONBIT_TOOLCHAIN_SHA256  /tmp/moonbit.tar.gz" | sha256sum -c -; \
    mkdir -p "$MOON_HOME/lib"; \
    tar -xzf /tmp/moonbit.tar.gz -C "$MOON_HOME"; \
    chmod +x "$MOON_HOME"/bin/* "$MOON_HOME"/bin/internal/tcc; \
    curl -fsSL -o /tmp/moonbit-core.tar.gz "$base/moonbit-core.tar.gz"; \
    echo "$MOONBIT_CORE_SHA256  /tmp/moonbit-core.tar.gz" | sha256sum -c -; \
    tar -xzf /tmp/moonbit-core.tar.gz -C "$MOON_HOME/lib"; \
    rm -f /tmp/moonbit.tar.gz /tmp/moonbit-core.tar.gz; \
    moon -C "$MOON_HOME/lib/core" bundle --warn-list -a --all; \
    moon -C "$MOON_HOME/lib/core" bundle --warn-list -a --target wasm-gc --quiet
RUN moon version --all

# Warm the mooncakes registry and the moonbitlang/x dependency so a clone at
# run time can build without fetching packages (the registry index is large).
RUN mkdir -p /tmp/moonwarm && cd /tmp/moonwarm && \
    moon new warm >/dev/null 2>&1 && cd warm && \
    moon add moonbitlang/x >/dev/null && \
    cd / && rm -rf /tmp/moonwarm

# No #precompile block: this image is added before moonbit/ exists on the
# upstream main branch, so a clone-and-run at image-build time would fail.

WORKDIR /app
RUN mkdir -p /results
VOLUME /results
ENV GIT_PAT="" GIT_USER="zupat" GIT_EMAIL="zupat@users.noreply.github.com" \
    GIT_REPO="https://github.com/zupat/related_post_gen.git" GIT_REPO_NAME="related_post_gen" \
    TEST_NAME="all" BRANCH="main" DEVICE="Workflow-VM-2vCPU-7GBram" RUN2="20000" RUN3="60000"
COPY docker_start.sh /docker_start.sh
CMD ["/docker_start.sh"]
