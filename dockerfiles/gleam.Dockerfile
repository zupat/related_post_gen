# Self-contained image: Debian 12 + Erlang/OTP + Gleam 0.32.4.
# Debian's Erlang is OTP 25, which matches the Erlang benchmark image and
# provides the mutable `erts_internal:counters` used by the Gleam hot loop.
FROM debian:12-slim

# benchmark harness (+ erlang/rebar3: compile and run the BEAM application)
RUN apt-get update && apt-get install -y --no-install-recommends \
    git python3 python3-pip time which curl ca-certificates \
    erlang rebar3 && \
    rm -rf /var/lib/apt/lists/* && \
    pip3 install --break-system-packages --no-cache-dir faker nanoid

# Pin the compiler: the implementation is written against Gleam 0.32.
RUN curl -fsSL -o /tmp/gleam.tar.gz \
        https://github.com/gleam-lang/gleam/releases/download/v0.32.4/gleam-v0.32.4-x86_64-unknown-linux-musl.tar.gz && \
    tar -xzf /tmp/gleam.tar.gz -C /tmp && \
    mv /tmp/gleam /usr/local/bin/gleam && \
    chmod +x /usr/local/bin/gleam && \
    rm -f /tmp/gleam.tar.gz
RUN gleam --version

# No #precompile block: this image is added before gleam/ exists on the
# upstream main branch, so a clone-and-run at image-build time would fail.

WORKDIR /app
RUN mkdir -p /results
VOLUME /results
ENV GIT_PAT="" GIT_USER="zupat" GIT_EMAIL="zupat@users.noreply.github.com" \
    GIT_REPO="https://github.com/zupat/related_post_gen.git" GIT_REPO_NAME="related_post_gen" \
    TEST_NAME="all" BRANCH="main" DEVICE="Workflow-VM-2vCPU-7GBram" RUN2="20000" RUN3="60000"
COPY docker_start.sh /docker_start.sh
CMD ["/docker_start.sh"]
