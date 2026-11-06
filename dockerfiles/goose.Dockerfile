# Self-contained image: Debian 12 + the pinned Goose v2026.3 Linux release.
#
# Goose has no packaged Linux toolchain and building the compiler from source
# needs CMake 3.27+ (Debian 12 ships 3.25), so the official release archive is
# used. It contains the compiler, its C runtime support and the standard
# library; `goose build` still needs a C compiler on the machine.
FROM debian:12-slim

# benchmark harness (+ gcc/libc headers for `goose build`)
RUN apt-get update && apt-get install -y --no-install-recommends \
    git python3 python3-pip time which curl ca-certificates build-essential && \
    rm -rf /var/lib/apt/lists/* && \
    pip3 install --break-system-packages --no-cache-dir faker nanoid

RUN curl -fsSL -o /tmp/goose.tar.gz \
        https://github.com/aardappel/goose/releases/download/v2026.3/goose-v2026.3-linux-x64.tar.gz && \
    tar -xzf /tmp/goose.tar.gz -C /opt && \
    ln -s /opt/goose-v2026.3-linux-x64/goose /usr/local/bin/goose && \
    rm /tmp/goose.tar.gz
RUN goose version

# The compiler resolves the standard library next to its own binary; pin it
# explicitly so the symlink on PATH cannot change that.
ENV GOOSE_STDLIB=/opt/goose-v2026.3-linux-x64/stdlib

# No #precompile block: this image is added before goose/ exists on the upstream
# main branch, so a clone-and-run at image-build time would fail.

WORKDIR /app
RUN mkdir -p /results
VOLUME /results
ENV GIT_PAT="" GIT_USER="zupat" GIT_EMAIL="zupat@users.noreply.github.com" \
    GIT_REPO="https://github.com/zupat/related_post_gen.git" GIT_REPO_NAME="related_post_gen" \
    TEST_NAME="all" BRANCH="main" DEVICE="Workflow-VM-2vCPU-7GBram" RUN2="20000" RUN3="60000"
COPY docker_start.sh /docker_start.sh
CMD ["/docker_start.sh"]
