# Self-contained image: Debian 12 + Hare 0.26.0.1 built from source.
#
# Hare does not publish binary releases, and Debian has no hare package, so
# this builds the pinned toolchain: QBE (the compiler backend), harec (the
# frontend/compiler) and hare (the build driver and standard library).
FROM debian:12-slim

# benchmark harness (+ C11 toolchain for harec/QBE, scdoc for the man pages)
RUN apt-get update && apt-get install -y --no-install-recommends \
    git python3 python3-pip time which curl ca-certificates \
    build-essential scdoc && \
    rm -rf /var/lib/apt/lists/* && \
    pip3 install --break-system-packages --no-cache-dir faker nanoid

# QBE v1.3. The canonical repository is https://c9x.me/git/qbe.git; this is
# its public GitHub mirror, used because the c9x.me git server is unreliable.
RUN curl -fsSL -o /tmp/qbe.tar.gz \
        https://github.com/omenos/mirror-qbe/archive/refs/tags/v1.3.tar.gz && \
    tar -xzf /tmp/qbe.tar.gz -C /tmp && \
    cd /tmp/mirror-qbe-1.3 && \
    make -j"$(nproc)" PREFIX=/usr/local && \
    make install PREFIX=/usr/local && \
    cd / && rm -rf /tmp/qbe.tar.gz /tmp/mirror-qbe-1.3

# harec 0.26.0 is the compiler frontend paired with Hare 0.26.x.
RUN curl -fsSL -o /tmp/harec.tar.gz \
        https://git.sr.ht/~sircmpwn/harec/archive/0.26.0.tar.gz && \
    tar -xzf /tmp/harec.tar.gz -C /tmp && \
    cd /tmp/harec-0.26.0 && \
    cp configs/linux.mk config.mk && \
    make -j"$(nproc)" && \
    make install && \
    cd / && rm -rf /tmp/harec.tar.gz /tmp/harec-0.26.0

# 0.26.0.1 is the security release for 0.26.0 (CVE-2026-43923).
RUN curl -fsSL -o /tmp/hare.tar.gz \
        https://git.sr.ht/~sircmpwn/hare/archive/0.26.0.1.tar.gz && \
    tar -xzf /tmp/hare.tar.gz -C /tmp && \
    cd /tmp/hare-0.26.0.1 && \
    cp configs/linux.mk config.mk && \
    make -j"$(nproc)" && \
    make install && \
    cd / && rm -rf /tmp/hare.tar.gz /tmp/hare-0.26.0.1
RUN hare version

# No #precompile block: this image is added before hare/ exists on the upstream
# main branch, so a clone-and-run at image-build time would fail.

WORKDIR /app
RUN mkdir -p /results
VOLUME /results
ENV GIT_PAT="" GIT_USER="zupat" GIT_EMAIL="zupat@users.noreply.github.com" \
    GIT_REPO="https://github.com/zupat/related_post_gen.git" GIT_REPO_NAME="related_post_gen" \
    TEST_NAME="all" BRANCH="main" DEVICE="Workflow-VM-2vCPU-7GBram" RUN2="20000" RUN3="60000"
COPY docker_start.sh /docker_start.sh
CMD ["/docker_start.sh"]
