# Self-contained image: Debian 12 + the Nax compiler.
#
# naxlang/nax is not public yet, so this pins the latest Nature weekly release:
# Nax is the renamed Nature language, with the same compiler, syntax and std.
# The compiler is a musl-linked binary, hence the musl package for its loader.
FROM debian:12-slim

# benchmark harness (+ binutils: the compiler links through ld; gcc/libc6-dev
# keep the usual C toolchain available for its bundled static sysroot)
RUN apt-get update && apt-get install -y --no-install-recommends \
    git python3 python3-pip time which curl ca-certificates \
    musl binutils gcc libc6-dev && \
    rm -rf /var/lib/apt/lists/* && \
    pip3 install --break-system-packages --no-cache-dir faker nanoid

RUN curl -fsSL -o /tmp/nature.tar.gz \
        https://github.com/nature-lang/nature/releases/download/weekly.2026.44/nature-weekly.2026.44-linux-amd64.tar.gz && \
    tar -xzf /tmp/nature.tar.gz -C /tmp && \
    mv /tmp/nature /usr/local/nature && \
    rm -f /tmp/nature.tar.gz
ENV NATURE_ROOT=/usr/local/nature
ENV PATH="/usr/local/nature/bin:$PATH"
RUN nature -v

# No #precompile block: this image is added before nax/ exists on the upstream
# main branch, so a clone-and-run at image-build time would fail.

WORKDIR /app
RUN mkdir -p /results
VOLUME /results
ENV GIT_PAT="" GIT_USER="zupat" GIT_EMAIL="zupat@users.noreply.github.com" \
    GIT_REPO="https://github.com/zupat/related_post_gen.git" GIT_REPO_NAME="related_post_gen" \
    TEST_NAME="all" BRANCH="main" DEVICE="Workflow-VM-2vCPU-7GBram" RUN2="20000" RUN3="60000"
COPY docker_start.sh /docker_start.sh
CMD ["/docker_start.sh"]
