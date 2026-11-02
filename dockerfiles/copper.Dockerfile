# Self-contained image: Debian 12 + Copper 11.1 built from source.
#
# Copper publishes source only (no Linux binaries). It bootstraps itself from a
# C file, then uses its own elf64 backend to build the final compiler. The
# bootstrap C file uses `true`/`false` as identifiers, so newer compilers that
# default to C23 need -std=gnu17; Debian 12's gcc 12 already defaults to gnu17.
FROM debian:12-slim

# benchmark harness (+ C toolchain used by the Copper bootstrap and linker)
RUN apt-get update && apt-get install -y --no-install-recommends \
    git python3 python3-pip time which curl ca-certificates \
    gcc make libc6-dev && \
    rm -rf /var/lib/apt/lists/* && \
    pip3 install --break-system-packages --no-cache-dir faker nanoid

RUN curl -fsSL -o /tmp/copper.tar.gz \
        https://tibleiz.net/download/copper-11.1-src.tar.gz && \
    tar -xzf /tmp/copper.tar.gz -C /tmp && \
    cd /tmp/copper-11.1 && \
    make CC="gcc -std=gnu17" std && \
    make CC="gcc -std=gnu17" boot-elf64 && \
    install -m 755 scripts/copper /usr/local/bin/copper && \
    install -m 755 build/stage3/copper-elf64 /usr/local/bin/copper-elf64 && \
    mkdir -p /usr/local/share/copper/include/std && \
    cp -r lib/std/include /usr/local/share/copper/include/std/include && \
    cp lib/std/index.co /usr/local/share/copper/include/std/ && \
    install -m 644 build/libcopper-std.a /usr/local/lib/ && \
    ldconfig && \
    cd / && rm -rf /tmp/copper.tar.gz /tmp/copper-11.1
RUN copper --help > /dev/null

# No #precompile block: this image is added before copper/ exists on the
# upstream main branch, so a clone-and-run at image-build time would fail.

WORKDIR /app
RUN mkdir -p /results
VOLUME /results
ENV GIT_PAT="" GIT_USER="zupat" GIT_EMAIL="zupat@users.noreply.github.com" \
    GIT_REPO="https://github.com/zupat/related_post_gen.git" GIT_REPO_NAME="related_post_gen" \
    TEST_NAME="all" BRANCH="main" DEVICE="Workflow-VM-2vCPU-7GBram" RUN2="20000" RUN3="60000"
COPY docker_start.sh /docker_start.sh
CMD ["/docker_start.sh"]
