# Self-contained image: Debian 12 + pinned Zig, no distro Zig package.
# The sources are kept on the 0.17 std.Io API (the 0.15 std.fs/std.time paths
# were removed). Replaces the Arch image that was 5.4GB.
#
# Zig 0.17.0 ships LLVM 22, whose loop vectorizer no longer vectorizes the
# is_top byte reduction (LLVM 20 in 0.15.1 did, and that loop is ~40% of the
# 60k runtime). zig/main.zig therefore writes the reduction with an explicit
# @Vector; keep that comment when this pin moves.
FROM debian:12-slim

# benchmark harness (+ libc headers: run.sh compiles with "zig build-exe -lc")
RUN apt-get update && apt-get install -y --no-install-recommends \
    git python3 python3-pip time which curl ca-certificates xz-utils libc6-dev gcc && \
    rm -rf /var/lib/apt/lists/* && \
    pip3 install --break-system-packages --no-cache-dir faker nanoid

RUN curl -fsSL https://ziglang.org/download/0.17.0/zig-x86_64-linux-0.17.0.tar.xz -o /tmp/zig.tar.xz && \
    mkdir -p /opt/zig && tar -C /opt/zig --strip-components=1 -xf /tmp/zig.tar.xz && rm -f /tmp/zig.tar.xz
ENV PATH="/opt/zig:$PATH"

#precompile
RUN git clone https://github.com/jinyus/related_post_gen.git /tmp/repo && cd /tmp/repo && ./run.sh zig

WORKDIR /app
RUN mkdir -p /results
VOLUME /results
ENV GIT_PAT="" GIT_USER="zupat" GIT_EMAIL="zupat@users.noreply.github.com" \
    GIT_REPO="https://github.com/zupat/related_post_gen.git" GIT_REPO_NAME="related_post_gen" \
    TEST_NAME="all" BRANCH="main" DEVICE="Workflow-VM-2vCPU-7GBram" RUN2="20000" RUN3="60000"
COPY docker_start.sh /docker_start.sh
CMD ["/docker_start.sh"]
