# Self-contained image: Node 26 on Debian 12 + pinned scriptc (TypeScript -> native).
# scriptc lowers typed TypeScript to C/LLVM IR and links it with clang, so the
# C toolchain (and libc headers) must stay in the image.
FROM node:26.12.0-bookworm-slim

# benchmark harness (+ clang/lld: scriptc emits C and invokes the system linker)
RUN apt-get update && apt-get install -y --no-install-recommends \
    git python3 python3-pip time which curl ca-certificates clang lld build-essential && \
    rm -rf /var/lib/apt/lists/* && \
    pip3 install --break-system-packages --no-cache-dir faker nanoid

RUN npm install -g scriptc@0.4.5
RUN scriptc --version

#precompile
RUN git clone https://github.com/jinyus/related_post_gen.git /tmp/repo && cd /tmp/repo && ./run.sh scriptc

WORKDIR /app
RUN mkdir -p /results
VOLUME /results
ENV GIT_PAT="" GIT_USER="zupat" GIT_EMAIL="zupat@users.noreply.github.com" \
    GIT_REPO="https://github.com/zupat/related_post_gen.git" GIT_REPO_NAME="related_post_gen" \
    TEST_NAME="all" BRANCH="main" DEVICE="Workflow-VM-2vCPU-7GBram" RUN2="20000" RUN3="60000"
COPY docker_start.sh /docker_start.sh
CMD ["/docker_start.sh"]
