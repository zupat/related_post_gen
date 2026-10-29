# Self-contained image: Node 26 on Debian 12 + pinned Perry (TypeScript -> native).
# Perry compiles through SWC + LLVM but links the executable with the system C
# toolchain, so gcc and the libc headers have to stay in the image.
FROM node:26.12.0-bookworm-slim

# benchmark harness (+ build-essential/clang: Perry links with the system C
# toolchain and its LLVM backend uses clang as the assembler for statepoints)
RUN apt-get update && apt-get install -y --no-install-recommends \
    git python3 python3-pip time which curl ca-certificates build-essential clang && \
    rm -rf /var/lib/apt/lists/* && \
    pip3 install --break-system-packages --no-cache-dir faker nanoid

RUN npm install -g @perryts/perry@0.5.1520
RUN perry --version

#precompile
RUN git clone https://github.com/jinyus/related_post_gen.git /tmp/repo && cd /tmp/repo && ./run.sh perry

WORKDIR /app
RUN mkdir -p /results
VOLUME /results
ENV GIT_PAT="" GIT_USER="zupat" GIT_EMAIL="zupat@users.noreply.github.com" \
    GIT_REPO="https://github.com/zupat/related_post_gen.git" GIT_REPO_NAME="related_post_gen" \
    TEST_NAME="all" BRANCH="main" DEVICE="Workflow-VM-2vCPU-7GBram" RUN2="20000" RUN3="60000"
COPY docker_start.sh /docker_start.sh
CMD ["/docker_start.sh"]
