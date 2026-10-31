# Self-contained image: Debian + pinned Mojo 1.2.0 from PyPI.
# Mojo's syntax and standard library still move quickly; 1.2.0 is the release
# this implementation was built and verified against.
FROM python:3.12-slim

# benchmark harness (+ gcc/binutils: `mojo build` links the generated object
# against libc and the Mojo runtime libraries bundled in the wheel)
RUN apt-get update && apt-get install -y --no-install-recommends \
    git time which curl ca-certificates \
    gcc g++ libc6-dev binutils && \
    rm -rf /var/lib/apt/lists/* && \
    pip3 install --no-cache-dir faker nanoid mojo==1.2.0
RUN mojo --version

# No #precompile block: this image is added before mojo/ exists on the upstream
# main branch, so a clone-and-run at image-build time would fail.

WORKDIR /app
RUN mkdir -p /results
VOLUME /results
ENV GIT_PAT="" GIT_USER="zupat" GIT_EMAIL="zupat@users.noreply.github.com" \
    GIT_REPO="https://github.com/zupat/related_post_gen.git" GIT_REPO_NAME="related_post_gen" \
    TEST_NAME="all" BRANCH="main" DEVICE="Workflow-VM-2vCPU-7GBram" RUN2="20000" RUN3="60000"
COPY docker_start.sh /docker_start.sh
CMD ["/docker_start.sh"]
