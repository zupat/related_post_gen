# Self-contained image: official Haxe on Debian 12 + hxcpp (native C++ target).
FROM haxe:4.3.7-bookworm

# benchmark harness (git, GNU time, which and python for the generator/verifier)
RUN apt-get update && apt-get install -y --no-install-recommends \
    git python3 python3-pip time which curl wget ca-certificates sudo && \
    rm -rf /var/lib/apt/lists/* && \
    pip3 install --break-system-packages --no-cache-dir faker nanoid

# hxcpp is the C++ backend used by haxe/build.hxml. Pin it: the benchmark is
# built inside the container, so a rolling haxelib release could change the
# generated C++ or its build flags between runs.
RUN haxelib install hxcpp 4.3.2 && haxelib list

# No #precompile block: this image is added before haxe/ exists on the upstream
# main branch, so a clone-and-run at image-build time would fail. The first
# benchmark run compiles the hxcpp runtime and the executable inside the image.

WORKDIR /app
RUN mkdir -p /results
VOLUME /results
ENV GIT_PAT="" GIT_USER="zupat" GIT_EMAIL="zupat@users.noreply.github.com" \
    GIT_REPO="https://github.com/zupat/related_post_gen.git" GIT_REPO_NAME="related_post_gen" \
    TEST_NAME="all" BRANCH="main" DEVICE="Workflow-VM-2vCPU-7GBram" RUN2="20000" RUN3="60000"
COPY docker_start.sh /docker_start.sh
CMD ["/docker_start.sh"]
