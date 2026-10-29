# Self-contained image: Eclipse Temurin JDK 21 + the Flix compiler.
#
# Flix is a JVM language. The project is compiled to a jar once per campaign and
# every timed run is a plain `java -jar`, so compiler time is outside the
# measured section (the first build happens before run_command).
FROM eclipse-temurin:21-jdk-jammy

# benchmark harness
RUN apt-get update && apt-get install -y --no-install-recommends \
    git python3 python3-pip time which curl ca-certificates && \
    rm -rf /var/lib/apt/lists/* && \
    pip3 install --no-cache-dir faker nanoid

# Pin the compiler: 0.79.0 is the version the implementation is written against.
RUN curl -fsSL -o /opt/flix.jar \
        https://github.com/flix/flix/releases/download/v0.79.0/flix.jar && \
    java -jar /opt/flix.jar --version

# No #precompile block: this image is added before flix/ exists on the upstream
# main branch, so a clone-and-run at image-build time would fail.

WORKDIR /app
RUN mkdir -p /results
VOLUME /results
ENV GIT_PAT="" GIT_USER="zupat" GIT_EMAIL="zupat@users.noreply.github.com" \
    GIT_REPO="https://github.com/zupat/related_post_gen.git" GIT_REPO_NAME="related_post_gen" \
    TEST_NAME="all" BRANCH="main" DEVICE="Workflow-VM-2vCPU-7GBram" RUN2="20000" RUN3="60000"
COPY docker_start.sh /docker_start.sh
CMD ["/docker_start.sh"]
