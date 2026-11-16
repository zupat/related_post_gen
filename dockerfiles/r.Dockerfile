# r2u provides Ubuntu CRAN binaries and bspm-backed install.packages().
FROM rocker/r2u:24.04

RUN apt-get update && apt-get install -y --no-install-recommends \
    git python3-pip time which curl ca-certificates && \
    rm -rf /var/lib/apt/lists/* && \
    pip3 install --break-system-packages --no-cache-dir faker nanoid

RUN Rscript -e 'install.packages(c("jsonlite", "fastmap", "yyjsonr", "here", "rlang"), quiet = TRUE)'

WORKDIR /app
RUN mkdir -p /results
VOLUME /results
ENV GIT_PAT="" GIT_USER="zupat" GIT_EMAIL="zupat@users.noreply.github.com" \
    GIT_REPO="https://github.com/zupat/related_post_gen.git" GIT_REPO_NAME="related_post_gen" \
    TEST_NAME="all" BRANCH="main" DEVICE="Workflow-VM-2vCPU-7GBram" RUN2="20000" RUN3="60000"
COPY docker_start.sh /docker_start.sh
CMD ["/docker_start.sh"]
