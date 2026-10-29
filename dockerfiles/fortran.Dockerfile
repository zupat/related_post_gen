# Self-contained image: Debian 12 + gfortran + pinned json-fortran 9.3.1.
# json-fortran is built once here; run.sh only compiles the benchmark source
# against the installed static library.
FROM debian:12-slim

# benchmark harness (+ gfortran/cmake/make: compile the parser and the test)
RUN apt-get update && apt-get install -y --no-install-recommends \
    git python3 python3-pip time which curl ca-certificates \
    gfortran cmake make && \
    rm -rf /var/lib/apt/lists/* && \
    pip3 install --break-system-packages --no-cache-dir faker nanoid

RUN git clone --depth 1 --branch 9.3.1 https://github.com/jacobwilliams/json-fortran.git /tmp/json-fortran && \
    cmake -S /tmp/json-fortran -B /tmp/json-fortran/build \
        -DCMAKE_BUILD_TYPE=Release \
        -DCMAKE_INSTALL_PREFIX=/usr/local \
        -DJSONFORTRAN_ENABLE_TESTS=OFF \
        -DJSONFORTRAN_STATIC_LIBRARY_ONLY=ON \
        -DUSE_GNU_INSTALL_CONVENTION=ON \
        -DCMAKE_INSTALL_LIBDIR=lib \
        -DCMAKE_INSTALL_INCLUDEDIR=include && \
    cmake --build /tmp/json-fortran/build --parallel 2 && \
    cmake --install /tmp/json-fortran/build && \
    rm -rf /tmp/json-fortran
RUN gfortran --version | head -1

#precompile
RUN git clone https://github.com/jinyus/related_post_gen.git /tmp/repo && cd /tmp/repo && ./run.sh fortran

WORKDIR /app
RUN mkdir -p /results
VOLUME /results
ENV GIT_PAT="" GIT_USER="zupat" GIT_EMAIL="zupat@users.noreply.github.com" \
    GIT_REPO="https://github.com/zupat/related_post_gen.git" GIT_REPO_NAME="related_post_gen" \
    TEST_NAME="all" BRANCH="main" DEVICE="Workflow-VM-2vCPU-7GBram" RUN2="20000" RUN3="60000"
COPY docker_start.sh /docker_start.sh
CMD ["/docker_start.sh"]
