#!/usr/bin/env bash

# this is a mapping of each test name to the docker image that they should run on
# based on the dockerfiles in ./dockerfiles
declare -A NameToImage=(
    ["go"]="go"
    ["go_con"]="go"
    ["cpp"]="base"
    ["cpp_con"]="base"
    ["rust"]="rust"
    ["rust_con"]="rust"
    ["rust_ho"]="rust"
    ["py"]="base"
    ["numba"]="base"
    ["numba_con"]="base"
    ["pypy"]="pypy"
    ["cr"]="crystal"
    ["zig"]="zig"
    ["zig_con"]="zig"
    ["julia"]="julia"
    ["julia_highly_optimized"]="julia"
    ["julia_con"]="julia"
    ["odin"]="odin"
    ["c3"]="c3"
    ["v"]="v"
    ["dart"]="base"
    ["dart_aot"]="base"
    ["swift"]="swift"
    ["swift_con"]="swift"
    ["node"]="js"
    ["deno"]="js"
    ["bun"]="js"
    ["java"]="java"
    ["java_con"]="java"
    ["java_graal"]="java"
    ["java_graal_con"]="java"
    ["nim"]="nim"
    ["nim_con"]="nim"
    ["fsharp"]="dotnet"
    ["fsharp_aot"]="dotnet"
    ["fsharp_con"]="dotnet"
    ["fsharp_con_aot"]="dotnet"
    ["csharp"]="dotnet"
    ["csharp_aot"]="dotnet"
    ["csharp_con"]="dotnet"
    ["csharp_con_aot"]="dotnet"
    ["luajit"]="lua"
    ["luajit_off"]="lua"
    ["lua"]="lua"
    ["ocaml"]="ocaml"
    ["haskell"]="haskell"
    ["d"]="d"
    ["d2"]="d"
    ["d_optimized"]="d"
    ["d_con"]="d"
    ["d_con_v2"]="d"
    ["erlang"]="erlang"
    ["cl"]="lisp"
    ["clj"]="clojure"
    ["ruby"]="ruby"
    ["php"]="php"
    ["racket"]="racket"
    ["typed_racket"]="racket"
    ["lobster"]="lobster"
    ["scala_native"]="java"
    ["inko"]="inko"
    ["neat"]="neat"
    ["ada"]="ada"

)

TEST_NAME="$1"
shift

run_test() {
    local test_name="$1"
    shift
    local image="${NameToImage[$test_name]}"

    if [[ -z "$image" ]]; then
        echo "Error: No Docker image found for test '$test_name'."
        exit 1
    fi

    echo "Running test '$test_name' using Docker image '${image}_databench'"
    cmd="sudo docker run -it --rm -v ./new_results:/results $@ -e TEST_NAME=\"$test_name\" \"${image}_databench\""
    eval "$cmd"
}

if [[ "$TEST_NAME" == "all" ]]; then

    for key in "${!NameToImage[@]}"; do
        run_test "$key" "$@"
    done

elif [[ -n "${NameToImage[$TEST_NAME]}" ]]; then

    run_test "$TEST_NAME" "$@"

else
    echo "Error: '$TEST_NAME' is not a valid test name."
    exit 1
fi

echo "Other arguments: $@"
