#!/usr/bin/env bash

# Read the first arg passed to the script
first_arg=$1
outfile=$2

# only truncate file when this is empty, otherwise append
appendToFile=$3

if [ -z "$appendToFile" ]; then
    runs=10
    slow_lang_runs=10
else
    runs=3
    slow_lang_runs=2
fi

if [[ -n $outfile ]]; then

    if [ -f "$outfile" ] && [ -z "$appendToFile" ]; then
        truncate -s 0 "$outfile"
    fi

    # this will be used in langauge directories
    # so we need to go up one level
    outfile="../$outfile"
fi

time="$(which gtime 2>/dev/null || which time 2>/dev/null)"
if [[ -z "${time}" ]]; then
    echo 'time or gtime must be available in $PATH'
    exit 1
fi

run_command() {
    local title="$1"
    local num_times="$2"
    local cmd="$3"

    if [ -z "$title" ] || [ -z "$cmd" ] || [ -z "$num_times" ]; then
        echo "Error: Invalid input. Usage: run_command <title> <num_of_times> <command> [args...]"
        return 1
    fi

    # remove the first three args
    shift 3

    # Determine which time command to use
    local TIME_CMD="time"
    if [ "$(uname)" = "Darwin" ]; then
        if command -v gtime >/dev/null 2>&1; then
            TIME_CMD="gtime"
        else
            echo "Error: GNU time (gtime) is required on macOS. Please install it using Homebrew:"
            echo "       brew install gnu-time"
            return 1
        fi
    fi

    # Use awk to indent the output so it shows up as a codeblock in markdown
    if [ -z "$outfile" ]; then
        # outfile is empty, so write to stdout
        echo -e "$title:\n"
        for ((i = 1; i <= num_times; i++)); do
            ($TIME_CMD -f 'total: %es memory: %Mk' $cmd "$@" 2>&1) | awk '{print "\t" $0}'
        done
    else
        # write to a file and stdout
        echo -e "\n$title:\n" | tee -a "$outfile"
        for ((i = 1; i <= num_times; i++)); do
            ($TIME_CMD -f 'total: %es memory: %Mk' $cmd "$@" 2>&1) | awk '{print "\t" $0}' | tee -a "$outfile"
        done
    fi
}

run_go() {
    echo "Running Go" &&
        cd ./go &&
        go build &&
        run_command "Go" $runs ./related &&
        check_output "related_posts_go.json"
}

run_go_concurrent() {
    echo "Running Go Concurrent" &&
        cd ./go_con &&
        go build &&
        run_command "Go Concurrent" $runs ./related_concurrent &&
        check_output "related_posts_go_con.json"
}

run_cpp() {
    echo "Running C++" &&
        cd ./cpp &&
        if [ -z "$appendToFile" ]; then # only build on 5k run
            clang++ -O3 -std=c++20 -I./include main.cpp -o main
        fi &&
        run_command "C++" $runs ./main &&
        check_output "related_posts_cpp.json"
}

run_cpp_con() {
    echo "Running C++ Concurrent" &&
        cd ./cpp &&
        if [ -z "$appendToFile" ]; then # only build on 5k run
            clang++ -std=c++20 -pthread -I./include -O3 main_con.cpp -o main_con
        fi &&
        run_command "C++ Concurrent" $runs ./main_con &&
        check_output "related_posts_cpp_con.json"
}

run_rust() {
    echo "Running Rust" &&
        cd ./rust &&
        cargo build --release &&
        run_command "Rust" $runs ./target/release/rust &&
        check_output "related_posts_rust.json"
}

run_rust_con() {
    echo "Running Rust Rayon" &&
        cd ./rust_con &&
        cargo build --release &&
        run_command "Rust Concurrent" $runs ./target/release/rust_rayon &&
        check_output "related_posts_rust_con.json"

}

run_rust_ho() {
    echo "Running Rust HO" &&
        cd ./rust_ho &&
        cargo build --release &&
        run_command "Rust HO" $runs ./target/release/rust_ho &&
        check_output "related_posts_rust_ho.json"
}

run_python() {
    echo "Running Python" &&
        cd ./python &&
        if [ ! -d "venv" ]; then
            python3 -m venv venv
        fi &&
        source venv/bin/activate &&
        run_command "Python" $slow_lang_runs python3 ./related.py &&
        deactivate &&
        check_output "related_posts_python.json"

}

run_pypy() {
    echo "Running PyPy" &&
        cd ./python &&
        if [ ! -d "venv" ]; then
            python3 -m venv venv
        fi &&
        source venv/bin/activate &&
        run_command "Pypy" $runs pypy3 ./related.py &&
        deactivate &&
        check_output "related_posts_python.json"

}

run_python_np() {
    echo "Running Numpy" &&
        cd ./python &&
        if [ ! -d "venv" ]; then
            python3 -m venv venv
        fi &&
        source venv/bin/activate &&
        (pip freeze | grep scipy) || pip install -r requirements.txt &&
        run_command "Numpy" $slow_lang_runs python3 ./related_np.py &&
        deactivate &&
        check_output "related_posts_python_np.json"

}

run_python_numba() {
    echo "Running Numba" &&
        cd ./python &&
        if [ ! -d "venv" ]; then
            python3 -m venv venv
        fi &&
        source venv/bin/activate &&
        (pip freeze | grep numba) || pip install -r requirements.txt &&
        run_command "Numba" $slow_lang_runs python3 ./related_numba.py &&
        deactivate &&
        check_output "related_posts_python_numba.json"

}

run_python_numba_con() {
    echo "Running Numba Concurrent" &&
        cd ./python &&
        if [ ! -d "venv" ]; then
            python3 -m venv venv
        fi &&
        source venv/bin/activate &&
        (pip freeze | grep numba) || pip install -r requirements.txt &&
        run_command "Numba Concurrent" $slow_lang_runs python3 ./related_numba_con.py &&
        deactivate &&
        check_output "related_posts_python_numba_con.json"

}

run_crystal() {
    echo "Running Crystal" &&
        cd ./crystal &&
        crystal build --release src/crystal.cr &&
        run_command "Crystal" $runs ./crystal &&
        check_output "related_posts_cr.json"
}

run_zig() {
    echo "Running Zig" &&
        cd ./zig &&
        if [ -z "$appendToFile" ]; then # only build on 5k run
            zig build-exe -lc -O ReleaseSafe main.zig
        fi &&
        run_command "Zig" $runs ./main &&
        check_output "related_posts_zig.json"
}

run_zig_fast() {
    echo "Running Zig Fast" &&
        cd ./zig &&
        if [ -z "$appendToFile" ]; then # only build on 5k run
            zig build-exe -lc -O ReleaseFast main.zig
        fi &&
        run_command "Zig Fast" $runs ./main &&
        check_output "related_posts_zig.json"
}


run_zig_con() {
    echo "Running Zig" &&
        cd ./zig_con &&
        if [ -z "$appendToFile" ]; then # only build on 5k run
            zig build-exe -O ReleaseSafe main.zig
        fi &&
        run_command "Zig Concurrent" $runs ./main &&
        check_output "related_posts_zig_con.json"
}


run_zig_con_fast() {
    echo "Running Zig" &&
        cd ./zig_con &&
        if [ -z "$appendToFile" ]; then # only build on 5k run
            zig build-exe -O ReleaseFast main.zig
        fi &&
        run_command "Zig Concurrent" $runs ./main &&
        check_output "related_posts_zig_con.json"
}


run_julia() {
    echo "Running Julia" &&
        cd ./julia &&
        julia -e 'using Pkg; Pkg.activate("Related"); Pkg.instantiate()' &&
        run_command "Julia" $runs julia --startup-file=no --project=Related -e "using Related; main()" &&
        check_output "related_posts_julia.json"
}

run_julia_highly_optimized() {
    echo "Running Julia Highly Optimized" &&
        cd ./julia_highly_optimized &&
        julia -e 'using Pkg; Pkg.activate("RelatedHO"); Pkg.instantiate()' &&
        run_command "Julia HO" $runs julia --startup-file=no --project=RelatedHO -e "using RelatedHO; main()" &&
        check_output "related_posts_julia_highly_optimized.json"
}

run_julia_con() {
    echo "Running Julia Concurrent" &&
        cd ./julia_con &&
        julia -e 'using Pkg; Pkg.activate("RelatedCon"); Pkg.instantiate()' &&
        run_command "Julia Concurrent" $runs julia --threads=auto --startup-file=no --project=RelatedCon -e "using RelatedCon; main()" &&
        check_output "related_posts_julia_con.json"
}

run_odin() {
    echo "Running Odin" &&
        odin build ./odin -o:aggressive -out:./odin/related &&
        cd ./odin &&
        run_command "Odin" $runs ./related &&
        check_output "related_posts_odin.json"
}

run_c3() {
    echo "Running c3" &&
        cd ./c3 &&
        c3c compile related.c3 json.c3 -O3 &&
        run_command "c3" $runs ./related &&
        check_output "related_posts_c3.json"
}

run_vlang() {
    echo "Running Vlang" &&
        cd ./v &&
        v -prod -skip-unused related.v &&
        run_command "Vlang" $runs ./related &&
        check_output "related_posts_v.json"
}

run_jq() {
    # run once as it's very slow. ~50s
    echo "Running jq" &&
        cd ./jq &&
        run_command "JQ" $slow_lang_runs jq -c -f ./related.jq ../posts.json >../related_posts_jq.json &&
        check_output "related_posts_jq.json"

}

run_dart() {
    echo "Running Dart VM" &&
        cd ./dart &&
        run_command "Dart VM" $runs dart related.dart &&
        check_output "related_posts_dart.json"
}

run_dart_aot() {
    echo "Running Dart AOT" &&
        cd ./dart &&
        if [ -z "$appendToFile" ]; then # only build on 5k run
            dart compile exe related.dart -o related
        fi &&
        run_command "Dart AOT" $runs ./related &&
        check_output "related_posts_dart.json"
}

run_swift() {
    echo "Running Swift" &&
        cd ./swift &&
        swift build -c release &&
        run_command "Swift" $runs ./.build/release/related &&
        check_output "related_posts_swift.json"
}

run_swift_con() {
    echo "Running Swift Concurrent" &&
        cd ./swift_con &&
        swift build -c release &&
        run_command "Swift Concurrent" $runs ./.build/release/related &&
        check_output "related_posts_swift_con.json"
}

run_arturo() {
    echo "Running Arturo" &&
        cd ./arturo &&
        run_command "Arturo" $runs arturo related.art &&
        check_output "related_posts_arturo.json"
}

run_js() {
    echo "Running $1" &&
        cd ./js &&
        title=$(echo "$1" | sed 's/\b\(.\)/\u\1/g') &&
        if [ "$1" = "deno" ]; then
            run_command "JS ($title)" $runs deno run --allow-read --allow-write deno.js
        else
            run_command "JS ($title)" $runs $1 $1.js
        fi &&
        check_output "related_posts_$1.json"
}

run_java() {
    VM_OPTIONS="-XX:+UseSerialGC"
    echo "Running Java (JIT)" &&
        cd ./java &&
        java -version &&
        mvn -q -B -Pjvm clean package &&
        run_command "Java (JIT)" $runs java $VM_OPTIONS -jar ./target/main.jar &&
        check_output "related_posts_java.json"
}

# weirdly slower, need to investigate
run_java_con() {
    VM_OPTIONS="-XX:+UseSerialGC"
    echo "Running Java (JIT)" &&
        cd ./java &&
        java -version &&
        mvn -q -B -Pjvm_con clean package &&
        run_command "Java Concurrent (JIT)" $runs java $VM_OPTIONS -jar ./target/main.jar &&
        check_output "related_posts_java_con.json"

}

run_java_graal() {
    export JAVA_HOME="$GRAALVM_HOME"
    echo "Running Java (GraalVM)" &&
        cd ./java &&
        java -version &&
        mvn -q -B clean package &&
        mvn -q -B -Pnative,pgo package &&
        run_command "Java (GraalVM)" $runs ./target/related &&
        check_output "related_posts_java.json"

}

run_java_graal_con() {
    echo "Running Java (GraalVM) Concurrent" &&
        cd ./java &&
        java -version &&
        mvn -q -B -Pnative,pgo,parallel clean package &&
        run_command "Java (GraalVM) Concurrent" $runs ./target/related &&
        check_output "related_posts_java_con.json"

}

run_nim() {
    echo "Running Nim" &&
        cd ./nim &&
        if [ -z "$appendToFile" ]; then # only build on 5k run
            nimble -y install -d &&
                nimble --verbose build -d:release --cc:clang
        fi &&
        run_command "Nim" $runs ./related &&
        check_output "related_posts_nim.json"
}

run_nim_con() {
    echo "Running Nim Concurrent" &&
        cd ./nim_con &&
        if [ -z "$appendToFile" ]; then # only build on 5k run
            nimble -y install -d &&
                ./build.sh clang
        fi &&
        run_command "Nim Concurrent" $runs ./build/related_con &&
        check_output "related_posts_nim_con.json"
}

run_fsharp() {
    echo "Running FSharp (JIT)" &&
        cd ./fsharp/jit &&
        if [ -z "$appendToFile" ]; then # only build on 5k run
            dotnet restore &&
                dotnet publish -c release
        fi &&
        run_command "F# (JIT)" $runs ./bin/release/net10.0/fsharp_jit &&
        cd .. &&
        check_output "related_posts_fsharp_jit.json"
}

run_fsharp_aot() {
    echo "Running FSharp (AOT)" &&
        cd ./fsharp/aot &&
        if [ -z "$appendToFile" ]; then # only build on 5k run
            dotnet restore &&
                dotnet publish -c release --self-contained -p PublishAot=true -o "bin/release/net10.0/aot"
        fi &&
        run_command "F# (AOT)" $runs ./bin/release/net10.0/aot/fsharp_aot &&
        cd .. &&
        check_output "related_posts_fsharp_aot.json"
}

run_fsharp_con() {
    echo "Running FSharp Concurrent" &&
        cd ./fsharp_con &&
        if [ -z "$appendToFile" ]; then # only build on 5k run
            dotnet restore &&
                dotnet publish -c release
        fi &&
        run_command "F# Concurrent" $runs ./bin/release/net10.0/fsharp_con &&
        check_output "related_posts_fsharp_con.json"
}

run_fsharp_con_aot() {
    echo "Running FSharp Concurrent (AOT)" &&
        cd ./fsharp_con &&
        if [ -z "$appendToFile" ]; then # subsequent runs
            dotnet restore &&
                dotnet publish -c release --self-contained -p PublishAot=true -o "bin/release/net10.0/aot"
        fi &&
        run_command "F# Concurrent (AOT)" $runs ./bin/release/net10.0/aot/fsharp_con &&
        check_output "related_posts_fsharp_con.json"
}

run_csharp() {
    echo "Running CSharp (JIT)" &&
        cd ./csharp &&
        if [ -z "$appendToFile" ]; then # subsequent runs
            dotnet publish -c release --self-contained -o "bin/release/net10.0/jit"
        fi &&
        run_command "C# (JIT)" $runs ./bin/release/net10.0/jit/related &&
        check_output "related_posts_csharp.json"
}

run_csharp_aot() {
    echo "Running CSharp (AOT)" &&
        cd ./csharp &&
        if [ -z "$appendToFile" ]; then # subsequent runs
            dotnet publish -c release --self-contained -p PublishAot=true -o "bin/release/net10.0/aot"
        fi &&
        run_command "C# (AOT)" $runs ./bin/release/net10.0/aot/related &&
        check_output "related_posts_csharp.json"
}

run_csharp_con() {
    echo "Running CSharp Concurrent (JIT)" &&
        cd ./csharp_con &&
        if [ -z "$appendToFile" ]; then # subsequent runs
            dotnet publish -c release --self-contained -o "bin/release/net10.0/jit"
        fi &&
        run_command "C# Concurrent (JIT)" $runs ./bin/release/net10.0/jit/related &&
        check_output "related_posts_csharp_con.json"
}

run_csharp_con_aot() {
    echo "Running CSharp Concurrent (AOT)" &&
        cd ./csharp_con &&
        if [ -z "$appendToFile" ]; then # subsequent runs
            dotnet publish -c release --self-contained -p PublishAot=true -o "bin/release/net10.0/aot"
        fi &&
        run_command "C# Concurrent (AOT)" 1 ./bin/release/net10.0/aot/related &&
        check_output "related_posts_csharp_con.json"
}

run_luajit() {
    echo "Running LuaJIT" &&
        cd ./lua &&
        if [ -z "$appendToFile" ]; then # subsequent runs
            sudo luarocks --lua-version 5.1 install luasocket
        fi &&
        run_command "LuaJIT" $runs luajit only_lua.lua &&
        check_output "related_posts_lua.json"

}

run_luajit_jit_off() {
    echo "Running LuaJIT (JIT OFF)" &&
        cd ./lua &&
        if [ -z "$appendToFile" ]; then # subsequent runs
            sudo luarocks --lua-version 5.1 install luasocket
        fi &&
        run_command "LuaJIT (JIT OFF)" $slow_lang_runs luajit -joff only_lua.lua &&
        check_output "related_posts_lua.json"
}

run_lua() {
    echo "Running Lua" &&
        sudo luarocks install luasocket &&
        cd ./lua &&
        run_command "Lua" $slow_lang_runs lua only_lua.lua &&
        check_output "related_posts_lua.json"
}

run_ocaml() {
    echo "Running Ocaml" &&
        cd ./ocaml &&
        if [ -z "$appendToFile" ]; then # only build on 5k run
            opam install . --deps-only -y &&
                opam exec -- dune build
        fi &&
        run_command "Ocaml" $runs ./_build/default/bin/main.exe &&
        check_output "related_posts_ocaml.json"
}

run_haskell() {
    echo "Running Haskell" &&
        cd ./haskell &&
        cabal build &&
        run_command "Haskell" $runs "$(cabal list-bin related-post-gen)" &&
        check_output "related_posts_haskell.json"
}

run_d() {
    echo "Running D" &&
        cd ./d &&
        dub build --build=release &&
        run_command "D" $runs ./related &&
        check_output "related_posts_d.json"
}

run_d_v2() {
    echo "Running D v2" &&
        cd ./d_v2 &&
        dub build --build=release &&
        run_command "D (v2)" $runs ./related &&
        check_output "related_posts_d2.json"
}

run_d_optimized() {
    echo "Running D Highly optimized" &&
        cd ./d_highly_optimized &&
        dub build --build=release &&
        run_command "D HO" $runs ./related &&
        check_output "related_posts_d_optimized.json"
}

run_d_con() {
    echo "Running D Concurrent" &&
        cd ./d_con &&
        dub build --build=release &&
        run_command "D Concurrent" $runs ./related_concurrent &&
        check_output "related_posts_d_con.json"
}

run_d_con_v2() {
    echo "Running D Concurrent (v2)" &&
        cd ./d_con_v2 &&
        dub build --build=release &&
        run_command "D Concurrent (v2)" $runs ./related_concurrent &&
        check_output "related_posts_d_con_v2.json"
}

run_erlang() {
    echo "Running Erlang" &&
        cd ./erlang &&
        rebar3 escriptize &&
        run_command "Erlang" $slow_lang_runs ./_build/default/bin/related_erl &&
        check_output "related_posts_erlang.json"
}

run_common_lisp_sbcl() {
    echo "Running Common Lisp (SBCL)" &&
        cd ./common-lisp &&
        sbcl --control-stack-size 10 --non-interactive --load related.lisp
    run_command "Common Lisp (SBCL)" $slow_lang_runs ./related &&
        check_output "related-cl.json"
}

run_clojure() {
    echo "Running Clojure" &&
        cd ./clojure &&
        lein uberjar &&
        run_command "Clojure" $runs java -jar ./target/related.jar &&
        check_output "related_posts_clj.json"
}

run_ruby() {
    echo "Running ruby" &&
        cd ./ruby &&
        run_command "Ruby" $slow_lang_runs ruby related.rb &&
        check_output "related_posts_ruby.json"
}

run_php() {
    echo "Running PHP" &&
        cd ./php &&
        run_command "PHP" $slow_lang_runs php ./related.php &&
        check_output "related_posts_php.json"
}

run_dascript() {
    echo "Running daScript (interpreted)" &&
        cd ./dascript &&
        run_command "daScript (interpreted)" $slow_lang_runs das related.das &&
        check_output "related_posts_dascript.json"
}

run_racket() {
    echo "Running Racket" &&
        cd ./racket &&
        if [ -z "$appendToFile" ]; then # only build on 5k run
            raco pkg install --auto --name related --no-docs --skip-installed &&
                raco make related.rkt
        fi &&
        run_command "Racket" $runs racket related.rkt &&
        check_output "related_posts_racket.json"
}

run_typed_racket() {
    echo "Running Typed Racket" &&
        cd ./racket &&
        if [ -z "$appendToFile" ]; then # only build on 5k run
            raco pkg install --auto --name related --no-docs --skip-installed &&
                raco make typed/related.rkt
        fi &&
        run_command "Typed Racket" $runs racket typed/related.rkt &&
        check_output "related_posts_typed_racket.json"
}

run_lobster_jit() {
    echo "Running Lobster (JIT)" &&
        cd ./lobster &&
        run_command "Lobster (JIT)" $slow_lang_runs lobster related.lobster &&
        check_output "related_posts_lobster.json"
}

run_lobster_cpp() {
    lobster_bin=$(which lobster)

    if [ -z "$lobster_bin" ]; then
        echo "Error: Lobster binary not found in PATH."
        exit 1
    fi

    lobster_git_dir=$(dirname "$(dirname "$lobster_bin")")

    current_directory=$(pwd)

    echo "Running Lobster (C++ Backend)" &&
        cp lobster "$lobster_git_dir/lobster" -r --force &&
        cd "$lobster_git_dir" &&
        lobster --cpp lobster/related.lobster &&
        cd dev &&
        cmake -DCMAKE_BUILD_TYPE=Release -DLOBSTER_ENGINE=OFF -DLOBSTER_TOCPP=ON && make -j8 &&
        cd "$current_directory" &&
        cd ./lobster &&
        run_command "Lobster (C++)" $runs compiled_lobster &&
        check_output "related_posts_lobster.json"
}

run_scala_native() {
    echo "Running Scala Native" &&
        cd ./scala_native &&
        if [ -z "$appendToFile" ]; then # only build on 5k run
            sbt nativeLink
        fi &&
        scala_version=$(ls -d target/*/ | grep -o 'scala-[^/]*' | head -1) &&
        run_command "Scala Native" $runs ./target/$scala_version/scala_native-out &&
        check_output "related_posts_scala.json"
}

run_r() {
    echo "Running R" &&
        cd ./r &&
        run_command "R" $slow_lang_runs Rscript ./related.R &&
        check_output "related_posts_r.json"
}

run_inko() {
    echo "Running Inko" &&
        cd ./inko &&
        if [ -z "$appendToFile" ]; then # only build on 5k run
            inko build --opt aggressive
        fi &&
        run_command "Inko" $slow_lang_runs ./build/aggressive/main &&
        check_output "related_posts_inko.json"
}

run_neat() {
    echo "Running Neat" &&
        cd ./neat &&
        neat -optimize related.nt &&
        run_command "Neat" $runs ./related &&
        check_output "related_posts_neat.json"
}

check_output() {
    cd ..

    # only check output if we're not appending to a file. ie: 1st round
    if [ -z "$appendToFile" ]; then
        echo "Checking output" &&
            python3 verify.py "$1"
    fi
}

if [ "$first_arg" = "go" ]; then

    run_go

elif [ "$first_arg" = "go_con" ]; then

    run_go_concurrent

elif [ "$first_arg" = "cpp" ]; then

    run_cpp

elif [ "$first_arg" = "cpp_con" ]; then

    run_cpp_con

elif [ "$first_arg" = "rust" ]; then

    run_rust

elif [ "$first_arg" = "rust_con" ]; then

    run_rust_con

elif [ "$first_arg" = "rust_ho" ]; then

    run_rust_ho

elif [ "$first_arg" = "py" ]; then

    run_python

elif [ "$first_arg" = "pypy" ]; then

    run_pypy

elif [ "$first_arg" = "numpy" ]; then

    run_python_np

elif [ "$first_arg" = "numba" ]; then

    run_python_numba

elif [ "$first_arg" = "numba_con" ]; then

    run_python_numba_con

elif [ "$first_arg" = "cr" ]; then

    run_crystal

elif [ "$first_arg" = "zig" ]; then

    run_zig

elif [ "$first_arg" = "zig_fast" ]; then

    run_zig_fast

elif [ "$first_arg" = "zig_con" ]; then

    run_zig_con

elif [ "$first_arg" = "zig_con_fast" ]; then

    run_zig_con_fast

elif [ "$first_arg" = "julia" ]; then

    run_julia

elif [ "$first_arg" = "julia_highly_optimized" ]; then

    run_julia_highly_optimized

elif [ "$first_arg" = "julia_con" ]; then

    run_julia_con

elif [ "$first_arg" = "odin" ]; then

    run_odin

elif [ "$first_arg" = "c3" ]; then

    run_c3

elif [ "$first_arg" = "jq" ]; then

    run_jq

elif [ "$first_arg" = "v" ]; then

    run_vlang

elif [ "$first_arg" = "dart" ]; then

    run_dart

elif [ "$first_arg" = "dart_aot" ]; then

    run_dart_aot

elif [ "$first_arg" = "swift" ]; then

    run_swift

elif [ "$first_arg" = "art" ]; then

    run_arturo

elif [ "$first_arg" = "swift_con" ]; then

    run_swift_con

elif [ "$first_arg" = "node" ]; then

    run_js "node"

elif [ "$first_arg" = "bun" ]; then

    run_js "bun"

elif [ "$first_arg" = "deno" ]; then

    run_js "deno"

elif [ "$first_arg" = "java" ]; then

    run_java

elif [ "$first_arg" = "java_con" ]; then

    run_java_con

elif [ "$first_arg" = "java_graal" ]; then

    run_java_graal

elif [ "$first_arg" = "java_graal_con" ]; then

    run_java_graal_con

elif [ "$first_arg" = "nim" ]; then

    run_nim

elif [ "$first_arg" = "nim_con" ]; then

    run_nim_con

elif [ "$first_arg" = "fsharp" ]; then

    run_fsharp

elif [ "$first_arg" = "fsharp_aot" ]; then

    run_fsharp_aot

elif [ "$first_arg" = "csharp" ]; then

    run_csharp

elif [ "$first_arg" = "csharp_aot" ]; then

    run_csharp_aot

elif [ "$first_arg" = "csharp_con" ]; then

    run_csharp_con

elif [ "$first_arg" = "csharp_con_aot" ]; then

    run_csharp_con_aot

elif [ "$first_arg" = "fsharp_con" ]; then

    run_fsharp_con

elif [ "$first_arg" = "fsharp_con_aot" ]; then

    run_fsharp_con_aot

elif [ "$first_arg" = "luajit" ]; then

    run_luajit

elif [ "$first_arg" = "luajit_off" ]; then

    run_luajit_jit_off

elif [ "$first_arg" = "lua" ]; then

    run_lua

elif [ "$first_arg" = "ocaml" ]; then

    run_ocaml

elif [ "$first_arg" = "haskell" ]; then

    run_haskell

elif [ "$first_arg" = "d" ]; then

    run_d

elif [ "$first_arg" = "d2" ]; then

    run_d_v2

elif [ "$first_arg" = "d_optimized" ]; then

    run_d_optimized

elif [ "$first_arg" = "d_con" ]; then

    run_d_con

elif [ "$first_arg" = "d_con_v2" ]; then

    run_d_con_v2

elif [ "$first_arg" = "erlang" ]; then

    run_erlang

elif [ "$first_arg" = "cl" ]; then

    run_common_lisp_sbcl

elif [ "$first_arg" = "clj" ]; then

    run_clojure

elif [ "$first_arg" = "ruby" ]; then

    run_ruby

elif [ "$first_arg" = "php" ]; then

    run_php

elif [ "$first_arg" = "dascript" ]; then

    run_dascript

elif [ "$first_arg" = "racket" ]; then

    run_racket

elif [ "$first_arg" = "typed_racket" ]; then

    run_typed_racket

elif [ "$first_arg" = "lobster" ]; then

    run_lobster_jit

elif [ "$first_arg" = "lobster_cpp" ]; then

    run_lobster_cpp

elif [ "$first_arg" = "scala_native" ]; then

    run_scala_native

elif [ "$first_arg" = "r" ]; then

    run_r

elif [ "$first_arg" = "inko" ]; then

    run_inko

elif [ "$first_arg" = "neat" ]; then

    run_neat

elif [ "$first_arg" = "all" ]; then

    echo -e "Running all\n" &&
        run_go || echo -e "\n" &&
        run_go_concurrent || echo -e "\n" &&
        run_rust || echo -e "\n" &&
        run_rust_con || echo -e "\n" &&
        run_d || echo -e "\n" &&
        run_d_v2 || echo -e "\n" &&
        run_d_optimized || echo -e "\n" &&
        run_d_con || echo -e "\n" &&
        run_cpp || echo -e "\n" &&
        run_cpp_con || echo -e "\n" &&
        run_python || echo -e "\n" &&
        run_pypy || echo -e "\n" &&
        run_python_np || echo -e "\n" &&
        numba_con || echo -e "\n" &&

        # run_python_numba || echo -e "\n" && break rules but very interesting
        run_crystal || echo -e "\n" &&
        run_zig || echo -e "\n" &&
        run_zig_fast || echo -e "\n" &&
        run_zig_con || echo -e "\n" &&
        run_zig_con_fast || echo -e "\n" &&
        run_julia || echo -e "\n" &&
        run_julia_highly_optimized || echo -e "\n" &&
        run_julia_con || echo -e "\n" &&
        run_odin || echo -e "\n" &&
        run_c3 || echo -e "\n" &&
        run_vlang || echo -e "\n" &&
        run_dart || echo -e "\n" &&
        run_dart_aot || echo -e "\n" &&
        run_swift || echo -e "\n" &&
        run_swift_con || echo -e "\n" &&
        run_js "node" || echo -e "\n" &&
        run_js "bun" || echo -e "\n" &&
        run_js "deno" || echo -e "\n" &&
        run_java || echo -e "\n" &&
        run_java_con || echo -e "\n" &&
        run_java_graal || echo -e "\n" &&
        run_java_graal_con || echo -e "\n" &&
        run_nim || echo -e "\n" &&
        run_nim_con || echo -e "\n" &&
        run_fsharp || echo -e "\n" &&
        run_fsharp_aot || echo -e "\n" &&
        run_fsharp_con || echo -e "\n" &&
        run_fsharp_con_aot || echo -e "\n" &&
        run_csharp || echo -e "\n" &&
        run_csharp_aot || echo -e "\n" &&
        run_csharp_con || echo -e "\n" &&
        run_csharp_con_aot || echo -e "\n" &&
        run_luajit || echo -e "\n" &&
        run_luajit_jit_off || echo -e "\n" &&
        run_lua || echo -e "\n" &&
        run_ocaml || echo -e "\n" &&
        run_erlang || echo -e "\n" &&
        # run_ruby || echo -e "\n" && # too slow
        run_php || echo -e "\n" &&
        # run_dascript || echo -e "\n" && #not installed in docker
        run_racket || echo -e "\n" &&
        run_typed_racket || echo -e "\n" &&
        run_lobster_jit || echo -e "\n" &&
        run_lobster_cpp || echo -e "\n" &&
        run_scala_native || echo -e "\n" &&
        # run_inko || echo -e "\n" &&
        run_neat || echo -e "\n" &&
        run_common_lisp_sbcl || echo -e "\n" &&
        # run_r || echo -e "\n" && # remove for now until docker is setup
        run_haskell || echo -e "\n" &&
        echo -e "Finished running all\n"

elif [ "$first_arg" = "clean" ]; then

    echo "cleaning" &&
        cd go && rm -f related &&
        cd .. &&
        cd go_con && rm -f related_concurrent &&
        cd .. &&
        cd rust && cargo clean &&
        cd .. &&
        cd rust_con && cargo clean &&
        cd .. &&
        cd c3 && rm -f related &&
        cd .. &&
        cd d && rm -f related &&
        cd .. &&
        cd d_con && rm -f related_concurrent &&
        cd .. &&
        cd d_v2 && rm -f related &&
        cd .. &&
        cd d_highly_optimized && rm -f related &&
        cd .. &&
        cd d_con_v2 && rm -f related_concurrent &&
        cd .. &&
        cd erlang && rm -rf _build/ rebar.lock &&
        cd .. &&
        cd python && rm -rf venv/ &&
        cd .. &&
        cd swift && swift package reset &&
        cd .. &&
        cd swift_con && swift package reset &&
        cd .. &&
        cd zig && rm -f main main.o &&
        cd .. &&
        cd zig_con && rm -f main main.o &&
        cd .. &&
        cd java && mvn -q -B clean &&
        cd .. &&
        rm -f related_*.json

else

    echo "Valid args: go | go_con | rust | rust_con | d | d_con | r | py | numpy | erlang | cl | numba | numba_con | cr | zig_fast | zig_con | odin | c3 | jq | julia | julia_highly_optimized | julia_con | v | dart | swift | swift_con | node | bun | deno | java | java_graal | java_graal_con | nim | luajit | lua | fsharp | fsharp_aot | fsharp_con | csharp | csharp_aot | | php | dascript | all | clean. Unknown argument: $first_arg"

fi
