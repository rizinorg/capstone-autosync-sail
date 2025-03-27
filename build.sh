if [ -z "$1" ]; then
	echo "$0 <sail-riscv git tag>"
	exit 1
fi

eval $(opam env)

SAIL_RISCV_GIT_TAG="$1"

git -C sail-riscv checkout "$SAIL_RISCV_GIT_TAG"
SAIL_RISCV_GIT_HASH=$(git -C sail-riscv/ rev-parse --short 0.7)

dune build --profile release
./_build/default/bin/main.exe -f "conf/sail-riscv-$SAIL_RISCV_GIT_TAG.txt" -o "generated_output" -c "$SAIL_RISCV_GIT_HASH"

echo "clang-format generated sources"
find 'generated_output' -type f | xargs -I % /opt/clang-format-static/clang-format-16 --verbose -i %
