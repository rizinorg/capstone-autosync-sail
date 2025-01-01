#!/bin/sh

if [ $# -eq 0 ]; then
  echo "No sail model path given, must be given."
  exit
elif [ $# -eq 1 ]; then
  SAIL_RISCV_ROOT=$1
  echo "Path to sail model given as $1"
else
  echo "Unrecognized arguments. Usage: riscv-ls [path to riscv sail model repo]"
  exit
fi

MKFILE="$SAIL_RISCV_ROOT/Makefile"

# add instrumentation to the makefile in order to be able to print any variables
echo 'print-% : ; @echo $* = $($*)' >> "$MKFILE"

TOOL_ROOT=$PWD
(
cd $SAIL_RISCV_ROOT
make print-SAIL_SRCS | xargs -n 1 | grep ".sail" | xargs -I {} echo "$SAIL_RISCV_ROOT"{} > "$TOOL_ROOT/conf/sail-files-paths.txt"
)

# remove instrumentation from the makefile again to avoid version control headache
sed -i '$d' $MKFILE 