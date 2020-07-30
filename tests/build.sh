#!/bin/bash

set -e
set -x

SDK_PATH=/home/omer/wasi-sdk-11.0

C_FILES="empty.c return_1.c"

for c_file in $C_FILES; do
    fname="$(basename "$c_file" .c)"
    $SDK_PATH/bin/clang-10 --sysroot $SDK_PATH/share/wasi-sysroot --target=wasm32-wasi $c_file -g -o ${fname}.wasm
    wasm2wat ${fname}.wasm > ${fname}.wat
done
