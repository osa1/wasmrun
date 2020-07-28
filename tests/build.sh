#!/bin/bash

set -e
set -x

SDK_PATH=/home/omer/wasi-sdk-11.0

$SDK_PATH/bin/clang-10 --sysroot $SDK_PATH/share/wasi-sysroot --target=wasm32-wasi empty.c -g -o empty.wasm
wasm2wat empty.wasm > empty.wat
