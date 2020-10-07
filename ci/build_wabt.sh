#!/bin/bash

# wabt master has unreleased fixes so we build it ourselves
# See https://github.com/WebAssembly/wabt/issues/1551

set -e
set -x

git clone https://github.com/WebAssembly/wabt --recurse-submodules
mkdir wabt/build
pushd .
cd wabt/build
cmake .. -DCMAKE_BUILD_TYPE=RELEASE
make -j
popd
cp wabt/build/wast2json .
