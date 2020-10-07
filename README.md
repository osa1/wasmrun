# wasmrun

Name is subject to change.

This will eventually be a Wasm interpreter with debugging and inspection
capabilities.

## Current status

This currently passes [WebAssembly test suite][3]. To run the tests, first pull
the submodule (`git submodule update --init`), then run `cargo run --bin
spec-test`.

I'm currently implementing [WASI][4] support.

## Running tests

To run [spec tests][1] we use [wast2json from wabt][2] so make sure `wasm2json`
in in `$PATH`.

[1]: https://github.com/WebAssembly/spec
[2]: https://github.com/WebAssembly/wabt
[3]: https://github.com/WebAssembly/testsuite
[4]: https://wasi.dev/
