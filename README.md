# wasmrun

Name is subject to change.

This will eventually be a Wasm interpreter with debugging and inspection
capabilities.

## Current status

wasmrun currently passes [WebAssembly test suite][1]. To run the tests, first
pull the submodule (`git submodule update --init`), then run `cargo run --bin
spec-test`.

We use [wast2json from wabt][2] in tests so make sure `wasm2json` is in `$PATH`.

I'm currently implementing [WASI][3] support.

[1]: https://github.com/WebAssembly/testsuite
[2]: https://github.com/WebAssembly/wabt
[3]: https://wasi.dev/
