# wasmrun

Name is subject to change.

This will eventually be a Wasm interpreter with debugging and inspection
capabilities.

## Current status

I'm currently working on supporting the changes/extensions made to the Wasm
spec in the last ~10 months.

## Development

To run spec tests, first pull the submodule (`git submodule update --init`),
then run `cargo run --bin spec-test`.

We use [wast2json from wabt][2] in tests so make sure `wasm2json` is in
`$PATH`.

[1]: https://github.com/WebAssembly/testsuite
[2]: https://github.com/WebAssembly/wabt
[3]: https://wasi.dev/
