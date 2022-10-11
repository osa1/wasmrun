# wasmrun

Name is subject to change.

This will eventually be a Wasm interpreter with debugging and inspection
capabilities.

## Current status

wasmrun currently passes Wasm [reference test suite][1], with exception
handling proposal.

## Development

To run spec tests, first pull the submodules (`git submodule update --init`),
then run `cargo run --bin spec-test`.

`spec-test` uses `wast2json` from [wabt][2] so make sure `wast2json` is in
`$PATH`.

[1]: https://github.com/WebAssembly/testsuite
[2]: https://github.com/WebAssembly/wabt
