# wasmrun

Name is subject to change.

This will eventually be a Wasm interpreter with debugging and inspection
capabilities.

## Current status

wasmrun currently passes Wasm [reference test suite][1], with exception
handling and extended constant expressions proposals.

## Development

To run spec tests, first pull the submodules (`git submodule update --init`),
then run `cargo run --bin spec-test`.

[1]: https://github.com/WebAssembly/testsuite
