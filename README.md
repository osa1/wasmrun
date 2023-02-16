# wasmrun

Name is subject to change.

This will eventually be a Wasm interpreter with debugging and inspection
capabilities.

## Current status

wasmrun currently passes Wasm [reference test suite][1], with the following
proposals:

- [Exception handling][2]
- [Extended constant expressions][3]
- [Tail calls][4]
- [Function references][8]

## Development

To run spec tests, first pull the submodules (`git submodule update --init`),
then run `cargo run --bin spec-test`.

## Releated projects

- [wain][5] and [wasmi][7] are Wasm interpreters written in Rust
- [wasm-spec-interpreter][6] provides Rust bindings for the Wasm reference
  interpreter (written in OCaml).

[1]: https://github.com/WebAssembly/testsuite
[2]: https://github.com/WebAssembly/exception-handling
[3]: https://github.com/WebAssembly/extended-const
[4]: https://github.com/WebAssembly/tail-call
[5]: https://github.com/rhysd/wain
[6]: https://github.com/bytecodealliance/wasm-spec-interpreter
[7]: https://github.com/paritytech/wasmi
[8]: https://github.com/WebAssembly/function-references
