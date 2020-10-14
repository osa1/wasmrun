// wasm-preopen test.txt ../test.txt

fn main() {
    let contents = std::fs::read_to_string("test.txt").unwrap();
    println!("Hello, {}!", contents);
}
