fn main() {
    println!("{:?}", ::std::env::args().into_iter().collect::<Vec<_>>());
}
