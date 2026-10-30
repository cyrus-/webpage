fn main() {
    let s = String::from("hello");
    takes_ownership(s);
    let mut x = 5;
    let y = x;
    x = 6
}