#[no_mangle]
pub fn factorial(num: i32) -> i32 {
    if num <= 0 { return 1 };
    num * factorial(num - 1)
}
