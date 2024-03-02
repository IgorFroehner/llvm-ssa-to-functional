define i32 @square(i32 %x) {
  %1 = mul i32 %x, %x
  ret i32 %1
}

define i64 @no_overflow_square(i32 %x) {
  %1 = sext i32 %x to i64
  %2 = mul i64 %1, %1
  ret i64 %2
}