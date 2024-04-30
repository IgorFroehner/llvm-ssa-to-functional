define i64 @safe_div(i64 %n, i64 %d) {
entry:
  %1 = icmp eq i64 %d, 0
  br i1 %1, label %iszero, label %nonzero

iszero:
  ret i64 -1

nonzero:
  %2 = udiv i64 %n, %d
  ret i64 %2
}