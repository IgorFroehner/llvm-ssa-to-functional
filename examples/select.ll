define i64 @safe_div(i64 %n, i64 %d) {
entry:
  %1 = icmp eq i64 %d, 0
  %2 = udiv i64 %n, %d
  %3 = select i1 %1, i64 -1, i64 %2
  ret i64 %3
}