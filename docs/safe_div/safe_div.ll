define i32 @safe_div(i32 %0, i32 %1) {
2:
  %3 = icmp eq i32 %1, 0
  br i1 %3, label %6, label %4

4:
  %5 = sdiv i32 %0, %1
  br label %6

6:
  %7 = phi i32 [ %5, %4 ], [ -1, %2 ]
  ret i32 %7
}
