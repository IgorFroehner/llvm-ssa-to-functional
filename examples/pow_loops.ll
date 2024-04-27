define i32 @pow(i32 %x, i32 %y) {
start:
  br label %loop_start

loop_start:
  %i.0 = phi i32 [0, %start], [%i.new, %loop]
  %r.0 = phi i32 [1, %start], [%r.new, %loop]
  %done = icmp eq i32 %i.0, %y
  br i1 %done, label %exit, label %loop

loop:
  %r.new = mul i32 %r.0, %x
  %i.new = add i32 %i.0, 1
  %done = icmp eq i32 %i.new, %y
  br i1 %done, label %exit, label %loop

exit:
  ret i32 %r.0
}