define dso_local noundef i32 @main() local_unnamed_addr #0 {
  %1 = call i32 @foo(i32 42)
  ret i32 %1
}

define i32 @foo(i32 %0) {
  ret i32 %0
}
