define dso_local noundef i32 @fib(i32 noundef %0) local_unnamed_addr #0 {
  %2 = icmp eq i32 %0, 0
  br i1 %2, label %10, label %3

3:
  %4 = phi i32 [ %7, %3 ], [ %0, %1 ]
  %5 = phi i32 [ %8, %3 ], [ 1, %1 ]
  %6 = phi i32 [ %5, %3 ], [ 0, %1 ]
  %7 = add nsw i32 %4, -1
  %8 = add nsw i32 %5, %6
  tail call void @test()
  %9 = icmp eq i32 %7, 0
  br i1 %9, label %10, label %3

10:
  %11 = phi i32 [ 1, %1 ], [ %8, %3 ]
  ret i32 %11
}

declare void @test() local_unnamed_addr #1