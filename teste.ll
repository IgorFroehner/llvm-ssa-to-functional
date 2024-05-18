define i32 @fib(i32 %0) {
1:
  %2 = icmp sgt i32 %0, 99
  br i1 %2, label %12, label %3

3:
  %4 = icmp sgt i32 %0, 0
  br i1 %4, label %5, label %12

5:
  %6 = phi i32 [ %8, %5 ], [ 0, %3 ]
  %7 = phi i32 [ %10, %5 ], [ 0, %3 ]
  %8 = phi i32 [ %9, %5 ], [ 1, %3 ]
  %9 = add nsw i32 %6, %8
  %10 = add nuw nsw i32 %7, 1
  %11 = icmp eq i32 %10, %0
  br i1 %11, label %12, label %5

12:
  %13 = phi i32 [ -1, %1 ], [ 0, %3 ], [ %8, %5 ]
  ret i32 %13
}