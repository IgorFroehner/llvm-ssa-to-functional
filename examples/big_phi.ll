define dso_local noundef zeroext i1 @verify_prime(i32 noundef %0) local_unnamed_addr #0 {
entry:
  %2 = icmp eq i32 %0, 1
  br i1 %2, label %16, label %3

3:
  %4 = icmp slt i32 %0, 4
  br i1 %4, label %16, label %5

5:
  %6 = lshr i32 %0, 1
  %7 = urem i32 %0, %6
  %8 = icmp eq i32 %7, 0
  br i1 %8, label %16, label %9

9:
  %10 = phi i32 [ %13, %12 ], [ %6, %5 ]
  %11 = icmp slt i32 %10, 3
  br i1 %11, label %16, label %12

12:
  %13 = add nsw i32 %10, -1
  %14 = srem i32 %0, %13
  %15 = icmp eq i32 %14, 0
  br i1 %15, label %16, label %9

16:
  %17 = phi i1 [ false, %entry ], [ %4, %3 ], [ %4, %5 ], [ %11, %12 ], [ %11, %9 ]
  ret i1 %17
}