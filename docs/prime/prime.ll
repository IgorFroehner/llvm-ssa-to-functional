define i32 @is_prime(i32 %0) {
1:
  %2 = icmp slt i32 %0, 2
  br i1 %2, label %19, label %3

3:
  %4 = icmp slt i32 %0, 4
  %5 = and i32 %0, 1
  %6 = icmp eq i32 %5, 0
  %7 = or i1 %4, %6
  br i1 %7, label %16, label %8

8:
  %9 = phi i32 [ %10, %13 ], [ 2, %3 ]
  %10 = add nuw nsw i32 %9, 1
  %11 = mul nsw i32 %10, %10
  %12 = icmp sgt i32 %11, %0
  br i1 %12, label %16, label %13

13:
  %14 = srem i32 %0, %10
  %15 = icmp eq i32 %14, 0
  br i1 %15, label %16, label %8

16:
  %17 = phi i1 [ %4, %3 ], [ %12, %8 ], [ %12, %13 ]
  %18 = zext i1 %17 to i32
  br label %19

19:
  %20 = phi i32 [ 0, %1 ], [ %18, %16 ]
  ret i32 %20
}
