define dso_local i32 @phi(i32 noundef %0) local_unnamed_addr #0 {
  %2 = icmp slt i32 %0, 4
  br i1 %2, label %3, label %7

3:                                                ; preds = %21, %1
  %4 = phi i32 [ %0, %1 ], [ %22, %21 ]
  %5 = phi i32 [ %0, %1 ], [ %23, %21 ]
  %6 = icmp sgt i32 %4, 1
  br i1 %6, label %27, label %30

7:                                                ; preds = %1, %21
  %8 = phi i32 [ %24, %21 ], [ 2, %1 ]
  %9 = phi i32 [ %23, %21 ], [ %0, %1 ]
  %10 = phi i32 [ %22, %21 ], [ %0, %1 ]
  %11 = srem i32 %10, %8
  %12 = icmp eq i32 %11, 0
  br i1 %12, label %13, label %21

13:                                               ; preds = %7, %13
  %14 = phi i32 [ %15, %13 ], [ %10, %7 ]
  %15 = sdiv i32 %14, %8
  %16 = srem i32 %15, %8
  %17 = icmp eq i32 %16, 0
  br i1 %17, label %13, label %18

18:                                               ; preds = %13
  %19 = sdiv i32 %9, %8
  %20 = sub nsw i32 %9, %19
  br label %21

21:                                               ; preds = %7, %18
  %22 = phi i32 [ %15, %18 ], [ %10, %7 ]
  %23 = phi i32 [ %20, %18 ], [ %9, %7 ]
  %24 = add nuw nsw i32 %8, 1
  %25 = mul nuw nsw i32 %24, %24
  %26 = icmp sgt i32 %25, %22
  br i1 %26, label %3, label %7

27:                                               ; preds = %3
  %28 = sdiv i32 %5, %4
  %29 = sub nsw i32 %5, %28
  br label %30

30:                                               ; preds = %27, %3
  %31 = phi i32 [ %29, %27 ], [ %5, %3 ]
  ret i32 %31
}

attributes #0 = { nofree norecurse nosync nounwind memory(none) uwtable "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

