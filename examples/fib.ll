define dso_local i32 @fib(i32 noundef %0) local_unnamed_addr #0 {
  %2 = icmp eq i32 %0, 0
  br i1 %2, label %10, label %3

3:                                                ; preds = %1, %3
  %4 = phi i32 [ %8, %3 ], [ %0, %1 ]
  %5 = phi i32 [ %6, %3 ], [ 0, %1 ]
  %6 = phi i32 [ %7, %3 ], [ 1, %1 ]
  %7 = add nsw i32 %5, %6
  %8 = add nsw i32 %4, -1
  %9 = icmp eq i32 %8, 0
  br i1 %9, label %10, label %3

10:                                               ; preds = %3, %1
  %11 = phi i32 [ 1, %1 ], [ %7, %3 ]
  ret i32 %11
}

attributes #0 = { nofree norecurse nosync nounwind memory(none) uwtable "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

