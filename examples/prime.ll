define dso_local noundef zeroext i1 @verify_prime(i32 noundef %0) local_unnamed_addr #0 {
entry:
  %2 = icmp slt i32 %0, 3
  %3 = and i32 %0, 1
  %4 = icmp eq i32 %3, 0
  %5 = or i1 %2, %4
  br i1 %5, label %15, label %6

6:
  %7 = phi i32 [ %8, %10 ], [ 2, %entry ]
  %8 = add nuw nsw i32 %7, 1
  %9 = icmp eq i32 %8, %0
  br i1 %9, label %13, label %10

10:
  %11 = urem i32 %0, %8
  %12 = icmp eq i32 %11, 0
  br i1 %12, label %13, label %6

13:
  %14 = icmp sge i32 %8, %0
  br label %15

15:
  %16 = phi i1 [ %2, %entry ], [ %14, %13 ]
  ret i1 %16
}

attributes #0 = { mustprogress nofree norecurse nosync nounwind willreturn memory(none) uwtable "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nocallback nofree nosync nounwind speculatable willreturn memory(none) }