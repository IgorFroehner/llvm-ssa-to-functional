define dso_local noundef i32 @factorial(i32 noundef %0) local_unnamed_addr #0 {
entry:
  br label %2

2:
  %3 = phi i32 [ 1, %entry ], [ %8, %6 ]
  %4 = phi i32 [ %0, %entry ], [ %7, %6 ]
  %5 = icmp slt i32 %4, 2
  br i1 %5, label %9, label %6

6:
  %7 = add nsw i32 %4, -1
  %8 = mul nsw i32 %3, %4
  br label %2

9:
  %10 = mul nsw i32 %3, 1
  ret i32 %10
}

attributes #0 = { mustprogress nofree nosync nounwind willreturn memory(none) uwtable "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

; int factorial(int n) {
;     if (n <= 1) return 1;
;     else return n * factorial(n - 1);
; }