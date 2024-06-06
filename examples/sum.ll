define dso_local noundef i32 @asum(i32 noundef %0) local_unnamed_addr #0 {
entry:
  %2 = icmp slt i32 %0, 1
  br i1 %2, label %14, label %3

3:
  %4 = shl nuw i32 %0 , 1
  %5 = add nsw i32 %0 , -1
  %6 = zext i32 %5 to i33
  %7 = add nsw i32 %0, -2
  %8 = zext i32 %7 to i33
  %9 = mul i33 %6, %8
  %10 = lshr i33 %9, 1
  %11 = trunc i33 %10 to i32
  %12 = add i32 %4, %11
  %13 = add i32 %12, -1
  br label %14

14:
  %15 = phi i32 [ 0, %entry ], [ %13, %3 ]
  ret i32 %15
}

attributes #0 = { mustprogress nofree norecurse nosync nounwind willreturn memory(none) uwtable "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

; int sumOfNaturalNumbers(int n) {
;     int sum = 0;
;     for (int i = 1; i <= n; i++) {
;         sum += i;
;     }
;     return sum;
; }
