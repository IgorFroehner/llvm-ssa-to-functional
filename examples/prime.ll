define dso_local noundef zeroext i1 @isPrime(i32 noundef %0) local_unnamed_addr #0 {
entry:
  %2 = icmp slt i32 %0, 2
  br i1 %2, label %16, label %3

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
  %14 = urem i32 %0, %10
  %15 = icmp eq i32 %14, 0
  br i1 %15, label %16, label %8

16:
  %17 = phi i1 [ false, %entry ], [ %4, %3 ], [ %12, %13 ], [ %12, %8 ]
  ret i1 %17
}

attributes #0 = { mustprogress nofree norecurse nosync nounwind willreturn memory(none) uwtable "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

; bool isPrime(int num) {
;     if (num <= 1) return false;
;     for (int i = 2; i * i <= num; i++) {
;         if (num % i == 0) return false;
;     }
;     return true;
; }