define dso_local noundef i32 @agcd(i32 noundef %0, i32 noundef %1) local_unnamed_addr #0 {
entry:
  br label %3

3:
  %4 = phi i32 [ %0, %entry ], [ %5, %7 ]
  %5 = phi i32 [ %1, %entry ], [ %8, %7 ]
  %6 = icmp eq i32 %5, 0
  br i1 %6, label %9, label %7

7:
  %8 = srem i32 %4, %5
  br label %3

9:
  ret i32 %4
}

attributes #0 = { mustprogress nofree nosync nounwind willreturn memory(none) uwtable "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

; int gcd(int a, int b) {
;     if (b == 0)
;         return a;
;     else
;         return gcd(b, a % b);
; }