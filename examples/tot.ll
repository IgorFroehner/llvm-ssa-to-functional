define dso_local noundef i32 @phi(i32 noundef %0) local_unnamed_addr #0 {
entry:
  %2 = icmp slt i32 %0, 4
  br i1 %2, label %3, label %7

3:
  %4 = phi i32 [ %0, %entry ], [ %26, %25 ]
  %5 = phi i32 [ %0, %entry ], [ %27, %25 ]
  %6 = icmp sgt i32 %4, 1
  br i1 %6, label %31, label %34

7:
  %8 = phi i32 [ %28, %25 ], [ 2, %entry ]
  %9 = phi i32 [ %27, %25 ], [ %0, %entry ]
  %10 = phi i32 [ %26, %25 ], [ %0, %entry ]
  %11 = srem i32 %10, %8
  %12 = icmp eq i32 %11, 0
  br i1 %12, label %13, label %25

13:
  %14 = srem i32 %10, %8
  %15 = icmp eq i32 %14, 0
  br i1 %15, label %16, label %21

16:
  %17 = phi i32 [ %18, %16 ], [ %10, %13 ]
  %18 = sdiv i32 %17, %8
  %19 = srem i32 %18, %8
  %20 = icmp eq i32 %19, 0
  br i1 %20, label %16, label %21

21:
  %22 = phi i32 [ %10, %13 ], [ %18, %16 ]
  %23 = sdiv i32 %9, %8
  %24 = sub nsw i32 %9, %23
  br label %25

25:
  %26 = phi i32 [ %22, %21 ], [ %10, %7 ]
  %27 = phi i32 [ %24, %21 ], [ %9, %7 ]
  %28 = add nuw nsw i32 %8, 1
  %29 = mul nsw i32 %28, %28
  %30 = icmp sgt i32 %29, %26
  br i1 %30, label %3, label %7

31:
  %32 = sdiv i32 %5, %4
  %33 = sub nsw i32 %5, %32
  br label %34

34:
  %35 = phi i32 [ %33, %31 ], [ %5, %3 ]
  ret i32 %35
}

attributes #0 = { mustprogress nofree norecurse nosync nounwind willreturn memory(none) uwtable "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

; int phi(int n) {
;     int result = n;
;     for (int i = 2; i * i <= n; i++) {
;         if (n % i == 0) {
;             while (n % i == 0) {
;                 n /= i;
;             }
;             result -= result / i;
;         }
;     }
;     if (n > 1) {
;         result -= result / n;
;     }
;     return result;
; }