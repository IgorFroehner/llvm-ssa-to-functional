define dso_local noundef i32 @bin_search(i32 noundef %0) local_unnamed_addr #0 {
entry:
  %2 = icmp sgt i32 %0, 1
  br i1 %2, label %3, label %16

3:
  %4 = add nsw i32 %0, -1
  br label %5

5:
  %6 = phi i32 [ %14, %5 ], [ 0, %3 ]
  %7 = phi i32 [ %13, %5 ], [ %4, %3 ]
  %8 = add nsw i32 %6, %7
  %9 = sdiv i32 %8, 2
  %10 = mul nsw i32 %9, %9
  %11 = icmp slt i32 %10, %0
  %12 = add nsw i32 %9, 1
  %13 = select i1 %11, i32 %7, i32 %9
  %14 = select i1 %11, i32 %12, i32 %6
  %15 = icmp slt i32 %14, %13
  br i1 %15, label %5, label %16

16:
  %17 = phi i32 [ 0, %entry ], [ %14, %5 ]
  ret i32 %17
}

attributes #0 = { mustprogress nofree norecurse nosync nounwind willreturn memory(none) uwtable "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

; int bin_search(int n) {
;     int l = 0, r = n - 1;
;     while (l < r) {
;         int m = (l + r) / 2;
;         if (m * m < n) {
;             l = m + 1;
;         } else {
;             r = m;
;         }
;     }
;     return l;
; }
