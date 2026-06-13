define dso_local noundef i32 @imin(i32 noundef %a, i32 noundef %b) local_unnamed_addr #0 {
  %3 = call i32 @llvm.smin.i32(i32 %a, i32 %b)
  ret i32 %3
}

define dso_local noundef i32 @imax(i32 noundef %a, i32 noundef %b) local_unnamed_addr #0 {
  %3 = call i32 @llvm.smax.i32(i32 %a, i32 %b)
  ret i32 %3
}

attributes #0 = { mustprogress nofree norecurse nosync nounwind willreturn memory(none) uwtable "no-trapping-math"="true" "stack-protector-buffer-size"="8" }

; int imin(int a, int b) { return a < b ? a : b; }   // -> llvm.smin.i32
; int imax(int a, int b) { return a > b ? a : b; }   // -> llvm.smax.i32
