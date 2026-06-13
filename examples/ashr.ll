define dso_local i32 @arith_shr(i32 noundef %x, i32 noundef %n) local_unnamed_addr #0 {
  %3 = ashr i32 %x, %n
  ret i32 %3
}

attributes #0 = { mustprogress nofree norecurse nosync nounwind willreturn memory(none) uwtable "no-trapping-math"="true" "stack-protector-buffer-size"="8" }

; int arith_shr(int x, int n) {
;     return x >> n;   // arithmetic (sign-propagating) shift
; }
