define dso_local i32 @iabs(i32 noundef %x) local_unnamed_addr #0 {
  %2 = call i32 @llvm.abs.i32(i32 %x, i1 true)
  ret i32 %2
}

attributes #0 = { mustprogress nofree norecurse nosync nounwind willreturn memory(none) uwtable "no-trapping-math"="true" "stack-protector-buffer-size"="8" }

; int iabs(int x) { return x < 0 ? -x : x; }   // -> llvm.abs.i32 (i1 immarg dropped)
