; Hand-written: clang does not emit `freeze` for straightforward -O1 C, but the
; instruction is valid LLVM-IR. In the pure subset it is the identity, so this
; computes (x + 1). Kept minimal to exercise the `freeze` translation path.
define dso_local i32 @freeze_inc(i32 noundef %x) local_unnamed_addr #0 {
  %2 = freeze i32 %x
  %3 = add i32 %2, 1
  ret i32 %3
}

attributes #0 = { mustprogress nofree norecurse nosync nounwind willreturn memory(none) uwtable "no-trapping-math"="true" "stack-protector-buffer-size"="8" }
