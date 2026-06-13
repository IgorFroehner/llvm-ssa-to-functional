define dso_local noundef zeroext i1 @is_positive(i32 noundef %x) local_unnamed_addr #0 {
  %2 = icmp sgt i32 %x, 0
  ret i1 %2
}

attributes #0 = { mustprogress nofree norecurse nosync nounwind willreturn memory(none) uwtable "no-trapping-math"="true" "stack-protector-buffer-size"="8" }

; _Bool is_positive(int x) { return x > 0; }   // i1 return; faithful 0/1 already works
