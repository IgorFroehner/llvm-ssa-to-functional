define i32 @count_positive(i32 noundef %a, i32 noundef %b, i32 noundef %c) local_unnamed_addr #0 {
  %cmp = icmp sgt i32 %a, 0
  %conv = zext i1 %cmp to i32
  %cmp1 = icmp sgt i32 %b, 0
  %conv2 = zext i1 %cmp1 to i32
  %add = add nuw nsw i32 %conv2, %conv
  %cmp3 = icmp sgt i32 %c, 0
  %conv4 = zext i1 %cmp3 to i32
  %add5 = add nuw nsw i32 %add, %conv4
  ret i32 %add5
}

attributes #0 = { mustprogress nofree norecurse nosync nounwind willreturn memory(none) uwtable "no-trapping-math"="true" "stack-protector-buffer-size"="8" }

; int count_positive(int a, int b, int c) { return (a > 0) + (b > 0) + (c > 0); }
