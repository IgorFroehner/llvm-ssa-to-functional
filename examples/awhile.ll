define dso_local noundef i32 @function(i32 noundef %0, i32 noundef %1) local_unnamed_addr #0 {
  %3 = tail call i32 @test(i32 noundef %0, i32 noundef %1)
  ret i32 %3
}

declare i32 @test(i32 noundef, i32 noundef) local_unnamed_addr #1

define dso_local noundef i32 @main() local_unnamed_addr #2 {
  br label %1

1:
  %2 = phi i32 [ 0, %0 ], [ %4, %1 ]
  %3 = tail call noundef i32 @test(i32 noundef 1, i32 noundef 2)
  %4 = add nsw i32 %3, %2
  %5 = icmp slt i32 %4, 2
  br i1 %5, label %1, label %6

6:
  ret i32 %4
}

attributes #0 = { mustprogress uwtable "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #2 = { mustprogress norecurse uwtable "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }