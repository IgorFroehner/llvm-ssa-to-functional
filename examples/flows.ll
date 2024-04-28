define dso_local noundef i32 @main() local_unnamed_addr #0 {
    %0 = add nsw i32 1, 2
    br label %entry

entry:
    %1 = icmp eq i32 %0, 3
    br i1 %1, label %if, label %else

return:
    %2 = phi i32 [ %0, %entry ], [ %3, %some_loop ], [ %7, %then ]
    ret i32 %2

if:
    br label %some_loop

some_loop:
    %3 = add nsw i32 %2, 1
    %4 = icmp ult i32 %3, 3
    br i1 %4, label %some_loop, label %return

else:
    %5 = add nsw i32 %1, %0
    %6 = icmp eq i32 %5, 4
    br label %6, label %then, label %return

then:
    %7 = add nsw i32 %5, 1
    br label %return
}