define i64 @main() {
    br label %bb2

bb1:
    %2 = phi i64 [ 0, %1 ], [ %6, %bb2 ]
    %3 = phi i64 [ 0, %1 ], [ %7, %bb2 ]
    %4 = mul i64 2, 3
    %5 = add i64 %2, 4
    br label %ret

bb2:
    %6 = mul i64 5, 6
    %7 = add i64 %4, 7
    br label %bb1

ret:
    %8 = phi i64 [ %5, %bb1 ], [ %7, %bb2 ]
    ret i64 %8
}