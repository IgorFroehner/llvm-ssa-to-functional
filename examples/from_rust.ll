define noundef i32 @factorial(i32 noundef %num) unnamed_addr #0 {
start:
  br label %tailrecurse

tailrecurse:                                      ; preds = %bb2, %start
  %accumulator.tr = phi i32 [ 1, %start ], [ %0, %bb2 ]
  %num.tr = phi i32 [ %num, %start ], [ %_4, %bb2 ]
  %_2 = icmp slt i32 %num.tr, 1
  br i1 %_2, label %bb4, label %bb2

bb2:                                              ; preds = %tailrecurse
  %_4 = add nsw i32 %num.tr, -1
  %0 = mul i32 %accumulator.tr, %num.tr
  br label %tailrecurse

bb4:                                              ; preds = %tailrecurse
  %accumulator.ret.tr = mul i32 %accumulator.tr, 1
  ret i32 %accumulator.ret.tr
}

; #[no_mangle]
; pub fn factorial(num: i32) -> i32 {
;     if num <= 0 { return 1 };
;     num * factorial(num - 1)
; }