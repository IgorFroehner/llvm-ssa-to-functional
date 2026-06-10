define i32 @euclides_gcd(i32 noundef %0, i32 noundef %1) local_unnamed_addr #0 {
  br label %3

3:
  %4 = phi i32 [ %0, %2 ], [ %5, %7 ]
  %5 = phi i32 [ %1, %2 ], [ %8, %7 ]
  %6 = icmp eq i32 %5, 0
  br i1 %6, label %9, label %7

7:
  %8 = srem i32 %4, %5
  br label %3

9:
  ret i32 %4
}

; int gcd(int a, int b) {
;     if (b == 0)
;         return a;
;     else
;         return gcd(b, a % b);
; }
