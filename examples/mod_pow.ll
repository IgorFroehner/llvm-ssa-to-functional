define dso_local noundef i64 @exp_mod(i64 noundef %0, i64 noundef %1, i64 noundef %2) local_unnamed_addr #0 {
entry:
  %4 = icmp eq i64 %1, 0
  br i1 %4, label %21, label %5

5:
  %6 = phi i64 [ %15, %14 ], [ 1, %entry ]
  %7 = phi i64 [ %17, %14 ], [ %0, %entry ]
  %8 = phi i64 [ %18, %14 ], [ %1, %entry ]
  %9 = and i64 %8, 1
  %10 = icmp eq i64 %9, 0
  br i1 %10, label %14, label %11

11:
  %12 = mul nsw i64 %6, %7
  %13 = srem i64 %12, %2
  br label %14

14:
  %15 = phi i64 [ %13, %11 ], [ %6, %5 ]
  %16 = mul nsw i64 %7, %7
  %17 = srem i64 %16, %2
  %18 = sdiv i64 %8, 2
  %19 = add i64 %8, 1
  %20 = icmp ult i64 %19, 3
  br i1 %20, label %21, label %5

21:
  %22 = phi i64 [ 1, %entry ], [ %15, %14 ]
  ret i64 %22
}

; long long exp_mod(long long base, long long exp, long long mod) {
;     long long b = base, res = 1;
;     while (exp) {
;         if (exp & 1) {
;             res = (res * b) % mod;
;         }
;         b = (b * b) % mod;
;         exp /= 2;
;     }
;     return res;
; }