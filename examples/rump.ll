define double @rump(double noundef %a, double noundef %b) local_unnamed_addr #0 {
entry:
  %mul = fmul double %b, %b
  %mul1 = fmul double %mul, %mul
  %mul2 = fmul double %mul, %mul1
  %mul3 = fmul double %mul1, %mul1
  %mul4 = fmul double %a, %a
  %mul5 = fmul double %mul2, 3.337500e+02
  %mul6 = fmul double %mul4, 1.100000e+01
  %mul7 = fmul double %mul6, %mul
  %sub = fsub double %mul7, %mul2
  %mul8 = fmul double %mul1, 1.210000e+02
  %sub9 = fsub double %sub, %mul8
  %sub10 = fadd double %sub9, -2.000000e+00
  %mul11 = fmul double %mul4, %sub10
  %add = fadd double %mul5, %mul11
  %mul12 = fmul double %mul3, 5.500000e+00
  %add13 = fadd double %mul12, %add
  %mul14 = fmul double %b, 2.000000e+00
  %div = fdiv double %a, %mul14
  %add15 = fadd double %div, %add13
  ret double %add15
}

; // Rump (1988): a rational/polynomial expression whose naive floating-point
; // evaluation is catastrophically wrong. At (a, b) = (77617, 33096) the exact
; // value is approx -0.827396, but float/double arithmetic gives wildly wrong
; // results (often the wrong sign and magnitude) due to cancellation.
; // Pure scalar +, -, *, / — no intrinsics.
; extern "C" double rump(double a, double b) {
;     double b2 = b * b;
;     double b4 = b2 * b2;
;     double b6 = b4 * b2;
;     double b8 = b4 * b4;
;     double a2 = a * a;
;     return 333.75 * b6
;          + a2 * (11.0 * a2 * b2 - b6 - 121.0 * b4 - 2.0)
;          + 5.5 * b8
;          + a / (2.0 * b);
; }
