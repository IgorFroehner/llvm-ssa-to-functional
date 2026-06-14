define noundef double @twosum_err(double noundef %a, double noundef %b) local_unnamed_addr #0 {
entry:
  %add = fadd double %a, %b
  %sub = fsub double %add, %a
  %sub1 = fsub double %add, %sub
  %sub2 = fsub double %a, %sub1
  %sub3 = fsub double %b, %sub
  %add4 = fadd double %sub3, %sub2
  ret double %add4
}

; // Knuth / Moller TwoSum (an "error-free transformation"): for s = a + b it
; // recovers the exact rounding error err such that a + b == s + err in exact
; // arithmetic, using only floating additions/subtractions. Foundational
; // primitive of compensated-arithmetic algorithms (cf. Kahan summation).
; extern "C" double twosum_err(double a, double b) {
;     double s   = a + b;
;     double bb  = s - a;
;     double err = (a - (s - bb)) + (b - bb);
;     return err;
; }
