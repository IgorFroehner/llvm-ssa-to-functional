define double @kahan_harmonic(i32 noundef %n) local_unnamed_addr #0 {
entry:
  %cmp.not9 = icmp slt i32 %n, 1
  br i1 %cmp.not9, label %for.cond.cleanup, label %for.body

for.cond.cleanup:                                 ; preds = %for.body, %entry
  %sum.0.lcssa = phi double [ 0.000000e+00, %entry ], [ %add, %for.body ]
  ret double %sum.0.lcssa

for.body:                                         ; preds = %entry, %for.body
  %sum.012 = phi double [ %add, %for.body ], [ 0.000000e+00, %entry ]
  %c.011 = phi double [ %sub2, %for.body ], [ 0.000000e+00, %entry ]
  %k.010 = phi i32 [ %inc, %for.body ], [ 1, %entry ]
  %conv = uitofp nneg i32 %k.010 to double
  %div = fdiv double 1.000000e+00, %conv
  %sub = fsub double %div, %c.011
  %add = fadd double %sum.012, %sub
  %sub1 = fsub double %add, %sum.012
  %sub2 = fsub double %sub1, %sub
  %inc = add nuw i32 %k.010, 1
  %exitcond.not = icmp eq i32 %k.010, %n
  br i1 %exitcond.not, label %for.cond.cleanup, label %for.body
}

; // Kahan (1965) compensated summation of the harmonic series
; //   sum_{k=1}^{n} 1/k.
; // The running compensation term c recovers low-order bits otherwise lost to
; // rounding, so the result is far more accurate than a naive accumulator.
; // Exercises an int->double conversion (the (double) k cast) inside a loop;
; // since k is provably >= 1, clang emits uitofp (not sitofp).
; extern "C" double kahan_harmonic(int n) {
;     double sum = 0.0;
;     double c   = 0.0;            // running compensation
;     for (int k = 1; k <= n; k++) {
;         double y = 1.0 / (double) k - c;
;         double t = sum + y;
;         c   = (t - sum) - y;     // recovers the part of y lost in (sum + y)
;         sum = t;
;     }
;     return sum;
; }
