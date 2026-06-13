define double @logistic(double noundef %r, double noundef %x0, i32 noundef %iters) local_unnamed_addr #0 {
entry:
  %cmp5 = icmp sgt i32 %iters, 0
  br i1 %cmp5, label %for.body, label %for.cond.cleanup

for.cond.cleanup:                                 ; preds = %for.body, %entry
  %x.0.lcssa = phi double [ %x0, %entry ], [ %mul1, %for.body ]
  ret double %x.0.lcssa

for.body:                                         ; preds = %entry, %for.body
  %i.07 = phi i32 [ %inc, %for.body ], [ 0, %entry ]
  %x.06 = phi double [ %mul1, %for.body ], [ %x0, %entry ]
  %mul = fmul double %r, %x.06
  %sub = fsub double 1.000000e+00, %x.06
  %mul1 = fmul double %mul, %sub
  %inc = add nuw nsw i32 %i.07, 1
  %exitcond.not = icmp eq i32 %inc, %iters
  br i1 %exitcond.not, label %for.cond.cleanup, label %for.body
}

; // May (1976) logistic map  x_{n+1} = r * x_n * (1 - x_n).
; // A pure floating-point recurrence exhibiting sensitive dependence on initial
; // conditions (chaos for r ~ 3.57..4). Run for a runtime number of iterations
; // so it stays a loop with a double-typed phi rather than unrolling.
; extern "C" double logistic(double r, double x0, int iters) {
;     double x = x0;
;     for (int i = 0; i < iters; i++) {
;         x = r * x * (1.0 - x);
;     }
;     return x;
; }
