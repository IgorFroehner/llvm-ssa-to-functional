define double @newton_sqrt(double noundef %a, i32 noundef %iters) local_unnamed_addr #0 {
entry:
  %cmp = fcmp ugt double %a, 0.000000e+00
  br i1 %cmp, label %for.cond.preheader, label %return

for.cond.preheader:                               ; preds = %entry
  %cmp17 = icmp sgt i32 %iters, 0
  br i1 %cmp17, label %for.body, label %return

for.body:                                         ; preds = %for.cond.preheader, %for.body
  %i.09 = phi i32 [ %inc, %for.body ], [ 0, %for.cond.preheader ]
  %x.08 = phi double [ %mul, %for.body ], [ %a, %for.cond.preheader ]
  %div = fdiv double %a, %x.08
  %add = fadd double %x.08, %div
  %mul = fmul double %add, 5.000000e-01
  %inc = add nuw nsw i32 %i.09, 1
  %exitcond.not = icmp eq i32 %inc, %iters
  br i1 %exitcond.not, label %return, label %for.body

return:                                           ; preds = %for.body, %for.cond.preheader, %entry
  %retval.0 = phi double [ 0.000000e+00, %entry ], [ %a, %for.cond.preheader ], [ %mul, %for.body ]
  ret double %retval.0
}

; // Babylonian / Heron / Newton iteration for sqrt(a):
; //   x_{n+1} = (x_n + a/x_n) / 2
; // Implemented WITHOUT the llvm.sqrt intrinsic (so it stays inside the subset)
; // and as a real loop over a runtime iteration count. A non-positive input is
; // guarded, giving an early-return branch that exercises fcmp + control flow.
; extern "C" double newton_sqrt(double a, int iters) {
;     if (a <= 0.0) return 0.0;
;     double x = a;                 // initial guess
;     for (int i = 0; i < iters; i++) {
;         x = 0.5 * (x + a / x);
;     }
;     return x;
; }
