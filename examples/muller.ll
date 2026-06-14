define double @muller(i32 noundef %n) local_unnamed_addr #0 {
entry:
  %cmp6 = icmp sgt i32 %n, 0
  br i1 %cmp6, label %for.body, label %for.cond.cleanup

for.cond.cleanup:                                 ; preds = %for.body, %entry
  %cur.0.lcssa = phi double [ 4.250000e+00, %entry ], [ %sub2, %for.body ]
  ret double %cur.0.lcssa

for.body:                                         ; preds = %entry, %for.body
  %prev.09 = phi double [ %cur.07, %for.body ], [ 4.000000e+00, %entry ]
  %i.08 = phi i32 [ %inc, %for.body ], [ 0, %entry ]
  %cur.07 = phi double [ %sub2, %for.body ], [ 4.250000e+00, %entry ]
  %div = fdiv double 1.500000e+03, %prev.09
  %sub = fsub double 8.150000e+02, %div
  %div1 = fdiv double %sub, %cur.07
  %sub2 = fsub double 1.080000e+02, %div1
  %inc = add nuw nsw i32 %i.08, 1
  %exitcond.not = icmp eq i32 %inc, %n
  br i1 %exitcond.not, label %for.cond.cleanup, label %for.body
}

; // Muller's recurrence (J.-M. Muller, Elementary Functions):
; //   E_{n+1} = 108 - (815 - 1500/E_{n-1}) / E_n,   E0 = 4, E1 = 4.25
; // The exact limit is 5, but in floating point the iteration is drawn to the
; // repulsive fixed point 100. A two-state (E_{n-1}, E_n) recurrence: exercises
; // double-typed phi nodes carried around a loop.
; extern "C" double muller(int n) {
;     double prev = 4.0;     // E_{n-1}
;     double cur  = 4.25;    // E_n
;     for (int i = 0; i < n; i++) {
;         double next = 108.0 - (815.0 - 1500.0 / prev) / cur;
;         prev = cur;
;         cur  = next;
;     }
;     return cur;
; }
