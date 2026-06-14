// Returns i1: the i1-aware translation gives it a Haskell `Bool` return type
// (docs/roadmap/10-boolean-types.md). The differential harness maps the C _Bool
// and the Haskell Bool back to 0/1 to certify them equal.
_Bool is_positive(int x) { return x > 0; }
