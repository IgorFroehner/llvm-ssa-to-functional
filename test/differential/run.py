#!/usr/bin/env python3
"""Differential testing of the SSA -> ANF translation.

For each example in ``examples/`` we obtain two executables of the *same*
function and compare them over a fuzzed range of integer inputs:

  * native    -- clang compiles the LLVM-IR ``.ll`` directly. This is the
                 ground truth: the exact IR we translate, with real iN
                 wraparound semantics.
  * pipeline  -- this project translates the ``.ll`` to Haskell (ANF), which
                 GHC then compiles. Integers become Haskell ``Int`` (64-bit).

Each trial is classified:

  EXACT    native == pipeline
  MISMATCH otherwise -- a genuine divergence.

Since the translation maps each LLVM iN to the matching sized Haskell integer
(docs/roadmap/bit-width-fidelity.md), wraparound is faithful and every example
is expected to be bit-for-bit EXACT. Exit status is non-zero iff any MISMATCH is
found, so this doubles as a CI gate that also catches bit-width regressions.

Usage:
    python3 test/differential/run.py [-n TRIALS] [--seed SEED] [case ...]

Requires ``clang`` and ``stack`` on PATH. Run from the repository root.
"""

import argparse
import os
import random
import subprocess
import sys
import tempfile

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
EXAMPLES = os.path.join(ROOT, "examples")

# C type for each LLVM integer width we emit.
CTYPE = {"i32": "int", "i64": "long long"}


def i(lo, hi):
    """An i32-ish sampler: random in [lo, hi]."""
    return lambda r: r.randint(lo, hi)


# --- The corpus -----------------------------------------------------------
#
# Each case: the .ll file, the target function, its signature, and a sampler
# producing one argument tuple. Samplers encode the domain restrictions of the
# subset (e.g. avoid the div-by-zero that is UB natively and an exception in
# Haskell). Ranges for the pure-arithmetic kernels deliberately span the i32
# overflow boundary so the bit-width gap is exercised, not hidden.

def case(ll, func, args, ret, sampler, note=""):
    return dict(ll=ll, func=func, args=args, ret=ret, sampler=sampler, note=note)


CASES = [
    # Arithmetic kernels: ranges cross the i32 overflow boundary on purpose.
    case("factorial", "factorial", ["i32"], "i32",
         lambda r: (r.randint(-3, 34),), "n! overflows i32 for n>=13"),
    case("from_rust", "factorial", ["i32"], "i32",
         lambda r: (r.randint(-3, 34),), "Rust-sourced factorial"),
    case("fib", "fib", ["i32"], "i32",
         lambda r: (r.randint(0, 60),), "fib overflows i32 around n=47"),
    case("sum", "asum", ["i32"], "i32",
         lambda r: (r.randint(-5, 120000),), "triangular number, overflows i32"),
    case("square", "square", ["i32"], "i32",
         lambda r: (r.randint(-200000, 200000),), "x*x overflows i32"),
    case("square", "no_overflow_square", ["i32"], "i64",
         lambda r: (r.randint(-200000, 200000),), "i32 in, i64 out"),

    # Bounded-result kernels: expected to be EXACT everywhere (full
    # certification), inputs kept clear of overflow in control-flow tests.
    case("gcd", "euclides_gcd", ["i32", "i32"], "i32",
         lambda r: (r.randint(-10000, 10000), r.randint(-10000, 10000)),
         "Euclid; signed rem matches C %"),
    # Full range: m*m now overflows in true i32 in both native and pipeline, so
    # the loop-branch behaviour matches and stays EXACT. (Previously capped at
    # 80000 to dodge the 64-bit pipeline's divergent control flow.)
    case("bin_search", "bin_search", ["i32"], "i32",
         lambda r: (r.randint(-100, 1 << 20),), "isqrt; m*m overflows i32"),
    case("tot", "phi", ["i32"], "i32",
         lambda r: (r.randint(-50, 100000),), "Euler totient"),
    case("prime", "is_prime", ["i32"], "i32",
         lambda r: (r.randint(-10, 100000),), "i1 return, 0/1"),
    case("safediv", "safe_div", ["i32", "i32"], "i32",
         lambda r: (r.randint(-100000, 100000), r.randint(-1000, 1000)),
         "guards b==0 -> -1"),
    case("select", "safe_div", ["i64", "i64"], "i64",
         lambda r: (r.randint(-(1 << 40), 1 << 40), r.randint(-1000, 1000)),
         "ternary, i64"),
    case("mod_pow", "exp_mod", ["i64", "i64", "i64"], "i64",
         lambda r: (r.randint(-1000, 1000), r.randint(0, 64), r.randint(1, 100000)),
         "exp>=0, mod!=0; products stay in i64"),
]


def sh(cmd, **kw):
    return subprocess.run(cmd, capture_output=True, text=True, **kw)


def build_native(c, workdir):
    """clang-compile the .ll plus a generated C driver; return the exe path."""
    ret_c = CTYPE[c["ret"]]
    params = ", ".join(CTYPE[a] for a in c["args"])
    parse = ", ".join(f"({CTYPE[a]}) strtoll(argv[{n + 1}], 0, 10)"
                      for n, a in enumerate(c["args"]))
    drv = f"""#include <stdio.h>
#include <stdlib.h>
extern {ret_c} {c['func']}({params});
int main(int argc, char **argv) {{
    printf("%lld\\n", (long long) {c['func']}({parse}));
    return 0;
}}
"""
    dc = os.path.join(workdir, "driver.c")
    with open(dc, "w") as f:
        f.write(drv)
    exe = os.path.join(workdir, "native")
    # -w: silence the x86 "target-cpu" attribute warnings carried in the .ll.
    r = sh(["clang", "-w", "-O1", dc, os.path.join(EXAMPLES, c["ll"] + ".ll"),
            "-o", exe])
    if r.returncode != 0:
        raise RuntimeError("clang failed:\n" + r.stderr)
    return exe


def build_pipeline(c, workdir):
    """Translate .ll -> Haskell, append a driver main, GHC-compile it."""
    hs = os.path.join(workdir, "out.hs")
    r = sh(["stack", "exec", "llvm-ir-to-functional-exe", "--",
            os.path.join(EXAMPLES, c["ll"] + ".ll"), "-o", hs], cwd=ROOT)
    if r.returncode != 0 or not os.path.exists(hs):
        raise RuntimeError("translation failed:\n" + r.stderr)
    src = open(hs).read()
    # getArgs must be imported with the other imports, before any definition.
    src = src.replace("import Data.Bits",
                      "import Data.Bits\nimport System.Environment (getArgs)", 1)
    # The translated function is now bit-width-typed (iN -> IntN), so feed it
    # fromIntegral-converted args and widen the signed IntN result back to
    # Integer for printing -- this matches native's signed (long long) cast.
    argv = " ".join(f"(fromIntegral (xs!!{n}))" for n in range(len(c["args"])))
    src += (
        "\nmain :: IO ()\n"
        "main = do\n"
        "  as <- getArgs\n"
        "  let xs = map read as :: [Integer]\n"
        f"  print (fromIntegral ({c['func']} {argv}) :: Integer)\n"
    )
    open(hs, "w").write(src)
    exe = os.path.join(workdir, "pipeline")
    r = sh(["stack", "ghc", "--", "-v0", "-O0", hs, "-o", exe], cwd=ROOT)
    if r.returncode != 0 or not os.path.exists(exe):
        raise RuntimeError("ghc failed:\n" + r.stderr + r.stdout)
    return exe


def run_int(exe, argtuple):
    r = sh([exe, *[str(a) for a in argtuple]])
    if r.returncode != 0:
        raise RuntimeError(f"{exe} {argtuple} exited {r.returncode}: {r.stderr}")
    return int(r.stdout.strip())


def run_case(c, trials, rng):
    with tempfile.TemporaryDirectory() as wd:
        native = build_native(c, wd)
        pipeline = build_pipeline(c, wd)
        exact = 0
        mismatches = []
        seen = set()
        for _ in range(trials):
            args = c["sampler"](rng)
            if args in seen:
                continue
            seen.add(args)
            n = run_int(native, args)
            p = run_int(pipeline, args)
            if n == p:
                exact += 1
            else:
                mismatches.append((args, n, p))
        return exact, mismatches, len(seen)


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("-n", "--trials", type=int, default=200)
    ap.add_argument("--seed", type=int, default=1)
    ap.add_argument("cases", nargs="*", help="restrict to these function names")
    opts = ap.parse_args()

    selected = CASES
    if opts.cases:
        selected = [c for c in CASES if c["func"] in opts.cases or c["ll"] in opts.cases]
        if not selected:
            print("no matching cases", file=sys.stderr)
            return 2

    print("Building the translator (stack build)...", flush=True)
    b = sh(["stack", "build"], cwd=ROOT)
    if b.returncode != 0:
        print(b.stderr, file=sys.stderr)
        return 2

    rng = random.Random(opts.seed)
    hdr = f"{'case':<22}{'tested':>7}{'exact':>7}{'mism':>6}  verdict"
    print("\n" + hdr)
    print("-" * len(hdr))

    any_mismatch = False
    for c in selected:
        label = f"{c['ll']}:{c['func']}"
        try:
            exact, mis, tested = run_case(c, opts.trials, rng)
        except RuntimeError as e:
            print(f"{label:<22}  ERROR: {e}".rstrip())
            any_mismatch = True
            continue
        if mis:
            verdict = f"BUG ({len(mis)} mismatch)"
            any_mismatch = True
        else:
            verdict = "certified (exact)"
        print(f"{label:<22}{tested:>7}{exact:>7}{len(mis):>6}  {verdict}")
        for args, n, p in mis[:3]:
            print(f"    args={args} native={n} pipeline={p}")

    print()
    if any_mismatch:
        print("RESULT: genuine mismatches found -- see BUG rows above.")
        return 1
    print("RESULT: all examples bit-for-bit exact.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
