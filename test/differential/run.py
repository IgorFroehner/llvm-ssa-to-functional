#!/usr/bin/env python3
"""Differential testing of the SSA -> ANF translation.

For each example in ``examples/`` we obtain two executables of the *same*
function and compare them over a fuzzed range of inputs:

  * native    -- clang compiles the LLVM-IR ``.ll`` directly. This is the
                 ground truth: the exact IR we translate, with real iN
                 wraparound and IEEE-754 semantics.
  * pipeline  -- this project translates the ``.ll`` to Haskell (ANF), which
                 GHC then compiles. Each iN becomes the matching sized Haskell
                 integer; float/double become Float/Double.

Each trial is classified:

  EXACT    native == pipeline
  MISMATCH otherwise -- a genuine divergence.

Integer results are compared as values; floating results are compared by their
raw IEEE bit pattern (so the check is exact and NaN-robust). The translation
maps each iN to the matching sized integer (docs/roadmap/bit-width-fidelity.md)
and each f-op to its strict-FP Haskell counterpart, so every example is expected
to be bit-for-bit EXACT. Exit status is non-zero iff any MISMATCH is found, so
this doubles as a CI gate that also catches bit-width / FP regressions.

The corpus is *auto-discovered*: ``discover`` scans ``examples/*.ll`` and reads
each function's signature off its ``define`` line, so dropping a new ``.ll`` in
``examples/`` certifies it automatically. The only hand-written bit is a sampler
override in ``OVERRIDES``, needed solely when the default wide range would leave
the subset's domain (undefined behaviour) or make a counting loop run forever.

Usage:
    python3 test/differential/run.py [-n TRIALS] [--seed SEED] [case ...]

Requires ``clang`` and ``stack`` on PATH. Run from the repository root.
"""

import argparse
import os
import random
import re
import subprocess
import sys
import tempfile

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
EXAMPLES = os.path.join(ROOT, "examples")

# C type for each LLVM type we accept. Integer widths are read through C's
# ``int``/``long long``; an ``i1`` (0/1 bool return) rides in an ``int`` too.
# ``float``/``double`` are the IEEE types; results are compared by their raw bit
# pattern (see ``build_native``/``build_pipeline``), so the comparison is exact
# and NaN-robust. ``void`` carries no value: a void case only certifies both
# sides build and run. Any other type (ptr, ...) is outside the subset, so
# ``discover`` skips functions that mention it.
INT_C = {"i1": "int", "i8": "int", "i16": "int", "i32": "int", "i64": "long long"}
FLOAT_C = {"float": "float", "double": "double"}
NUM_C = {**INT_C, **FLOAT_C}
CTYPE = {**NUM_C, "void": "void"}


def is_float(t):
    return t in FLOAT_C


# Default fuzz range per integer width. The i32 span deliberately crosses the
# i32 overflow boundary -- so x*x and triangular sums wrap, exercising bit-width
# fidelity -- while staying clear of INT_MIN (which is llvm.abs's poison case).
# Floating arguments are sampled uniformly from a moderate magnitude (kept away
# from NaN/Inf/overflow); both sides receive bit-identical inputs because the
# value is passed as a round-tripping decimal and parsed by matched routines.
DEFAULT_I32 = (-100_000, 100_000)
DEFAULT_RANGE = {"i64": (-(1 << 40), 1 << 40)}
DEFAULT_FLOAT = (-1.0e3, 1.0e3)


def sample_one(r, t):
    if is_float(t):
        return repr(r.uniform(*DEFAULT_FLOAT))
    return r.randint(*DEFAULT_RANGE.get(t, DEFAULT_I32))


def case(ll, func, args, ret, sampler, note=""):
    return dict(ll=ll, func=func, args=args, ret=ret, sampler=sampler, note=note)


def default_sampler(arg_types):
    """A wide sampler derived purely from the argument types (int or float)."""
    def sample(r):
        return tuple(sample_one(r, t) for t in arg_types)
    return sample


# --- Sampler overrides ----------------------------------------------------
#
# Discovery (below) reads every function's signature straight from its
# ``define`` line, so the *only* thing a new example needs is a sampler -- and
# only when the default wide range would leave the subset's domain (undefined
# behaviour) or make a counting loop run effectively forever. Keyed by
# (file, function); anything unlisted uses ``default_sampler``.
OVERRIDES = {
    # fib counts the input down to zero; a negative input wraps and loops ~2^32
    # times, so keep it non-negative (it still overflows i32 around n=47).
    ("fib", "fib"):
        (lambda r: (r.randint(0, 90),), "non-negative; overflows i32 ~n=47"),
    # ashr's shift amount is UB unless it is in [0, bitwidth).
    ("ashr", "arith_shr"):
        (lambda r: (r.randint(-(1 << 30), 1 << 30), r.randint(0, 31)),
         "shift amount in [0,31]"),
    # exp_mod: exponent must be >= 0, modulus != 0 (srem by it).
    ("mod_pow", "exp_mod"):
        (lambda r: (r.randint(-1000, 1000), r.randint(0, 64), r.randint(1, 100000)),
         "exp>=0, mod!=0; products stay in i64"),
    # bin_search: a wide range makes m*m overflow i32 (the divergence the
    # bit-width work fixed); a modest default would never reach it.
    ("bin_search", "bin_search"):
        (lambda r: (r.randint(-100, 1 << 20),), "wide range stresses m*m i32 overflow"),
}


DEFINE_RE = re.compile(r"define\b(?P<pre>.*?)@(?P<name>[\w.]+)\s*\((?P<args>[^)]*)\)")


def parse_defines(path):
    """Yield (func, arg_widths, ret_width) for every function defined in a .ll.

    The signature is read positionally from the ``define`` line: the return type
    is the token immediately before ``@name`` (so attribute soup like
    ``range(i32 0, -2147483648) i32`` still yields ``i32``), and each argument's
    type is the first token of its comma-separated group.
    """
    with open(path) as f:
        for line in f:
            line = line.strip()
            if not line.startswith("define"):
                continue
            m = DEFINE_RE.match(line)
            if not m:
                continue
            ret = m.group("pre").split()[-1]
            args = [a.split()[0] for a in m.group("args").split(",") if a.strip()]
            yield m.group("name"), args, ret


def discover():
    """Build the corpus by scanning examples/*.ll -- one case per function.

    A function is skipped when its signature uses a type outside the subset. A
    whole file is skipped when it defines ``main``: the pipeline translates the
    entire module and appends its own ``main`` driver, and the native side
    supplies a ``main`` too, so a translated ``main`` would collide with both.
    """
    cases = []
    for fn in sorted(os.listdir(EXAMPLES)):
        if not fn.endswith(".ll"):
            continue
        ll = fn[:-3]
        defs = list(parse_defines(os.path.join(EXAMPLES, fn)))
        if any(func == "main" for func, _, _ in defs):
            continue
        for func, args, ret in defs:
            if ret not in CTYPE or any(a not in NUM_C for a in args):
                continue
            sampler, note = OVERRIDES.get((ll, func), (default_sampler(args), ""))
            cases.append(case(ll, func, args, ret, sampler, note))
    return cases


CASES = discover()


def sh(cmd, **kw):
    return subprocess.run(cmd, capture_output=True, text=True, **kw)


def _parse_arg(t, idx):
    """C expression parsing argv[idx] into the parameter type ``t``.

    Floats parse with strtof/strtod (single decimal->IEEE rounding, matching the
    Haskell ``read``); integers go through strtoll.
    """
    if t == "float":
        return f"strtof(argv[{idx}], 0)"
    if t == "double":
        return f"strtod(argv[{idx}], 0)"
    return f"({CTYPE[t]}) strtoll(argv[{idx}], 0, 10)"


def build_native(c, workdir):
    """clang-compile the .ll plus a generated C driver; return the exe path."""
    ret_c = CTYPE[c["ret"]]
    params = ", ".join(CTYPE[a] for a in c["args"])
    parse = ", ".join(_parse_arg(a, n + 1) for n, a in enumerate(c["args"]))
    if c["ret"] == "void":
        # No value to print; call it and emit the same marker the pipeline does.
        body = f'    {c["func"]}({parse});\n    printf("()\\n");'
    elif c["ret"] == "double":
        # Print the raw IEEE bit pattern so the comparison is exact / NaN-robust.
        body = (f'    double _r = {c["func"]}({parse});\n'
                '    uint64_t _u; memcpy(&_u, &_r, 8);\n'
                '    printf("%llu\\n", (unsigned long long) _u);')
    elif c["ret"] == "float":
        body = (f'    float _r = {c["func"]}({parse});\n'
                '    uint32_t _u; memcpy(&_u, &_r, 4);\n'
                '    printf("%u\\n", (unsigned) _u);')
    else:
        body = f'    printf("%lld\\n", (long long) {c["func"]}({parse}));'
    drv = f"""#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
extern {ret_c} {c['func']}({params});
int main(int argc, char **argv) {{
{body}
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
    # getArgs and the bit-cast helpers must be imported with the other imports,
    # before any definition.
    src = src.replace(
        "import Data.Bits",
        "import Data.Bits\n"
        "import System.Environment (getArgs)\n"
        "import GHC.Float (castFloatToWord32, castDoubleToWord64)", 1)
    # Each argument is read at the type its position expects: a float/double
    # parameter reads as Float/Double (single decimal->IEEE rounding, matching
    # the C strtof/strtod), an integer parameter as Integer then fromIntegral
    # into its sized IntN. A nullary LLVM function takes a `()`.
    def arg_expr(n, t):
        if t == "float":
            return f"(read (as!!{n}) :: Float)"
        if t == "double":
            return f"(read (as!!{n}) :: Double)"
        return f"(fromIntegral (read (as!!{n}) :: Integer))"
    argv = (" ".join(arg_expr(n, t) for n, t in enumerate(c["args"]))
            if c["args"] else "()")
    call = f"{c['func']} {argv}"
    if c["ret"] == "void":
        # `f ()` has type () here; its Show instance prints "()" -- the same
        # marker native emits. Comparing these certifies compile-and-run.
        result = f"print ({call})"
    elif c["ret"] == "double":
        # Same raw IEEE bit pattern the native side prints.
        result = f"print (castDoubleToWord64 ({call}))"
    elif c["ret"] == "float":
        result = f"print (castFloatToWord32 ({call}))"
    else:
        result = f"print (fromIntegral ({call}) :: Integer)"
    src += (
        "\nmain :: IO ()\n"
        "main = do\n"
        "  as <- getArgs\n"
        f"  {result}\n"
    )
    open(hs, "w").write(src)
    exe = os.path.join(workdir, "pipeline")
    r = sh(["stack", "ghc", "--", "-v0", "-O0", hs, "-o", exe], cwd=ROOT)
    if r.returncode != 0 or not os.path.exists(exe):
        raise RuntimeError("ghc failed:\n" + r.stderr + r.stdout)
    return exe


def run_out(exe, argtuple):
    """Run the executable and return its stdout (stripped)."""
    r = sh([exe, *[str(a) for a in argtuple]])
    if r.returncode != 0:
        raise RuntimeError(f"{exe} {argtuple} exited {r.returncode}: {r.stderr}")
    return r.stdout.strip()


def run_int(exe, argtuple):
    return int(run_out(exe, argtuple))


def run_case(c, trials, rng):
    # ``void`` has no value to compare, so we compare stdout markers as strings;
    # a match means both sides built and ran. Otherwise compare integer results.
    runner = run_out if c["ret"] == "void" else run_int
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
            n = runner(native, args)
            p = runner(pipeline, args)
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
