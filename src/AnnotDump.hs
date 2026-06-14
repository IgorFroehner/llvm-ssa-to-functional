-- | A debugging backend that dumps the /annotated/ ANF tree, effect labels
-- included. It exists to demonstrate — and let the test-suite assert — that a
-- backend other than the Haskell printer can read the annotation the Haskell
-- printer ignores. It is the concrete witness that "PrintAnf is one of several
-- backends" is now true (docs/roadmap/plans/04-annotated-anf-ast.md §6), and it
-- doubles as an inspection aid for [item 05](docs/roadmap/05-effect-inference.md)
-- once effect inference makes the labels non-trivial.
--
-- The renderer is polymorphic over any 'Show'-able annotation, so it works on
-- the @()@ tree straight out of "Translate" as well as the @Effect@ tree out of
-- "Effect.annotate"; the backend value is the @Effect@ instantiation.
module AnnotDump
  ( dumpProgram
  , annotDumpBackend
  ) where

import Anf
import Backend (Backend(..))
import TypeSystem (rho)

import Data.List (intercalate)

-- | The Effect-annotated backend value selectable as @annot-dump@.
annotDumpBackend :: Backend
annotDumpBackend = Backend { backendName = "annot-dump", render = dumpProgram }

-- | Render the annotation-bearing structure of a program. Unlike the Haskell
-- backend this is not executable code; it surfaces the label on each /function/,
-- /block/ and /binding/ — the three granularities effect inference works at
-- (per-function, per-block, per-binding; see plan §3 D1 / open decision 4). Leaf
-- operand nodes ('Value', 'BinOp', 'Call', …) also carry a label, but it is the
-- inert unit\/⊥ and conveys nothing, so it is intentionally not shown.
dumpProgram :: Show a => Program a -> String
dumpProgram (Program fs) = intercalate "\n" (map dumpFunction fs)

dumpFunction :: Show a => Function a -> String
dumpFunction (Function ann name _ argTypes retType lambda _) =
  line 0 ("function " ++ name ++ " :: "
            ++ intercalate " -> " (map rho (argTypes ++ [retType]))
            ++ "    -- effect: " ++ show ann)
    ++ dumpLambda 1 lambda

dumpLambda :: Show a => Int -> Lambda a -> String
dumpLambda d (Lambda ann name _ exprs nested flow) =
  line d ("block " ++ name ++ "    -- effect: " ++ show ann)
    ++ concatMap (dumpExpr (d + 1)) exprs
    ++ concatMap (dumpLambda (d + 1)) nested
    ++ line (d + 1) ("flow: " ++ dumpFlow flow)

dumpExpr :: Show a => Int -> Expr a -> String
dumpExpr d (ExpDecl decl) = dumpDecl d decl

dumpDecl :: Show a => Int -> Decl a -> String
dumpDecl d decl = line d (kindAnn decl)
  where
    kindAnn x = describe x ++ "    -- effect: " ++ show (declAnn x)

-- | The label hung on a binding.
declAnn :: Decl a -> a
declAnn (DeclBinOp ann _ _ _) = ann
declAnn (DeclCall ann _ _ _)  = ann
declAnn (DeclIcmp ann _ _ _)  = ann
declAnn (DeclSelect ann _ _ _) = ann
declAnn (DeclConvOp ann _ _)  = ann
declAnn (DeclFreeze ann _ _ _) = ann

-- | A one-line summary of a binding (name and kind), label-free.
describe :: Decl a -> String
describe (DeclBinOp _ n ty _)  = n ++ " = binop :: " ++ rho ty
describe (DeclCall _ n ty _)   = n ++ " = call :: " ++ rho ty
describe (DeclIcmp _ n ty _)   = n ++ " = icmp :: " ++ rho ty
describe (DeclSelect _ n ty _) = n ++ " = select :: " ++ rho ty
describe (DeclConvOp _ n _)    = n ++ " = convop"
describe (DeclFreeze _ n ty _) = n ++ " = freeze :: " ++ rho ty

dumpFlow :: Flow a -> String
dumpFlow (FlowCall _) = "tail-call"
dumpFlow (FlowCond _) = "if/then/else"

line :: Int -> String -> String
line d s = replicate (d * 2) ' ' ++ s ++ "\n"
