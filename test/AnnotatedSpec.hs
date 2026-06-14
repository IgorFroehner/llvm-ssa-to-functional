-- | Spec for docs/roadmap/plans/04-annotated-anf-ast.md: the annotated ANF AST
-- and the backend seam. Because item 04 is a behaviour-preserving refactor, the
-- assertions target the two theorems that pin it down rather than any new
-- generated feature:
--
--   * T1 (annotation transparency / naturality, §2.4): the Haskell backend
--     factors through annotation erasure — relabelling never changes its output.
--   * T2 (pipeline factorisation, §2.4): inserting the (currently trivial)
--     effect pass is observationally invisible on the Haskell backend.
--
-- plus the proof that the seam is real (a second backend that /reads/ the label
-- the Haskell backend ignores) and that the effect lattice obeys its laws.
module AnnotatedSpec (spec) where

import Test.Hspec
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (isInfixOf)

import Lexer (runAlex)
import Parser (parseLLVMIR)
import Translate (translate)
import qualified Anf
import Effect (Effect(..), annotate)
import Backend (render)
import PrintAnf (printProgram, haskellBackend)
import AnnotDump (annotDumpBackend, dumpProgram)

-- | Parse a snippet to the bare (@()@-annotated) ANF term.
parseAnf :: String -> Anf.Program ()
parseAnf src = case runAlex (BL.pack src) parseLLVMIR of
  Left err  -> error ("parse failed: " ++ err)
  Right ast -> translate ast

-- A loop with a φ-carried accumulator: exercises blocks, bindings, a conditional
-- branch and tail calls — i.e. every node kind the annotation threads through.
loopExample :: String
loopExample = unlines
  [ "define i32 @sum_to(i32 %n) {"
  , "  br label %loop"
  , "loop:"
  , "  %acc = phi i32 [ 0, %0 ], [ %acc2, %loop ]"
  , "  %i = phi i32 [ 0, %0 ], [ %i2, %loop ]"
  , "  %acc2 = add i32 %acc, %i"
  , "  %i2 = add i32 %i, 1"
  , "  %c = icmp slt i32 %i2, %n"
  , "  br i1 %c, label %loop, label %done"
  , "done:"
  , "  ret i32 %acc2"
  , "}"
  ]

spec :: Spec
spec = do
  describe "annotated ANF / backend seam" $ do
    let prog = parseAnf loopExample

    describe "T1 — annotation transparency (Haskell backend ignores labels)" $ do
      it "renders the annotated tree exactly as the bare tree" $
        printProgram (annotate prog) `shouldBe` printProgram prog

      it "is invariant under an arbitrary relabelling (naturality)" $
        -- relabel every node to a String annotation; output must not move.
        printProgram (fmap (const "X") prog) `shouldBe` printProgram prog

      it "factors through erasure: render . erase = render" $
        printProgram (Anf.erase (annotate prog)) `shouldBe` printProgram (annotate prog)

    describe "T2 — pipeline factorisation (inserting the effect pass is inert)" $
      it "new (render . annotate . translate) equals old (printProgram . translate)" $
        render haskellBackend (annotate prog) `shouldBe` printProgram prog

    describe "the seam is real — a second backend reads what Haskell ignores" $ do
      it "produces output different from the Haskell backend on the same tree" $
        render annotDumpBackend (annotate prog) `shouldNotBe` render haskellBackend (annotate prog)

      it "surfaces the effect label that the Haskell backend never prints" $ do
        dumpProgram (annotate prog) `shouldSatisfy` ("effect: Pure" `isInfixOf`)
        printProgram (annotate prog) `shouldNotSatisfy` ("Pure" `isInfixOf`)

    describe "effect baseline — the pure subset infers as effect-free" $
      it "labels every node Pure (foldMap over the Foldable instance)" $
        -- every annotation in the whole tree is the lattice bottom.
        all (== Pure) (annotate prog) `shouldBe` True

  describe "effect lattice laws (join-semilattice)" $ do
    it "is associative" $
      (Pure <> Pure) <> Pure `shouldBe` Pure <> (Pure <> Pure)
    it "has mempty as identity" $ do
      mempty <> Pure `shouldBe` Pure
      Pure <> mempty `shouldBe` Pure
