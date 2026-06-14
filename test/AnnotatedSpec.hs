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
--
-- T1/T2 and the all-'Pure' baseline are checked twice: once corpus-wide over
-- every @examples/*.ll@ (so every node kind the project accepts — calls,
-- selects, conversions, freezes, floats, unit returns, … — is exercised), and
-- once on a single readable in-line fixture that documents the properties
-- self-containedly.
module AnnotatedSpec (spec) where

import Test.Hspec
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension)
import Data.List (isInfixOf)
import Control.Monad (forM_)

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
parseAnf src = case runAlex (BLC.pack src) parseLLVMIR of
  Left err  -> error ("parse failed: " ++ err)
  Right ast -> translate ast

-- | Parse a corpus file to the bare ANF term.
parseAnfFile :: FilePath -> IO (Anf.Program ())
parseAnfFile path = do
  s <- BL.readFile path
  case runAlex s parseLLVMIR of
    Left err  -> error ("parse failed for " ++ path ++ ": " ++ err)
    Right ast -> return (translate ast)

-- | Every @.ll@ under @examples/@ (the certified corpus).
corpusFiles :: FilePath -> IO [FilePath]
corpusFiles dir = do
  entries <- listDirectory dir
  return [ dir </> e | e <- entries, takeExtension e == ".ll" ]

-- | The three annotation invariants, asserted on one program. Bundled so the
-- corpus group stays one assertion per file.
invariantsHold :: Anf.Program () -> Expectation
invariantsHold prog = do
  -- T1: the Haskell backend renders the annotated tree exactly as the bare one.
  printProgram (annotate prog) `shouldBe` printProgram prog
  -- T1, naturality: invariant under an *arbitrary* relabelling, not just Pure.
  printProgram (fmap (const ("X" :: String)) prog) `shouldBe` printProgram prog
  -- T2: new pipeline (render . annotate) == old pipeline (printProgram).
  render haskellBackend (annotate prog) `shouldBe` printProgram prog
  -- Baseline: the pure subset labels every node with the lattice bottom.
  all (== Pure) (annotate prog) `shouldBe` True

-- A representative function used for the readable, self-contained theorem checks:
-- a φ-carried loop covering blocks, φ-args, arithmetic, an icmp, a conditional
-- branch, tail calls and a return. Exhaustive node-kind coverage of T1/T2 comes
-- from the corpus-wide group, not from this fixture.
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
  describe "T1/T2 hold corpus-wide (every accepted node kind)" $ do
    files <- runIO (corpusFiles "examples")
    forM_ files $ \path -> do
      prog <- runIO (parseAnfFile path)
      it ("preserves output and infers all-Pure: " ++ path) $
        invariantsHold prog

  describe "annotated ANF / backend seam (readable fixture)" $ do
    let prog = parseAnf loopExample

    describe "T1 — annotation transparency (Haskell backend ignores labels)" $ do
      it "renders the annotated tree exactly as the bare tree" $
        printProgram (annotate prog) `shouldBe` printProgram prog

      it "is invariant under an arbitrary relabelling (naturality)" $
        printProgram (fmap (const ("X" :: String)) prog) `shouldBe` printProgram prog

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

  describe "effect lattice laws (join-semilattice)" $ do
    it "is associative" $
      (Pure <> Pure) <> Pure `shouldBe` Pure <> (Pure <> Pure)
    it "has mempty as identity" $ do
      mempty <> Pure `shouldBe` Pure
      Pure <> mempty `shouldBe` Pure
