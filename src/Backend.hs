-- | The output-backend interface. A backend is a way of rendering an analysed
-- 'Anf.Program' to text; the translator and the effect pass are fixed, and the
-- backend is the interchangeable last stage.
--
-- Formally each backend is an /algebra/ over the ANF term: a structurally
-- recursive map @μA → String@. The Haskell printer ("PrintAnf") is one such
-- algebra (its hand-written recursion /is/ the fold); the monadic printer of
-- [item 06](docs/roadmap/06-monadic-effects-translation.md) will be another over
-- the same term. Adding a backend never touches the term or the other
-- backends — the open/closed property this seam buys. See
-- docs/roadmap/plans/04-annotated-anf-ast.md §2.3.
--
-- A backend is represented as a /record of functions/ rather than a type class
-- so that the set of available backends is an ordinary, heterogeneous value
-- ('backends'-style menus, selection by name) with no existential machinery.
--
-- Every backend receives the /annotated/ tree ('Anf.Program' 'Effect') so the
-- pipeline is single and uniform. An annotation-blind backend (the Haskell one)
-- simply ignores the labels; an annotation-aware backend (the monadic one, or
-- the debugging 'AnnotDump') reads them.
module Backend
  ( Backend(..)
  , runBackend
  , lookupBackend
  ) where

import qualified Anf
import Effect (Effect)

-- | A renderable output target: a name to select it by, and the rendering
-- algebra itself.
data Backend = Backend
  { backendName :: String
  , render      :: Anf.Program Effect -> String
  }

-- | Render with the backend identified by @name@, drawn from a menu. Returns
-- 'Left' with the offending name if it is unknown.
runBackend :: [Backend] -> String -> Anf.Program Effect -> Either String String
runBackend backends name prog =
  case lookupBackend backends name of
    Just b  -> Right (render b prog)
    Nothing -> Left name

-- | Find a backend by its 'backendName'.
lookupBackend :: [Backend] -> String -> Maybe Backend
lookupBackend backends name =
  case filter ((== name) . backendName) backends of
    (b:_) -> Just b
    []    -> Nothing
