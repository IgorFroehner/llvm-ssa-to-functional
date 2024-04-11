
module NameGenerator (generateName) where

import Control.Monad.State

type NameGenerator = State Int

generateName :: NameGenerator String
generateName = do
    count <- get
    put (count + 1)
    return $ 'a' : show count

-- Helper function to run the generator
runNameGenerator :: NameGenerator a -> a
runNameGenerator generator = evalState generator 1

-- Example usage within a monadic context
exampleUsage :: NameGenerator [String]
exampleUsage = sequence $ replicate 10 generateName

-- Running the example
main :: IO ()
main = print $ runNameGenerator exampleUsage
