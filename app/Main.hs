module Main where

import Math.Expression
import Math.Rules
import Math.Parsers
import Text.Megaparsec -- for testing

main :: IO ()
main = do
    let expr = Exists (Variable 0)
    let expr' = use (apply excludedMiddle (Rule (const expr))) SoTrue
    print $ use (apply lhs (apply excludedMiddle (Rule $ const SoTrue))) (expr')
