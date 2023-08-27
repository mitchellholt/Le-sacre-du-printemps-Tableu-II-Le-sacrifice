module Main where

import Math.Language
import Math.Expression
import Math.Rules
import Math.Parsers
-- for testing
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L

main :: IO ()
main = do
    let expr' = Exists (Variable 0)
    let expr'' = use (apply excludedMiddle (Rule (const expr'))) SoTrue
    print expr''
