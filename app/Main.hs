module Main where

import Math.Expression
import Math.Rules

main :: IO ()
main = do
    let expr = Exists (Variable 0)
    let expr' = use (apply excludedMiddle (Rule (const expr))) SoTrue
    print expr'
