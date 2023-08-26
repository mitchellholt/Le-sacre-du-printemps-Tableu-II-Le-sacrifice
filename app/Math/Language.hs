module Math.Language where

import qualified Data.Map as Map

import Math.Expression

data Value
    = ValueRule ( forall r. RR r )
    | ValueExpr ( forall b a. Expr b a )

data Term
    = Literal Value
    | Bound String
    | Apply Term Term

data Lang 
    = Let String Term
    | Proof Term Term


parse :: String -> Either String [Lang]
parse = undefined

tablify :: [Lang] -> (Map.Map String Term)
tablify = undefined

evaluate :: Map.Map String Term -> Map.Map String Value
evaluate = undefined

lookup :: Map.Map String Value -> Term -> Value
lookup = undefined

verify :: (Map.Map String Value) -> [Lang] -> Bool
verify = undefined