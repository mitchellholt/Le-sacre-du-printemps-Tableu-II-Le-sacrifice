{-# LANGUAGE GADTs #-}

module Math.Language where

import qualified Data.Map as Map

import Data.Typeable

import Math.Expression

class IsRule a where
    toRule_ :: a r -> RR r

class IsExpr e where
    toExpr_ :: e b a -> Expr b a

instance IsRule RR where
    toRule_ = id

instance IsExpr Expr where
    toExpr_ = id

data Value where
    ValueRule :: forall a r. (Typeable a, IsRule r) => r a -> Value
    ValueExpr :: forall b a e. (Typeable b, Typeable a, IsExpr e) => e b a -> Value

toExpr :: forall b a. (Typeable b, Typeable a) => Value -> Either String (Expr b a)
toExpr (ValueExpr e_) =
        case (cast (toExpr_ e_) :: Maybe (Expr b a) ) of
            (Just e) -> Right e
            _ -> Left "Type missmatch"
toExpr _ = Left "Expected a expression, got a rule"

fromExpr :: forall b a. (Typeable b, Typeable a) => Expr b a -> Value
fromExpr = ValueExpr @b @a @Expr


toRule :: forall r. (Typeable r) => Value -> Either String (RR r)
toRule (ValueRule r_) = 
        case (cast (toRule_ r_) :: Maybe (RR r)) of
            (Just r) -> Right r
            _ -> Left "Type missmatch"
toRule _ = Left "Expected a rule, got a expression"

fromRule :: forall r. (Typeable r) => RR r -> Value
fromRule r = ValueRule @r @RR r


meme :: Value
meme = fromExpr ( Exists (Variable 0) )

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
