{-# LANGUAGE GADTs #-}

module Math.Expression where

import Data.Natural

data Set a = Set (a -> Bool)

-- type (-->) a b = (a,b)

data Expr a where
    And :: Expr Bool -> Expr Bool -> Expr Bool
    Or :: Expr Bool -> Expr Bool -> Expr Bool
    Not :: Expr Bool -> Expr Bool
    Forall :: Expr Bool -> Expr Bool -- binds
    Exists :: Expr Bool -> Expr Bool -- binds
    Construct :: Expr Bool -> Expr (Set a) -- binds
    ElementOf :: Expr a -> Expr (Set a) -> Expr Bool

    -- each of the binds increments these fellas,
    -- that way the nat is kinda like the scope
    Variable :: Natural -> Expr a 

    Lift :: a -> Expr a -- illegal cheese


-- I'm thinking this is gonna land somewhere logic programmy

-- even = Construct x ( Exists n : x = 2*n )