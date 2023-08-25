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

    -- each of the binds increments these fellas,
    -- that way the nat is kinda like the scope
    Variable :: Natural -> Expr a 

    Construct :: Expr Bool -> Expr (Set a) -- binds
    ElementOf :: Expr a -> Expr (Set a) -> Expr Bool

    Equals :: Expr a -> Expr a -> Expr Bool

    -- not worrying about functions yet, but they might go here.

    Lift :: a -> Expr a -- illegal cheese


-- not set on these guys tbh - defo need to change.
data Rewrite a where
    Rule :: (Expr a -> Expr a) -> Rewrite a
    Builder :: Rewrite a -> Rewrite b

-- sametobothsides :: (a -> b) -> Rewrite a b


-- I'm thinking this is gonna land somewhere logic programmy

-- even = Construct x ( Exists n : x = 2*n )