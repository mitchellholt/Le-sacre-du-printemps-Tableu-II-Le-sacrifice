{-# LANGUAGE GADTs, LambdaCase, PartialTypeSignatures #-}

module Math.Expression where

import Data.Natural

newtype Set a = Set (a -> Bool)

-- type (-->) a b = (a,b)

-- a is outsie type, e.g. Equal :: forall b. Expr b Bool
-- b is the "inside" type, so 5 == 5 :: Expr Int Bool
data Expr b a where
    And :: Expr b Bool -> Expr c Bool -> Expr (b,c,Bool) Bool
    Or :: Expr b Bool -> Expr c Bool -> Expr (b,c,Bool) Bool
    Not :: Expr b Bool -> Expr (b, Bool) Bool
    Forall :: Expr b Bool -> Expr (b, Bool) Bool -- binds
    Exists :: Expr b Bool -> Expr (b, Bool) Bool -- binds
    SoTrue :: Expr () Bool

    -- each of the binds increments these fellas,
    -- that way the nat is kinda like the scope
    Variable :: Natural -> Expr () a

    Construct :: Expr b Bool -> Expr (b, Bool) (Set a) -- binds
    ElementOf :: Expr b a -> Expr c (Set a) -> Expr (b, c, Set a) Bool

    Equals :: Expr b a -> Expr c a -> Expr (b, c, a) Bool

    -- not worrying about functions yet, but they might go here.

    Lift :: a -> Expr () a -- illegal cheese

parens :: String -> String
parens str = "(" ++ str ++ ")"

instance Show (Expr b a) where
    show (And p q) = parens (show p) ++ " & " ++ parens (show q)
    show (Or p q) = parens (show p) ++ " || " ++ parens (show q)
    show (Not p) = "~" ++ parens (show p)
    show (Forall p) = "(A _)" ++ parens (show p)
    show (Exists p) = "(E _)" ++ parens (show p)
    show SoTrue = "T"
    show (Variable n) = parens ("var " ++ show n)
    show (Construct p) = "{ _ | " ++ show p ++ "}"
    show (ElementOf p q) = parens (parens (show p) ++ " in " ++ parens (show q))
    show (Equals p q) = parens (show p) ++ " = " ++ parens (show q)
    show (Lift _) = "illegal cheese"


newbind :: Expr b a -> Expr b a
newbind (Variable n) = Variable (n+1)
newbind (And a b) = And (newbind a) (newbind b)
newbind (Or a b) = Or (newbind a) (newbind b)
newbind (Not a) = Not (newbind a)
newbind (Forall a) = Forall (newbind a)
newbind (Exists a) = Exists (newbind a)
newbind SoTrue = SoTrue
newbind (Construct a) = Construct (newbind a)
newbind (ElementOf a b) = ElementOf (newbind a) (newbind b)
newbind (Equals a b) = Equals (newbind a) (newbind b)
newbind (Lift a) = Lift a


unbind :: Expr b a -> Expr b a
unbind (Variable n) = Variable (n-1)
unbind (And a b) = And (unbind a) (unbind b)
unbind (Or a b) = Or (unbind a) (unbind b)
unbind (Not a) = Not (unbind a)
unbind (Forall a) = Forall (unbind a)
unbind (Exists a) = Exists (unbind a)
unbind SoTrue = SoTrue
unbind (Construct a) = Construct (unbind a)
unbind (ElementOf a b) = ElementOf (unbind a) (unbind b)
unbind (Equals a b) = Equals (unbind a) (unbind b)
unbind (Lift a) = Lift a


newtype RRtoRR a = RRtoRR a
type (-->) a b = RRtoRR (a,b)
newtype AtoA a = AtoA a
type (-+>) a b = AtoA (a,b)
type R a = a -+> a

data RR a where
    Rule :: (a -> b) -> RR (a -+> b)
    Builder :: (RR a -> RR b) -> RR (a --> b)

use :: RR (a -+> b) -> a -> b
use (Rule f) = f


apply :: RR (a --> b) -> RR a -> RR b
apply (Builder f) = f
