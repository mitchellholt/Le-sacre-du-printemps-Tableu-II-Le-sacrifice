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


type (-->) = (,)
data RR a where
    Rule :: (a -> a) -> RR a
    Builder :: (RR a -> RR b) -> RR (a --> b)


-- Apply a rewrite to an expression
use :: RR a -> Expr b a -> Expr b a
use rule = \case
    --And :: Expr b Bool -> Expr c Bool -> Expr (b,c,Bool) Bool
    And p q -> And (use rule p) (use rule q)
    -- Or :: Expr b Bool -> Expr c Bool -> Expr (b,c,Bool) Bool
    Or p q -> Or (use rule p) (use rule q)
    -- Not :: Expr b Bool -> Expr (b, Bool) Bool
    Not p -> Not (use rule p)
    -- Forall :: Expr b Bool -> Expr (b, Bool) Bool -- binds
    Forall p -> Forall (use rule p)
    -- Exists :: Expr b Bool -> Expr (b, Bool) Bool -- binds
    Exists p -> Exists (use rule p)
    -- SoTrue :: Expr () Bool
    --Variable :: Natural -> Expr () a 
    --Construct :: Expr b Bool -> Expr (b, Bool) (Set a) -- binds
    --ElementOf :: Expr b a -> Expr c (Set a) -> Expr (b, c, Set a) Bool
    --Equals :: Expr b a -> Expr c a -> Expr (b, c, a) Bool
    --Lift :: a -> Expr () a -- illegal cheese


sametobothsides :: RR (Expr _ a --> Expr (_, _, a) Bool)
sametobothsides = Builder 
                    ( \(Rule (f :: Expr _ a -> Expr _ a)) ->
                        Rule ( \case
                            Equals a b -> Equals (f a) (f b)
                            _ -> error "must be applied to equality"
                        )
                    )


-- introduceForall :: RR (Expr b Bool --> Expr  Bool)

-- meme = 


-- I'm thinking this is gonna land somewhere logic programmy

-- even = Construct x ( Exists n : x = 2*n )
