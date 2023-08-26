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


newbind :: Expr b a -> Expr b a
newbind (Variable n) = Variable (n+1)
newbind (And a b) = And (newbind a) (newbind b)
newbind (Or a b) = Or (newbind a) (newbind b)
newbind (Not a) = Not (newbind a)
newbind (Forall a) = Forall (newbind a)
newbind (Exists a) = Exists (newbind a)
newbind (SoTrue) = SoTrue
newbind (Construct a) = Construct (newbind a)
newbind (ElementOf a b) = ElementOf (newbind a) (newbind b)
newbind (Equals a b) = Equals (newbind a) (newbind b)
newbind (Lift a) = Lift a

newtype RRtoRR a = RRtoRR a
type (-->) a b = RRtoRR (a,b)
newtype AtoA a = AtoA a
type (-+>) a b = AtoA (a,b)
type E a = AtoA (a,a)

data RR a where
    Rule :: (a -> b) -> RR (a -+> b)
    Builder :: (RR a -> RR b) -> RR (a --> b)


sametobothsides :: RR ( E (Expr _ a) --> E (Expr (_, _, a) Bool) )
sametobothsides = Builder 
                    ( \(Rule (f :: Expr _ a -> Expr _ a)) ->
                        Rule ( \case
                                Equals a b -> Equals (f a) (f b)
                                _ -> error "must be applied to equality"
                            )
                    )


introduceForall :: RR (Expr b Bool -+> Expr (b, Bool) Bool )
introduceForall = Rule (Forall . newbind)


-- meme = 


-- I'm thinking this is gonna land somewhere logic programmy

-- even = Construct x ( Exists n : x = 2*n )
