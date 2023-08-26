{-# LANGUAGE GADTs, LambdaCase, PartialTypeSignatures #-}

module Math.Rules where

import Math.Expression
import Data.Natural


sametobothsides :: RR ( R (Expr _ a) --> R (Expr (_, _, a) Bool) )
sametobothsides = Builder 
                    ( \(Rule (f :: Expr _ a -> Expr _ a)) ->
                        Rule ( \case
                                Equals a b -> Equals (f a) (f b)
                                _ -> error "must be applied to equality"
                            )
                    )


introduceForall :: RR (Expr b Bool -+> Expr (b, Bool) Bool )
introduceForall = Rule (Forall . newbind)


excludedMiddle :: RR ( (() -+> Expr b Bool) --> (Expr _ Bool -+> Expr ((b, Bool), b, Bool) Bool) )
excludedMiddle = Builder ( \(Rule f) -> Rule ( \_ -> let e = f () in (Not e) `Or` e ) )

rhs :: RR ( (Expr b1 a -+> Expr b2 a) --> (Expr (z,b1,a) w -+> Expr (z,b2,a) w) )
rhs = Builder 
    ( \( Rule f ) -> Rule ( \case 
        Or x y -> Or x (f y)
        And x y -> And x (f y)
        Equals x y -> Equals x (f y)
        ElementOf x y -> ElementOf x (f y)
     )
    )

lhs :: RR ( (Expr b1 a -+> Expr b2 a) --> (Expr (b1,z,a) w -+> Expr (b2,z,a) w) )
lhs = Builder 
    ( \( Rule f ) -> Rule ( \case 
        Or x y -> Or (f x) y
        And x y -> And (f x) y
        Equals x y -> Equals (f x) y
     )
    )