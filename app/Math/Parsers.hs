{-# LANGUAGE GADTs #-}
module Math.Parsers where

import Text.Megaparsec
import Data.Void
import qualified Data.Set as Set
import Control.Monad (void)
import Math.Language
import Math.Expression


type Parser = Parsec Void String


char :: MonadParsec e s m => Token s -> m (Token s)
char t = token testToken expected
    where
        testToken x = if x == t then Just x else Nothing
        expected = (Set.singleton . Tokens . pure) t


newline :: (MonadParsec e s m, Token s ~ Char) => m ()
newline = (void . single) '\n'


test :: (MonadParsec e s m, Tokens s ~ String) => m ()
test = void $ chunk "png" <|> chunk "pdf"


expr :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ String) => m Value
expr = choice
    [
        --andExpr,
        --orExpr,
        --notExpr,
        --forallExpr,
        --existsExpr,
        ValueExpr SoTrue <$ char 'T'
    ]
    -- And :: Expr b Bool -> Expr c Bool -> Expr (b,c,Bool) Bool
    -- Or :: Expr b Bool -> Expr c Bool -> Expr (b,c,Bool) Bool
    -- Not :: Expr b Bool -> Expr (b, Bool) Bool
    -- Forall :: Expr b Bool -> Expr (b, Bool) Bool -- binds
    -- Exists :: Expr b Bool -> Expr (b, Bool) Bool -- binds
    -- SoTrue :: Expr () Bool

    -- -- each of the binds increments these fellas,
    -- -- that way the nat is kinda like the scope
    -- Variable :: Natural -> Expr () a

    -- Construct :: Expr b Bool -> Expr (b, Bool) (Set a) -- binds
    -- ElementOf :: Expr b a -> Expr c (Set a) -> Expr (b, c, a, Set a) Bool

    -- Equals :: Expr b a -> Expr c a -> Expr (b, c, a) Bool

    -- -- not worrying about functions yet, but they might go here.

    -- Lift :: a -> Expr () a -- illegal cheese
