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


-- TODO maybe extend to include construct, elementOf, and equals
expr :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ String) => m Value
expr = choice
    [
        andExpr,
        orExpr,
        notExpr,
        forallExpr,
        existsExpr,
        fromExpr SoTrue <$ char 'T',
        variableExpr
    ]


andExpr :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ String) => m Value
andExpr = undefined
orExpr :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ String) => m Value
orExpr = undefined
notExpr :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ String) => m Value
notExpr = undefined
forallExpr :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ String) => m Value
forallExpr = undefined
existsExpr :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ String) => m Value
existsExpr = undefined
variableExpr :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ String) => m Value
variableExpr = undefined
