{-# LANGUAGE GADTs #-}
module Math.Parsers where

import Data.Void
import Control.Monad (void)
import Math.Language
import Math.Expression
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char


type Parser = Parsec Void String


symbol :: String -> Parser ()
symbol p = optional space' *> tok p <* optional space'
    where
        space' = L.space space1 lineComment blockComment

        tok :: String -> Parser ()
        tok = void . chunk


lineComment :: Parser ()
lineComment = start >> rest
    where
        start :: Parser ()
        start = void (chunk "--")

        rest :: Parser ()
        rest = void (takeWhileP (Just "comment") (/= '\n'))


blockComment :: Parser ()
blockComment = start >> middle >> tryEnd
    where
        start :: Parser ()
        start = void (chunk "{-")

        middle :: Parser ()
        middle = void (takeWhileP Nothing (/= '-'))

        tryEnd :: Parser ()
        tryEnd = (single '-' >> (void (try (single '}')) <|> (middle >> tryEnd)))
            <?> "end of commment"


-- TODO maybe extend to include construct, elementOf, and equals
expr :: Parser (Expr a b) 
expr = andExpr


orExpr :: Parser (Expr a b)
orExpr = flip (<|>) andExpr $ try $ do
    e1 <- andExpr
    symbol "||"
    e2 <- orExpr
    return (Or e1 e2)
    

andExpr :: Parser (Expr a b)
andExpr = undefined


notExpr :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ String) => m Value
notExpr = undefined


forallExpr :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ String) => m Value
forallExpr = undefined


existsExpr :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ String) => m Value
existsExpr = undefined


variableExpr :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ String) => m Value
variableExpr = undefined


soTrueExpr :: (MonadParsec e s m, Token s ~ Char, Tokens s ~ String) => m Value
soTrueExpr = fromExpr SoTrue <$ char 'T'
