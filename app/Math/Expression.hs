

data Expr a where
    And :: Expr Bool -> Expr Bool -> Expr 
