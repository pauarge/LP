data Command a =    Assign Ident a | Input Ident | Print Ident | 
                    Empty Ident | Push Ident a | Pop Ident | 
                    Size Ident | Seq [Command a] | Cond BExpr [Command a] |
                    Loop BExpr [Command a]

data Ident = String
    deriving (Show)

data BExpr = AND | OR | NOT
    deriving (Eq)
