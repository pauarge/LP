-- TODO: Afegir comentaris!
--       Eliminar parèntesis innecessaris


tabulate :: Int -> String
tabulate n = take (2*n) (cycle "  ")


type Ident = String

data Command a = Assign Ident a | Input Ident | Print Ident | 
                 Empty Ident | Push Ident a | Pop Ident Ident | 
                 Size Ident Ident | Seq [Command a] | 
                 Cond (BExpr a) (Command a) | 
                 Loop (BExpr a) (Command a)

data BExpr a = AND (BExpr a) (BExpr a) | OR (BExpr a) (BExpr a) | 
               NOT (BExpr a) | Gt (NExpr a) (NExpr a) | 
               Eq (NExpr a) (NExpr a)

data NExpr a = Var Ident | Const a | Plus (NExpr a) (NExpr a) | 
               Minus (NExpr a) (NExpr a) | Times (NExpr a) (NExpr a)
                                

instance Show a => Show (Command a) where
    show a = showCommand a 0

showCommand::Show a => Command a -> Int -> String
showCommand (Assign i a) t = (tabulate t) ++ (show i) ++ " := " ++ (show a)
showCommand (Input i) t = (tabulate t) ++ "INPUT " ++ (show i)
showCommand (Print i) t = (tabulate t) ++ "PRINT " ++ (show i)
showCommand (Empty i) t = (tabulate t) ++ "EMPTY " ++ (show i)
showCommand (Push i a) t = (tabulate t) ++ "PUSH " ++ (show i) ++ " " ++ (show a)
showCommand (Pop i j) t = (tabulate t) ++ "POP " ++ (show i) ++ " " ++ (show j)
showCommand (Size i j) t = (tabulate t) ++ "SIZE " ++ (show i) ++ " " ++ (show j)
showCommand (Seq []) t = ""
showCommand (Seq (c:cs)) t = (tabulate t) ++ (show c) ++ "\n" ++ (show cs) 
showCommand (Cond b c) t = (tabulate t) ++ (show b) ++ (showCommand c 1)
showCommand (Loop b c) t = (tabulate t) ++ (show b) ++ (showCommand c 1)


instance Show a => Show (BExpr a) where
    show a = showBExpr a 0

showBExpr::Show a => BExpr a -> Int -> String
showBExpr (AND a b) t = (tabulate t) ++ (show a) ++ " AND " ++ (show b)
showBExpr (OR a b) t = (tabulate t) ++ (show a) ++ " OR " ++ (show b)
showBExpr (NOT a) t = (tabulate t) ++ "NOT " ++ (show a)
showBExpr (Gt x y) t = (tabulate t) ++ (show x) ++ " > " ++ (show y)
showBExpr (Eq x y) t = (tabulate t) ++ (show x) ++ " = " ++ (show y)


instance Show a => Show (NExpr a) where
    show a = showNExpr a 0

showNExpr::Show a => NExpr a -> Int -> String
showNExpr (Var a) t = (tabulate t) ++ (show a)
showNExpr (Const a) t = (tabulate t) ++ (show a)
showNExpr (Plus x y) t = (tabulate t) ++ (show x) ++ " + " ++ (show y)
showNExpr (Minus x y) t = (tabulate t) ++ (show x) ++ " - " ++ (show y)
showNExpr (Times x y) t = (tabulate t) ++ (show x) ++ " * " ++ (show y)
