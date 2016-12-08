-- TODO: Afegir comentaris!
--       Eliminar parèntesis innecessaris


tabulate :: Int -> String
tabulate n = take (2*n) (cycle "  ")


type Ident = String

data Command a = Assign Ident a | Input Ident | Print Ident | 
                 Empty Ident | Push Ident a | Pop Ident Ident | 
                 Size Ident Ident | Seq [Command a] | 
                 Cond (BExpr a) (Command a) (Command a) | 
                 Loop (BExpr a) (Command a)
                 deriving (Read)

data BExpr a = AND (BExpr a) (BExpr a) | OR (BExpr a) (BExpr a) | 
               NOT (BExpr a) | Gt (NExpr a) (NExpr a) | 
               Eq (NExpr a) (NExpr a)
               deriving (Read)

data NExpr a = Var Ident | Const a | Plus (NExpr a) (NExpr a) | 
               Minus (NExpr a) (NExpr a) | Times (NExpr a) (NExpr a)
               deriving (Read)
                                

instance Show a => Show (Command a) where
    show a = showCommand a 0

showCommand::Show a => Command a -> Int -> String
showCommand (Assign i a) t = (tabulate t) ++ i ++ " := " ++ (show a) ++ "\n"
showCommand (Input i) t = (tabulate t) ++ "INPUT " ++ i ++ "\n"
showCommand (Print i) t = (tabulate t) ++ "PRINT " ++ i ++ "\n"
showCommand (Empty i) t = (tabulate t) ++ "EMPTY " ++ i ++ "\n"
showCommand (Push i a) t = (tabulate t) ++ "PUSH " ++ i ++ " " ++ (show a) ++ "\n"
showCommand (Pop i j) t = (tabulate t) ++ "POP " ++ i ++ " " ++ j ++ "\n"
showCommand (Size i j) t = (tabulate t) ++ "SIZE " ++ i ++ " " ++ j ++ "\n"
showCommand (Seq c) t = (tabulate t) ++ (show $ concatMap show c) ++ "\n"
showCommand (Cond b c0 c1) t = (tabulate t) ++ "IF " ++ (show b) ++ " THEN " ++ 
                               (showCommand c0 1) ++ " ELSE " ++ (showCommand c1 1) ++ "\n"
showCommand (Loop b c) t = (tabulate t) ++ (show b) ++ (showCommand c 1) ++ "\n"


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


data SymTable a = ST [(Ident, Either a [a])]
                  deriving (Show)

setVar :: SymTable a -> Ident -> Either a [a] -> SymTable a
setVar (ST xs) i a = ST ([(i, a)] ++ clearList xs i)
  where
    clearList [] _ = []
    clearList (x:xs) i
      | fst x == i = clearList xs i
      | otherwise = x : clearList xs i


getVar :: SymTable a -> Ident -> Maybe (Either a [a])
getVar (ST xs) i = lookup i xs

--class Evaluable e where
  --eval :: (Num a, Ord a) => (Ident -> Maybe a) -> (e a) -> (Either String a)

  --typeCheck :: (Ident -> String) -> (e a) -> Bool

--interpretCommand :: (Num a, Ord a) => SymTable a -> [a] -> Command a -> ((Either String [a]),SymTable a, [a])

--interpretProgram :: (Num a, Ord a) => [a] -> Command a -> (Either String [a])