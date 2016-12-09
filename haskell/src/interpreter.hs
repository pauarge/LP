-- TODO: Afegir comentaris!
--       Eliminar parèntesis innecessaris
--       Separar en subseccions
--       Typechecks on commands
--       Evaluables


tabulate :: Int -> String
tabulate n = take (2*n) (cycle "  ")


type Ident = String

data Command a = Assign Ident (NExpr a) | Input Ident | Print Ident | 
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

showCommand :: Show a => Command a -> Int -> String
showCommand (Assign i exp) t = (tabulate t) ++ i ++ " := " ++ (show exp) ++ "\n"
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

showBExpr :: Show a => BExpr a -> Int -> String
showBExpr (AND a b) t = (tabulate t) ++ (show a) ++ " AND " ++ (show b)
showBExpr (OR a b) t = (tabulate t) ++ (show a) ++ " OR " ++ (show b)
showBExpr (NOT a) t = (tabulate t) ++ "NOT " ++ (show a)
showBExpr (Gt x y) t = (tabulate t) ++ (show x) ++ " > " ++ (show y)
showBExpr (Eq x y) t = (tabulate t) ++ (show x) ++ " = " ++ (show y)


instance Show a => Show (NExpr a) where
    show a = showNExpr a 0

showNExpr :: Show a => NExpr a -> Int -> String
showNExpr (Var id) t = (tabulate t) ++ (show id)
showNExpr (Const c) t = (tabulate t) ++ (show c)
showNExpr (Plus x y) t = (tabulate t) ++ (show x) ++ " + " ++ (show y)
showNExpr (Minus x y) t = (tabulate t) ++ (show x) ++ " - " ++ (show y)
showNExpr (Times x y) t = (tabulate t) ++ (show x) ++ " * " ++ (show y)


data SymTable a = ST [(Ident, Either a [a])]
                  deriving (Show)

setVar :: SymTable a -> Ident -> Either String a -> SymTable a
setVar t _ (Left _) = t  
setVar (ST xs) i (Right x) = ST ([(i, Left x)] ++ clearList xs i)
    where
        clearList [] _ = []
        clearList (x:xs) i
            | fst x == i = xs
            | otherwise = x : clearList xs i


getVar :: SymTable a -> Ident -> Maybe a
getVar (ST xs) i = getTop (lookup i xs)


getTop :: Maybe (Either a [a]) -> Maybe a
getTop (Just (Left x)) = Just x
getTop (Just (Right (x:xs))) = Just x
getTop Nothing = Nothing


class Evaluable e where
    eval :: (Num a, Ord a) => (Ident -> Maybe a) -> (e a) -> Either String a
    typeCheck :: (Ident -> String) -> (e a) -> Bool

instance Evaluable BExpr where
    eval f (AND a b) = applyOp (*) (eval f a) (eval f b)
    -- TODO: Error handling on OR
    eval f (OR a b)
        | eval f a == Right 1 || eval f b == Right 1 = Right 1
        | otherwise = Right 0   
    -- TODO: Better error handling on NOT
    eval f (NOT a)
        | eval f a == Right 0 = Right 1
        | eval f a == Right 1 = Right 0
        | otherwise = Left "Error :("
    eval f (Gt x y) = castBool (applyOp (>) (eval f x) (eval f y))
    eval f (Eq x y) = castBool (applyOp (==) (eval f x) (eval f y))

    typeCheck f (AND a b) = (typeCheck f a) && (typeCheck f b)
    typeCheck f (OR a b) = (typeCheck f b) && (typeCheck f b)
    typeCheck f (Gt x y) = (typeCheck f x) && (typeCheck f y)
    typeCheck f (Eq x y) = (typeCheck f x) && (typeCheck f y)


instance Evaluable NExpr where
    eval f (Var id) = case f id of
        Just x -> Right x
        _ -> Left ("Could not find " ++ id)
    eval _ (Const a) = Right a
    eval f (Plus x y) = applyOp (+) (eval f x) (eval f y)
    eval f (Minus x y) = applyOp (-) (eval f x) (eval f y)
    eval f (Times x y) = applyOp (*) (eval f x) (eval f y)

    typeCheck f (Var id) = if f id == "single" then True else False
    typeCheck f (Const a) = True
    typeCheck f (Plus x y) = (typeCheck f x) && (typeCheck f y)
    typeCheck f (Minus x y) = (typeCheck f x) && (typeCheck f y)
    typeCheck f (Times x y) = (typeCheck f x) && (typeCheck f y)


castBool (Right True) = Right 1
castBool (Right False) = Right 0
castBool (Left err) = Left err

applyOp f (Right x) (Right y) = Right (f x y)
applyOp f (Right _) (Left e) = Left e
applyOp f (Left e) (Right _) = Left e
applyOp f (Left e0) (Left e1) = Left (e0 ++ " " ++ e1)


getTypeCheck :: Maybe (Either a [a]) -> String
getTypeCheck (Just (Left _)) = "single"
getTypeCheck (Just (Right _)) = "stack"
getTypeCheck Nothing = "invalid"


getPrintable :: Maybe a -> Either String [a]
getPrintable (Just x) = Right [x]
getPrintable Nothing = Left "Could not print that."


interpretCommand :: (Num a, Ord a) => SymTable a -> [a] -> Command a -> ((Either String [a]), SymTable a, [a])
interpretCommand t inp (Assign i exp) = (Right [], setVar t i (eval (getVar t) exp), inp)
interpretCommand t (x:xs) (Input i) = (Right [], setVar t i (Right x), xs)
interpretCommand t inp (Print i) = (getPrintable (getVar t i), t, inp)
--interpretCommand t inp@(x:xs) (Empty i) = (Right inp, t, inp)
--interpretCommand t inp@(x:xs) (Push i a) = (Right inp, t, inp)
--interpretCommand t inp@(x:xs) (Pop i a) = (Right inp, t, inp)
--interpretCommand t inp@(x:xs) (Size i a) = (Right inp, t, inp)
--interpretCommand t inp@(x:xs) (Seq i a) = (Right inp, t, inp)
--interpretCommand t inp@(x:xs) (Cond i a) = (Right inp, t, inp)
--interpretCommand t inp@(x:xs) (Loop i a) = (Right inp, t, inp)


--interpretProgram :: (Num a, Ord a) => [a] -> Command a -> (Either String [a])