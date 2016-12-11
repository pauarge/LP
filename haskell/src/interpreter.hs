import System.IO



-- Types & datatypes declarations

type Ident = String

data Command a = Assign Ident (NExpr a) | Input Ident | Print Ident | 
                 Empty Ident | Push Ident (NExpr a) | Pop Ident Ident | 
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

data SymTable a = SymTable [(Ident, Either a [a])]
                  deriving (Show)



-- Show functions for custom datatypes

instance Show a => Show (Command a) where
    show c = showCommand c 0

showCommand :: Show a => Command a -> Int -> String
showCommand (Assign id ne) t = tabulate t ++ id ++ " := " ++ show ne ++ "\n"
showCommand (Input id) t = tabulate t ++ "INPUT " ++ id ++ "\n"
showCommand (Print id) t = tabulate t ++ "PRINT " ++ id ++ "\n"
showCommand (Empty id) t = tabulate t ++ "EMPTY " ++ id ++ "\n"
showCommand (Push id ne) t = tabulate t ++ "PUSH " ++ id ++ " " ++ show ne ++ "\n"
showCommand (Pop idO idD) t = tabulate t ++ "POP " ++ idO ++ " " ++ idD ++ "\n"
showCommand (Size idO idD) t = tabulate t ++ "SIZE " ++ idO ++ " " ++ idD ++ "\n"
showCommand (Seq cs) t = tabulate t ++ (show $ concatMap show cs) ++ "\n"
showCommand (Cond be c c') t = tabulate t ++ "IF " ++ show be ++ " THEN " ++ 
                               showCommand c (t + 1) ++ " ELSE " ++ showCommand c' (t + 1) ++ "\n"
showCommand (Loop be c) t = tabulate t ++ "WHILE " ++ show be ++ "\n" ++ "DO" ++ "\n" ++
                            showCommand c (t + 1) ++ "END" ++ "\n" 


instance Show a => Show (BExpr a) where
    show a = showBExpr a 0

showBExpr :: Show a => BExpr a -> Int -> String
showBExpr (AND be be') t = tabulate t ++ show be ++ " AND " ++ show be'
showBExpr (OR be be') t = tabulate t ++ show be ++ " OR " ++ show be'
showBExpr (NOT be) t = tabulate t ++ "NOT " ++ show be
showBExpr (Gt ne ne') t = tabulate t ++ show ne ++ " > " ++ show ne'
showBExpr (Eq ne ne') t = tabulate t ++ show ne ++ " = " ++ show ne'


instance Show a => Show (NExpr a) where
    show a = showNExpr a 0

showNExpr :: Show a => NExpr a -> Int -> String
showNExpr (Var id) t = tabulate t ++ id
showNExpr (Const con) t = tabulate t ++ show con
showNExpr (Plus ne ne') t = tabulate t ++ show ne ++ " + " ++ show ne'
showNExpr (Minus ne ne') t = tabulate t ++ show ne ++ " - " ++ show ne'
showNExpr (Times ne ne') t = tabulate t ++ show ne ++ " * " ++ show ne'



-- SymTable getters

getVar :: SymTable a -> Ident -> Maybe a
getVar st id = case getElem st id of
    Just (Left x) -> Just x
    Just (Right (x:xs)) -> Just x
    Nothing -> Nothing


getType :: SymTable a -> Ident -> String
getType st id = case getElem st id of
    Just (Left _) -> "single"
    Just (Right _) -> "stack"
    Nothing -> "invalid"


getElem :: SymTable a -> Ident -> Maybe (Either a [a])
getElem (SymTable xs) id = lookup id xs



-- SymTable setters

setVar :: SymTable a -> Ident -> Either String a -> Either String (SymTable a)
setVar _ _ (Left err) = Left err  
setVar (SymTable xs) id (Right val) = Right (SymTable ((id, Left val) : clearList xs id))


-- TODO: "Type error when pushing to a single"
setStack :: SymTable a -> Ident -> Either String a -> Either String (SymTable a)
setStack _ _ (Left err) = Left err
setStack st@(SymTable xs) id (Right val) = 
    Right (SymTable ((id, Right (setStack' (getElem st id) val)) : clearList xs id))
        where
            setStack' (Just (Right xs)) val = val : xs  
            setStack' _ _ = []


popStack :: SymTable a -> Ident -> Either String (SymTable a, a)
popStack st@(SymTable xs) id = case getElem st id of
    Just (Right []) -> Left ("Empty stack " ++ id)
    Just (Right (y:ys)) -> Right (SymTable ((id, Right ys) : clearList xs id), y)
    Just (Left _) -> Left ("Type error: " ++ id ++ " is not a stack")
    Nothing -> Left ("Undefined variable " ++ id)


emptyStack :: SymTable a -> Ident -> SymTable a
emptyStack (SymTable xs) id = SymTable ((id, Right []) : clearList xs id)


stackSize :: Num a => SymTable a -> Ident -> Either String a
stackSize st id = case getElem st id of
    Just (Right xs) -> Right (sum (map (\_ -> 1) xs))
    _ -> Left ("Undefined stack " ++ id)



-- Evaluable implementation

class Evaluable e where
    eval :: (Num a, Ord a) => (Ident -> Maybe a) -> (e a) -> Either String a
    typeCheck :: (Ident -> String) -> (e a) -> Bool


instance Evaluable BExpr where
    eval f (AND be be') = applyOp (*) (eval f be) (eval f be')
    eval f (OR be be') = case eval f be of
        Right 1 -> Right 1
        Right 0 -> case eval f be' of
            Right 1 -> Right 1
            Right 0 -> Right 0
            Left err -> Left err
        Left err -> Left err
    eval f (NOT be) = case eval f be of
        Right 0 -> Right 1
        Right 1 -> Right 0
        Left err -> Left err
    eval f (Gt ne ne') = castBool (applyOp (>) (eval f ne) (eval f ne'))
    eval f (Eq ne ne') = castBool (applyOp (==) (eval f ne) (eval f ne'))

    typeCheck f (AND be be') = (typeCheck f be) && (typeCheck f be')
    typeCheck f (OR be be') = (typeCheck f be) && (typeCheck f be')
    typeCheck f (Gt ne ne') = (typeCheck f ne) && (typeCheck f ne')
    typeCheck f (Eq ne ne') = (typeCheck f ne) && (typeCheck f ne')


instance Evaluable NExpr where
    eval f (Var id) = case f id of
        Just x -> Right x
        _ -> Left ("Undefined variable " ++ id)
    eval _ (Const con) = Right con
    eval f (Plus ne ne') = applyOp (+) (eval f ne) (eval f ne')
    eval f (Minus ne ne') = applyOp (-) (eval f ne) (eval f ne')
    eval f (Times ne ne') = applyOp (*) (eval f ne) (eval f ne')

    typeCheck f (Var id) = if f id == "single" then True else False
    typeCheck _ (Const _) = True
    typeCheck f (Plus ne ne') = (typeCheck f ne) && (typeCheck f ne')
    typeCheck f (Minus ne ne') = (typeCheck f ne) && (typeCheck f ne')
    typeCheck f (Times ne ne') = (typeCheck f ne) && (typeCheck f ne')



-- Interpreter

interpretCommand :: (Num a, Ord a) => SymTable a -> [a] -> Command a -> ((Either String [a]), SymTable a, [a])

interpretCommand st inp (Assign id ne) = 
    if typeCheck (getType st) ne then
       case setVar st id (eval (getVar st) ne) of
            Right st' -> (Right [], st', inp)
            Left err -> (Left err, st, inp)
    else
        (Left "Type error on := statement", st, inp)

interpretCommand st [] _ = (Left "No input left", st, [])
interpretCommand st inp@(x:xs) (Input id) = case setVar st id (Right x) of
    Right st' -> (Right [], st', xs)
    Left err -> (Left err, st, inp)

interpretCommand st inp (Print id) = case getElem st id of
    Just (Left x) -> (Right [x], st, inp)
    Just (Right x) -> (Right x, st, inp)
    Nothing -> (Left ("Undefined variable " ++ id), st, inp)

interpretCommand st inp (Empty id) = (Right [], emptyStack st id, inp)

interpretCommand st inp (Push id ne) = 
    if typeCheck (getType st) ne then
        case setStack st id (eval (getVar st) ne) of
            Right st' -> (Right [], st', inp)
            Left err -> (Left err, st, inp)
    else
        (Left "Type error on PUSH statement", st, inp)

interpretCommand st inp (Pop idO idD) = case popStack st idO of
    Right (st, x) -> case setVar st idD (Right x) of
        Right st' -> (Right [], st', inp)
        Left err -> (Left err, st, inp)
    Left err -> (Left err, st, inp)

interpretCommand st inp (Size idO idD) = case stackSize st idO of
    Right x -> case setVar st idD (Right x) of
        Right st' -> (Right [], st', inp)
        Left err -> (Left err, st, inp)
    Left err -> (Left err, st, inp)

interpretCommand st inp (Seq []) = (Right [], st, inp)
interpretCommand st inp (Seq (c:cs)) = case interpretCommand st inp c of
    (Left err, _, _) -> (Left err, st, inp)
    (Right res, resSt, resInp) -> case interpretCommand resSt resInp (Seq cs) of
        (Left err, _, _) -> (Left err, resSt, resInp)
        (Right res', resSt', resInp') -> (Right (res ++ res'), resSt', resInp')

interpretCommand st inp (Cond cond exp exp') = 
    if typeCheck (getType st) cond then
        case eval (getVar st) cond of
            Left err -> (Left err, st, inp)
            Right 1 -> interpretCommand st inp exp
            Right 0 -> interpretCommand st inp exp'
    else
        (Left "Type error on conditional statement", st, inp)

interpretCommand st inp (Loop cond exp) = 
    if typeCheck (getType st) cond then
        case eval (getVar st) cond of
            Left err -> (Left err, st, inp)
            Right 1 -> interpretCommand st inp (Seq [exp, (Loop cond exp)])
            Right 0 -> (Right [], st, inp)
    else
        (Left "Type error on loop conditional statement", st, inp)


interpretProgram :: (Num a, Ord a) => [a] -> Command a -> (Either String [a])
interpretProgram inp com = case interpretCommand (SymTable []) inp com of
    (res, _, _) -> res



-- Helper functions

tabulate :: Int -> String
tabulate n = take (2*n) (cycle "  ")


clearList :: Eq a => [(a, b)] -> a -> [(a, b)]
clearList l i = filter (\x -> fst x /= i) l


castBool :: Num b => Either a Bool -> Either a b
castBool (Right True) = Right 1
castBool (Right False) = Right 0
castBool (Left err) = Left err


applyOp :: (t -> t1 -> b) -> Either String t -> Either String t1 -> Either String b
applyOp f (Right x) (Right y) = Right (f x y)
applyOp f (Right _) (Left e) = Left e
applyOp f (Left e) (Right _) = Left e
applyOp f (Left e0) (Left e1) = Left (e0 ++ " " ++ e1)



-- Main

exec p inp numType = case numType of
    0 -> case interpretProgram (read inp :: [Int]) (read p :: Command Int) of
        Right x -> show x
        Left x -> show x
    1 -> case interpretProgram (read inp :: [Float]) (read p :: Command Float) of
        Right x -> show x
        Left x -> show x


main :: IO ()
main = do
    handler <- openFile "programhs.txt" ReadMode
    p <- hGetLine handler

    putStrLn "What kind of numbers do you want to use?"
    putStrLn "  [0] Integer"
    putStrLn "  [1] Float"
    nt <- getLine
    let numType = read nt :: Int

    putStrLn "What kind of execution you want?"
    putStrLn "  [0] Manual execution"
    putStrLn "  [1] Unique test"
    putStrLn "  [2] Multiple test"
    et <- getLine
    let execType = read et :: Int

    case execType of
        0 -> do
            putStrLn "Enter a list of values"
            lv <- getLine
            putStrLn $ exec p lv numType
        1 -> putStrLn $ exec p "" numType
        2 -> putStrLn $ exec p "" numType

    hClose handler    
