tabulate :: Int -> String
tabulate n = take n (cycle "  ")


type Ident = String

data Command a = 	Assign Ident a | Input Ident | Print Ident | 
									Empty Ident | Push Ident a | Pop Ident Ident | 
									Size Ident Ident | Seq [Command a] | 
									Cond (BExpr a) (Command a) | 
									Loop (BExpr a) (Command a)

data BExpr a = 	AND (BExpr a) (BExpr a) | OR (BExpr a) (BExpr a) | 
								NOT (BExpr a) | Gt (NExpr a) (NExpr a) | 
								Eq (NExpr a) (NExpr a)

data NExpr a = 	Var Ident | Const a | Plus (NExpr a) (NExpr a) | 
								Minus (NExpr a) (NExpr a) | Times (NExpr a) (NExpr a)
								

instance Show a => Show (Command a) where
	show a = customShow a 0


customShow::Show a => Command a -> Int -> String
customShow (Assign i a) t = (tabulate t) ++ "ASSIGN"
customShow (Input i) t = (tabulate t) ++ "INPUT"
