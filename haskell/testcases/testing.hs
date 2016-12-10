let st = ST ([("x", Left 23), ("y", Left 12)])
let inp = []
let com0 = Assign "z" (Plus (Var "x") (Plus (Var "y") (Const 3)))
let com1 = Empty "p"
let com = Seq [com0, com1]

interpretCommand st inp com