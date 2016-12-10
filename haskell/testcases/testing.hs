let inp = [2, 4]
let com0 = Input "x"
let com1 = Input "y"
let com3 = Empty "p"
let com4 = Assign "x" (Minus (Const 900) (Const 9))
let com5 = Print "x"
let com6 = Print "y"
let com7 = Size "p" "size"
let com8 = Print "size"
let com9 = Cond (Gt (Var "x") (Var "y")) (Assign "tmp" (Const 99)) (Assign "tmp" (Const 3))
let com10 = Print "tmp"
let com = Seq [com0, com1, com2, com3, com4, com5, com6, com7, com8, com9, com10]

interpretProgram inp com


let st = SymTable []
let inp = [2, 4]
let com0 = Input "x"
let com1 = Input "y"
let com2 = Empty "p"
let com3 = Push "p" (Var "x")
let com4 = Push "p" (Var "y")
let com5 = Print "sw"
let com = Seq [com0, com1, com2, com3, com4, com5]

interpretCommand st inp com