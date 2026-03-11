mod Test.Exp.ChooseStar.Recursive {

    restrictable enum Expr[s] {
        case Cst(Bool)
        case Var(Int32)
        case Not(Expr[s])
        case And(Expr[s], Expr[s])
        case Or(Expr[s], Expr[s])
        case Xor(Expr[s], Expr[s])
    }

}
