mod Test.Exp.Choose.Recursive {

    restrictable enum Expr[s] {
        case Cst(Bool)
        case Var(Int32)
        case Not(Expr[s])
        case And(Expr[s], Expr[s])
        case Or(Expr[s], Expr[s])
        case Xor(Expr[s], Expr[s])
    }

    pub def testChoose01(): Bool = {
        choose Expr.Cst(false) {
            case Expr.Cst(_) => true
        }
    }

    pub def testChoose02(): Bool = {
        choose Expr.Cst(true) {
            case Expr.Cst(_) => true
            case Expr.Var(_) => false
        }
    }

    pub def testChoose03(): Bool = {
        let cstOrVar = if (true) open_variant Expr.Cst(true) else open_variant Expr.Var(123);
        choose cstOrVar {
            case Expr.Cst(_) => true
            case Expr.Var(_) => false
        }
    }

    pub def testChoose04(): Bool = {
        let cstOrVar = if (true) open_variant Expr.Cst(true) else open_variant Expr.Var(123);
        choose cstOrVar {
            case Expr.Xor(_, _) => false
            case Expr.Cst(_) => true
            case Expr.Var(_) => false
        }
    }

    pub def testChoose05(): Bool = {
        let f = x -> choose x {
            case Expr.Cst(_) => false
            case Expr.Var(_) => true
        };
        let g = x -> choose x {
            case Expr.Cst(_) => false
            case Expr.Xor(_, _) => true
        };
        let h = if (true) f else g;
        h(Expr.Cst(true))
    }

    pub def testChoose06(): Bool = {
        let f = x -> choose x {
            case Expr.Cst(_) => false
            case Expr.Var(_) => true
            case Expr.Not(_) => false
        };
        let g = x -> choose x {
            case Expr.Cst(_) => false
            case Expr.Xor(_, _) => true
            case Expr.Not(_) => false
        };
        let h = if (true) f else g;

        let cstOrNot = if (true) open_variant Expr.Cst(true) else open_variant Expr.Not(open_variant Expr.Cst(true));
        h(cstOrNot)
    }

}
