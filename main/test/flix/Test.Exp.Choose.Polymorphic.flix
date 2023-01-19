pub restrictable enum Expr[_][t] {
    case Cst(t)
    case Var(String)
    case Not(Option[t])
    case And(t, t)
    case Or(t, t)
    case Xor(t, t)
}

mod Test.Exp.Choose.Polymorphic {

    pub def testChoose01(): Bool = {
        choose Expr.Cst(12312) {
            case Expr.Cst(_) => true
        }
    }

    pub def testChoose02(): Bool = {
        choose Expr.Cst('a') {
            case Expr.Cst(_) => true
            case Expr.Var(_) => false
        }
    }

    pub def testChoose03(): Bool = {
        let cstOrVar = if (true) Expr.Cst(123) else Expr.Var("hi");
        choose cstOrVar {
            case Expr.Cst(_) => true
            case Expr.Var(_) => false
        }
    }

    pub def testChoose04(): Bool = {
        let cstOrVar = if (true) Expr.Cst(123) else Expr.Var("hi");
        choose cstOrVar {
            case Expr.Xor(_) => false
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
            case Expr.Xor(_) => true
        };
        let h = if (true) f else g;
        h(Expr.Cst(123123))
    }

    pub def testChoose06(): Bool = {
        let f = x -> choose x {
            case Expr.Cst(_) => false
            case Expr.Var(_) => true
            case Expr.Not(_) => false
        };
        let g = x -> choose x {
            case Expr.Cst(_) => false
            case Expr.Xor(_) => true
            case Expr.Not(_) => false
        };
        let h = if (true) f else g;

        let cstOrNot = if (true) Expr.Cst("hello") else Expr.Not(Some("hello"));
        h(cstOrNot)
    }
}
