restrictable enum Expr[s] {
    case Cst, Var, Not, And, Or, Xor
}

mod Test.Exp.Choose.Simple {
    pub def testChoose01(): Bool = {
        choose Expr.Cst {
            case Expr.Cst(_) => true
        }
    }

    pub def testChoose02(): Bool = {
        choose Expr.Cst {
            case Expr.Cst(_) => true
            case Expr.Var(_) => false
        }
    }

    pub def testChoose03(): Bool = {
        let cstOrVar = if (true) open Expr.Cst else open Expr.Var;
        choose cstOrVar {
            case Expr.Cst(_) => true
            case Expr.Var(_) => false
        }
    }

    pub def testChoose04(): Bool = {
        let cstOrVar = if (true) open Expr.Cst else open Expr.Var;
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
        h(Expr.Cst)
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

        let cstOrNot = if (true) open Expr.Cst else open Expr.Not;
        h(cstOrNot)
    }

    pub def testChoose07(): Bool = {
        let id = x -> choose x {
            case Expr.And(_)    => open Expr.And
            case Expr.Cst(_)    => open Expr.Cst
            case Expr.Not(_)    => open Expr.Not
            case Expr.Or(_)     => open Expr.Or
        };
        let _ = id(Expr.Cst);
        true
    }
}
