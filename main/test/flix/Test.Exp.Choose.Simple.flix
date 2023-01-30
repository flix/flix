restrictable enum Expr[s] {
    case Cst, Var, Not, And, Or, Xor
}

mod Test.Exp.Choose.Simple {
    pub def testChoose01(): Bool = {
        choose Expr.Cst {
            case Expr.Cst => true
        }
    }

    pub def testChoose02(): Bool = {
        choose Expr.Cst {
            case Expr.Cst => true
            case Expr.Var => false
        }
    }

    pub def testChoose03(): Bool = {
        let cstOrVar = if (true) open Expr.Cst else open Expr.Var;
        choose cstOrVar {
            case Expr.Cst => true
            case Expr.Var => false
        }
    }

    pub def testChoose04(): Bool = {
        let cstOrVar = if (true) open Expr.Cst else open Expr.Var;
        choose cstOrVar {
            case Expr.Xor => false
            case Expr.Cst => true
            case Expr.Var => false
        }
    }

    pub def testChoose05(): Bool = {
        let f = x -> choose x {
            case Expr.Cst => false
            case Expr.Var => true
        };
        let g = x -> choose x {
            case Expr.Cst => false
            case Expr.Xor => true
        };
        let h = if (true) f else g;
        h(Expr.Cst)
    }

    pub def testChoose06(): Bool = {
        let f = x -> choose x {
            case Expr.Cst => false
            case Expr.Var => true
            case Expr.Not => false
        };
        let g = x -> choose x {
            case Expr.Cst => false
            case Expr.Xor => true
            case Expr.Not => false
        };
        let h = if (true) f else g;

        let cstOrNot = if (true) open Expr.Cst else open Expr.Not;
        h(cstOrNot)
    }

//     pub def testChoose07(): Bool = {
//         let id = x -> choose x {
//             case Expr.And    => open Expr.And
//             case Expr.Cst    => open Expr.Cst
//             case Expr.Not    => open Expr.Not
//             case Expr.Or     => open Expr.Or
//         };
//         let _ = id(Expr.Cst);
//         true
//     }
}
