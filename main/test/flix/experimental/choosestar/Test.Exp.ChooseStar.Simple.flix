restrictable enum Expr[s] {
    case Cst, Var, Not, And, Or, Xor
}

//
// Properties:
//   P1) Input upper bounded by case set (like normal choose)
//   P2) Output lower bounded by non-stable case set
//   P3) Output upper bounded by case set union non-stable set
//

mod Test.Exp.ChooseStar.Simple {

    pub def testChooseStar01(): Bool = {
        // P2: check the lower bound by using result in a choose
        let star = choose* Expr.Cst {
            case Expr.Cst(_) => Expr.Var(_)
        };
        choose star {
            case Expr.Var(_) => true
        }
    }

    pub def testChooseStar02(): Bool = {
        // P2: check the lower bound by using result in a choose
        let star = choose* Expr.Cst {
            case Expr.Cst(_) => Expr.Var(_)
            case Expr.Not(_) => Expr.Var(_)
            case Expr.Xor(_) => Expr.Var(_)
        };
        choose star {
            case Expr.Var(_) => true
        }
    }

    pub def testChooseStar3(): Bool = {
        // P2: check the lower bound by using result in a choose
        let star = choose* Expr.Cst {
            case Expr.Cst(_) => Expr.Var(_)
            case Expr.Not(_) => Expr.Var(_)
            case Expr.Xor(_) => Expr.Not(_)
        };
        choose star {
            case Expr.Var(_) => true
            case Expr.Not(_) => true
        }
    }

    pub def testChooseStar3(): Bool = {
        // P2: check the lower bound by using result in a choose
        let star = choose* Expr.Cst {
            case Expr.Cst(_) => Expr.Var(_)
            case Expr.Not(_) => Expr.Var(_)
            case Expr.Xor(_) => Expr.Not(_)
        };
        choose star {
            case Expr.Var(_) => true
            case Expr.Not(_) => true
            case Expr.Xor(_) => false
        }
    }

}
