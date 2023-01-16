restrictable enum Expr[s] {
    case Cst, Var, Not, And, Or, Xor
}

//
// Properties:
//   P1) Input upper bounded by pattern case set (like normal choose)
//   P2) Output lower bounded by non-stable case set
//   P3) Output maintains information on the absence of stable cases
//

mod Test.Exp.ChooseStar.Simple {

    pub def testChooseStar01(): Bool = {
        // P2: check the lower bound by using result in a choose
        let star = choose* Expr.Cst {
            case Expr.Cst(_) => Expr.Var()
        };
        choose star {
            case Expr.Var(_) => true
        }
    }

    pub def testChooseStar02(): Bool = {
        // P2: check the lower bound by using result in a choose
        let star = choose* Expr.Cst {
            case Expr.Cst(_) => Expr.Var()
            case Expr.Not(_) => Expr.Var()
            case Expr.Xor(_) => Expr.Var()
        };
        choose star {
            case Expr.Var(_) => true
        }
    }

    pub def testChooseStar3(): Bool = {
        // P2: check the lower bound by using result in a choose
        let star = choose* Expr.Cst {
            case Expr.Cst(_) => Expr.Var()
            case Expr.Not(_) => Expr.Var()
            case Expr.Xor(_) => Expr.Not()
        };
        choose star {
            case Expr.Var(_) => true
            case Expr.Not(_) => true
        }
    }

    pub def testChooseStar4(): Bool = {
        // P2: check the lower bound by using result in a choose
        let star = choose* Expr.Cst {
            case Expr.Cst(_) => Expr.Var()
            case Expr.Not(_) => Expr.Var()
            case Expr.Xor(_) => Expr.Not()
        };
        choose star {
            case Expr.Var(_) => true
            case Expr.Not(_) => true
            case Expr.Xor(_) => false
        }
    }

    pub def testChooseStar5(): Bool = {
        // P3: Check that not is not present
        let star = choose* Expr.Cst {
            case Expr.Not(_) => Expr.Not()
            case Expr.Cst(_) => Expr.Var()
        };
        choose star {
            case Expr.Var(_) => false
        }
    }

    pub def testChooseStar6(): Bool = {
        // P3: Check that not is not present
        let star = choose* Expr.Cst {
            case Expr.Not(_) => Expr.Not()
            case Expr.Cst(_) => Expr.Cst()
        };
        choose star {
            case Expr.Cst(_) => true
        }
    }

//    pub def testChooseStar7(): Bool = {
//        // P3: Check that Not is not present
//        let f = c -> choose* c {
//            case Expr.Not(_) => Expr.Not()
//            case Expr.Cst(_) => Expr.Var()
//        };
//        let g = c -> choose* c {
//            case Expr.Not(_) => Expr.Not()
//            case Expr.Xor(_) => Expr.And()
//        };
//        if (true) f else g
//    }

}
