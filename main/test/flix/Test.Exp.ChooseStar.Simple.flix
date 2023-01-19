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

    pub def testChooseStar03(): Bool = {
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

    pub def testChooseStar04(): Bool = {
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

    pub def testChooseStar05(): Bool = {
        // P3: Check that not is not present
        let star = choose* Expr.Cst {
            case Expr.Not(_) => Expr.Not()
            case Expr.Cst(_) => Expr.Var()
        };
        choose star {
            case Expr.Var(_) => false
        }
    }

    pub def testChooseStar06(): Bool = {
        // P3: Check that not is not present
        let star = choose* Expr.Cst {
            case Expr.Not(_) => Expr.Not()
            case Expr.Cst(_) => Expr.Cst()
        };
        choose star {
            case Expr.Cst(_) => true
        }
    }

    pub def testChooseStar07(): Bool = {
        // P3: Check that Not is not present
        let f = c -> choose* c {
            case Expr.Not(_) => Expr.Not()
            case Expr.Cst(_) => Expr.Var()
        };
        let g = c -> choose* c {
            case Expr.Not(_) => Expr.Not()
            case Expr.Xor(_) => Expr.And()
        };
        let _ = if (true) f else g;
        true
    }

    pub def testChoose08(): Bool = {
        // P1: Check that choose* upperbounds the input like choose
        let thing = choose* Expr.Cst {
            case Expr.Cst(_) => Expr.Cst
        };
        choose thing {case Expr.Cst(_) => true}
    }

    pub def testChoose09(): Bool = {
        // P1: Check that choose* upperbounds the input like choose
        let thing = choose* Expr.Cst {
            case Expr.Cst(_) => Expr.Cst
            case Expr.Var(_) => Expr.Cst
        };
        choose thing {case Expr.Cst(_) => true}
    }

    pub def testChoose10(): Bool = {
        // P1: Check that choose* upperbounds the input like choose
        let cstOrVar = if (true) Expr.Cst else Expr.Var;
        let thing = choose* cstOrVar {
            case Expr.Cst(_) => Expr.Cst
            case Expr.Var(_) => Expr.Cst
        };
        choose thing {case Expr.Cst(_) => true}
    }

    pub def testChoose11(): Bool = {
        // P1: Check that choose* upperbounds the input like choose
        let cstOrVar = if (true) Expr.Cst else Expr.Var;
        let thing = choose* cstOrVar {
            case Expr.Xor(_) => Expr.Xor
            case Expr.Cst(_) => Expr.Cst
            case Expr.Var(_) => Expr.Xor
        };
        choose thing {
            case Expr.Xor(_) => true
            case Expr.Cst(_) => true
        }
    }


//    pub def testChoose12(): Bool = {
//        // P1: Check that choose* upperbounds the input like choose
//        let f = x -> choose* x {
//            case Expr.Cst(_) => Expr.Cst
//            case Expr.Var(_) => Expr.Xor
//        };
//        let g = x -> choose* x {
//            case Expr.Cst(_) => Expr.Cst
//            case Expr.Xor(_) => Expr.Var
//        };
//        let h = if (true) f else g;
//        choose h(Expr.Cst) {
//            case Expr.Cst(_) => true
//            case Expr.Var(_) => true
//            case Expr.Xor(_) => true
//        }
//    }

    pub def testChoose13(): Bool = {
        // P1: Check that choose* upperbounds the input like choose
        let f = x -> choose* x {
            case Expr.Cst(_) => Expr.Cst
            case Expr.Var(_) => Expr.Cst
            case Expr.Not(_) => Expr.Cst
        };
        let g = x -> choose* x {
            case Expr.Cst(_) => Expr.Cst
            case Expr.Xor(_) => Expr.Cst
            case Expr.Not(_) => Expr.Cst
        };
        let h = if (true) f else g;

        let cstOrNot = if (true) Expr.Cst else Expr.Not;
        choose h(cstOrNot) {case Expr.Cst(_) => true}
    }

    pub def testChoose14(): Bool = {
        // P1: Check that choose* upperbounds the input like choose
        let id = x -> choose* x {
            case Expr.And(_)    => Expr.And
            case Expr.Cst(_)    => Expr.Cst
            case Expr.Not(_)    => Expr.Not
            case Expr.Or(_)     => Expr.Or
        };
        choose id(Expr.Cst) {case Expr.Cst(_) => true}
    }

}
