pub restrictable enum Expr[_][t] {
    case Cst(t)
    case Var(String)
    case Not(Option[t])
    case And(t, t)
    case Or(t, t)
    case Xor(t, t)
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
        let star = choose* Expr.Cst(Some('a')) {
            case Expr.Cst(_) => Expr.Var("Cst")
        };
        choose star {
            case Expr.Var(_) => true
        }
    }

    pub def testChooseStar02(): Bool = {
        // P2: check the lower bound by using result in a choose
        let star = choose* Expr.Cst(2i8) {
            case Expr.Cst(_) => Expr.Var("tmp")
            case Expr.Not(_) => Expr.Var("tmp")
            case Expr.Xor(_) => Expr.Var("tmp")
        };
        choose star {
            case Expr.Var(_) => true
        }
    }

    pub def testChooseStar3(): Bool = region rc {
        // P2: check the lower bound by using result in a choose
        let star = choose* Expr.Cst(Ref.fresh(rc, 2)) {
            case Expr.Cst(_) => Expr.Var("tmp")
            case Expr.Not(_) => Expr.Var("tmp")
            case Expr.Xor(x, _) => Expr.Not(Some(x))
        };
        choose star {
            case Expr.Var(_) => true
            case Expr.Not(_) => true
        }
    }

// TODO RESTR-VARS
//    pub def testChooseStar4(): Bool = {
//        // P2: check the lower bound by using result in a choose
//        let star = choose* open_variant Expr.Cst(32.1f32) {
//            case Expr.Cst(_) => open_variant Expr.Var("tmp")
//            case Expr.Not(_) => open_variant Expr.Var("tmp")
//            case Expr.Xor(x, _) => open_variant Expr.Not(Some(x))
//        };
//        choose star {
//            case Expr.Var(_) => true
//            case Expr.Not(_) => true
//            case Expr.Xor(_) => false
//        }
//    }
//
//    pub def testChooseStar5(): Bool = {
//        // P3: Check that not is not present
//        let star = choose* open_variant Expr.Cst(None) {
//            case Expr.Not(opt) => open_variant Expr.Not(opt)
//            case Expr.Cst(opt) => open_variant Expr.Var(match opt {
//                case None => "None"
//                case Some(_) => "Some"
//            })
//        };
//        choose star {
//            case Expr.Var(_) => false
//        }
//    }
//
//    pub def testChooseStar6(): Bool = {
//        // P3: Check that not is not present
//        let star = choose* open_variant Expr.Cst(12) {
//            case Expr.Not(opt) => open_variant Expr.Not(opt)
//            case Expr.Cst(x) => open_variant Expr.Cst(x)
//        };
//        choose star {
//            case Expr.Cst(_) => true
//        }
//    }
//
//    pub def testChooseStar7(): Bool = {
//        // P3: Check that Not is not present
//        let f = c -> choose* c {
//            case Expr.Not(opt) => open_variant Expr.Not(opt)
//            case Expr.Cst(_) => open_variant Expr.Var("tmp")
//        };
//        let g = c -> choose* c {
//            case Expr.Not(opt) => open_variant Expr.Not(opt)
//            case Expr.Xor(x, y) => open_variant Expr.And(y, x)
//        };
//        let _ = if (true) f else g;
//        true
//    }

}
