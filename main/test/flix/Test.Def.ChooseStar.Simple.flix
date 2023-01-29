restrictable enum Expr[s] {
    case Cst, Var, Not, And, Or, Xor
}

//
// Properties:
//   P1) Input upper bounded by pattern case set (like normal choose)
//   P2) Output lower bounded by non-stable case set
//   P3) Output maintains information on the absence of stable cases
//

mod Test.Def.ChooseStar.Simple {

    def helper01(e: Expr[s && <Expr.Cst>]): Expr[<Expr.Var>] = choose* e {
        case Expr.Cst => Expr.Var
    }

    pub def testChooseStar01(): Bool = {
        let star = helper01(Expr.Cst);
        choose star {
            case Expr.Var => true
        }
    }

    def helper02(e: Expr[s && <Expr.Cst, Expr.Not, Expr.Xor>]): Expr[<Expr.Var>] = choose* e {
        case Expr.Cst => Expr.Var
        case Expr.Not => Expr.Var
        case Expr.Xor => Expr.Var
    }

    pub def testChooseStar02(): Bool = {
        let star = helper02(Expr.Cst);
        choose star {
            case Expr.Var => true
        }
    }

    def helper03(e: Expr[s && <Expr.Cst, Expr.Not, Expr.Xor>]): Expr[<Expr.Var, Expr.Not>] = choose* e {
        case Expr.Cst => Expr.Var
        case Expr.Not => Expr.Var
        case Expr.Xor => Expr.Not
    }

    pub def testChooseStar03(): Bool = {
        let star = helper03(Expr.Cst);
        choose star {
            case Expr.Var => true
            case Expr.Not => true
        }
    }

    def helper04(e: Expr[s && <Expr.Cst, Expr.Not, Expr.Xor>]): Expr[<Expr.Var, Expr.Not>] = choose* e {
        case Expr.Cst => Expr.Var
        case Expr.Not => Expr.Var
        case Expr.Xor => Expr.Not
    }

    pub def testChooseStar04(): Bool = {
        let star = helper04(Expr.Cst);
        choose star {
            case Expr.Var => true
            case Expr.Not => true
            case Expr.Xor => false
        }
    }

    def helper05(e: Expr[s && <Expr.Not, Expr.Cst>]): Expr[(s && <Expr.Not>) ++ <Expr.Var>] = choose* e {
        case Expr.Not => Expr.Not
        case Expr.Cst => Expr.Var
    }

    pub def testChooseStar05(): Bool = {
        let star = helper05(Expr.Cst);
        choose star {
            case Expr.Var => true
        }
    }

    def helper06(e: Expr[s && <Expr.Not, Expr.Cst>]): Expr[(s && <Expr.Not, Expr.Cst>)] = choose* e {
        case Expr.Not => Expr.Not
        case Expr.Cst => Expr.Cst
    }

    pub def testChooseStar06(): Bool = {
        let star = helper06(Expr.Cst);
        choose star {
            case Expr.Cst => true
        }
    }

    def helper07_1(e: Expr[s && <Expr.Not, Expr.Cst>]): Expr[(s && <Expr.Not>) ++ <Expr.Var> ++ _] = choose* e {
        case Expr.Not => Expr.Not
        case Expr.Cst => Expr.Var
    }

    def helper07_2(e: Expr[s && <Expr.Not, Expr.Xor>]): Expr[(s && <Expr.Not>) ++ <Expr.And> ++ _] = choose* e {
        case Expr.Not => Expr.Not
        case Expr.Xor => Expr.And
    }

    pub def testChooseStar07(): Bool = {
        let f = if (true) helper07_1 else helper07_2;
        let star = f(Expr.Not);
        ???
        // TODO RESTR-VARS too copmlex
        // choose* star {
        //     case Expr.Not => true
        // }
    }
}

// TODO RESTR-VARS translate to explicit sigs
//    pub def testChoose08(): Bool = {
//        // P1: Check that choose* upperbounds the input like choose
//        let thing = choose* Expr.Cst {
//            case Expr.Cst => Expr.Cst
//        };
//        choose thing {case Expr.Cst => true}
//    }
//
//    pub def testChoose09(): Bool = {
//        // P1: Check that choose* upperbounds the input like choose
//        let thing = choose* Expr.Cst {
//            case Expr.Cst => Expr.Cst
//            case Expr.Var => Expr.Cst
//        };
//        choose thing {case Expr.Cst => true}
//    }
//
//    pub def testChoose10(): Bool = {
//        // P1: Check that choose* upperbounds the input like choose
//        let cstOrVar = if (true) open Expr.Cst else open Expr.Var;
//        let thing = choose* cstOrVar {
//            case Expr.Cst => Expr.Cst
//            case Expr.Var => Expr.Cst
//        };
//        choose thing {case Expr.Cst => true}
//    }
//
//    pub def testChoose11(): Bool = {
//        // P1: Check that choose* upperbounds the input like choose
//        let cstOrVar = if (true) open Expr.Cst else open Expr.Var;
//        let thing = choose* cstOrVar {
//            case Expr.Xor => Expr.Xor
//            case Expr.Cst => Expr.Cst
//            case Expr.Var => Expr.Xor
//        };
//        choose thing {
//            case Expr.Xor => true
//            case Expr.Cst => true
//        }
//    }
//
////    pub def testChoose12(): Bool = {
////        // P1: Check that choose* upperbounds the input like choose
////        let f = x -> choose* x {
////            case Expr.Cst => Expr.Cst
////            case Expr.Var => Expr.Xor
////        };
////        let g = x -> choose* x {
////            case Expr.Cst => Expr.Cst
////            case Expr.Xor => Expr.Var
////        };
////        let h = if (true) f else g;
////        choose h(Expr.Cst) {
////            case Expr.Cst => true
////            case Expr.Var => true
////            case Expr.Xor => true
////        }
////    }
////
////    pub def testChoose13(): Bool = {
////        // P1: Check that choose* upperbounds the input like choose
////        let f = x -> choose* x {
////            case Expr.Cst => Expr.Cst
////            case Expr.Var => Expr.Cst
////            case Expr.Not => Expr.Cst
////        };
////        let g = x -> choose* x {
////            case Expr.Cst => Expr.Cst
////            case Expr.Xor => Expr.Cst
////            case Expr.Not => Expr.Cst
////        };
////        let h = if (true) f else g;
////
////        let cstOrNot = if (true) open Expr.Cst else open Expr.Not;
////        choose h(cstOrNot) {case Expr.Cst => true}
////    }
//
//    pub def testChoose14(): Bool = {
//        // P1: Check that choose* upperbounds the input like choose
//        let id = x -> choose* x {
//            case Expr.And    => Expr.And
//            case Expr.Cst    => Expr.Cst
//            case Expr.Not    => Expr.Not
//            case Expr.Or     => Expr.Or
//        };
//        choose id(Expr.Cst) {case Expr.Cst => true}
//    }

}
