restrictable enum Expr[_] {
    case Cst, Var, Not, And, Or, Xor
}

restrictable enum Color[_] {
    case Red
    case Blue
    case Green
}

mod Test.Def.ChooseStar.Simple {

    pub def colorId(c: Color[s]): Color[s] = choose* c {
        case Color.Red => Color.Red
        case Color.Blue => Color.Blue
        case Color.Green => Color.Green
    }

    pub def rbSwap(c: Color[s rvand <Color.Red, Color.Blue>]): Color[<Color.Red, Color.Blue>] = choose* c {
        case Color.Red => Color.Blue
        case Color.Blue => Color.Red
    }

    pub def rbSwapAny(c: Color[s]): Color[(s rvand <Color.Green>) rvadd <Color.Red, Color.Blue>] = choose* c {
        case Color.Red => rbSwap(Color.Red)
        case Color.Blue => rbSwap(Color.Blue)
        case Color.Green => Color.Green
    }

    pub def approx(c: Color[s]): Color[(s rvand <Color.Red>) rvadd <Color.Blue>] = choose* c {
        case Color.Red => Color.Red
        case Color.Blue => Color.Blue
        case Color.Green => Color.Blue
    }

    pub def approxSwap(c: Color[s]): Color[<Color.Red, Color.Blue>] = {
        c |> approx |> rbSwap
    }

    def helper01(e: Expr[s rvand <Expr.Cst>]): Expr[<Expr.Var>] = choose* e {
        case Expr.Cst => Expr.Var
    }

    pub def testChooseStar01(): Bool = {
        let star = helper01(Expr.Cst);
        choose star {
            case Expr.Var => true
        }
    }

    def helper02(e: Expr[s rvand <Expr.Cst, Expr.Not, Expr.Xor>]): Expr[<Expr.Var>] = choose* e {
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

    def helper03(e: Expr[s rvand <Expr.Cst, Expr.Not, Expr.Xor>]): Expr[<Expr.Var, Expr.Not>] = choose* e {
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

    def helper04(e: Expr[s rvand <Expr.Cst, Expr.Not, Expr.Xor>]): Expr[<Expr.Var, Expr.Not>] = choose* e {
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

    def helper05(e: Expr[s rvand <Expr.Not, Expr.Cst>]): Expr[(s rvand <Expr.Not>) rvadd <Expr.Var>] = choose* e {
        case Expr.Not => Expr.Not
        case Expr.Cst => Expr.Var
    }

    pub def testChooseStar05(): Bool = {
        let star = helper05(Expr.Cst);
        choose star {
            case Expr.Var => true
        }
    }

    def helper06(e: Expr[s rvand <Expr.Not, Expr.Cst>]): Expr[(s rvand <Expr.Not, Expr.Cst>)] = choose* e {
        case Expr.Not => Expr.Not
        case Expr.Cst => Expr.Cst
    }

    pub def testChooseStar06(): Bool = {
        let star = helper06(Expr.Cst);
        choose star {
            case Expr.Cst => true
        }
    }

// TODO RESTR-VARS too copmlex
//    def helper07_1(e: Expr[s rvand <Expr.Not, Expr.Cst>]): Expr[(s rvand <Expr.Not>) rvadd <Expr.Var> rvadd _] = choose* e {
//        case Expr.Not => Expr.Not
//        case Expr.Cst => Expr.Var
//    }
//
//    def helper07_2(e: Expr[s rvand <Expr.Not, Expr.Xor>]): Expr[(s rvand <Expr.Not>) rvadd <Expr.And> rvadd _] = choose* e {
//        case Expr.Not => Expr.Not
//        case Expr.Xor => Expr.And
//    }
//
//    pub def testChooseStar07(): Bool = {
//        let f = if (true) helper07_1 else helper07_2;
//        let star = f(Expr.Not);
//         choose* star {
//             case Expr.Not => true
//         }
//    }

    def helper08(e: Expr[<Expr.Cst>]): Expr[<Expr.Cst>] = choose* e {
        case Expr.Cst => Expr.Cst
    }

    pub def testChooseStar08(): Bool = {
        let thing = helper08(Expr.Cst);
        choose thing {case Expr.Cst => true}
    }

    def helper09(e: Expr[s rvand <Expr.Cst, Expr.Var>]): Expr[<Expr.Cst>] = choose* e {
        case Expr.Cst => Expr.Cst
        case Expr.Var => Expr.Cst
    }

    pub def testChooseStar09(): Bool = {
        let thing = helper09(Expr.Cst);
        choose thing {case Expr.Cst => true}
    }

    def helper10(e: Expr[s rvand <Expr.Cst, Expr.Var>]): Expr[<Expr.Cst>] = choose* e {
        case Expr.Cst => Expr.Cst
        case Expr.Var => Expr.Cst
    }

    pub def testChooseStar10(): Bool = {
        let cstOrVar = if (true) open_variant Expr.Cst else open_variant Expr.Var;
        let thing = helper10(cstOrVar);
        choose thing {case Expr.Cst => true}
    }

// TODO RESTR-VARS too complex
//    def helper11(e: Expr[s rvand <Expr.Xor, Expr.Cst, Expr.Var>]): Expr[(s rvand <Expr.Xor, Expr.Cst>) rvadd <Expr.Xor>] = choose* e {
//        case Expr.Xor => Expr.Xor
//        case Expr.Cst => Expr.Cst
//        case Expr.Var => Expr.Xor
//    }
//
//    pub def testChooseStar11(): Bool = {
//        let cstOrVar = if (true) open_variant Expr.Cst else open_variant Expr.Var;
//        let thing = helper11(cstOrVar);
//        choose thing {
//            case Expr.Xor => true
//            case Expr.Cst => true
//        }
//    }
//
//    def helper12_1(e: Expr[s rvand <Expr.Cst, Expr.Var>]): Expr[(s rvand <Expr.Cst>) rvadd <Expr.Xor> rvadd _] = choose* e {
//        case Expr.Cst => Expr.Cst
//        case Expr.Var => Expr.Xor
//    }
//
//    def helper12_2(e: Expr[s rvand <Expr.Cst, Expr.Xor>]): Expr[(s rvand <Expr.Cst>) rvadd <Expr.Var> rvadd _] = choose* e {
//        case Expr.Cst => Expr.Cst
//        case Expr.Xor => Expr.Var
//    }
//
//    pub def testChooseStar12(): Bool = {
//        let h = if (true) helper12_1 else helper12_2;
//        let thing = h(Expr.Cst);
//        ???
//          choose h(Expr.Cst) {
//              case Expr.Cst => true
//              case Expr.Var => true
//              case Expr.Xor => true
//          }
//    }
//
//    def helper13_1(e: Expr[s rvand <Expr.Cst, Expr.Var, Expr.Not>]): Expr[<Expr.Cst>] = choose* e {
//        case Expr.Cst => Expr.Cst
//        case Expr.Var => Expr.Cst
//        case Expr.Not => Expr.Cst
//    }
//
//    def helper13_2(e: Expr[s rvand <Expr.Cst, Expr.Xor, Expr.Not>]): Expr[<Expr.Cst>] = choose* e {
//        case Expr.Cst => Expr.Cst
//        case Expr.Xor => Expr.Cst
//        case Expr.Not => Expr.Cst
//    }
//
//    pub def testChooseStar13(): Bool = {
//        let h = if (true) helper13_1 else helper13_2;
//        let cstOrNot = if (true) open_variant Expr.Cst else open_variant Expr.Not;
//        choose h(cstOrNot) {case Expr.Cst => true}
//    }
//
//    def helper14(e: Expr[s rvand <Expr.And, Expr.Cst, Expr.Not, Expr.Or>]): Expr[s rvand <Expr.And, Expr.Cst, Expr.Not, Expr.Or>] = choose* e {
//        case Expr.And    => Expr.And
//        case Expr.Cst    => Expr.Cst
//        case Expr.Not    => Expr.Not
//        case Expr.Or     => Expr.Or
//    }
//
//    pub def testChooseStar14(): Bool = {
//        choose helper14(Expr.Cst) {case Expr.Cst => true}
//    }

}
