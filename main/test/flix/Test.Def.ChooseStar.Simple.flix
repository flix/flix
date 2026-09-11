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

    @CompileTest
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

    @CompileTest
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

    @CompileTest
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

    @CompileTest
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

    @CompileTest
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

    @CompileTest
    pub def testChooseStar06(): Bool = {
        let star = helper06(Expr.Cst);
        choose star {
            case Expr.Cst => true
        }
    }

    def helper08(e: Expr[<Expr.Cst>]): Expr[<Expr.Cst>] = choose* e {
        case Expr.Cst => Expr.Cst
    }

    @CompileTest
    pub def testChooseStar08(): Bool = {
        let thing = helper08(Expr.Cst);
        choose thing {case Expr.Cst => true}
    }

    def helper09(e: Expr[s rvand <Expr.Cst, Expr.Var>]): Expr[<Expr.Cst>] = choose* e {
        case Expr.Cst => Expr.Cst
        case Expr.Var => Expr.Cst
    }

    @CompileTest
    pub def testChooseStar09(): Bool = {
        let thing = helper09(Expr.Cst);
        choose thing {case Expr.Cst => true}
    }

    def helper10(e: Expr[s rvand <Expr.Cst, Expr.Var>]): Expr[<Expr.Cst>] = choose* e {
        case Expr.Cst => Expr.Cst
        case Expr.Var => Expr.Cst
    }

    @CompileTest
    pub def testChooseStar10(): Bool = {
        let cstOrVar = if (true) open_variant Expr.Cst else open_variant Expr.Var;
        let thing = helper10(cstOrVar);
        choose thing {case Expr.Cst => true}
    }

}
