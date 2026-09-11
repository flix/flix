mod Test.Exp.ChooseStar.Simple {

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

    @CompileTest
    pub def testChooseStar01(): Bool = {
        // P2: check the lower bound by using result in a choose
        let star = choose* Expr.Cst(Some('a')) {
            case Expr.Cst(_) => Expr.Var("Cst")
        };
        choose star {
            case Expr.Var(_) => true
        }
    }

    @CompileTest
    pub def testChooseStar02(): Bool = {
        // P2: check the lower bound by using result in a choose
        let star = choose* Expr.Cst(2i8) {
            case Expr.Cst(_) => Expr.Var("tmp")
            case Expr.Not(_) => Expr.Var("tmp")
            case Expr.Xor(_, _) => Expr.Var("tmp")
        };
        choose star {
            case Expr.Var(_) => true
        }
    }

    @CompileTest
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

}
