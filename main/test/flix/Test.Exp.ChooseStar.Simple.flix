mod Test.Exp.ChooseStar.Simple {

    restrictable enum Expr[s] {
        case Cst, Var, Not, And, Or, Xor
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
        let star = choose* Expr.Cst {
            case Expr.Cst => Expr.Var
        };
        choose star {
            case Expr.Var => true
        }
    }

    @CompileTest
    pub def testChooseStar02(): Bool = {
        // P2: check the lower bound by using result in a choose
        let star = choose* Expr.Cst {
            case Expr.Cst => Expr.Var
            case Expr.Not => Expr.Var
            case Expr.Xor => Expr.Var
        };
        choose star {
            case Expr.Var => true
        }
    }

    @CompileTest
    pub def testChooseStar03(): Bool = {
        // P2: check the lower bound by using result in a choose
        let star = choose* Expr.Cst {
            case Expr.Cst => Expr.Var
            case Expr.Not => Expr.Var
            case Expr.Xor => Expr.Not
        };
        choose star {
            case Expr.Var => true
            case Expr.Not => true
        }
    }

   @CompileTest
   pub def testChooseStar04(): Bool = {
       // P2: check the lower bound by using result in a choose
       let star = choose* Expr.Cst {
           case Expr.Cst => Expr.Var
           case Expr.Not => Expr.Var
           case Expr.Xor => Expr.Not
       };
       choose star {
           case Expr.Var => true
           case Expr.Not => true
           case Expr.Xor => false
       }
   }

   @CompileTest
   pub def testChooseStar05(): Bool = {
       // P3: Check that not is not present
       let star = choose* Expr.Cst {
           case Expr.Not => Expr.Not
           case Expr.Cst => Expr.Var
       };
       choose star {
           case Expr.Var => false
       }
   }

   @CompileTest
   pub def testChooseStar06(): Bool = {
       // P3: Check that not is not present
       let star = choose* Expr.Cst {
           case Expr.Not => Expr.Not
           case Expr.Cst => Expr.Cst
       };
       choose star {
           case Expr.Cst => true
       }
   }

    @CompileTest
    pub def testChooseStar07(): Bool = {
        // P3: Check that Not is not present
        let _f = c -> choose* c {
            case Expr.Not => Expr.Not
            case Expr.Cst => Expr.Var
        };
        let _g = c -> choose* c {
            case Expr.Not => Expr.Not
            case Expr.Xor => Expr.And
        };
        true
    }

    @CompileTest
    pub def testChoose08(): Bool = {
        // P1: Check that choose* upperbounds the input like choose
        let thing = choose* Expr.Cst {
            case Expr.Cst => Expr.Cst
        };
        choose thing {case Expr.Cst => true}
    }

    @CompileTest
    pub def testChoose09(): Bool = {
        // P1: Check that choose* upperbounds the input like choose
        let thing = choose* Expr.Cst {
            case Expr.Cst => Expr.Cst
            case Expr.Var => Expr.Cst
        };
        choose thing {case Expr.Cst => true}
    }

    @CompileTest
    pub def testChoose10(): Bool = {
        // P1: Check that choose* upperbounds the input like choose
        let cstOrVar = if (true) open_variant Expr.Cst else open_variant Expr.Var;
        let thing = choose* cstOrVar {
            case Expr.Cst => Expr.Cst
            case Expr.Var => Expr.Cst
        };
        choose thing {case Expr.Cst => true}
    }

    @CompileTest
    pub def testChoose11(): Bool = {
        // P1: Check that choose* upperbounds the input like choose
        let cstOrVar = if (true) open_variant Expr.Cst else open_variant Expr.Var;
        let thing = choose* cstOrVar {
            case Expr.Xor => Expr.Xor
            case Expr.Cst => Expr.Cst
            case Expr.Var => Expr.Xor
        };
        choose thing {
            case Expr.Xor => true
            case Expr.Cst => true
        }
    }

    @CompileTest
    pub def testChoose14(): Bool = {
        // P1: Check that choose* upperbounds the input like choose
        let id = x -> choose* x {
            case Expr.And    => Expr.And
            case Expr.Cst    => Expr.Cst
            case Expr.Not    => Expr.Not
            case Expr.Or     => Expr.Or
        };
        choose id(Expr.Cst) {case Expr.Cst => true}
    }

}
