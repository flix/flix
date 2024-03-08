restrictable enum Expr[s] {
    case Cst(Char)
    case Var(String)
    case Not((Int32, BigInt, BigDecimal))
    case And(Float64, Float32)
    case Or(Option[Bool], (Int8, Int16))
    case Xor(Array[Array[Int64, Univ], Univ], Unit)
}

mod Test.Exp.Choose.SimpleTerms {
    pub def testChoose01(): Bool = {
        choose Expr.Cst('a') {
            case Expr.Cst(_) => true
        }
    }

    pub def testChoose02(): Bool = {
        choose Expr.Cst('a') {
            case Expr.Cst(_) => true
            case Expr.Var(_) => false
        }
    }

// TODO RESTR-VARS
    pub def testChoose03(): Bool = {
        let cstOrVar = if (true) open_variant Expr.Cst('a') else open_variant Expr.Var("lol");
        choose cstOrVar {
            case Expr.Cst(_) => true
            case Expr.Var(_) => false
        }
    }

    pub def testChoose04(): Bool = {
        let cstOrVar = if (true) open_variant Expr.Cst('a') else open_variant Expr.Var("lol");
        choose cstOrVar {
            case Expr.Xor(_) => false
            case Expr.Cst(_) => true
            case Expr.Var(_) => false
        }
    }

    pub def testChoose05(): Bool = {
        let f = x -> choose x {
            case Expr.Cst(_) => false
            case Expr.Var(_) => true
        };
        let g = x -> choose x {
            case Expr.Cst(_) => false
            case Expr.Xor(_) => true
        };
        let h = if (true) f else g;
        h(Expr.Cst('a'))
    }

    pub def testChoose06(): Bool = {
        let f = x -> choose x {
            case Expr.Cst(_) => false
            case Expr.Var(_) => true
            case Expr.Not(_) => false
        };
        let g = x -> choose x {
            case Expr.Cst(_) => false
            case Expr.Xor(_) => true
            case Expr.Not(_) => false
        };
        let h = if (true) f else g;

        let cstOrNot = if (true) open_variant Expr.Cst('a') else open_variant Expr.Not((123, 123ii, 1e23ff));
        h(cstOrNot)
    }
}
