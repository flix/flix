restrictable enum Exp[s] {
        case Var(Int32)
        case Cst(Bool)
        case Not(Exp[s])
        case Or(Exp[s], Exp[s])
        case And(Exp[s], Exp[s])
        case Xor(Exp[s], Exp[s])
    }

def simplify(e: Exp[s rvsub <Exp.Var>]): Exp[(s rvsub <Exp.Xor>) rvadd <Exp.And>] =
    choose* e {
        case Exp.Cst(b)     => open_variant Exp.Cst(b)
        case Exp.Not(x)     => open_variant Exp.Not(simplify(x))
        case Exp.Or(x, y)   => open_variant Exp.Or(simplify(x), simplify(y))
        case Exp.And(x, y)  => open_variant Exp.And(simplify(x), simplify(y))
        case Exp.Xor(x, y)  => open_variant Exp.And(simplify(x), simplify(y))
    }
