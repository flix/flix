mod BooleanFormulas {

    pub restrictable enum Exp[s] {
        case Var(Int32)
        case Cst(Bool)
        case Not(Exp[s])
        case Or(Exp[s], Exp[s])
        case And(Exp[s], Exp[s])
        case Xor(Exp[s], Exp[s])
    }

    pub def main(): Unit \ IO = {
        let subst = Map#{1 => true, 2 => false};
        // ~v1 and (v2 xor true)
        let example = open_variant Exp.And(open_variant Exp.Not(open_variant Exp.Var(1)), open_variant Exp.Xor(open_variant Exp.Var(2), open_variant Exp.Cst(true)));
        println("input is '${toString(example)}'");
        println("input size ${size(example)}");
        println("input evaluates to '${fastrun(subst, example)}'");
        println("using the mapping ${subst}");
        let fls = Exp.Cst(false);
        println("input ${if (eq(example, fls)) "is" else "is not"} structurally equal to '${toString(fls)}'")
    }

    pub def evalNaive(e: Exp[rvnot<Exp.Var>]): Bool =
        choose e {
            // Var case omitted: We can only evaluate closed terms.
            case Exp.Cst(b)     => b
            case Exp.Not(x)     => not evalNaive(x)
            case Exp.Or(x, y)   => evalNaive(x) or evalNaive(y)
            case Exp.And(x, y)  => evalNaive(x) and evalNaive(y)
            case Exp.Xor(x, y)  => evalNaive(x) != evalNaive(y)
        }

    pub def eval(e: Exp[s rvsub <Exp.Var>]): Bool =
        choose e {
            // Var case omitted: We can only evaluate closed terms.
            case Exp.Cst(b)     => b
            case Exp.Not(x)     => not eval(x)
            case Exp.Or(x, y)   => eval(x) or eval(y)
            case Exp.And(x, y)  => eval(x) and eval(y)
            case Exp.Xor(x, y)  => eval(x) != eval(y)
        }

    pub def simplifyNaive(e: Exp[s]): Exp[rvnot<Exp.Xor>] =
        choose e {
            case Exp.Var(x)     => open_variant Exp.Var(x)
            case Exp.Cst(b)     => open_variant Exp.Cst(b)
            case Exp.Not(x)     => open_variant Exp.Not(simplifyNaive(x))
            case Exp.Or(x, y)   => open_variant Exp.Or(simplifyNaive(x), simplifyNaive(y))
            case Exp.And(x, y)  => open_variant Exp.And(simplifyNaive(x), simplifyNaive(y))
            case Exp.Xor(x, y)  =>
                let x1 = simplifyNaive(x);
                let y1 = simplifyNaive(y);
                open_variant Exp.Or(open_variant Exp.And(x1, open_variant Exp.Not(y1)), open_variant Exp.And(open_variant Exp.Not(x1), y1))
        }

    pub def map(f: Int32 -> Int32, e: Exp[s]): Exp[s] =
        choose* e {
            case Exp.Var(x)     => Exp.Var(f(x))
            case Exp.Cst(b)     => Exp.Cst(b)
            case Exp.Not(x)     => open_variant Exp.Not(map(f, x))
            case Exp.Or(x, y)   => open_variant Exp.Or(map(f, x), map(f, y))
            case Exp.And(x, y)  => open_variant Exp.And(map(f, x), map(f, y))
            case Exp.Xor(x, y)  => open_variant Exp.Xor(map(f, x), map(f, y))
        }

    pub def simplify(e: Exp[s]): Exp[(s rvsub <Exp.Xor>) rvadd <Exp.Or, Exp.And, Exp.Not>] =
        choose* e {
            case Exp.Var(x)     => open_variant Exp.Var(x)
            case Exp.Cst(b)     => open_variant Exp.Cst(b)
            case Exp.Not(x)     => open_variant Exp.Not(simplify(x))
            case Exp.Or(x, y)   => open_variant Exp.Or(simplify(x), simplify(y))
            case Exp.And(x, y)  => open_variant Exp.And(simplify(x), simplify(y))
            case Exp.Xor(x, y)  =>
                let x1 = simplify(x);
                let y1 = simplify(y);
                open_variant Exp.Or(open_variant Exp.And(x1, open_variant Exp.Not(y1)), open_variant Exp.And(open_variant Exp.Not(x1), y1))
    }

    pub def run(e: Exp[s rvsub <Exp.Var>]): Bool =
        let f = simplify >> eval;
        f(e)

    pub def subst(m: Map[Int32, Bool], e: Exp[s]): Exp[(s rvsub <Exp.Var>) rvadd <Exp.Cst>] =
        choose* e {
            case Exp.Var(x)     => Exp.Cst(Map.getWithDefault(x, false, m))
            case Exp.Cst(b)     => Exp.Cst(b)
            case Exp.Not(x)     => open_variant Exp.Not(subst(m, x))
            case Exp.Or(x, y)   => open_variant Exp.Or(subst(m, x), subst(m, y))
            case Exp.And(x, y)  => open_variant Exp.And(subst(m, x), subst(m, y))
            case Exp.Xor(x, y)  => open_variant Exp.Xor(subst(m, x), subst(m, y))
        }

    pub def fasteval(e: Exp[s rvand <Exp.Cst, Exp.Not, Exp.And, Exp.Or>]): Bool =
        choose e {
            // Var case omitted: We can only evaluate closed terms.
            case Exp.Cst(b)     => b
            case Exp.Not(x)     => not eval(x)
            case Exp.Or(x, y)   => eval(x) or eval(y)
            case Exp.And(x, y)  => eval(x) and eval(y)
        }

    pub def fastrun(m: Map[Int32, Bool], e: Exp[s]): Bool =
        let f = m1 -> simplify >> subst(m1) >> fasteval;
        f(m, e)

    //
    // Functions not found in the paper
    //

    pub def size(e: Exp[s]): Int32 = choose e {
        case Exp.Cst(_) => 1
        case Exp.Var(_) => 1
        case Exp.Not(x) => size(x) + 1
        case Exp.And(x, y) => size(x) + size(y) + 1
        case Exp.Or(x, y) => size(x) + size(y) + 1
        case Exp.Xor(x, y) => size(x) + size(y) + 1
    }


    /// Structural Equality
    pub def eq(e1: Exp[s1], e2: Exp[s2]): Bool = choose e1 {
        case Exp.Var(x1) => choose e2 {
            case Exp.Var(x2) => x1 == x2
            case Exp.Cst(_) => false
            case Exp.Not(_) => false
            case Exp.And(_, _) => false
            case Exp.Or(_, _) => false
            case Exp.Xor(_, _) => false
        }
        case Exp.Cst(b1) => choose e2 {
            case Exp.Cst(b2) => b1 == b2
            case Exp.Var(_) => false
            case Exp.Not(_) => false
            case Exp.And(_, _) => false
            case Exp.Or(_, _) => false
            case Exp.Xor(_, _) => false
        }
        case Exp.Not(x1) => choose e2 {
            case Exp.Not(x2) => eq(x1, x2)
            case Exp.Var(_) => false
            case Exp.Cst(_) => false
            case Exp.And(_, _) => false
            case Exp.Or(_, _) => false
            case Exp.Xor(_, _) => false
        }
        case Exp.And(x1, y1) => choose e2 {
            case Exp.And(x2, y2) => eq(x1, x2) and eq(y1, y2)
            case Exp.Var(_) => false
            case Exp.Cst(_) => false
            case Exp.Not(_) => false
            case Exp.Or(_, _) => false
            case Exp.Xor(_, _) => false
        }
        case Exp.Or(x1, y1) => choose e2 {
            case Exp.Or(x2, y2) => eq(x1, x2) and eq(y1, y2)
            case Exp.Var(_) => false
            case Exp.Cst(_) => false
            case Exp.Not(_) => false
            case Exp.And(_, _) => false
            case Exp.Xor(_, _) => false
        }
        case Exp.Xor(x1, y1) => choose e2 {
            case Exp.Xor(x2, y2) => eq(x1, x2) and eq(y1, y2)
            case Exp.Var(_) => false
            case Exp.Cst(_) => false
            case Exp.Not(_) => false
            case Exp.And(_, _) => false
            case Exp.Or(_, _) => false
        }
    }

    pub def toString(e: Exp[s]): String = choose e {
        case Exp.Var(x) => "v${x}"
        case Exp.Cst(b) => "${b}"
        case Exp.Not(x) => "~(${toString(x)})"
        case Exp.And(x, y) => "(${toString(x)} and ${toString(y)})"
        case Exp.Or(x, y) => "(${toString(x)} or ${toString(y)})"
        case Exp.Xor(x, y) => "(${toString(x)} xor ${toString(y)})"
    }

}

instance ToString[BooleanFormulas.Exp[s]] {
    pub def toString(x: BooleanFormulas.Exp[s]): String =
        BooleanFormulas.toString(x)
}

instance Eq[BooleanFormulas.Exp[s]] {
    pub def eq(x: BooleanFormulas.Exp[s], y: BooleanFormulas.Exp[s]): Bool =
        BooleanFormulas.eq(x, y)
}
