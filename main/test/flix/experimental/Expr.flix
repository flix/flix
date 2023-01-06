namespace Test/Restrictable/Expr {

enum Expr[s] {
    case Var(Int32)
    case Cst(Bool)
    case Not(Expr[s])
    case Or(Expr[s], Expr[s])
    case And(Expr[s], Expr[s])
    case Xor(Expr[s], Expr[s])
}

def eval(e: Expr[{~Var}]): Bool =
    choose e {
        // Variable case omitted -- We can only evaluate closed terms.
        case Cst(b)     => b
        case Not(x)     => not eval(x)
        case Or(x, y)   => eval(x) or eval(y)
        case And(x, y)  => eval(x) and eval(y)
        case Xor(x, y)  =>
            let v1 = eval(x);
            let v2 = eval(y);
            (v1 and not v2) or (not v1 and v2)
    }

def simplifySimple(e: Expr[s]): Expr[{~Xor}] =
    choose e {
        case Var(x)     => Var(x)
        case Cst(b)     => Cst(b)
        case Not(x)     => Not(simplify(x))
        case Or(x, y)   => Or(simplify(x), simplify(y))
        case And(x, y)  => And(simplify(x), simplify(y))
        case Xor(x, y)  => Or(And(x, Not(y)), And(Not(x), y))
}

def simplify(e: Expr[s]): Expr[{s - Xor}] =
    choose* e {
        case Var(x)     => Var(x)
        case Cst(b)     => Cst(b)
        case Not(x)     => Not(simplify(x))
        case Or(x, y)   => Or(simplify(x), simplify(y))
        case And(x, y)  => And(simplify(x), simplify(y))
        case Xor(x, y)  => Or(And(x, Not(y)), And(Not(x), y))
}


def map(f: Int32 -> Int32, e: Expr[s]): Expr[s] =
    choose* e {
        case Var(x)     => Var(f(x))
        case Cst(b)     => Cst(b)
        case Not(x)     => Not(map(f, x))
        case Or(x, y)   => Or(map(f, x), map(y))
        case And(x, y)  => And(map(f, x), map(f, y))
        case Xor(x, y)  => Xor(map(f, x), map(f, y))
    }

def subst(e: Expr[s], m: Map[Int32, Bool]): Expr[{(s - Var) + Cst}] =
    choose* e {
        case Var(x)     => Cst(Map.getWithDefault(x, false, m))
        case Cst(b)     => Cst(b)
        case Not(x)     => Not(subst(x, m))
        case Or(x, y)   => Or(subst(x, m), subst(y, m))
        case And(x, y)  => And(subst(x, m), subst(y, m))
        case Xor(x, y)  => Xor(subst(x, m), subst(y, m))
    }

}
