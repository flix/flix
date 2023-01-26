restrictable enum Expr[s][t] {
    case Cst(Bool)
    case Var(t)
    case Not(Expr[s][t])
    case And(Expr[s][t], Expr[s][t])
    case Or(Expr[s][t], Expr[s][t])
    case Xor(Expr[s][t], Expr[s][t])
}

def size(e: Expr[s][t]): Int32 = choose e {
    case Expr.Cst(_) => 1
    case Expr.Var(_) => 1
    case Expr.Not(x) => size(x) + 1
    case Expr.And(x, y) => size(x) + size(y) + 1
    case Expr.Or(x, y) => size(x) + size(y) + 1
    case Expr.Xor(x, y) => size(x) + size(y) + 1
}

def eq(e1: Expr[s1][t], e2: Expr[s2][t]): Bool = ??? // nested choose

def simplify(e: Expr[s][t]): Expr[(s -- <Expr.Xor>) ++ <Expr.Not>][t] =
    choose* e {
        case Expr.Var(x) => Expr.Var(x)
        case Expr.Cst(b) => Expr.Cst(b)
        case Expr.Not(x) => open Expr.Not(openExpr(simplify(x)))
        case Expr.And(x, y) => Expr.And(???, ???)
        case Expr.Or(x, y) => Expr.Or(???, ???)
        case Expr.Xor(x, y) => Expr.Not(???)
    }

def subst(e: Expr[s][t]): Expr[(s -- <Expr.Var>) ++ <Expr.Cst>][t] =
    choose* e {
        case Expr.Var(x) => Expr.Cst(true)
        case Expr.Cst(b) => Expr.Cst(b)
        case Expr.Not(x) => open Expr.Not(openExpr(subst(x)))
        case Expr.Or(x, y) => Expr.Or(???, ???)
        case Expr.And(x, y) => Expr.And(???, ???)
        case Expr.Xor(x, y) => Expr.Xor(???, ???)
    }

def fasteval(e: Expr[s -- <Expr.Var, Expr.Xor>][t]): Bool =
    choose e {
        case Expr.Cst(b) => b
        case Expr.Not(x) => fasteval(x)
        case Expr.Or(x, y) => fasteval(x) or fasteval(y)
        case Expr.And(x, y) => fasteval(x) and fasteval(y)
    }

def openExpr(e: Expr[s][t]): Expr[s ++ any][t] =
    unsafe_cast e as Expr[s ++ any][t]

def eval(e: Expr[s][t]): Bool = {
    let f = simplify >> subst >> fasteval;
    f(e)
}

def main(): Unit \ IO = {
    let input = open Expr.Not(open Expr.Var('a'));
    println("input is Not(a)");
    println("input size ${size(input)}");
    println("input evaluates to ${eval(input)}")
}
