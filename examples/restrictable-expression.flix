restrictable enum Expr[s] {
    case Cst(Bool)
    case Var(Int32)
    case Not(Expr[s])
    case And(Expr[s], Expr[s])
    case Or(Expr[s], Expr[s])
    case Xor(Expr[s], Expr[s])
}

def size(e: Expr[s]): Int32 = choose e {
    case Expr.Cst(_) => 1
    case Expr.Var(_) => 1
    case Expr.Not(x) => size(x) + 1
    case Expr.And(x, y) => size(x) + size(y) + 1
    case Expr.Or(x, y) => size(x) + size(y) + 1
    case Expr.Xor(x, y) => size(x) + size(y) + 1
}

def simplify(e: Expr[s]): Expr[(s -- <Expr.Xor>) ++ <Expr.Not>] =
    choose* e {
        case Expr.Var(x) => Expr.Var(x)
        case Expr.Cst(b) => Expr.Cst(b)
        case Expr.Not(x) => open Expr.Not(openExpr(simplify(x)))
        case Expr.And(x, y) => Expr.And(???, ???)
        case Expr.Or(x, y) => Expr.Or(???, ???)
        case Expr.Xor(x, y) => Expr.Not(???)
    }

def subst(e: Expr[s]): Expr[(s -- <Expr.Var>) ++ <Expr.Cst>] =
    choose* e {
        case Expr.Var(x) => Expr.Cst(true)
        case Expr.Cst(b) => Expr.Cst(b)
        case Expr.Not(x) => open Expr.Not(openExpr(subst(x)))
        case Expr.Or(x, y) => Expr.Or(???, ???)
        case Expr.And(x, y) => Expr.And(???, ???)
        case Expr.Xor(x, y) => Expr.Xor(???, ???)
    }

def fasteval(e: Expr[s -- <Expr.Var, Expr.Xor>]): Bool =
    choose e {
        case Expr.Cst(b) => b
        case Expr.Not(x) => fasteval(x)
        case Expr.Or(x, y) => fasteval(x) or fasteval(y)
        case Expr.And(x, y) => fasteval(x) and fasteval(y)
    }

def openExpr(e: Expr[s]): Expr[s ++ any] =
    unsafe_cast e as Expr[s ++ any]

def eval(e: Expr[s]): Bool = {
    let f = simplify >> subst >> fasteval;
    f(e)
}

def main(): Unit \ IO = {
    let input = open Expr.Not(open Expr.Var(42));
    println("input is Not(v42)");
    println("input size ${size(input)}");
    println("input evaluates to ${eval(input)}")
}


/// Structural Equality
def eq(e1: Expr[s1], e2: Expr[s2]): Bool = choose e1 {
    case Expr.Var(x1) => choose e2 {
        case Expr.Var(x2) => x1 == x2
        case Expr.Cst(_) => false
        case Expr.Not(_) => false
        case Expr.And(_, _) => false
        case Expr.Or(_, _) => false
        case Expr.Xor(_, _) => false
    }
    case Expr.Cst(b1) => choose e2 {
        case Expr.Cst(b2) => b1 == b2
        case Expr.Var(_) => false
        case Expr.Not(_) => false
        case Expr.And(_, _) => false
        case Expr.Or(_, _) => false
        case Expr.Xor(_, _) => false
    }
    case Expr.Not(x1) => choose e2 {
        case Expr.Not(x2) => eq(x1, x2)
        case Expr.Var(_) => false
        case Expr.Cst(_) => false
        case Expr.And(_, _) => false
        case Expr.Or(_, _) => false
        case Expr.Xor(_, _) => false
    }
    case Expr.And(x1, y1) => choose e2 {
        case Expr.And(x2, y2) => eq(x1, x2) and eq(y1, y2)
        case Expr.Var(_) => false
        case Expr.Cst(_) => false
        case Expr.Not(_) => false
        case Expr.Or(_, _) => false
        case Expr.Xor(_, _) => false
    }
    case Expr.Or(x1, y1) => choose e2 {
        case Expr.Or(x2, y2) => eq(x1, x2) and eq(y1, y2)
        case Expr.Var(_) => false
        case Expr.Cst(_) => false
        case Expr.Not(_) => false
        case Expr.And(_, _) => false
        case Expr.Xor(_, _) => false
    }
    case Expr.Xor(x1, y1) => choose e2 {
        case Expr.Xor(x2, y2) => eq(x1, x2) and eq(y1, y2)
        case Expr.Var(_) => false
        case Expr.Cst(_) => false
        case Expr.Not(_) => false
        case Expr.And(_, _) => false
        case Expr.Or(_, _) => false
    }
}
