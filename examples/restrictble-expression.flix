/// Boolean expressions using `t` as variable indentifiers.
restrictable enum Expr[s][t] {
    case Cst(Bool)
    case Var(t)
    case Not(Expr[s][t])
    case And(Expr[s][t], Expr[s][t])
    case Or(Expr[s][t], Expr[s][t])
    case Xor(Expr[s][t], Expr[s][t])
}

def eq(e1: Expr[s1][t], e2: Expr[s1][t]): Bool = ??? // nested choose

def main(): Unit = {
    let example0 = Expr.Cst(true);
    let example1 = open Expr.Not(open Expr.Cst(true));
    let example2 = open Expr.And(
        open Expr.Var("x"),
        open Expr.Not(open Expr.Cst(false))
    );
    let example3 = open Expr.And(
        open Expr.Xor(
            open Expr.Var(1),
            open Expr.Not(open Expr.Var(2))),
        open Expr.Not(
            open Expr.Xor(
                open Expr.Cst(true),
                open Expr.Var(3)))
        );
    let eval = e -> choose e {
        case Expr.Cst(b)    => b
        case Expr.Not(x)    => not eval(x)
        case Expr.And(x, y) => eval(x) and eval(y)
        case Expr.Or(x, y)  => eval(x) or eval(y)
        case Expr.Xor(x, y) =>
            let xx = eval(x);
            let yy = eval(y);
            (xx and (not yy)) or ((not xx) and yy)
    };
    let simpleSimplify = e -> choose e {
        case Expr.Cst(b)    => open Expr.Cst(b)
        case Expr.Not(x)    => open Expr.Not(x)
        case Expr.And(x, y) => open Expr.And(simpleSimplify(x), simpleSimplify(y))
        case Expr.Or(x, y)  => open Expr.Or(simpleSimplify(x), simpleSimplify(y))
        case Expr.Xor(x, y) =>
            let xx = simpleSimplify(x);
            let yy = simpleSimplify(y);
            open Expr.Or(
                open Expr.And(xx, open Expr.Not(yy)),
                open Expr.And(open Expr.Not(xx), yy)
            )
    };
    ???
}
