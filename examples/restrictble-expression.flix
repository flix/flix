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
    ???
}
