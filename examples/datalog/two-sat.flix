// https://github.com/souffle-lang/souffle/tree/master/tests/example/2sat

def main(_args: Array[String]): Int & Impure =

    let clauses = #{
        Clause("Pos", "x0", "Pos", "x2").
        Clause("Pos", "x0", "Neg", "x3").
        Clause("Pos", "x2", "Neg", "x3").
        Clause("Pos", "x1", "Neg", "x4").
        Clause("Neg", "x2", "Neg", "x4").
        Clause("Neg", "x0", "Neg", "x5").
        Clause("Neg", "x1", "Neg", "x5").
        Clause("Neg", "x2", "Neg", "x5").
        Clause("Pos", "x3", "Pos", "x6").
        Clause("Pos", "x4", "Pos", "x6").
        Clause("Pos", "x5", "Pos", "x6").
        Clause("Pos", "x6", "Neg", "x2").
        Clause("Neg", "x6", "Pos", "x2").
        Clause("Neg", "x6", "Neg", "x2").
    };

    let twoSatSolver = #{
        Not("Pos", "Neg").
        Not("Neg", "Pos").

        Impl(m, u, n, v) :- Not(m, p), Clause(p, u, n, v).
        Impl(m, u, n, v) :- Not(m, p), Clause(n, v, p, u).

        Impl(m, u, n, v) :- Impl(m, u, p, w), Impl(p, w, n, v).

        Incon(u) :- Impl("Pos", u, "Neg", u), Impl("Neg", u, "Pos", u).

        // Original Form
        //Satisfiable("Yes") :- not Incon(_).
        //Satisfiable("No") :- Incon(_).

        // Fix for Flix
        Satisfiable("Yes") :- not AnyIncon().
        Satisfiable("No") :- AnyIncon().
        AnyIncon() :- Incon(_).

    };

    query twoSatSolver <+> clauses select x from Satisfiable(x) |> println;

    0
