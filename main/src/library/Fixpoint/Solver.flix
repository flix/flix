// TODO

namespace Fixpoint/Solver {

    use Fixpoint/Ast.Datalog;
    use Fixpoint/Ast.PredSym;

    pub def solve[v: Order](d: Datalog[v]): Datalog[v] =
        let _f = Console.printLine("Hello from solve!") as & Pure;
        d

    pub def compose[v: Order](d1: Datalog[v], _d2: Datalog[v]): Datalog[v] =
        let _f = Console.printLine("Hello from compose!") as & Pure;
        d1

    pub def project[v: Order](d: Datalog[v], _p: PredSym): Datalog[v] =
        let _f = Console.printLine("Hello from project!") as & Pure;
        d

    // TODO: Entails

    // TODO: FOLD

}
