// TODO: Replace by real implementation

namespace Fixpoint/Solver {

    use Fixpoint/Ast.Datalog;
    use Fixpoint/Ast.PredSym;

    pub def solve[v: Order](d: Datalog[v]): Datalog[v] =
        let _f = Console.printLine("Hello from solve!") as & Pure;
        d

    pub def compose[v: Order](d1: Datalog[v], _d2: Datalog[v]): Datalog[v] =
        let _f = Console.printLine("Hello from compose!") as & Pure;
        d1

    pub def entails[v: Order](_d1: Datalog[v], _d2: Datalog[v]): Bool =
        let _f = Console.printLine("Hello from entails!") as & Pure;
        true

    pub def project[v: Order](_p: PredSym, d: Datalog[v]): Datalog[v] =
        let _f = Console.printLine("Hello from project!") as & Pure;
        d

    pub def fold[v: Order, b](_d: Datalog[v], _p: PredSym, i: b, _f: (Array[v], b) -> b): b =
        let _f = Console.printLine("Hello from fold!") as & Pure;
        i

}
