mod IFDS {

    ///
    /// An IFDS instance where:
    ///
    /// - p is the type of procedures.
    /// - n is the type of program point.
    /// - d is the type of analysis data elements.
    ///
    type alias IFDS[p, n, d] = {
        zero            = d,
        main            = p,
        cfg             = List[(n, n)],
        startNodes      = List[(p, n)],
        endNodes        = List[(p, n)],
        callGraph       = List[(n, p)],
        eshIntra        = (n, d) -> Vector[d],
        eshCallStart    = (n, d, p) -> Vector[d],
        eshEndReturn    = (p, d, n) -> Vector[d]
    }

    ///
    /// Solves the given IFDS instances.
    ///
    pub def run(ifds: IFDS[p, n, d]): Vector[(n, d)]
        with Order[n], Order[d], Order[p] =

        // Extract main.
        let main = ifds#main;

        def prependZero(v) = Vector#{ifds#zero} ++ v;
        def augmentForZero(d, v) = if (d == ifds#zero) prependZero(v) else v;

        def eshIntra1(n, d) = augmentForZero(d, ifds#eshIntra(n, d));
        def eshCallStart1(n, d, p) = augmentForZero(d, ifds#eshCallStart(n, d, p));
        def eshEndReturn1(p, d, n) = augmentForZero(d, ifds#eshEndReturn(p, d, n));

        //
        // The core IFDS Framework.
        //
        let p = #{
            // nodes in a given procedure
            InProc(p, start) :- StartNode(p, start).
            InProc(p, m) :- InProc(p, n), CFG(n, m).

            // procedures in the program.
            Proc(p) :- InProc(p, _).

            // intraproc
            PathEdge(d1, m, d3) :-
                CFG(n, m),
                PathEdge(d1, n, d2),
                let d3 = eshIntra1(n, d2).

            // use summary
            PathEdge(d1, m, d3) :-
                CFG(n, m),
                PathEdge(d1, n, d2),
                SummaryEdge(n, d2, d3).

            EshCallStart(call, d2, target, d3) :-
                PathEdge(_d1, call, d2),
                CallGraph(call, target),
                let d3 = eshCallStart1(call, d2, target).

            PathEdge(d3, start, d3) :-
                EshCallStart(call, d2, target, d3),
                StartNode(target, start).

            // Line [2] from Figure 3 in IFDS paper
            PathEdge(ifds#zero, n, ifds#zero) :- StartNode(main, n).

            // compute summary
            SummaryEdge(call, d4, d5) :-
                CallGraph(call, target),
                StartNode(target, _start),
                EndNode(target, end),
                EshCallStart(call, d4, target, d1),
                PathEdge(d1, end, d2),
                let d5 = eshEndReturn1(target, d2, call).

            Results(n, d2) :- PathEdge(_,n,d2).
        };


        let f1 = inject ifds#cfg, ifds#callGraph, ifds#startNodes, ifds#endNodes into CFG, CallGraph, StartNode, EndNode;

        query p, f1 select (n, d) from Results(n, d)

}

pub enum IR with Eq {
    case MainEntry(Vector[String]), // main entry point of program
    case Nop,
    case Call(String, String), // call a procedure, pass variable as argument
    case Read(String), // read input into a variable
    case Assign(String, String, String) // v1 := v2 - v3
}

///
/// Run the unintialized variables example from IFDS paper.
///
def main(): Unit \ IO =
    println("Running IFDS");

    let cfg =
        ("smain","n1") ::
        ("n1","n2") ::
        ("n2","n3") ::
        ("n3","emain") ::

        ("sp","n4") ::
        ("n4","n5") ::
        ("n4","ep") ::
        ("n5","n6") ::
        ("n6","n7") ::
        ("n7","n8") ::
        ("n8","n9") ::
        ("n9","ep") :: Nil;

    let callGraph =
        ("n2","p") ::
        ("n7","p") :: Nil;

    let startNodes =
        ("main","smain") ::
        ("p","sp") :: Nil;

    let endNodes =
        ("main","emain") ::
        ("p","ep") :: Nil;

    def procedureParameters(p) = match p {
        case "p" => "a"
        case _ => ?unreachable
    };

    let globalVars = Set#{"g"};
    def isGlobalVar(v) = Set.memberOf(v, globalVars);

    let allVars = Set#{"a", "x"} ++ globalVars;

    def instruction(n) = match n {
        case "smain" => IR.MainEntry(Vector#{"x"})
        case "n1" => IR.Read("x")
        case "n2" => IR.Call("p", "x")
        case "n3" => IR.Nop
        case "emain" => IR.Nop

        case "sp" => IR.Nop
        case "n4" => IR.Nop
        case "n5" => IR.Read("g")
        case "n6" => IR.Assign("a", "a", "g")
        case "n7" => IR.Call("p", "a")
        case "n8" => IR.Nop
        case "n9" => IR.Nop
        case "ep" => IR.Nop

        case _ => ?unreachable
    };

    def eshIntra(n, d) = match instruction(n) {
        case IR.MainEntry(vars) =>
            if (d == "zero") vars ++ Foldable.toVector(globalVars)
            else Vector#{}
        case IR.Nop => Vector#{d}
        case IR.Call(target, arg) =>
            if (isGlobalVar(d)) Vector#{} else Vector#{d}
        case IR.Read(v) =>
            if (d == v) Vector#{} else Vector#{d}
        case IR.Assign(v1, v2, v3) =>
            let kill = if (d == v1) Vector#{} else Vector#{d};
            let gen = if (d == v2 or d == v3) Vector#{v1} else Vector#{};
            kill ++ gen
    };

    def eshCallStart(n, d, p) = match instruction(n) {
        case IR.Call(proc, arg) if (proc == p) =>
          let parm = procedureParameters(proc);
          if (d == arg) Vector#{parm}
          else if (d == parm) Vector#{}
          else Vector#{d}
        case _ => Vector#{}
    };

    def eshEndReturn(p, d, n) = match instruction(n) {
        case IR.Call(proc,_) if p == proc and isGlobalVar(d) =>
          Vector#{d}
        case _ => Vector#{}
    };

    let result = IFDS.run({
        zero            = "zero",
        main            = "main",
        cfg             = cfg,
        startNodes      = startNodes,
        endNodes        = endNodes,
        callGraph       = callGraph,
        eshIntra        = eshIntra,
        eshCallStart    = eshCallStart,
        eshEndReturn    = eshEndReturn
        });

    println(result)

    // Prints:
    // Vector#{
    //    (emain, g),
    //    (emain, zero),
    //    (ep, g),
    //    (ep, zero),
    //    (n1, g),
    //    (n1, x),
    //    (n1, zero),
    //    (n2, g),
    //    (n2, zero),
    //    (n3, g),
    //    (n3, zero),
    //    (n4, g),
    //    (n4, zero),
    //    (n5, g),
    //    (n5, zero),
    //    (n6, zero),
    //    (n7, zero),
    //    (n8, zero),
    //    (n9, zero),
    //    (smain, zero),
    //    (sp, g),
    //    (sp, zero)
    // }
