// Inputs
rel EshIntra(node: String, factin: String, factout: String)
rel EshCallStart(node: String, factin: String, proc: String, factout: String)
rel EshEndReturn(proc: String, factin: String, node: String, factout: String)

// Call-to-Return edges should be included above in EshIntra
rel CFG(from: String, to: String)
rel CallGraph(site: String, proc: String)
rel StartNode(proc: String, node: String) // TODO: should really be a function
rel EndNode(proc: String, node: String)

// Internal lattices
rel PathEdge(factin: String, node: String, fact: String)
rel SummaryEdge(node: String, factin: String, factout: String)

// Output
rel Results(node: String, fact: String)

rel Facts(fact: String)
rel Id(node: String)

// Rules
def main(): Unit =
    let p = #{
        // intraproc
        PathEdge(d1,m,d3) :-
            PathEdge(d1,n,d2),
            EshIntra(n,d2,d3),
            CFG(n,m).

        // use summary
        PathEdge(d1,m,d3) :-
            PathEdge(d1,n,d2),
            SummaryEdge(n,d2,d3),
            CFG(n,m).

        // call-to-start
        PathEdge(d3,start,d3) :-
            PathEdge(_d1,call,d2),
            CallGraph(call,target),
            EshCallStart(call,d2,target,d3),
            StartNode(target,start).

        // compute summary
        SummaryEdge(call, d4, d5s) :-
            CallGraph(call,target),
            StartNode(target,_start),
            EndNode(target,end),
            EshCallStart(call,d4,target,d1),
            PathEdge(d1,end,d2),
            EshEndReturn(target,d2,call,d5s).

        // tabulate result
        Results(n,d2) :- PathEdge(_,n,d2).

        // Example (uninitialized variables) from IFDS paper
        CFG("smain","n1").
        CFG("n1","n2").
        CFG("n2","n3").
        CFG("n3","emain").
        CFG("sp","n4").
        CFG("n4","n5").
        CFG("n4","ep").
        CFG("n5","n6").
        CFG("n6","n7").
        CFG("n7","n8").
        CFG("n8","n9").
        CFG("n9","ep").

        StartNode("main","smain").
        StartNode("p","sp").
        EndNode("main","emain").
        EndNode("p","ep").

        CallGraph("n2","p").
        CallGraph("n7","p").

        Facts("x").
        Facts("g").
        Facts("a").

        EshIntra(n,"zero","zero") :- CFG(n,_).
        EshIntra("smain","zero","x").
        EshIntra("smain","zero","g").
        EshIntra("n1","g","g").
        EshIntra("n2","x","x").

        Id("n3").
        Id("sp").
        Id("n4").
        Id("n6").
        Id("n8").
        Id("n9").

        EshIntra(idnode,f,f) :- Facts(f), Id(idnode).
        EshIntra("n5","a","a").
        EshIntra("n6","g","a").
        EshIntra("n7","a","a").

        EshCallStart(call,"zero",target,"zero") :- CallGraph(call,target).
        EshEndReturn(target,"zero",call,"zero") :- CallGraph(call,target).

        EshCallStart("n2","x","p","a").
        EshCallStart("n2","g","p","g").
        EshEndReturn("p","g","n2","g").

        EshCallStart("n7","a","p","a").
        EshCallStart("n7","g","p","g").
        EshEndReturn("p","g","n7","g").

        // Entrypoint
        PathEdge("zero","smain","zero").
    };
    let _m = solve p;
    ()
