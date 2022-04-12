// https://github.com/souffle-lang/benchmarks/tree/master/benchmarks/topological_ordering

def genEdges(n: Int32): #{Edge(Int32, Int32) | _} = {
    if (n <= 0) #{}
    else #{
        Edge(n, n*2).
        Edge(n, n*2+1).
    } <+> genEdges(n-1)
}

def main(): Int32 & Impure =

    let edges = genEdges(20);

    let lp = #{

        // Added for Flix
        IndicesPlusOne(x, i+1) :- Indices(x, i).
        EdgeSecond(x) :- Edge(_, x).
        EdgeFirst(x) :- Edge(x, _).

        // Original Program

        Vertex(v) :- Edge(v,_).
        Vertex(v) :- Edge(_,v).

        IsBefore(x, y) :- Edge(x, y).

        IsBefore(x, y) :-
            IsBefore(x, z),
            IsBefore(z, y).

        IsAfter(x, y) :-
            Edge(y, x).

        IsAfter(x, y) :-
            IsAfter(z, x),
            IsAfter(y, z).

        Indices(x, 0) :-
            Vertex(x),
            not EdgeSecond(x),
            not EdgeFirst(x).

        Indices(x, 1) :-
            Vertex(x),
            not EdgeSecond(x),
            Edge(x, _).

        Indices(x, i+1) :-
            IsBefore(y, x),
            not IsBefore(x, y),
            IsAfter(x, y),
            Indices(y, i).

        Index(x, i) :-
            Indices(x, i),
            not IndicesPlusOne(x, i).
    };

    query lp <+> edges select (x, i) from Index(x, i) |> println;

    0
