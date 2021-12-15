def main(_args: Array[String]): Int32 & Impure =
    let sample = sampleConnections();
    let fix = connectNetwork(sampleConnections());
    fix |> println;
    "${fix.length - List.length(sample)} edges were added" |> println;
    0

type alias NetworkNode = Int32
type alias Connection = (NetworkNode, NetworkNode)

def sampleConnections(): List[Connection] & Impure =
    [(0, 4), (0, 7), (1, 3), (2, 3), (5, 6)] |> Array.toList
    //    ┌─┐           ┌─┐
    //    │5│       ┌─┐ │2│
    //    └─┴─┐     │3├─┴─┘
    //  ┌─┐   ├─┐<─>└─┘
    //  │1│   │6│    ∧
    //  └─┴───┴─┘    |
    //  ┌─┐    ∧     |
    //  │4│    └─ ─ ─┐
    //  └─┴──┬─┐     ∨
    //       │0│    ┌─┐
    //       └─┴────┤7│
    //              └─┘

def connectNetwork(connections: List[Connection]): Array[Connection] =
    let layout = project connections into Connection;
    let reach = #{
        Node(c) :- Connection(c, _).
        Node(c) :- Connection(_, c).
        Reach(c, c) :- Node(c).
        Reach(c1, c2) :- Connection(c1, c2).
        Reach(c1, c2) :- Reach(c2, c1).
        Reach(c1, c2) :- Reach(c1, m), Reach(m, c2).
        OrderReach(c1) :- Reach(c1, c2), if c1 < c2.
        SccRepr(c1) :- Node(c1), not OrderReach(c1).
    };
    let sccNetwork = solve layout, reach project Node, Connection, SccRepr;
    let missingConnections = #{
        // Illegal attempt
        // Connection(c1, c2) :- SccRepr(c1), SccRepr(c2), if c1 != c2 and c1 < c2.


        // Workaround
        NewConnection(c1, c2) :- SccRepr(c1), SccRepr(c2), if c1 != c2 and c1 < c2.
        NewConnection(c1, c2) :- Connection(c1, c2).
    };
    // Illegal attempt
    // query sccNetwork, missingConnections select (c1, c2) from Connection(c1, c2)

    // Workaround
    query sccNetwork, missingConnections select (c1, c2) from NewConnection(c1, c2)
