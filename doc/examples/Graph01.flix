namespace Graph {

    type Vertex = Str;

    var Edge:           Vertex -> {Vertex};
    var Reachable:      Vertex -> {Vertex};
    var Cycle:          {Vertex};

    fact Edge "a" -> {"b"};
    fact Edge "b" -> {"a"};

    rule Reachable x -> {y} if Edge x -> {y};
    rule Reachable x -> {z} if Reachable x -> {y}, Reachable y -> {z};

    rule Cycle {x} if Reachble x -> {x};

};
