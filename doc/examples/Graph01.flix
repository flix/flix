namespace Graph {

    var Edge:           Str -> {Str};
    var Reachable:      Str -> {Str};
    var Cycle:          {Str};

    fact Edge "a" -> {"b"};
    fact Edge "b" -> {"a"};

    rule Reachable x -> {y} if Edge x -> {y};
    rule Reachable x -> {z} if Reachable x -> {y}, Reachable y -> {z};

    rule Cycle {x} if Reachble x -> {x};

};
