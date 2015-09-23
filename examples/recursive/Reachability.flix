namespace Reachability {
    type Vertex = Str;

    //var Edge: Vertex -> { Bool }; // TODO

    // Edges
    fact Edge "x" -> {"y"};
    fact Edge "y" -> {"z"};
    fact Edge "z" -> {"x"};

    fact Edge "a" -> {"b", "c", "d"};
    fact Edge "b" -> {"c", "d"};
    fact Edge "c" -> {"a"};

    // Reflexivity
    rule Edge x -> {x} if Edge x -> {_};

    // Transitivity
    rule Edge x -> {z} if Edge x -> {y}, Edge y -> {z};
};