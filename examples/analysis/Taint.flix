namespace Taint {

    enum Taint {
        case Secret,
        case NotSecret
    };

// TODO
//    def leq(e1: Taint, e2: Taint): Bool = match (e1, e2) with {
//        case (NotSecret, _)     => true;
//        case (Secret, Secret)   => true;
//        case _                  => false;
//    };
//
//    def lub(e1: Taint, e2: Taint): Bool = match (e1, e2) with {
//        case (Secret, Secret)   => Secret;
//        case _                  => NotSecret;
//    };
//
//    lat Taint = record {
//        bot = NotSecret,
//        leq = leq,
//        lub = lub
//    };
};

//namespace Taint {
//
//    var Var:    Str -> Taint;
//    var Edge:   Str -> {Str};
//
//    fact Var "b" -> Secret;
//
//    fact Edge "a" -> {"b"};
//    fact Edge "b" -> {"c"};
//    fact Edge "c" -> {"f", "g"};
//
//    fact Edge "x" -> {"y"};
//    fact Edge "y" -> {"z"};
//    fact Edge "z" -> {"x"};
//
//    rule Var y -> t if Edge x -> {y}, Var x -> t;
//
//};

