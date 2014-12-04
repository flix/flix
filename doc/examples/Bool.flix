namespace MyBool {

    type Elm = enum {
        case Top,
        case True,
        case False,
        case Bot
    };

    def leq(e1: Elm, e2: Elm): Bool = match (e1, e2) with {
        case (Bot, _)       => true;
        case (True, True)   => true;
        case (False, False) => true;
        case (_, Top)       => true;
        case _              => false;
    };

    def lub(e1: Elm, e2: Elm): Elm = match (e1, e2) with {
        case (Bot, x)       => x;
        case (x, Bot)       => x;
        case (True, True)   => True;
        case (False, False) => False;
        case _              => Top;
    };

    @monotone @strict @associative
    def and(e1: Elm, e2: Elm): Elm = match (e1, e2) with {
        case (Bot, _)       => Bot;

    };

    lat Lattice = record {
        bot = Bot,
        leq = leq,
        lub = lub
    };

};

namespace Example {

    var A: MyBool.Lattice;
    var B: MyBool.Lattice;
    var R: MyBool.Lattice;

    fact A(MyBool.True);
    fact A(MyBool.False);

    rule R(x) if A(x);
    rule R(x) if B(x);

};

