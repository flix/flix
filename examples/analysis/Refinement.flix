//
// Implementation of Abstraction Refinement in Flix!
//

// We begin by defining some data constructors.

// TODO: Use parity, it works better for division examples.

//
// We assume a simple program language which has three instructions:
//
rel LitStm(stm: Int, localVar: Str, const: Int)
rel AddStm(stm: Int, r: Str, x: Str, y: Str)
rel DivStm(stm: Int, r: Str, x: Str, y: Str)

//
// The statements of the program are connected by at control-flow graph.
//
rel CFG(s1: Int, s2: Int)

//
// We now introduce the idea of refinement
//
enum Domain {
    case Interval,
    case Constant,
    case Sign
}

enum Fuel {
    case F(Int)
}

enum Ref {
    case Ref(Domain, Fuel)
}
// TODO: Define partial order ^

//
// At each program point, i.e. control-flow graph node we will assign an element
// of the refinement lattice to every local variable.
//
lat Refinement(stm: Int, localVar: Str, ref: Ref)

//
// We can now define the semantics of the analysis.
//
lat LocalVar(stm: Int, var: Str, value: Value)

//
// Literal statements.
//
// We introduce an abstration rule for each abstract domain.
//
// The rule for the lowest abstract domain has no pre-conditions,
// whereas the rules for the other abstract domains requires refinement.
//
LocalVar(s, x, alpha2s(c)) :- LitStm(s, x, c).

LocalVar(s, x, alpha2c(c)) :- requiresConstant(r), LitStm(s, x, c), Refinement(s, x, r).

LocalVar(s, x, alpha2i(c)) :- enableInterval(r), LitStm(s, x, c), Refinement(s, x, r).

//
// Addition statements.
//
LocalVar(s2, r, sum(v1, v2)) :- AddStm(s2, r, x, y),
                                LocalVar(s1, x, v1),
                                LocalVar(s2, y, v2),
                                CFG(s1, s2).

//
// Division statements.
//
LocalVar(s2, r, div(v1, v2)) :- DivStm(s2, r, x, y),
                                LocalVar(s1, x, v1),
                                LocalVar(s2, y, v2),
                                CFG(s1, s2).

//
// Refinement.
//


def plus(x: RefLat, y: RefLat): RefLat = match (x, y,) with {
    case (RefLat.Sign(s), RefLat.Sign(s)) => RefLat.Sign(s1 `signPlus` s2)
}
