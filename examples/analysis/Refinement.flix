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

// TODO: Don't use r for both result and refinement.

//
// Literal statements.
//
// We introduce an abstration rule for each abstract domain.
//
// The rule for the lowest abstract domain has no pre-conditions,
// whereas the rules for the other abstract domains requires refinement.
//
LocalVar(s, x, alpha2s(c)) :- LitStm(s, x, c).

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
// Refinement of division.
//
Refinement(s1, x, r) :- DivStm(s2, r, _, y),
                        LocalVar(s2, y, v2),
                        CFG(s1, s2),
                        mustRefineSign(v2),
                        r := Ref.Ref(Domain.Constant, 5).

//
// Backwards propagate refinement.
//
Refinement(s1, x, step(r)) :- Refinement(s2, x, r),
                              CFG(s1, s2).

//
// Refinement of literal statements s: z = c.
//
// refine the literal to the constant propagation domain.
LocalVar(s, z, lift2cst(c)) :- LitStm(s, z, c),
                               Refinement(s, z, r),
                               refineWithCst(r).

// refine the literal to the interval domain.
LocalVar(s, z, lift2inv(c)) :- LitStm(s, z, c),
                               Refinement(s, z, r),
                               refineWithInv(r).


//
// Refinement through addition statements s2: z = x + y.
//
// refine the input variable 'x'.
Refinement(s1, x, step(r)) :- AddStm(s2, z, x, _),
                              Refinement(s2, z, r),
                              CFG(s1, s2).
// refine the input variable 'y'.
Refinement(s1, y, step(r)) :- AddStm(s2, z, _, y),
                              Refinement(s2, z, r),
                              CFG(s1, s2).

//
// Refinement through division statements s2: z = x / y.
//
// refine the input variable 'x'.
Refinement(s1, x, step(r)) :- DivStm(s2, z, x, _),
                              Refinement(s2, z, r),
                              CFG(s1, s2).
// refine the input variable 'y'.
Refinement(s1, y, step(r)) :- DivStm(s2, z, _, y),
                              Refinement(s2, z, r),
                              CFG(s1, s2).


def plus(x: RefLat, y: RefLat): RefLat = match (x, y) with {
    case (RefLat.Sign(s), RefLat.Sign(s)) => RefLat.Sign(s1 `signPlus` s2)
}

//
// Example program.
//

//
// line1: var x = 42
// line2: var y = 21
// line3:

