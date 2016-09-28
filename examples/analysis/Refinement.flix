//
// Implementation of Abstraction Refinement in Flix!
//

//
// We assume a simple program language which has three instructions:
//
rel LitStm(s: Str, z: Str, c: Int)
rel AddStm(s: Str, z: Str, x: Str, y: Str)
rel DivStm(s: Str, z: Str, x: Str, y: Str)

//
// The statements of the program are connected by at control-flow graph.
//
rel CFG(s1: Str, s2: Str)

//
// We now introduce the idea of refinement
//

enum Domain {
    case C,
    case B,
    case A
}

enum Ref {
    case Ref(Domain, Int)
}

def step(e: Ref): Ref = match e with {
    case Ref.Ref(Domain.A, f) => if (f <= 0) Ref.Ref(Domain.A, f) else Ref.Ref(Domain.A, f - 1)
    case Ref.Ref(Domain.B, f) => if (f <= 0) Ref.Ref(Domain.B, f) else Ref.Ref(Domain.A, f - 1)
    case Ref.Ref(Domain.C, f) => if (f <= 0) Ref.Ref(Domain.C, f) else Ref.Ref(Domain.A, f - 1)
}

/**
 * Returns `true` if the given element `e1` is smaller or equal to the given element `e2`.
 */
def leq2(e1: Ref, e2: Ref): Bool = match (e1, e2) with {
    case (Ref.Ref(Domain.A, f1), Ref.Ref(Domain.A, f2)) => f1 <= f2
    case (Ref.Ref(Domain.B, f1), Ref.Ref(Domain.B, f2)) => f1 <= f2
    case (Ref.Ref(Domain.C, f1), Ref.Ref(Domain.C, f2)) => f1 <= f2
    case (Ref.Ref(Domain.A, _), Ref.Ref(Domain.B, _))   => true
    case (Ref.Ref(Domain.A, _), Ref.Ref(Domain.C, _))   => true
    case (Ref.Ref(Domain.B, _), Ref.Ref(Domain.C, _))   => true
    case _                                              => false
}

/**
 * Returns the least upper bound of the two given elements `e1` and `e2`.
 */
def lub2(e1: Ref, e2: Ref): Ref = match (e1, e2) with {
    case (Ref.Ref(Domain.A, f1), Ref.Ref(Domain.A, f2)) => Ref.Ref(Domain.A, max(f1, f2))
    case (Ref.Ref(Domain.B, f1), Ref.Ref(Domain.B, f2)) => Ref.Ref(Domain.A, max(f1, f2))
    case (Ref.Ref(Domain.C, f1), Ref.Ref(Domain.C, f2)) => Ref.Ref(Domain.A, max(f1, f2))

    case (Ref.Ref(Domain.A, _), Ref.Ref(Domain.B, f))   => Ref.Ref(Domain.B, f)
    case (Ref.Ref(Domain.B, f), Ref.Ref(Domain.A, _))   => Ref.Ref(Domain.B, f)

    case (Ref.Ref(Domain.A, _), Ref.Ref(Domain.C, f))   => Ref.Ref(Domain.C, f)
    case (Ref.Ref(Domain.C, f), Ref.Ref(Domain.A, _))   => Ref.Ref(Domain.C, f)

    case (Ref.Ref(Domain.B, _), Ref.Ref(Domain.C, f))   => Ref.Ref(Domain.C, f)
    case (Ref.Ref(Domain.C, f), Ref.Ref(Domain.B, _))   => Ref.Ref(Domain.C, f)
}

def max(x: Int, y: Int): Int = if (x >= y) x else y

/**
 * Returns the greatest lower bound of the two given elements `e1` and `e2`.
 */
def glb2(e1: Ref, e2: Ref): Ref = ??? // TODO

let Ref<> = (Ref.Ref(Domain.A, 0), ???, leq2, lub2, glb2);

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
LocalVar(s, x, alpha2s(c)) :-
    LitStm(s, x, c).

//
// Addition statements.
//
LocalVar(s2, r, sum(v1, v2)) :-
    AddStm(s2, r, x, y),
    LocalVar(s1, x, v1),
    LocalVar(s2, y, v2),
    CFG(s1, s2).

//
// Division statements.
//
LocalVar(s2, r, div(v1, v2)) :-
    DivStm(s2, r, x, y),
    LocalVar(s1, x, v1),
    LocalVar(s2, y, v2),
    CFG(s1, s2).

//
// Refinement of division.
//
Refinement(s1, x, Ref.Ref(Domain.C, 5)) :-
    DivStm(s2, r, _, y),
    LocalVar(s2, y, v2),
    CFG(s1, s2),
    mustRefineSign(v2).

//
// Backwards propagate refinement.
//
Refinement(s1, x, step(r)) :-
    Refinement(s2, x, r),
    CFG(s1, s2).

//
// Refinement of literal statements s: z = c.
//
// refine the literal to the constant propagation domain.
LocalVar(s, z, alpha2p(c)) :-
    LitStm(s, z, c),
    Refinement(s, z, r),
    refineWithParity(r).

// refine the literal to the interval domain.
LocalVar(s, z, alpha2c(c)) :-
    LitStm(s, z, c),
    Refinement(s, z, r),
    refineWithCst(r).


//
// Refinement through addition statements s2: z = x + y.
//
// refine the input variable 'x'.
Refinement(s1, x, step(r)) :-
    AddStm(s2, z, x, _),
    Refinement(s2, z, r),
    CFG(s1, s2).

// refine the input variable 'y'.
Refinement(s1, y, step(r)) :-
    AddStm(s2, z, _, y),
    Refinement(s2, z, r),
    CFG(s1, s2).

//
// Refinement through division statements s2: z = x / y.
//
// refine the input variable 'x'.
Refinement(s1, x, step(r)) :-
    DivStm(s2, z, x, _),
    Refinement(s2, z, r),
    CFG(s1, s2).

// refine the input variable 'y'.
Refinement(s1, y, step(r)) :-
    DivStm(s2, z, _, y),
    Refinement(s2, z, r),
    CFG(s1, s2).



//
// We will use the simple "is zero" lattice as the A lattice.
//
enum A {
    case ATop,
    case AZer,
    case ABot
}

def leqA(e1: A, e2: A): Bool = match (e1, e2) with {
    case (A.ABot, _)        => true
    case (A.AZer, A.AZer)   => true
    case (_, A.ATop)        => true
    case _                  => false
}

def lubA(e1: A, e2: A): A = match (e1, e2) with {
    case (A.ABot, x)        => x
    case (x, A.ABot)        => x
    case (A.AZer, A.AZer)   => A.AZer
    case (A.ATop, _)        => A.ATop
    case (_, A.ATop)        => A.ATop
}




//def alpha2a(i: Int): A = if (i == 0) A.AZer else A.ATop

//
// We now define the three lattices stacked on top of each other.
//
enum Value {
         case Top,

        case Cst(Int),

         case Top2,

    case Odd, case Even,

         case Top1,

         case Two,

         case Bot
}

def leq(e1: Value, e2: Value): Bool = match (e1, e2) with {
    case (Value.Bot, _)                 => true
    case _                              => false
}

def lub(e1: Value, e2: Value): Value = ???

def glb(e1: Value, e2: Value): Value = ???

let Value<> = (Value.Bot, Value.Top, leq, lub, glb);


/**
 * Returns the given integer `i` as an element of the sign lattice.
 */
def alpha2s(i: Int): Value = switch {
    case i <  0 => Value.Neg
    case i == 0 => Value.Zer
    case i >  0 => Value.Pos
}

/**
 * Returns the given integer `i` as an element of the parity lattice.
 */
def alpha2p(i: Int): Value =
  if (i % 2 == 0) Value.Even else Value.Odd

/**
 * Returns the given integer `i` as an element of the constant propagation lattice.
 */
def alpha2c(i: Int): Value = Value.Cst(i)

/**
 * Returns the sum of the two given values `e1` and `e2`.
 */
def sum(e1: Value, e2: Value): Value = match (e1, e2) with {
    case (Value.Bot, _)                         => Value.Bot
    case (_, Value.Bot)                         => Value.Bot
    case (Value.Neg, Value.Neg)                 => Value.NegPos
    case (Value.Neg, Value.Zer)                 => Value.Neg
    case (Value.Neg, Value.Pos)                 => Value.NegPos
    case (Value.Zer, Value.Neg)                 => Value.Neg
    case (Value.Zer, Value.Zer)                 => Value.Zer
    case (Value.Zer, Value.Pos)                 => Value.Pos
    case (Value.Pos, Value.Neg)                 => Value.NegPos
    case (Value.Pos, Value.Zer)                 => Value.Pos
    case (Value.Pos, Value.Pos)                 => Value.Pos
    case (Value.Neg, Value.Odd)                 => Value.OddEven


    case (Value.Cst(n1), Value.Cst(n2))         => Value.Cst(n1 + n2)
    case _                                          => Value.Top
}

/**
 * Returns `true` if ...
 */
def refineWithParity(r: Ref): Bool = ???

def refineWithCst(r: Ref): Bool = ???

def mustRefineSign(e: Value): Bool = ???

//
// Example program.
//

// l1: var x = +2
// l2: var y = -2
// l3: var z = x + y
// l4: var w = x / z
// l5: var q = 42

LitStm("l1", "x", +2).
LitStm("l2", "x", -2).
LitStm("l5", "x", 42).

AddStm("l3", "z", "x", "y").
DivStm("l4", "w", "x", "z").

CFG("l1", "l2").
CFG("l2", "l3").
CFG("l3", "l4").
CFG("l4", "l5").

