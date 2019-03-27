///
/// This example shows how to define a lattice and use it
/// in the constraint language.
///

/// We begin by defining the elements of the Sign lattice.
enum Sign {
              case Top,

    case Neg, case Zer, case Pos,

              case Bot
}

/// Next, we define the the lattice operations as functions:

/// The equality relation on Sign lattice elements.
def equ(e1: Sign, e2: Sign): Bool = e1 == e2

/// The partial order relation on Sign lattice elements.
def leq(e1: Sign, e2: Sign): Bool = match (e1, e2) with {
    case (Bot, _)   => true
    case (Neg, Neg) => true
    case (Zer, Zer) => true
    case (Pos, Pos) => true
    case (_, Top)   => true
    case _          => false
}

/// The least upper bound of two sign lattice elements.
def lub(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
    case (Bot, x)   => x
    case (x, Bot)   => x
    case (Neg, Neg) => Neg
    case (Zer, Zer) => Zer
    case (Pos, Pos) => Pos
    case _          => Top
}

def glb(e1: Sign, e2: Sign): Sign = match (e1, e2) with {
    case (Top, x)   => x
    case (x, Top)   => x
    case (Neg, Neg) => Neg
    case (Zer, Zer) => Zer
    case (Pos, Pos) => Pos
    case _          => Bot
}

/// We define the Sign lattice attaching its components.
let Sign<> = (Bot, Top, equ, leq, lub, glb)

lat A(x: Str, s: Sign)
lat B(x: Str, s: Sign)
lat R(x: Str, s: Sign)

A("a", Pos).
A("b", Neg).
B("a", Top).

R("c", s) :- A("a", s).
R("c", s) :- A("b", s).
R("d", s) :- A(x, s), B(x, s).
