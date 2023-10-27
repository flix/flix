// None of these should cause redundancy errors,
// as top-level declarations are implicitly public.

def f(): Bool = ???

enum E0

enum E2 {
    case C1
    case C2
}

enum Ot(Bool)

type alias Ta = Bool

trait T0[a]
