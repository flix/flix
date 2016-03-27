// Lift over extra bottom element.
// Opt[A] allows lifting over one bottom element, but not multiple new, so that is why we have LiftOne

enum LiftOne[E] {
    case LiftOne(E)
}

impl PartialOrder[LiftOne[E: PartialOrder]] {
    def leq(x: Lifted, y: Lifted): Bool =
        // TODO...
}



// Lift one lattice over another.
// b lifted over a:
enum Lifted[A, B] {
// TODO: Or introduce either.
// sucks because now we cant infer the type ...
    case LiftA(a: A)
    case LiftB(b: B)
}


impl PartialOrder[Lifted[A: PartialOrder, B: PartialOrder]] {
    def leq(x: Lifted, y: Lifted): Bool =
        let Lifted.Lifted(xa, xb) = x in
        let Lifted.Lifted(ya, yb) = y in
            xa ⊑
}