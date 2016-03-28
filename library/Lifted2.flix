/**
 * Combinator to lift one lattice over another.
 */
enum Lifted[A, B] {
    case Two(b: B)
    case One(a: A)
}

/**
 * Implementation of `PartialOrder` for `Lifted[A, B]`.
 */
impl PartialOrder[Lifted[A, B]] <= PartialOrder[A], PartialOrder[B] {
    def leq(x: Lifted[A, B], y: Lifted[A, B]): Bool = match (x, y) with {
        case (Lifted.One(x1), Lifted.Two(_)) => true
        case (Lifted.One(x1), Lifted.One(y1)) => x1 ⊑ y1
        case (Lifted.Two(x1), Lifted.Two(y1)) => x1 ⊑ y1
        case _ => false
    }
}

/**
 * Implementation of `JoinLattice` for `Lifted[A, B]`.
 */
impl JoinLattice[Lifted[A, B]] <= JoinLattice[A], JoinLattice[B] {
    def bot: Lifted[A, B] = One(bot)

    def lub(x: Lifted[A, B], y: Lifted[A, B]): Lifted[A, B] = match (x, y) with {
        case (Lifted.One(x1), Lifted.One(y1)) => Lifted.One(x1 ⊔ y1)
        case (Lifted.Two(x1), Lifted.Two(y1)) => Lifted.Two(x1 ⊔ y1)
        case (Lifted.Two(_), _) => x
        case (_, Lifted.Two(_)) => y
    }
}

/**
 * Implementation of `MeetLattice` for `Lifted[A, B]`.
 */
impl MeetLattice[Lifted[A, B]] <= JoinLattice[A], JoinLattice[B] {
    def top: Lifted[A, B] = Two(top)

    def glb(x: Lifted[A, B], y: Lifted[A, B]): Lifted[A, B] = match (x, y) with {
        case (Lifted.One(x1), Lifted.One(y1)) => Lifted.One(x1 ⊓ y1)
        case (Lifted.Two(x1), Lifted.Two(y1)) => Lifted.Two(x1 ⊓ y1)
        case (Lifted.One(_), _) => x
        case (_, Lifted.One(_)) => y
    }
}
