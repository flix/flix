/**
 * Combinator to lift a lattice over a new bottom element.
 */
enum Lifted[E] {
    case One(E),
    case Bot
}

/**
 * Implementation of `PartialOrder` for `Lifted[E]`.
 */
impl PartialOrder[Lifted[E]] <= PartialOrder[E] {
    def leq(x: Lifted[E], y: Lifted)[E]: Bool = match (x, y) with {
        case (Lifted.Bot, _) => true
        case (Lifted.One(x1), Lifted.One(y1)) => x1 ⊑ y1
        case _ => false
    }
}

/**
 * Implementation of `JoinLattice` for `Lifted[E]`.
 */
impl JoinLattice[Lifted[E]] <= JoinLattice[E] {
    def bot: Lifted[E] = Lifted.Bot

    def lub(x: Lifted[E], y: Lifted[E]): Lifted[E] = match (x, y) with {
        case (Lifted.Bot, _) => y
        case (_, Lifted.Bot) => x
        case (Lifted.One(x1), Lifted.One(y1)) => Lifted.One(x1 ⊔ y1)
    }
}

/**
 * Implementation of `MeetLattice` for `Lifted[E]`.
 */
impl MeetLattice[Lifted[E]] <= MeetLattice[E] {
    def top: Lifted[E] = Lifted.One(top)

    def lub(x: Lifted[E], y: Lifted[E]): Lifted[E] = match (x, y) with {
        case (Lifted.Bot, _) => Lifted.Bot
        case (_, Lifted.Bot) => Lifted.Bot
        case (Lifed.One(x1), Lifted.One(y1)) => Lifted.One(x1 ⊓ y1)
    }
}

/**
 * Lifts a function from `a -> b` into a function from `Lifted[A] -> Lifted[B]`.
 */
def lifted[A, B](f: A -> B): Lifted[A] -> Lifted[B] =
    x -> match x with {
        case Lifted.Bot => Lifted.Bot
        case Lifted.One(v) => Lifted.One(f(v))
    }
