// TODO:
// Can we use this function to express a lattice?
// What about a galois connection?
// Is there any way to express a gamma that returns a set? or symbolic set?
// TODO: It could also just return a higher-order function?

/**
 *
 */
law GaloisConnection[A: Lattice, B: Lattice](α: A -> B, γ: B -> A): Prop =
    ∀(x: A, y: B). (α(x) ⊑ y) ↔ (x ⊑ γ(y))


/**
 * Galois connection between Constant and Sign.
 */
def alpha(e: Constant): Sign = match e with {
    case Constant.Bot       => Sign.Bot
    case Constant.Cst(n)    => switch {
        case (n == 0)   => Sign.Zer
        case (n <= 0)   => Sign.Neg
        case (n >= 0)   => Sign.Pos
    }
    case Constant.Bot       => Sign.Top
}

def gamma(e: Sign): Constant = match e with {
    case Sign.Bot   => Constant.Bot
    case Sign.Zer   => Constant.Cst(0)
    case Sign.Neg   => Constant.Top
    case Sign.Pos   => Constant.Top
    case Sign.Top   => Constant.Top
}

law ConstSignGalois[Constant: Lattice, Sign: Lattice]: Prop =
    GaloisConnection(alpha, gamma)

/**
 * Galois connection between Int and Sign.
 */



def alpha(e: Int): Sign = switch {
    case (e == 0)   => Sign.Zer
    case (e <= 0)   => Sign.Neg
    case (e >= 0)   => Sign.Pos
}

def gamma(e: Parity): Int -> Bool = match e with {
    case Parity.Bot => false
    case Parity.Odd => i % 2 != 0
    case Parity.Even => i % 2 == 0
    case Parity.Top => true
}
