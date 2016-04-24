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
// The challenge is that we can't define an infinite set.
// This means we have to define the concrete lattice
// as some kind of symbolic set/formulae/characteristic function.

// Let us try with functions.
// Let f: L = Int -> Bool (i.e. if f(42) is true then 42 is in the set).

impl PartialOrder[Int -> Bool] {
    def leq(x: Int -> Bool, y: Int -> Bool): Bool // ups, this does not work...
}

// Let us try with a notion of symbolic set.
impl PartialOrder[SymSet[Int]] {
    def leq(x: SymSet[Int], y: SymSet[Int]): Bool = x `subsetOf` y
}

impl JoinLattice[SymSet[Int]] {
    def lub(x: SymSet[Int], y: SymSet[Int]): SymSet[Int] = x `union` y
}

// An alternative is to change the definition of gamma:
// Unfortunately, we have to do something quiet heavy weight
// and fuse leq with gamma, to get:
// γ': (B, A) -> Bool iff x ⊑ γ(y)
// This is not very elegant, but it should work.
law GaloisConnection[A: Lattice, B: Lattice](α: A -> B, γ: (B, A) -> Bool): Prop =
 ∀(x: A, y: B). (α(x) ⊑ y) ↔ γ(y, x)

// Let us try with this data type:
enum InfSet {
    case InfSet(Int -> Bool)

    // TODO: We could even consider adding a finite set here.
    // For materialized sets.
}

def union(xs: InfSet, ys: InfSet): InfSet =
    let InfSet.InfSet(f) = xs in
    let InfSet.InfSet(g) = ys in
        InfSet.InfSet(x -> f(x) || g(x))

def intersection(xs: InfSet, ys: InfSet): InfSet =
    let InfSet.InfSet(f) = xs in
    let InfSet.InfSet(g) = ys in
        InfSet.InfSet(x -> f(x) && g(x))

def difference(xs: InfSet, ys: InfSet): InfSet =
    let InfSet.InfSet(f) = xs in
    let InfSet.InfSet(g) = ys in
        InfSet.InfSet(x -> f(x) && !g(x))

// Uh oh... Stuck again ....
def subset(xs: InfSet, ys: InfSet): Bool =
    let InfSet.InfSet(f) = xs in
    let InfSet.InfSet(g) = ys in
         ∀(x: Int). f(x) → g(x)

// TODO: Another issue is that if we do this, then we have to express addition on sets.
// E.g.
def plus(x: InfSet, y: InfSet): InfSet = ???

// That requires some heavy lifting...

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
