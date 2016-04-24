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
         ∀(x: Int). f(x) → g(x) // unless we allow this.

// TODO: Another issue is that if we do this, then we have to express addition on sets.
// E.g.
def plus(x: InfSet, y: InfSet): InfSet = ???

// That requires some heavy lifting...

/**
 * Galois connection between InfSet[Int] and Sign.
 */
def alpha(e: InfSet[Int]): Sign = ??? // TODO: This now requires a fold.

def gamma(e: Parity): InfSet[Int] = match e with {
    case Parity.Bot     => InfSet.InfSet(x -> false)
    case Parity.Odd     => InfSet.InfSet(x -> x % 2 != 0)
    case Parity.Even    => InfSet.InfSet(x -> x % 2 == 0)
    case Parity.Top     => InfSet.InfSet(x -> true)
}

// Can we perhaps change the definition so we don't need a concrete lattice?
// or some how avoid the concretization function for the proofs?

// We need to show that
// If α(x) ⊑ x' and  α(y) ⊑ y' then α(x + y) ⊑ x' aplus y'
// i.e. if x' and y' are safe approximations of x and y, then x' aplus y' is safe.
// Something like:
law Safe2[A: Lattice, B: Lattice](α: A -> B): Prop =
    ∀(x: A, y: B). ((α(x1) ⊑ x2) ∧ (α(y1) ⊑ y2)) → α(x1 + y1) ⊑ (x2 `aplus` y2)

// Trouble here is that x1 and y1 are sets...

// To sum up so far, the definition of galois connection seems to work nicely for e.g. sign and constant.
// The trouble occurs when we have to consider the concrete semantics, which unfortunately is the most interesting
// case.

