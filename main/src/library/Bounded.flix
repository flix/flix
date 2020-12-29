// TODO: Deprecated, but some old stuff still depends on it.
namespace Bounded {

    ///
    /// The least element law asserts that the bottom element `⊥` is the smallest element of a partial order `⊑`.
    ///
    law leastElement[e](⊥: e, ⊑: (e, e) -> Bool): Bool = ∀(x: e). ⊥ ⊑ x

    ///
    /// The greatest element law asserts that the top element `⊤` is the largest element of a partial order `⊑`.
    ///
    law greatestElement[e](⊤: e, ⊑: (e, e) -> Bool): Bool = ∀(x: e). x ⊑ ⊤

    ///
    /// The strictness law asserts that a function `f` between two orders must preserve bottoms: `bot` and `cobot`.
    ///
    /// NB: This particular law is for unary functions.
    ///
    law strict1[a, b : PreOrder](f: a -> b, bot: a, cobot: b): Bool = PreOrder.equ(f(bot), cobot)

    ///
    /// The strictness law asserts that a function `f` between two orders must preserve bottoms: `bot` and `cobot`.
    ///
    /// NB: This particular law is for binary functions.
    ///
    law strict2[a, b, c : PreOrder](f: (a, b) -> c, bot1: a, bot2: b, cobot: c): Bool = PreOrder.equ(f(bot1, bot2), cobot)

}

