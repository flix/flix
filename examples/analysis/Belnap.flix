/**
 * An implementation of the boolean lattice named for Nuel Belnap.
 */
namespace Belnap {

    /**
     * The boolean lattice.
     */
    enum Belnap {
             case Top,

        case True, case False,

             case Bot
    }

    /**
     * Returns `true` iff `e1` is less than or equal to `e2`.
     */
    def leq(e1: Belnap, e2: Belnap): Bool = match (e1, e2) with {
        case (Belnap.Bot, _) => true;
        case (Belnap.True, Belnap.True) => true;
        case (Belnap.False, Belnap.False) => true;
        case (_, Belnap.Top) => true;
        case _ => false;
    }

    /**
     * Returns the least upper bound of `e1` and `e2`.
     */
    def lub(e1: Belnap, e2: Belnap): Belnap = match (e1, e2) with {
        case (Belnap.Bot, _) => e2;
        case (_, Belnap.Bot) => e1;
        case (Belnap.True, Belnap.True) => Belnap.True;
        case (Belnap.False, Belnap.False) => Belnap.False;
        case _ => Belnap.Top;
    }

    /**
     * Over-approximates the logical `and` operator.
     */
    def and(e1: Belnap, e2: Belnap): Belnap = match (e1, e2) with {
        case (Belnap.Bot, _) => Belnap.Bot;
        case (_, Belnap.Bot) => Belnap.Bot;
        case (Belnap.True, Belnap.True) => Belnap.True;
        case (Belnap.False, Belnap.True) => Belnap.False;
        case (Belnap.True, Belnap.False) => Belnap.False;
        case (Belnap.False, Belnap.False) => Belnap.False;
        case _ => Belnap.Top;
    }

    /**
     * Over-approximates the logical `or` operator.
     */
    def or(e1: Belnap, e2: Belnap): Belnap =
        if (e1 == Belnap.Bot || e2 == Belnap.Bot)
            Belnap.Bot
        else
            if (e1 == Belnap.Top || e2 == Belnap.Top)
                Belnap.Top
            else
                let answer = (e1 == Belnap.True || e2 == Belnap.True) in
                    if (answer)
                        Belnap.True
                    else
                        Belnap.False




}