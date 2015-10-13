/**
 * An implementation of the boolean lattice named for Nuel Belnap.
 */
namespace Belnap {

    enum Belnap {
             case Top,

        case True, case False,

             case Bot
    }

    def leq(e1: Belnap, e2: Belnap): Bool = match (e1, e2) with {
        case ()
    }

}