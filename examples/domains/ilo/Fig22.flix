namespace Fig22 {

    // NB: Example of something which is NOT a lattice.

    enum Elm {
                     case Top,
                  case H, case I, case J,
        case E, case F, case G,
        case A,          case B, case C, case D,

                  case Bot
    }

    /**
     * Returns `true` iff `e1` is less than or equal to `e2`.
     */
    fn leq(e1: Elm, e2: Elm): Bool = match (e1, e2) with {
        case (Elm.Bot, Elm.A)   => true
        case (Elm.Bot, Elm.B)   => true
        // ...
        case (Elm.J, Elm.Top)   => true
        case _ => false
    }


}