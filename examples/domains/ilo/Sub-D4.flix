namespace SubD4 {

    // See Figure 2.5 on page 38. The diagram below may be misleading.

    enum L {
                          case Top,

                 case Y1, case Y2, case Y3,

        case X1, case X2, case X3, case X4, case X5,

                          case Bot
    }

    /**
     * Returns `true` iff `e1` is less than or equal to `e2`.
     */
    def leq(e1: L, e2: L): Bool = match (e1, e2) with {
        case (L.Bot, _) => true

        case (L.X1, L.X1) => true
        case (L.X1, L.Y1) => true

        case (L.X2, L.X2) => true
        case (L.X2, L.Y1) => true

        case (L.X3, L.X3) => true
        case (L.X3, L.Y1) => true
        case (L.X3, L.Y2) => true
        case (L.X3, L.Y3) => true

        case (L.X4, L.X4) => true
        case (L.X4, L.Y3) => true

        case (L.X5, L.X5) => true
        case (L.X5, L.Y3) => true

        case (L.Y1, L.Y1) => true
        case (L.Y2, L.Y2) => true
        case (L.Y3, L.Y3) => true

        case (_, L.Top)   => true

        case _            => false
    }

    /**
     * Returns the least upper bound of `e1` and `e2`.
     */
    @commutative @associative
    def lub(e1: L, e2: L): L = match (e1, e2) with {
    //    case (M2.Bot, x)     => x
    //    case (x, M2.Bot)     => x
        case _               => L.Top
    }

    /**
     * Returns the greatest lower bound of `e1` and `e2`.
     */
    @commutative @associative
    def glb(e1: L, e2: L): L = match (e1, e2) with {
        //case (M2.Top, x)    => x
    //    case (x, M2.Top)    => x
        case _              => L.Bot
    }

    let L<> = (L.Bot, L.Top, leq, lub, glb)

}