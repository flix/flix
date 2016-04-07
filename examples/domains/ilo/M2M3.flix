namespace M2M3 {

    enum L {
                 case Top,

        case Y1, case Y2, case Y3,

                 case B1,

                 case T1,

            case X1, case X2,

                 case Bot
    }



    /**
     * Returns `true` iff `e1` is less than or equal to `e2`.
     */
    def leq(e1: L, e2: L): Bool = match (e1, e2) with {
        case (L.Bot, _)   => true

        case (L.X1, L.X1)   => true
        case (L.X1, L.T1)   => true
        case (L.X1, L.B1)   => true
        case (L.X1, L.Y1)   => true
        case (L.X1, L.Y2)   => true
        case (L.X1, L.Y3)   => true

        case (L.X2, L.X2)   => true
        case (L.X2, L.T1)   => true
        case (L.X2, L.B1)   => true
        case (L.X2, L.Y1)   => true
        case (L.X2, L.Y2)   => true
        case (L.X2, L.Y3)   => true

        case (L.T1, L.T1)   => true
        case (L.T1, L.B1)   => true
        case (L.T1, L.Y1)   => true
        case (L.T1, L.Y2)   => true
        case (L.T1, L.Y3)   => true

        case (L.B1, L.B1)   => true
        case (L.B1, L.Y1)   => true
        case (L.B1, L.Y2)   => true
        case (L.B1, L.Y3)   => true

        case (L.Y1, L.Y1)   => true
        case (L.Y2, L.Y2)   => true
        case (L.Y3, L.Y3)   => true

        case (L.Top, L.Top) => true

        case _            => false
    }

    /**
     * Returns the least upper bound of `e1` and `e2`.
     */
    @commutative @associative
    def lub(e1: L, e2: L): L = match (e1, e2) with {
        case (L.Bot, x) => x
        case (x, L.Bot) => x

        case (L.X1, L.X1) => L.X1
        case (L.X1, L.X2) => L.T1
        case (L.X1, L.T1) => L.T1
        case (L.X1, L.B1) => L.B1
        case (L.X1, L.Y1) => L.Y1
        case (L.X1, L.Y2) => L.Y2
        case (L.X1, L.Y3) => L.Y3

        case (L.X2, L.X2) => L.X2
        case (L.X2, L.X1) => L.T1
        case (L.X2, L.T1) => L.T1
        case (L.X2, L.B1) => L.B1
        case (L.X2, L.Y1) => L.Y1
        case (L.X2, L.Y2) => L.Y2
        case (L.X2, L.Y3) => L.Y3

        case (L.T1, L.T1) => L.T1
        case (L.T1, L.X1) => L.T1
        case (L.T1, L.X2) => L.T1
        case (L.T1, L.B1) => L.B1
        case (L.T1, L.Y1) => L.Y1
        case (L.T1, L.Y2) => L.Y2
        case (L.T1, L.Y3) => L.Y3

        case (L.B1, L.B1) => L.B1
        case (L.B1, L.X1) => L.B1
        case (L.B1, L.X2) => L.B1
        case (L.B1, L.T1) => L.B1
        case (L.B1, L.Y1) => L.Y1
        case (L.B1, L.Y2) => L.Y2
        case (L.B1, L.Y3) => L.Y3

        case (L.Y1, L.Y1) => L.Y1
        case (L.Y1, L.X1) => L.Y1
        case (L.Y1, L.X2) => L.Y1
        case (L.Y1, L.T1) => L.Y1
        case (L.Y1, L.B1) => L.Y1
        case (L.Y1, L.Y2) => L.Top
        case (L.Y1, L.Y3) => L.Top

        case (L.Y2, L.Y2) => L.Y2
        case (L.Y2, L.X1) => L.Y2
        case (L.Y2, L.X2) => L.Y2
        case (L.Y2, L.T1) => L.Y2
        case (L.Y2, L.B1) => L.Y2
        case (L.Y2, L.Y1) => L.Top
        case (L.Y2, L.Y3) => L.Top

        case (L.Y3, L.Y3) => L.Y3
        case (L.Y3, L.X1) => L.Y3
        case (L.Y3, L.X2) => L.Y3
        case (L.Y3, L.T1) => L.Y3
        case (L.Y3, L.B1) => L.Y3
        case (L.Y3, L.Y1) => L.Top
        case (L.Y3, L.Y2) => L.Top

        case _            => L.Top
    }

    /**
     * Returns the greatest lower bound of `e1` and `e2`.
     */
    @commutative @associative
    def glb(e1: L, e2: L): L = match (e1, e2) with {

        case _              => L.Bot
    }

    let L<> = (L.Bot, L.Top, leq, lub, glb)

}