namespace SubZeroL {

    // See the figure on the front page.

    enum L {
                       case Top,

                    case Z1, case Z2,

        case Y1, case Y2, case Y3, case Y4, case Y5,

           case X1, case X2, case X3, case X4,

                       case Bot
    }

    /**
     * Returns `true` iff `e1` is less than or equal to `e2`.
     */
    def leq(e1: L, e2: L): Bool = match (e1, e2) with {
        case (L.Bot, _)     => true

        case (L.X1, L.X1)   => true
        case (L.X1, L.Y1)   => true
        case (L.X1, L.Y2)   => true
        case (L.X1, L.Z1)   => true

        case (L.X2, L.X2)   => true
        case (L.X2, L.Y1)   => true
        case (L.X2, L.Y3)   => true
        case (L.X2, L.Y4)   => true
        case (L.X2, L.Z1)   => true
        case (L.X2, L.Z2)   => true

        case (L.X3, L.X3)   => true
        case (L.X3, L.Y2)   => true
        case (L.X3, L.Y3)   => true
        case (L.X3, L.Y5)   => true
        case (L.X3, L.Z1)   => true
        case (L.X3, L.Z2)   => true

        case (L.X4, L.X4)   => true
        case (L.X4, L.Y4)   => true
        case (L.X4, L.Y5)   => true
        case (L.X4, L.Z2)   => true

        case (L.Y1, L.Y1)   => true
        case (L.Y1, L.Z1)   => true
        case (L.Y2, L.Y2)   => true
        case (L.Y2, L.Z1)   => true
        case (L.Y3, L.Y3)   => true
        case (L.Y3, L.Z1)   => true
        case (L.Y3, L.Z2)   => true
        case (L.Y4, L.Y4)   => true
        case (L.Y4, L.Z2)   => true
        case (L.Y5, L.Y5)   => true
        case (L.Y5, L.Z2)   => true

        case (L.Z1, L.Z1)   => true
        case (L.Z2, L.Z2)   => true

        case (_, L.Top)     => true
        case _              => false
    }

    /**
     * Returns the least upper bound of `e1` and `e2`.
     */
    @commutative @associative
    def lub(e1: L, e2: L): L = match (e1, e2) with {
        case (L.Bot, x)     => x
        case (x, L.Bot)     => x

        case (L.X1, L.X1)   => L.X1
        case (L.X1, L.X2)   => L.Y1
        case (L.X1, L.X3)   => L.Y2
        case (L.X1, L.Y1)   => L.Y1
        case (L.X1, L.Y2)   => L.Y2
        case (L.X1, L.Y3)   => L.Z1
        case (L.X1, L.Z1)   => L.Z1

        case (L.X2, L.X2)   => L.X2
        case (L.X2, L.X1)   => L.Y1
        case (L.X2, L.X3)   => L.Y3
        case (L.X2, L.X4)   => L.Y4
        case (L.X2, L.Y1)   => L.Y1
        case (L.X2, L.Y2)   => L.Z1
        case (L.X2, L.Y3)   => L.Y3
        case (L.X2, L.Y4)   => L.Y4
        case (L.X2, L.Y5)   => L.Z2
        case (L.X2, L.Z1)   => L.Z1
        case (L.X2, L.Z2)   => L.Z2

        case (L.X3, L.X3)   => L.X3
        case (L.X3, L.X1)   => L.Y2
        case (L.X3, L.X2)   => L.Y3
        case (L.X3, L.X4)   => L.Y5
        case (L.X3, L.Y1)   => L.Z1
        case (L.X3, L.Y2)   => L.Y2
        case (L.X3, L.Y3)   => L.Y3
        case (L.X3, L.Y4)   => L.Z2
        case (L.X3, L.Y5)   => L.Y5
        case (L.X3, L.Z1)   => L.Z1
        case (L.X3, L.Z2)   => L.Z2

        case (L.X4, L.X4)   => L.X4
        case (L.X4, L.X2)   => L.Y4
        case (L.X4, L.X3)   => L.Y5
        case (L.X4, L.Y3)   => L.Z2
        case (L.X4, L.Y4)   => L.Y4
        case (L.X4, L.Y5)   => L.Y5
        case (L.X4, L.Z2)   => L.Z2

        case (L.Y1, L.Y1)   => L.Y1
        case (L.Y1, L.X1)   => L.Y1
        case (L.Y1, L.X2)   => L.Y1
        case (L.Y1, L.X3)   => L.Z1
        case (L.Y1, L.Y2)   => L.Z1
        case (L.Y1, L.Y3)   => L.Z1

        case (L.Y2, L.Y2)   => L.Y2
        case (L.Y2, L.X1)   => L.Y2
        case (L.Y2, L.X2)   => L.Z1
        case (L.Y2, L.X3)   => L.Y2
        case (L.Y2, L.Y1)   => L.Z1
        case (L.Y2, L.Y3)   => L.Z1



        case (L.Z1, L.Z1)   => L.Z1
        case (L.Z2, L.Z2)   => L.Z2

        case _              => L.Top
    }

    /**
     * Returns the greatest lower bound of `e1` and `e2`.
     */
    @commutative @associative
    def glb(e1: L, e2: L): L = match (e1, e2) with {
        case (L.Top, x)   => x
        case (x, L.Top)   => x

        case _            => L.Bot
    }

    let L<> = (L.Bot, L.Top, leq, lub, glb)

}