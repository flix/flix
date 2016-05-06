namespace Cube {

    // See Figure 2.8 on page 45. The diagram below may be misleading.

    enum Cube {
                 case Top,

        case Y1, case Y2, case Y3,

        case X1, case X2, case X3,
        
                 case Bot
    }
    
    /**
     * Returns `true` iff `e1` is less than or equal to `e2`.
     */
    def leq(e1: Cube, e2: Cube): Bool = match (e1, e2) with {
        case (Cube.Bot, _)      => true

        case (Cube.X1, Cube.X1) => true
        case (Cube.X1, Cube.Y1) => true
        case (Cube.X1, Cube.Y2) => true

        case (Cube.X2, Cube.X2) => true
        case (Cube.X2, Cube.Y1) => true
        case (Cube.X2, Cube.Y3) => true

        case (Cube.X3, Cube.X3) => true
        case (Cube.X3, Cube.Y2) => true
        case (Cube.X3, Cube.Y3) => true

        case (Cube.Y1, Cube.Y1) => true
        case (Cube.Y2, Cube.Y2) => true
        case (Cube.Y3, Cube.Y3) => true

        case (_, Cube.Top)      => true

        case _                  => false
    }

    /**
     * Returns the least upper bound of `e1` and `e2`.
     */
    @commutative @associative
    def lub(e1: Cube, e2: Cube): Cube = match (e1, e2) with {
        case (Cube.Bot, x)      => x
        case (x, Cube.Bot)      => x

        case (Cube.X1, Cube.X1) => Cube.X1
        case (Cube.X1, Cube.X2) => Cube.Y1
        case (Cube.X1, Cube.X3) => Cube.Y2
        case (Cube.X1, Cube.Y1) => Cube.Y1
        case (Cube.X1, Cube.Y2) => Cube.Y2

        case (Cube.X2, Cube.X2) => Cube.X2
        case (Cube.X2, Cube.X1) => Cube.Y1
        case (Cube.X2, Cube.X3) => Cube.Y3
        case (Cube.X2, Cube.Y1) => Cube.Y1
        case (Cube.X2, Cube.Y3) => Cube.Y3

        case (Cube.X3, Cube.X3) => Cube.X3
        case (Cube.X3, Cube.X1) => Cube.Y2
        case (Cube.X3, Cube.X2) => Cube.Y3
        case (Cube.X3, Cube.Y2) => Cube.Y2
        case (Cube.X3, Cube.Y3) => Cube.Y3

        case (Cube.Y1, Cube.Y1) => Cube.Y1
        case (Cube.Y1, Cube.X1) => Cube.Y1
        case (Cube.Y1, Cube.X2) => Cube.Y1

        case (Cube.Y2, Cube.Y2) => Cube.Y2
        case (Cube.Y2, Cube.X1) => Cube.Y2
        case (Cube.Y2, Cube.X3) => Cube.Y2

        case (Cube.Y3, Cube.Y3) => Cube.Y3
        case (Cube.Y3, Cube.X2) => Cube.Y3
        case (Cube.Y3, Cube.X3) => Cube.Y3

        case _                  => Cube.Top
    }

    /**
     * Returns the greatest lower bound of `e1` and `e2`.
     */
    @commutative @associative
    def glb(e1: Cube, e2: Cube): Cube = match (e1, e2) with {
        case (Cube.Top, x)      => x
        case (x, Cube.Top)      => x

        case (Cube.X1, Cube.X1) => Cube.X1
        case (Cube.X1, Cube.Y1) => Cube.X1
        case (Cube.X1, Cube.Y2) => Cube.X1

        case (Cube.X2, Cube.X2) => Cube.X2
        case (Cube.X2, Cube.Y1) => Cube.X2
        case (Cube.X2, Cube.Y3) => Cube.X2

        case (Cube.X3, Cube.X3) => Cube.X3
        case (Cube.X3, Cube.Y2) => Cube.X3
        case (Cube.X3, Cube.Y3) => Cube.X3

        case (Cube.Y1, Cube.Y1) => Cube.Y1
        case (Cube.Y1, Cube.X1) => Cube.X1
        case (Cube.Y1, Cube.X2) => Cube.X2
        case (Cube.Y1, Cube.Y2) => Cube.X1
        case (Cube.Y1, Cube.Y3) => Cube.X2

        case (Cube.Y2, Cube.Y2) => Cube.Y2
        case (Cube.Y2, Cube.X1) => Cube.X1
        case (Cube.Y2, Cube.X3) => Cube.X3
        case (Cube.Y2, Cube.Y1) => Cube.X1
        case (Cube.Y2, Cube.Y3) => Cube.X3

        case (Cube.Y3, Cube.Y3) => Cube.Y3
        case (Cube.Y3, Cube.X2) => Cube.X2
        case (Cube.Y3, Cube.X3) => Cube.X3
        case (Cube.Y3, Cube.Y1) => Cube.X2
        case (Cube.Y3, Cube.Y2) => Cube.X3

        case _                  => Cube.Bot
    }
    
    let Cube<> = (Cube.Bot, Cube.Top, leq, lub, glb)

}
