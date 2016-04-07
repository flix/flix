namespace M2 {

    enum L {
             case Top,
             
        case C1, case C2,
        
             case Bot
    }
    
    /**
     * Returns `true` iff `e1` is less than or equal to `e2`.
     */
    def leq(e1: L, e2: L): Bool = match (e1, e2) with {
        case (L.Bot, _)   => true
        case (L.C1, L.C1) => true
        case (L.C2, L.C2) => true
        case (_, L.Top)   => true
        case _            => false
    }

    /**
     * Returns the least upper bound of `e1` and `e2`.
     */
    @commutative @associative
    def lub(e1: L, e2: L): L = match (e1, e2) with {
        case (L.Bot, x)   => x
        case (x, L.Bot)   => x
        case (L.C1, L.C1) => L.C1
        case (L.C2, L.C2) => L.C2
        case _            => L.Top
    }

    /**
     * Returns the greatest lower bound of `e1` and `e2`.
     */
    @commutative @associative
    def glb(e1: L, e2: L): L = match (e1, e2) with {
        case (L.Top, x)   => x
        case (x, L.Top)   => x
        case (L.C1, L.C1) => L.C1
        case (L.C2, L.C2) => L.C2
        case _            => L.Bot
    }
    
    let L<> = (L.Bot, L.Top, leq, lub, glb)

}