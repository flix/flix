namespace M3 {

    enum M3 {
                 case Top,
             
        case C1, case C2, case C3,
        
                 case Bot
    }
    
    /**
     * Returns `true` iff `e1` is less than or equal to `e2`.
     */
    def leq(e1: M3, e2: M3): Bool = match (e1, e2) with {
        case (M3.Bot, _)    => true
        case (M3.C1, M3.C1) => true
        case (M3.C2, M3.C2) => true
        case (M3.C3, M3.C3) => true
        case (_, M3.Top)    => true
        case _              => false
    }

    /**
     * Returns the least upper bound of `e1` and `e2`.
     */
    @commutative @associative
    def lub(e1: M3, e2: M3): M3 = match (e1, e2) with {
        case (M3.Bot, x)    => x
        case (x, M3.Bot)    => x
        case (M3.C1, M3.C1) => M3.C1
        case (M3.C2, M3.C2) => M3.C2
        case (M3.C3, M3.C3) => M3.C3
        case _              => M3.Top
    }

    /**
     * Returns the greatest lower bound of `e1` and `e2`.
     */
    @commutative @associative
    def glb(e1: M3, e2: M3): M3 = match (e1, e2) with {
        case (M3.Top, x)    => x
        case (x, M3.Top)    => x
        case (M3.C1, M3.C1) => M3.C1
        case (M3.C2, M3.C2) => M3.C2
        case (M3.C3, M3.C3) => M3.C3
        case _              => M3.Bot
    }
    
    let M3<> = (M3.Bot, M3.Top, leq, lub, glb)

}