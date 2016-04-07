namespace M2 {

    enum M2 {
           case Top,
             
        case C1, case C2,
        
           case Bot
    }
    
    /**
     * Returns `true` iff `e1` is less than or equal to `e2`.
     */
    def leq(e1: M2, e2: M2): Bool = match (e1, e2) with {
        case (M2.Bot, _)    => true
        case (M2.C1, M2.C1) => true
        case (M2.C2, M2.C2) => true
        case (_, M2.Top)    => true
        case _              => false
    }

    /**
     * Returns the least upper bound of `e1` and `e2`.
     */
    @commutative @associative
    def lub(e1: M2, e2: M2): M2 = match (e1, e2) with {
        case (M2.Bot, x)     => x
        case (x, M2.Bot)     => x
        case (M2.C1, M2.C1)  => M2.C1
        case (M2.C2, M2.C2)  => M2.C2
        case _               => M2.Top
    }

    /**
     * Returns the greatest lower bound of `e1` and `e2`.
     */
    @commutative @associative
    def glb(e1: M2, e2: M2): M2 = match (e1, e2) with {
        case (M2.Top, x)    => x
        case (x, M2.Top)    => x
        case (M2.C1, M2.C1) => M2.C1
        case (M2.C2, M2.C2) => M2.C2
        case _              => M2.Bot
    }
    
    let M2<> = (M2.Bot, M2.Top, leq, lub, glb)

}