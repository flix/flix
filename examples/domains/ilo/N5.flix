namespace N5 {

    enum N5 {
               case Top,
             
        case U, 
        
                        case V,
        
        case W,
        
               case Bot
    }
    
    /**
     * Returns `true` iff `e1` is less than or equal to `e2`.
     */
    def leq(e1: N5, e2: N5): Bool = match (e1, e2) with {
        case (N5.Bot, _)    => true
        case (N5.W, N5.W)   => true
        case (N5.V, N5.V)   => true
        case (N5.U, N5.U)   => true
        case (N5.W, N5.U)   => true
        case (_, N5.Top)    => true
        case _              => false
    }

    /**
     * Returns the least upper bound of `e1` and `e2`.
     */
    @commutative @associative
    def lub(e1: N5, e2: N5): N5 = match (e1, e2) with {
        case (N5.Bot, x)    => x
        case (x, N5.Bot)    => x
        case (N5.W, N5.W)   => N5.W
        case (N5.U, N5.U)   => N5.U
        case (N5.V, N5.V)   => N5.V
        case (N5.W, N5.U)   => N5.U
        case (N5.U, N5.W)   => N5.U
        case _              => N5.Top
    }

    /**
     * Returns the greatest lower bound of `e1` and `e2`.
     */
    @commutative @associative
    def glb(e1: N5, e2: N5): N5 = match (e1, e2) with {
        case (N5.Top, x)    => x
        case (x, N5.Top)    => x
        case (N5.W, N5.W)   => N5.W
        case (N5.U, N5.U)   => N5.U
        case (N5.V, N5.V)   => N5.V
        case (N5.W, N5.U)   => N5.W
        case (N5.U, N5.W)   => N5.W
        case _              => N5.Bot
    }
    
    let N5<> = (N5.Bot, N5.Top, leq, lub, glb)

}
