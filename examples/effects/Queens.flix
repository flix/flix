///
/// From Effekt Lang
///
mod EffectExamples.Queens {

    ///
    /// Type alias for readability.
    ///
    type alias Solution = List[Int32]

    ///
    /// Algebraic effect that allows the caller
    /// to direct the search for a solution.
    ///
    eff Searchable {
        pub def pick(size: Int32): Int32
        pub def fail(): Void
    }

    ///
    /// Returns `true` iff placing a queen at the position
    /// represented by `queen` does not collide with other queens.
    ///
    def safe(queen: Int32, diagonal: Int32, sol: Solution): Bool = match sol {
        case Nil     => true
        case q :: qs =>
            let sfe = queen != q and queen != q + diagonal and queen != q - diagonal;
            sfe and safe(queen, diagonal + 1, qs)
    }

    pub def findOneSolution(size: Int32, queens: Int32): Solution \ Searchable =
        if (queens == 0)
            Nil
        else
            let sol = findOneSolution(size, queens - 1);
            let next = do Searchable.pick(size);
            if (safe(next, 1, sol))
                next :: sol
            else
                do Searchable.fail()

    pub def countSolutions(size: Int32): Int32 =
        try {
            findOneSolution(size, size);
            1
        } with Searchable {
            def fail(_) = 0
            def pick(sz, k) = {
                def loop(i, acc) = {
                    if (i == sz)
                        k(i) + acc
                    else
                        loop(i + 1, k(i) + acc)
                };
                loop(1, 0)
            }
        }
}
