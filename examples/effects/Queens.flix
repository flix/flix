///
/// From Effekt Lang
///
mod EffectExamples.Queens {

    ///
    /// Type alias for readability.
    ///
    type alias Placement = List[Int32]

    ///
    /// Algebraic effect that allows the caller
    /// to direct the search for a solution.
    ///
    eff Searchable {
        pub def pick(size: Int32): Int32
        pub def fail(): Void
    }

    ///
    /// Returns `true` if placing a queen at the position
    /// represented by `queen` does not collide with other queens.
    ///
    def isSafePosition(queen: Int32, diagonal: Int32, sol: Solution): Bool = match sol {
        case Nil     => true
        case q :: qs =>
            let isSafe = queen != q and queen != q + diagonal and queen != q - diagonal;
            isSafe and isSafePosition(queen, diagonal + 1, qs)
    }

    ///
    /// Attempts to find a solution for the queens problem for a
    /// board of size `size * size` for `queens` number of queens.
    ///
    /// Raises the `Searchable` effect to obtain the next candidate
    /// solution and attempts to find a solution with that index.
    /// If no such solution exists, it raises the `Searchable` effect
    /// again to let the caller handle failures.
    ///
    pub def findOneSolution(size: Int32, queens: Int32): Placement \ Searchable =
        if (queens <= 0)
            Nil
        else
            let sol = findOneSolution(size, queens - 1);
            let next = do Searchable.pick(size);
            if (isSafePosition(next, 1, sol))
                next :: sol
            else
                do Searchable.fail()

    ///
    /// Returns the number of possible solutions for a board
    /// of size `size * size` with `size` number of queens.
    ///
    pub def countSolutions(size: Int32): Int32 =
        try {
            findOneSolution(size, size);
            1
        } with Searchable {
            ///
            /// Returns 0 without calling the continuation,
            /// so control is never transferred back to
            /// `findOneSolution` or the try-block above.
            ///
            def fail(_) = 0

            ///
            /// Places a queen at every index up to `sz`
            /// and accumulates the number of solutions.
            ///
            /// Note here that the continuation `k` is invoked
            /// with the index up to `sz` and that it returns
            /// 1 because `findOneSolution` returns a solution
            /// without raising `Searchable.fail()`
            /// and the try-block above returns 1.
            ///
            /// Thus, if a solution exists, `findOneSolution`
            /// returns and 1 is returned to the `pick` handler
            /// where the `k(i)` expression is written.
            ///
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
