///
/// From Effekt Lang
///
mod EffectExamples.Queens {

    ///
    /// Type alias that represents a partial or complete
    /// solution to the Queens problem.
    /// A placement of queens i denoted by a list where the
    /// `i`th element corresponds to row `i` on the board
    /// where the value of the `i`th element corresponds to
    /// the column. Taken together they represent a position
    /// (or coordinate) on the board.
    ///
    type alias Placement = List[Int32]

    ///
    /// Algebraic effect that allows the caller
    /// to direct the search for a solution.
    ///
    eff Searchable {

        ///
        /// Should return a value in the range `[0, size]`.
        ///
        pub def pick(size: Int32): Int32

        pub def fail(): Void
    }

    ///
    /// Returns `true` if placing a queen at the position
    /// represented by `pos` does not collide with other queens.
    ///
    /// `diagonal` represents the distance in the diagonal to which
    /// it must search for another queen placement. If such a queen
    /// is placed at that position then there is a collision and the
    /// position is not safe.
    ///
    def isSafePosition(pos: Int32, diagonal: Int32, sol: Placement): Bool = match sol {
        case Nil     => true
        case q :: qs =>
            let isSafe = pos != q and pos != q + diagonal and pos != q - diagonal;
            isSafe and isSafePosition(pos, diagonal + 1, qs)
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
            let next = Searchable.pick(size);
            if (isSafePosition(next, 1, sol))
                next :: sol
            else
                Searchable.fail()

    ///
    /// Returns the number of possible solutions for a board
    /// of size `size * size` with `size` number of queens.
    ///
    pub def countSolutions(size: Int32): Int32 =
        try {
            findOneSolution(size, size);
            1
        } with Searchable {
            //
            // Returns 0 without calling the continuation,
            // so control is never transferred back to
            // `findOneSolution` or the try-block above.
            //
            def fail(_) = 0

            //
            // Places a queen at every index up to `sz`
            // and accumulates the number of solutions.
            //
            // Note here that the continuation `k` is invoked
            // with the index up to `sz` and that it returns
            // 1 because `findOneSolution` returns a solution
            // without raising `Searchable.fail()`
            // and the try-block above returns 1.
            //
            // Thus, if a solution exists, `findOneSolution`
            // returns and 1 is returned to the `pick` handler
            // where the `k(i)` expression is written.
            //
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
