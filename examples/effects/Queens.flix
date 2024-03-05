///
/// From Effekt Lang
///
mod EffectExamples.Queens {

    type alias Solution = List[Int32]

    eff Searchable {
        pub def pick(size: Int32): Int32
        pub def fail(): Void
    }

    def safe(queen: Int32, diagonal: Int32, sol: Solution): Bool = match sol {
        case Nil     => true
        case q :: qs =>
            let sfe = queen != q and queen != q + diagonal and queen != q - diagonal;
            sfe and safe(queen, diagonal + 1, qs)
    }

    pub def findOneSolution(size: Int32, queen: Int32): Solution \ Searchable =
        if (queen == 0)
            Nil
        else
            let sol = findOneSolution(size, queen - 1);
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
