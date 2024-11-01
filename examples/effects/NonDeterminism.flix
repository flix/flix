///
/// From Eff lang
///
mod EffectExamples.NonDeterminism {

    eff Failure {
        pub def fail(): Void
    }

    eff Decision {
        pub def decide(): Bool
    }

    pub def decide(): Bool \ Decision =
        Decision.decide()

    pub def chooseTrue(f: Unit -> a \ Decision): a =
        try {
            f()
        } with Decision {
            def decide(k) = k(true)
        }

    pub def chooseMax(f: Unit -> Int32 \ Decision): Int32 =
        try {
            f()
        } with Decision {
            def decide(k) = Int32.max(k(true), k(false))
        }

    pub def chooseAll(f: Unit -> a \ Decision + Failure): List[a] =
        try {
            f() :: Nil
        } with Failure {
            def fail(_) = Nil
        } with Decision {
            def decide(k) = k(true) ++ k(false)
        }

    pub def chooseInt(m: Int32, n: Int32): Int32 \ Failure + Decision =
        if (m > n)
            Failure.fail()
        else if (decide())
            m
        else
            chooseInt(m + 1, n)

    ///
    /// Finds a square root of `m` if it exists.
    ///
    pub def sqrt(m: Int32): Option[Int32] = {
        def loop(n) = {
            let n2 = n * n;
            if (n2 > m)
                None
            else if (n2 == m)
                Some(n)
            else
                loop(n + 1)
        };
        loop(0)
    }

    ///
    /// Returns a pythagorean triple for `m, n`.
    ///
    pub def pythagorean(m: Int32, n: Int32): (Int32, Int32, Int32) \ Decision + Failure =
        // Choose the bigger integer
        let a = chooseInt(m, n - 1);
        // Choose the smaller integer
        let b = chooseInt(a, n);
        // Check if they are part of a pythagorean triple
        match sqrt(a * a + b * b) {
            case None    => Failure.fail()
            case Some(c) => (a, b, c)
        }

    pub def backtrack(f: Unit -> a \ Decision + ef): a \ ef =
        try {
            f()
        } with Decision {
            def decide(k) =
                try {
                    k(false)
                } with Failure {
                    def fail(_) = k(true)
                }
        }

    pub def chooseOne(l: List[a]): a \ Decision + Failure = match l {
        case Nil     => Failure.fail()
        case x :: xs => if (decide()) x else chooseOne(xs)
    }

    pub def safe(pos1: (Int32, Int32), pos2: (Int32, Int32)): Bool = match (pos1, pos2) {
        case ((x1, y1), (x2, y2)) => x1 != x2 and y1 != y2 and Int32.abs(x1 - x2) != Int32.abs(y1 - y2)
    }

    pub def available(x: Int32, qs: List[(Int32, Int32)]): List[Int32] =
        List.range(1, 9) |> List.filter(y -> List.forAll(safe((x, y)), qs))

    pub def place(x: Int32, qs: List[(Int32, Int32)]): List[(Int32, Int32)] \ Decision + Failure =
        if (x == 9)
            qs
        else
            let y = chooseOne(available(x, qs));
            place(x + 1, (x, y) :: qs)


    @Test
    pub def testChooseTrue01(): Bool =
        let x = chooseTrue(() -> if (decide()) 10 else 20);
        let y = chooseTrue(() -> if (decide()) 0 else 5);
        Assert.eq(10, x - y)

    @Test
    pub def testChooseMax01(): Bool =
        let x = chooseMax(() -> if (decide()) 10 else 20);
        let y = chooseMax(() -> if (decide()) 0 else 5);
        Assert.eq(15, x - y)

    @Test
    pub def testChooseAll01(): Bool =
        let l =
            foreach (
                x <- chooseAll(() -> checked_ecast(if (decide()) 10 else 20));
                y <- chooseAll(() -> checked_ecast(if (decide()) 0 else 5))
            ) yield x - y;
        Assert.eq(10 :: 5 :: 20 :: 15 :: Nil, l)

    @Test
    pub def testSqrt01(): Bool =
        Assert.eq(Some(2), sqrt(4))

    @Test
    pub def testSqrt02(): Bool =
        Assert.eq(Some(3), sqrt(9))

   @Test
   pub def testPythagorean01(): Bool =
       let res =
            try {
                backtrack(() -> pythagorean(5, 15))
            } with Failure {
                def fail(_) = (-1, -1, -1)
            };
       Assert.eq((9, 12, 15), res)

   @Test
   pub def testPythagorean02(): Bool =
       let res = chooseAll(() -> pythagorean(3, 4));
       Assert.eq((3, 4, 5) :: Nil, res)

   @Test
   pub def testPythagorean03(): Bool =
       let res = chooseAll(() -> pythagorean(5, 15));
       Assert.eq((5, 12, 13) :: (6, 8, 10) :: (8, 15, 17) :: (9, 12, 15) :: Nil, res)

    @Test
    pub def testEightQueens01(): Bool =
        let res =
            try {
                backtrack(() -> place(1, Nil))
            } with Failure {
                def fail(_) = Nil
            };
        Assert.eq((8, 5) :: (7, 7) :: (6, 2) :: (5, 6) :: (4, 3) :: (3, 1) :: (2, 4) :: (1, 8) :: Nil, res)

}
