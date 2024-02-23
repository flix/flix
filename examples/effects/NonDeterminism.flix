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
        do Decision.decide()

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
            do Failure.fail()
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
            case None    => do Failure.fail()
            case Some(c) => (a, b, c)
        }

    pub def backtrack(f: Unit -> a \ Decision + Failure): a =
        try {
            try { f() }
            with Failure {
                def fail(_k) = ???
            }
        } with Decision {
            def decide(k) = k(false)
        }

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
       let res = backtrack(() -> pythagorean(5, 15));
       Assert.eq((5, 15, 10), res)

}
