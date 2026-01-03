mod Test.Def.Recursion {

    use Assert.{assertEq, assertTrue}

    @Test
    def simple01(): Unit \ Assert =
        assertEq(expected = None, simple(emptyList()))

    @Test
    def simple02(): Unit \ Assert =
        assertEq(expected = Some(1), simple(list1(1)))

    @Test
    def simple03(): Unit \ Assert =
        assertEq(expected = Some(1), simple(list2(1)))

    @Test
    def simple04(): Unit \ Assert =
        assertEq(expected = Some(1), simple(list3(1)))

    @Test
    def swapsWithExpr01(): Unit \ Assert =
        assertEq(expected = 1_000_000, swapsWithExpr(0, 0))

    @Test
    def swapsWithExpr02(): Unit \ Assert =
        assertEq(expected = 1_001_000, swapsWithExpr(1, 0))

    @Test
    def swapsWithExpr03(): Unit \ Assert =
        assertEq(expected = 1_001_000, swapsWithExpr(0, 1))

    @Test
    def swapsWithExpr04(): Unit \ Assert =
        assertEq(expected = 1_003_002, swapsWithExpr(1, 2))

    @Test
    def swapsWithExpr05(): Unit \ Assert =
        assertEq(expected = 1_003_002, swapsWithExpr(2, 1))

    @Test
    def swapsConstantParams01(): Unit \ Assert =
        assertEq(expected = 0, swapsConstantParams(0, 0, 0))

    @Test
    def swapsConstantParams02(): Unit \ Assert =
        assertEq(expected = 2, swapsConstantParams(1, 2, 0))

    @Test
    def swapsConstantParams03(): Unit \ Assert =
        assertEq(expected = 1, swapsConstantParams(1, 2, 51))

    @Test
    def swapsConstantParams04(): Unit \ Assert =
        assertEq(expected = 2, swapsConstantParams(1, 2, 30))

    @Test
    def swapsConstantParams05(): Unit \ Assert =
        assertEq(expected = 2, swapsConstantParams(1, 2, -10))

    @Test
    def mutual01(): Unit \ Assert =
        assertEq(expected = 0, mutual1(0, 1))

    @Test
    def mutual02(): Unit \ Assert =
        assertEq(expected = 0, mutual1(1000, 1))

    @Test
    def mutual03(): Unit \ Assert =
        assertEq(expected = 1, mutual1(1000, 2))

    @Test
    def mutualSwaps01(): Unit \ Assert =
        assertEq(expected = 0, mutual1(0, 1))

    @Test
    def mutualSwaps02(): Unit \ Assert =
        assertEq(expected = 0, mutual1(1000, 1))

    @Test
    def mutualSwaps03(): Unit \ Assert =
        assertEq(expected = 1, mutual1(1000, 2))

    @Test
    def mutualSimple01(): Unit \ Assert =
        assertEq(expected = None, mutualSimple1(emptyList()))

    @Test
    def mutualSimple02(): Unit \ Assert =
        assertEq(expected = Some(1), mutualSimple1(list1(1)))

    @Test
    def mutualSimple03(): Unit \ Assert =
        assertEq(expected = Some(1), mutualSimple1(list2(1)))

    @Test
    def mutualSimple04(): Unit \ Assert =
        assertEq(expected = Some(1), mutualSimple1(list3(1)))

    @Test
    def higherOrder01(): Unit \ Assert =
        assertEq(expected = None, higherOrder(x -> x + 1, emptyList()))

    @Test
    def higherOrder02(): Unit \ Assert =
        assertEq(expected = Some(2), higherOrder(x -> x + 1, list1(1)))

    @Test
    def higherOrder03(): Unit \ Assert =
        assertEq(expected = Some(2), higherOrder(x -> x + 1, list2(1)))

    @Test
    def higherOrder04(): Unit \ Assert =
        assertEq(expected = Some(2), higherOrder(x -> x + 1, list3(1)))

    @Test
    def higherOrderSwaps01(): Unit \ Assert =
        assertEq(expected = None, higherOrderSwaps(x -> x + 1, x -> x + 2, emptyList()))

    @Test
    def higherOrderSwaps02(): Unit \ Assert =
        assertEq(expected = Some(2), higherOrderSwaps(x -> x + 1, x -> x + 2, list1(1)))

    @Test
    def higherOrderSwaps03(): Unit \ Assert =
        assertEq(expected = Some(3), higherOrderSwaps(x -> x + 1, x -> x + 2, list2(1)))

    @Test
    def higherOrderSwaps04(): Unit \ Assert =
        assertEq(expected = Some(2), higherOrderSwaps(x -> x + 1, x -> x + 2, list3(1)))

    @Test
    def mutualHigherOrder01(): Unit \ Assert =
        assertEq(expected = None, mutualHigherOrder1(x -> x + 1, emptyList()))

    @Test
    def mutualHigherOrder02(): Unit \ Assert =
        assertEq(expected = Some(2), mutualHigherOrder1(x -> x + 1, list1(1)))

    @Test
    def mutualHigherOrder03(): Unit \ Assert =
        assertEq(expected = Some(2), mutualHigherOrder1(x -> x + 1, list2(1)))

    @Test
    def mutualHigherOrder04(): Unit \ Assert =
        assertEq(expected = Some(2), mutualHigherOrder1(x -> x + 1, list3(1)))

    @Test
    def mutualHigherOrderSwaps01(): Unit \ Assert =
        assertEq(expected = None, mutualHigherOrderSwaps1(x -> x + 1, x -> x + 2, emptyList()))

    @Test
    def mutualHigherOrderSwaps02(): Unit \ Assert =
        assertEq(expected = Some(2), mutualHigherOrderSwaps1(x -> x + 1, x -> x + 2, list1(1)))

    @Test
    def mutualHigherOrderSwaps03(): Unit \ Assert =
        assertEq(expected = Some(3), mutualHigherOrderSwaps1(x -> x + 1, x -> x + 2, list2(1)))

    @Test
    def mutualHigherOrderSwaps04(): Unit \ Assert =
        assertEq(expected = Some(2), mutualHigherOrderSwaps1(x -> x + 1, x -> x + 2, list3(1)))

    @Test
    def nonTailCallMap01(): Unit \ Assert =
        assertEq(expected = Nil, nonTailCallMap(x -> x * x, emptyList()))

    @Test
    def nonTailCallMap02(): Unit \ Assert =
        assertEq(expected = Cons(1, Nil), nonTailCallMap(x -> x * x, list1(1)))

    @Test
    def nonTailCallMap03(): Unit \ Assert =
        assertEq(expected = Cons(4, Nil), nonTailCallMap(x -> x * x, list1(2)))

    @Test
    def nonTailCallMap04(): Unit \ Assert =
        assertEq(expected = Cons(9, Cons(9, Cons(9, Nil))), nonTailCallMap(x -> x * x, list3(3)))

    @Test
    def tailCallMapReverse01(): Unit \ Assert =
        assertEq(expected = Nil, tailCallMapReverse(x -> x * x, emptyList(), Nil))

    @Test
    def tailCallMapReverse02(): Unit \ Assert =
        assertEq(expected = Cons(1, Nil), tailCallMapReverse(x -> x * x, list1(1), Nil))

    @Test
    def tailCallMapReverse03(): Unit \ Assert =
        assertEq(expected = Cons(4, Nil), tailCallMapReverse(x -> x * x, list1(2), Nil))

    @Test
    def tailCallMapReverse04(): Unit \ Assert =
        assertEq(expected = Cons(9, Cons(9, Cons(9, Nil))), tailCallMapReverse(x -> x * x, list3(3), Nil))

    @Test
    def allConstantParams01(): Unit \ Assert =
        assertTrue(allConstantParams(0))

    @Test
    def allConstantParams02(): Unit \ Assert =
        assertTrue(allConstantParams(true))

    @Test
    def allConstantParams03(): Unit \ Assert =
        assertTrue(allConstantParams("abc"))

    @Test
    def higherOrderCapturesItself01(): Unit \ Assert =
        assertEq(expected = 6, higherOrderCapturesItself(x -> cont -> cont() + x, 0, list123()))

    @Test
    def higherOrderCapturesItself02(): Unit \ Assert =
        assertEq(expected = 4, higherOrderCapturesItself(x -> _ -> x + 3, 0, list123()))

    @Test
    def higherOrderPartiallyApplies01(): Unit \ Assert =
        assertEq(expected = 6, higherOrderPartiallyApplies(x -> xs -> cont -> cont(xs) + x, 0, list123()))

    @Test
    def higherOrderPartiallyApplies02(): Unit \ Assert =
        assertEq(expected = 7, higherOrderPartiallyApplies(x -> xs -> cont -> if (x == 2) cont(list1(4)) + x else cont(xs) + x, 0, list123()))

    def emptyList(): List[Int32] = Nil

    def list1(x: a): List[a] = Cons(x, Nil)

    def list2(x: a): List[a] = Cons(x, Cons(x, Nil))

    def list3(x: a): List[a] = Cons(x, Cons(x, Cons(x, Nil)))

    def list123(): List[Int32] = Cons(1, Cons(2, Cons(3, Nil)))

    def simple(l: List[a]): Option[a] = match l {
        case Nil          => None
        case Cons(x, Nil) => Some(x)
        case Cons(_, xs)  => simple(xs)
    }

    def swapsWithExpr(x: Int32, y: Int32): Int32 =
        let res = x * y;
        if (res < 1_000_000)
            swapsWithExpr(1 + 2 + y, x + 1)
        else
            res

    def swapsConstantParams(x: Int32, y: Int32, i: Int32): Int32 =
        if (i > 50)
            x
        else
            swapsConstantParams(y, x, i + 1)

    def mutual1(x: Int32, y: Int32): Int32 =
        if (x / y == 0)
            x
        else
            mutual2(x - 1, y)

    def mutual2(x: Int32, y: Int32): Int32 =
        mutual1(x, y)

    def mutualSwaps1(x: Int32, y: Int32): Int32 =
        if (x / y == 0)
            1
        else
            mutualSwaps2(y, x)

    def mutualSwaps2(x: Int32, y: Int32): Int32 =
        mutualSwaps1(x + 1, y - 1)

    def mutualSimple1(l: List[a]): Option[a] = match l {
        case Nil          => None
        case Cons(x, Nil) => Some(x)
        case Cons(_, xs)   => mutualSimple2(xs)
    }

    def mutualSimple2(l: List[a]): Option[a] = match l {
        case Nil          => None
        case Cons(x, Nil) => Some(x)
        case Cons(_, xs)  => mutualSimple1(xs)
    }

    def higherOrder(f: a -> b, l: List[a]): Option[b] = match l {
        case Nil          => None
        case Cons(x, Nil) => Some(f(x))
        case Cons(_, xs)  => higherOrder(f, xs)
    }

    def higherOrderSwaps(f: a -> b, g: a -> b, l: List[a]): Option[b] = match l {
        case Nil          => None
        case Cons(x, Nil) => Some(f(x))
        case Cons(_, xs)  => higherOrderSwaps(g, f, xs)
    }

    def mutualHigherOrder1(f: a -> b, l: List[a]): Option[b] = match l {
        case Nil          => None
        case Cons(x, Nil) => Some(f(x))
        case Cons(_, xs)  => mutualHigherOrder2(f, xs)
    }

    def mutualHigherOrder2(f: a -> b, l: List[a]): Option[b] =match l {
        case Nil          => None
        case Cons(x, Nil) => Some(f(x))
        case Cons(_, xs)  => mutualHigherOrder1(f, xs)
    }

    def mutualHigherOrderSwaps1(f: a -> b, g: a -> b, l: List[a]): Option[b] = match l {
        case Nil          => None
        case Cons(x, Nil) => Some(f(x))
        case Cons(_, xs)  => mutualHigherOrderSwaps2(g, f, xs)
    }

    def mutualHigherOrderSwaps2(f: a -> b, g: a -> b, l: List[a]): Option[b] = match l {
        case Nil          => None
        case Cons(x, Nil) => Some(f(x))
        case Cons(_, xs)  => mutualHigherOrderSwaps1(g, f, xs)
    }

    def nonTailCallMap(f: a -> b, l: List[a]): List[b] = match l {
        case Nil         => Nil
        case Cons(x, xs) => Cons(f(x), nonTailCallMap(f, xs))
    }

    def tailCallMapReverse(f: a -> b, l: List[a], acc: List[b]): List[b] = match l {
        case Nil         => acc
        case Cons(x, xs) => tailCallMapReverse(f, xs, Cons(f(x), acc))
    }

    def allConstantParams(x: a): Bool with Eq[a] =
        if (x == x)
            true
        else
            allConstantParams(x)

    pub def higherOrderCapturesItself(f: (a, Unit -> b) -> b, z: b, l: List[a]): b = match l {
        case Nil         => z
        case Cons(x, xs) => f(x, _ -> higherOrderCapturesItself(f, z, xs))
    }

    pub def higherOrderPartiallyApplies(f: (a, List[a], List[a] -> b) -> b, z: b, l: List[a]): b = match l {
        case Nil         => z
        case Cons(x, xs) => f(x, xs, higherOrderPartiallyApplies(f, z))
    }

}
