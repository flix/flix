mod Test.Def.Recursion {
    use List.{Null, Conz};
    use Opt.{Empty, El};

    enum List[a] {
        case Null
        case Conz(a, List[a])
    }

    enum Opt[a] with Eq, Order, ToString {
        case Empty
        case El(a)
    }

    def emptyList(): List[Int32] = Null

    def list1(x: a): List[a] = Conz(x, Null)

    def list2(x: a): List[a] = Conz(x, Conz(x, Null))

    def list3(x: a): List[a] = Conz(x, Conz(x, Conz(x, Null)))

    def list123(): List[Int32] = Conz(1, Conz(2, Conz(3, Null)))

    def simple(l: List[a]): Opt[a] = match l {
        case Null          => Empty
        case Conz(x, Null) => El(x)
        case Conz(_, xs)   => simple(xs)
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

    def mutualSimple1(l: List[a]): Opt[a] = match l {
        case Null          => Empty
        case Conz(x, Null) => El(x)
        case Conz(_, xs)   => mutualSimple2(xs)
    }

    def mutualSimple2(l: List[a]): Opt[a] = match l {
        case Null          => Empty
        case Conz(x, Null) => El(x)
        case Conz(_, xs)   => mutualSimple1(xs)
    }

    def higherOrder(f: a -> b, l: List[a]): Opt[b] = match l {
        case Null          => Empty
        case Conz(x, Null) => El(f(x))
        case Conz(_, xs)   => higherOrder(f, xs)
    }

    def higherOrderSwaps(f: a -> b, g: a -> b, l: List[a]): Opt[b] = match l {
        case Null          => Empty
        case Conz(x, Null) => El(f(x))
        case Conz(_, xs)   => higherOrderSwaps(g, f, xs)
    }

    def mutualHigherOrder1(f: a -> b, l: List[a]): Opt[b] = match l {
        case Null          => Empty
        case Conz(x, Null) => El(f(x))
        case Conz(_, xs)   => mutualHigherOrder2(f, xs)
    }

    def mutualHigherOrder2(f: a -> b, l: List[a]): Opt[b] =match l {
        case Null          => Empty
        case Conz(x, Null) => El(f(x))
        case Conz(_, xs)   => mutualHigherOrder1(f, xs)
    }

    def mutualHigherOrderSwaps1(f: a -> b, g: a -> b, l: List[a]): Opt[b] = match l {
        case Null          => Empty
        case Conz(x, Null) => El(f(x))
        case Conz(_, xs)   => mutualHigherOrderSwaps2(g, f, xs)
    }

    def mutualHigherOrderSwaps2(f: a -> b, g: a -> b, l: List[a]): Opt[b] = match l {
        case Null          => Empty
        case Conz(x, Null) => El(f(x))
        case Conz(_, xs)   => mutualHigherOrderSwaps1(g, f, xs)
    }

    def nonTailCallMap(f: a -> b, l: List[a]): List[b] = match l {
        case Null        => Null
        case Conz(x, xs) => Conz(f(x), nonTailCallMap(f, xs))
    }

    def tailCallMapReverse(f: a -> b, l: List[a], acc: List[b]): List[b] = match l {
        case Null        => acc
        case Conz(x, xs) => tailCallMapReverse(f, xs, Conz(f(x), acc))
    }

    def allConstantParams(x: a): Bool with Eq[a] =
        if (x == x)
            true
        else
            allConstantParams(x)

    pub def higherOrderCapturesItself(f: (a, Unit -> b) -> b, z: b, l: List[a]): b = match l {
        case Null        => z
        case Conz(x, xs) => f(x, _ -> higherOrderCapturesItself(f, z, xs))
    }

    pub def higherOrderPartiallyApplies(f: (a, List[a], List[a] -> b) -> b, z: b, l: List[a]): b = match l {
        case Null        => z
        case Conz(x, xs) => f(x, xs, higherOrderPartiallyApplies(f, z))
    }

    @test
    def simple01(): Bool = {
        Empty == simple(emptyList())
    }

    @test
    def simple02(): Bool = {
        El(1) == simple(list1(1))
    }

    @test
    def simple03(): Bool = {
        El(1) == simple(list2(1))
    }

    @test
    def simple04(): Bool = {
        El(1) == simple(list3(1))
    }

    @test
    def swapsWithExpr01(): Int32 = {
        swapsWithExpr(0, 0)
    }

    @test
    def swapsWithExpr02(): Int32 = {
        swapsWithExpr(1, 0)
    }

    @test
    def swapsWithExpr03(): Int32 = {
        swapsWithExpr(0, 1)
    }

    @test
    def swapsWithExpr04(): Int32 = {
        swapsWithExpr(1, 2)
    }

    @test
    def swapsWithExpr05(): Int32 = {
        swapsWithExpr(2, 1)
    }

    @test
    def swapsConstantParams01(): Int32 = {
        swapsConstantParams(0, 0, 0)
    }

    @test
    def swapsConstantParams02(): Int32 = {
        swapsConstantParams(1, 2, 0)
    }

    @test
    def swapsConstantParams03(): Int32 = {
        swapsConstantParams(1, 2, 51)
    }

    @test
    def swapsConstantParams04(): Int32 = {
        swapsConstantParams(1, 2, 30)
    }

    @test
    def swapsConstantParams05(): Int32 = {
        swapsConstantParams(1, 2, -10)
    }

    @test
    def mutual01(): Int32 = {
        mutual1(0, 1)
    }

    @test
    def mutual02(): Int32 = {
        mutual1(1000, 1)
    }

    @test
    def mutual03(): Int32 = {
        mutual1(1000, 2)
    }

    @test
    def mutualSwaps01(): Int32 = {
        mutual1(0, 1)
    }

    @test
    def mutualSwaps02(): Int32 = {
        mutual1(1000, 1)
    }

    @test
    def mutualSwaps03(): Int32 = {
        mutual1(1000, 2)
    }

    @test
    def mutualSimple01(): Bool = {
        Empty == mutualSimple1(emptyList())
    }

    @test
    def mutualSimple02(): Bool = {
        El(1) == mutualSimple1(list1(1))
    }

    @test
    def mutualSimple03(): Bool = {
        El(1) == mutualSimple1(list2(1))
    }

    @test
    def mutualSimple04(): Bool = {
        El(1) == mutualSimple1(list3(1))
    }

    @test
    def higherOrder01(): Bool = {
        Empty == higherOrder(x -> x + 1, emptyList())
    }

    @test
    def higherOrder02(): Bool = {
        El(2) == higherOrder(x -> x + 1, list1(1))
    }

    @test
    def higherOrder03(): Bool = {
        El(2) == higherOrder(x -> x + 1, list2(1))
    }

    @test
    def higherOrder04(): Bool = {
        El(2) == higherOrder(x -> x + 1, list3(1))
    }

    @test
    def higherOrderSwaps01(): Bool = {
        Empty == higherOrderSwaps(x -> x + 1, x -> x + 2, emptyList())
    }

    @test
    def higherOrderSwaps02(): Bool = {
        El(2) == higherOrderSwaps(x -> x + 1, x -> x + 2, list1(1))
    }

    @test
    def higherOrderSwaps03(): Bool = {
        El(3) == higherOrderSwaps(x -> x + 1, x -> x + 2, list2(1))
    }

    @test
    def higherOrderSwaps04(): Bool = {
        El(2) == higherOrderSwaps(x -> x + 1, x -> x + 2, list3(1))
    }

    @test
    def mutualHigherOrder01(): Bool = {
        Empty == mutualHigherOrder1(x -> x + 1, emptyList())
    }

    @test
    def mutualHigherOrder02(): Bool = {
        El(2) == mutualHigherOrder1(x -> x + 1, list1(1))
    }

    @test
    def mutualHigherOrder03(): Bool = {
        El(2) == mutualHigherOrder1(x -> x + 1, list2(1))
    }

    @test
    def mutualHigherOrder04(): Bool = {
        El(2) == mutualHigherOrder1(x -> x + 1, list3(1))
    }

    @test
    def mutualHigherOrderSwaps01(): Bool = {
        Empty == mutualHigherOrderSwaps1(x -> x + 1, x -> x + 2, emptyList())
    }

    @test
    def mutualHigherOrderSwaps02(): Bool = {
        El(2) == mutualHigherOrderSwaps1(x -> x + 1, x -> x + 2, list1(1))
    }

    @test
    def mutualHigherOrderSwaps03(): Bool = {
        El(3) == mutualHigherOrderSwaps1(x -> x + 1, x -> x + 2, list2(1))
    }

    @test
    def mutualHigherOrderSwaps04(): Bool = {
        El(2) == mutualHigherOrderSwaps1(x -> x + 1, x -> x + 2, list3(1))
    }

    @test
    def nonTailCallMap01(): List[Int32] = {
        nonTailCallMap(x -> x * x, emptyList())
    }

    @test
    def nonTailCallMap02(): List[Int32] = {
        nonTailCallMap(x -> x * x, list1(1))
    }

    @test
    def nonTailCallMap03(): List[Int32] = {
        nonTailCallMap(x -> x * x, list1(2))
    }

    @test
    def nonTailCallMap04(): List[Int32] = {
        nonTailCallMap(x -> x * x, list3(3))
    }

    @test
    def tailCallMapReverse01(): List[Int32] = {
        tailCallMapReverse(x -> x * x, emptyList(), Null)
    }

    @test
    def tailCallMapReverse02(): List[Int32] = {
        tailCallMapReverse(x -> x * x, list1(1), Null)
    }

    @test
    def tailCallMapReverse03(): List[Int32] = {
        tailCallMapReverse(x -> x * x, list1(2), Null)
    }

    @test
    def tailCallMapReverse04(): List[Int32] = {
        tailCallMapReverse(x -> x * x, list3(3), Null)
    }

    @test
    def allConstantParams01(): Bool = {
        allConstantParams(0)
    }

    @test
    def allConstantParams02(): Bool = {
        allConstantParams(true)
    }

    @test
    def allConstantParams03(): Bool = {
        allConstantParams("abc")
    }

    @test
    def higherOrderCapturesItself01(): Int32 = {
        higherOrderCapturesItself(x -> cont -> cont() + x, 0, list123())
    }

    @test
    def higherOrderCapturesItself02(): Int32 = {
        higherOrderCapturesItself(x -> _ -> x + 3, 0, list123())
    }

    @test
    def higherOrderPartiallyApplies01(): Int32 = {
        higherOrderPartiallyApplies(x -> xs -> cont -> cont(xs) + x, 0, list123())
    }
    @test
    def higherOrderPartiallyApplies02(): Int32 = {
        higherOrderPartiallyApplies(x -> xs -> cont -> if (x == 2) cont(list1(4)) + x else cont(xs) + x, 0, list123())
    }
}
