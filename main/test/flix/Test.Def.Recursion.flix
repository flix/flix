mod Test.Def.Recursion {

    use Lizt.{Null, Conz};
    use Opt.{Empty, El};

    enum Lizt[a] {
        case Null
        case Conz(a, Lizt[a])
    }

    enum Opt[a] with Eq, Order, ToString {
        case Empty
        case El(a)
    }

    def emptyList(): Lizt[Int32] = Null

    def list1(x: a): Lizt[a] = Conz(x, Null)

    def list2(x: a): Lizt[a] = Conz(x, Conz(x, Null))

    def list3(x: a): Lizt[a] = Conz(x, Conz(x, Conz(x, Null)))

    def simple(l: Lizt[a]): Opt[a] = match l {
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


    def mutualSimple1(l: Lizt[a]): Opt[a] = match l {
        case Null          => Empty
        case Conz(x, Null) => El(x)
        case Conz(_, xs)   => mutualSimple2(xs)
    }

    def mutualSimple2(l: Lizt[a]): Opt[a] = match l {
        case Null          => Empty
        case Conz(x, Null) => El(x)
        case Conz(_, xs)   => mutualSimple1(xs)
    }

    def higherOrder(f: a -> b, l: Lizt[a]): Opt[b] = match l {
        case Null          => Empty
        case Conz(x, Null) => El(f(x))
        case Conz(_, xs)   => higherOrder(f, xs)
    }

    def nonTailCallMap(f: a -> b, l: Lizt[a]): Lizt[b] = match l {
        case Null        => Null
        case Conz(x, xs) => Conz(f(x), nonTailCallMap(f, xs))
    }

    def tailCallMapReverse(f: a -> b, l: Lizt[a], acc: Lizt[b]): Lizt[b] = match l {
        case Null        => acc
        case Conz(x, xs) => tailCallMapReverse(f, xs, Conz(f(x), acc))
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
    def nonTailCallMap01(): Lizt[Int32] = {
        nonTailCallMap(x -> x * x, emptyList())
    }

    @test
    def nonTailCallMap02(): Lizt[Int32] = {
        nonTailCallMap(x -> x * x, list1(1))
    }

    @test
    def nonTailCallMap03(): Lizt[Int32] = {
        nonTailCallMap(x -> x * x, list1(2))
    }

    @test
    def nonTailCallMap04(): Lizt[Int32] = {
        nonTailCallMap(x -> x * x, list3(3))
    }

    @test
    def tailCallMapReverse01(): Lizt[Int32] = {
        tailCallMapReverse(x -> x * x, emptyList(), Null)
    }

    @test
    def tailCallMapReverse02(): Lizt[Int32] = {
        tailCallMapReverse(x -> x * x, list1(1), Null)
    }

    @test
    def tailCallMapReverse03(): Lizt[Int32] = {
        tailCallMapReverse(x -> x * x, list1(2), Null)
    }

    @test
    def tailCallMapReverse04(): Lizt[Int32] = {
        tailCallMapReverse(x -> x * x, list3(3), Null)
    }
}
