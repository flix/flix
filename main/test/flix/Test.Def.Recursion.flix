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

    def simpleSwaps(x: Int32, y: Int32): Int32 =
        let res = x * y;
        if (res < 1_000_000)
            simpleSwaps(1 + 2 + y, x + 1)
        else
            res

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
    def simpleSwaps01(): Int32 = {
        simpleSwaps(0, 0)
    }

    @test
    def simpleSwaps02(): Int32 = {
        simpleSwaps(1, 0)
    }

    @test
    def simpleSwaps03(): Int32 = {
        simpleSwaps(0, 1)
    }

    @test
    def simpleSwaps04(): Int32 = {
        simpleSwaps(1, 2)
    }

    @test
    def simpleSwaps05(): Int32 = {
        simpleSwaps(2, 1)
    }
}
