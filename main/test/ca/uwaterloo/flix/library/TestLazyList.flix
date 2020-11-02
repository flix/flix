use LazyList.LazyList;
use LazyList.LazyList.{Empty, LazyCons};

namespace TestLazyList {

    /////////////////////////////////////////////////////////////////////////////
    // repeat                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def repeat01(): Bool =
        LazyList.take(1, LazyList.repeat(1)) == LazyCons(1, lazy Empty)

    @test
    def repeat02(): Bool =
        LazyList.take(1, LazyList.repeat('a')) == LazyCons('a', lazy Empty)

    @test
    def repeat03(): Bool =
        LazyList.take(1, LazyList.repeat("Hello, World!")) == LazyCons("Hello, World!", lazy Empty)

    @test
    def repeat04(): Bool =
        LazyList.take(2, LazyList.repeat(42)) == LazyCons(42, lazy LazyCons(42, lazy Empty))

    @test
    def repeat05(): Bool =
        LazyList.take(0, LazyList.repeat(42)) == LazyList.empty()

    /////////////////////////////////////////////////////////////////////////////
    // from                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def from01(): Bool =
        LazyList.take(0, LazyList.from(0)) == LazyList.empty()

    @test
    def from02(): Bool =
        LazyList.take(1, LazyList.from(0)) == LazyCons(0, lazy Empty)

    @test
    def from03(): Bool =
        LazyList.take(2, LazyList.from(107)) == LazyCons(107, lazy LazyCons(108, lazy Empty))

    @test
    def from04(): Bool = {
        let xs = LazyList.take(3, LazyList.from(42));
        xs == LazyCons(42, lazy LazyCons(43, lazy LazyCons(44, lazy Empty)))
    }

    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isEmpty01(): Bool =
        LazyList.isEmpty(LazyList.empty())

    @test
    def isEmpty02(): Bool =
        LazyList.isEmpty(LazyList.take(0, LazyList.from(5)))

    @test
    def isEmpty03(): Bool =
        LazyList.isEmpty(LazyList.take(3, LazyList.empty()))

    @test
    def isEmpty04(): Bool =
        not LazyList.isEmpty(LazyList.from(1))

    @test
    def isEmpty05(): Bool =
        not LazyList.isEmpty(LazyList.repeat(10))

    /////////////////////////////////////////////////////////////////////////////
    // length                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def length01(): Bool =
        LazyList.length(LazyList.empty()) == 0

    @test
    def length02(): Bool =
        LazyList.length(LazyList.take(1, LazyList.from(0))) == 1

    @test
    def length03(): Bool =
        LazyList.length(LazyList.take(2, LazyList.from(3))) == 2

    @test
    def length04(): Bool =
        LazyList.length(LazyList.take(10, LazyList.repeat(true))) == 10

    /////////////////////////////////////////////////////////////////////////////
    // head                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def head01(): Bool =
        LazyList.head(LazyList.empty()) == None

    @test
    def head02(): Bool =
        LazyList.head(LazyCons(1, lazy Empty)) == Some(1)

    @test
    def head03(): Bool =
        LazyList.head(LazyCons(42, lazy Empty)) == Some(42)

    @test
    def head04(): Bool =
        LazyList.head(LazyCons(42, lazy LazyCons(43, lazy Empty))) == Some(42)

    /////////////////////////////////////////////////////////////////////////////
    // last                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def last01(): Bool =
        LazyList.last(LazyList.empty()) == None

    @test
    def last02(): Bool =
        LazyList.last(LazyCons(1, lazy LazyList.empty())) == Some(1)

    @test
    def last03(): Bool =
        LazyList.last(LazyCons(42, lazy LazyCons(43, lazy Empty))) == Some(43)

    @test
    def last04(): Bool =
        LazyList.last(LazyCons('a', lazy LazyCons('b', lazy Empty))) == Some('b')

    /////////////////////////////////////////////////////////////////////////////
    // append                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def append01(): Bool =
        LazyList.append(LazyList.empty(), LazyList.empty()) == LazyList.empty()

    @test
    def append02(): Bool = {
        let xs = LazyList.take(4, LazyList.from(1));
        LazyList.append(xs, LazyList.empty()) == xs
    }

    @test
    def append03(): Bool = {
        let xs = LazyList.take(4, LazyList.from(1));
        LazyList.append(LazyList.empty(), xs) == xs
    }

    @test
    def append04(): Bool = {
        let xs = LazyList.take(4, LazyList.from(1));
        let ys = LazyList.take(9, LazyList.from(5));
        LazyList.append(xs, ys) == LazyList.take(13, LazyList.from(1))
    }

    @test
    def append05(): Bool = {
        let xs = LazyList.append(LazyCons(42, lazy Empty), LazyCons(107, lazy Empty));
        xs == LazyCons(42, lazy LazyCons(107, lazy Empty))
    }

    /////////////////////////////////////////////////////////////////////////////
    // reverse                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reverse01(): Bool =
        LazyList.reverse(LazyList.empty()) == LazyList.empty()

    @test
    def reverse02(): Bool = {
        let xs = LazyCons(42, lazy Empty);
        LazyList.reverse(xs) == xs
    }

    @test
    def reverse03(): Bool = {
        let xs = LazyCons(1, lazy LazyCons(2, lazy Empty));
        LazyList.reverse(xs) == LazyCons(2, lazy LazyCons(1, lazy Empty))
    }

    @test
    def reverse04(): Bool = {
        let xs = LazyList.take(7, LazyList.from(0));
        LazyList.reverse(xs) == LazyList.unfold(x -> if (x == 0) None else Some((x - 1, x - 1)), 7)
    }

    @test
    def reverse05(): Bool = {
        let xs = LazyList.unfold(x -> if (x == 42) None else Some((x - 1, x - 1)), 55);
        LazyList.reverse(xs) == LazyList.take(13, LazyList.from(42))
    }

    /////////////////////////////////////////////////////////////////////////////
    // take                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def take01(): Bool =
        LazyList.take(0, LazyList.take(5, LazyList.from(10))) == LazyList.empty()

    @test
    def take02(): Bool =
        LazyList.take(0, LazyList.repeat(4)) == LazyList.empty()

    @test
    def take03(): Bool =
        LazyList.take(1, LazyList.repeat(7)) == LazyCons(7, lazy Empty)

    @test
    def take04(): Bool =
        LazyList.take(2, LazyList.from(42)) == LazyCons(42, lazy LazyCons(43, lazy Empty))

    /////////////////////////////////////////////////////////////////////////////
    // drop                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def drop01(): Bool =
        LazyList.drop(0, LazyList.empty()) == LazyList.empty()

    @test
    def drop02(): Bool =
        LazyList.drop(42, LazyList.empty()) == LazyList.empty()

    @test
    def drop03(): Bool =
        LazyList.length(LazyList.drop(7, LazyList.take(10, LazyList.repeat(3)))) == 3

    @test
    def drop04(): Bool =
        LazyList.drop(9, LazyList.take(10, LazyList.repeat(3))) == LazyCons(3, lazy Empty)

    @test
    def drop05(): Bool =
        LazyList.drop(1, LazyList.take(5, LazyList.repeat(3))) == LazyList.take(4, LazyList.repeat(3))

    /////////////////////////////////////////////////////////////////////////////
    // tail                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def tail01(): Bool =
        LazyList.tail(LazyList.empty()) == LazyList.empty()

    @test
    def tail02(): Bool =
        LazyList.tail(LazyCons(42, lazy Empty)) == LazyList.empty()

    @test
    def tail03(): Bool =
        LazyList.tail(LazyList.take(4, LazyList.from(10))) == LazyList.take(3, LazyList.from(11))

    @test
    def tail04(): Bool =
        LazyList.tail(LazyList.take(10, LazyList.from(0))) == LazyList.take(9, LazyList.from(1))

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toList01(): Bool =
        LazyList.toList(LazyList.empty()) == Nil

    @test
    def toList02(): Bool =
        LazyList.toList(LazyCons(42, lazy Empty)) == 42 :: Nil

    @test
    def toList03(): Bool = {
        let xs = LazyList.unfold(x -> if (x == 50) None else Some((x - 1, x - 1)), 55);
        LazyList.toList(xs) == 54 :: 53 :: 52 :: 51 :: 50 :: Nil
    }

    @test
    def toList04(): Bool = {
        let xs = LazyList.unfold(s -> if (s >= 10) None else Some(Char.fromInt32(s + 48), s + 1), 0);
        LazyList.toList(xs) == '0' :: '1' :: '2' :: '3' :: '4' :: '5' :: '6' :: '7' :: '8' :: '9' :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // fold                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def fold01(): Bool =
        LazyList.fold((acc, x) -> acc + x, 0, LazyList.take(4, LazyList.repeat(1))) == 4

    @test
    def fold02(): Bool =
        LazyList.fold((acc, x) -> acc + x, "", LazyList.take(3, LazyList.repeat("z"))) == "zzz"

    @test
    def fold03(): Bool =
        LazyList.fold((acc, x) -> acc - x, 0, LazyList.take(3, LazyList.from(10))) == -33

    @test
    def fold04(): Bool =
        LazyList.fold((acc, x) -> acc * x, 1, LazyList.take(5, LazyList.from(1))) == 120

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeft01(): Bool =
        LazyList.foldLeft((acc, x) -> acc + x, 0, LazyList.take(4, LazyList.repeat(1))) == 4

    @test
    def foldLeft02(): Bool =
        LazyList.foldLeft((acc, x) -> acc + x, "", LazyList.take(3, LazyList.repeat("z"))) == "zzz"

    @test
    def foldLeft03(): Bool =
        LazyList.foldLeft((acc, x) -> acc - x, 0, LazyList.take(3, LazyList.from(10))) == -33

    @test
    def foldLeft04(): Bool =
        LazyList.foldLeft((acc, x) -> acc * x, 1, LazyList.take(5, LazyList.from(1))) == 120

    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRight01(): Bool =
        LazyList.foldRight((x, acc) -> x + (force acc), 0, LazyList.take(4, LazyList.repeat(1))) == 4

    @test
    def foldRight02(): Bool =
        not LazyList.foldRight((x, acc) -> x and (force acc), true, LazyList.repeat(false))

    @test
    def foldRight03(): Bool =
        LazyList.foldRight((x, acc) -> x or (force acc), false, LazyList.repeat(true))

    @test
    def foldRight04(): Bool =
        LazyList.foldRight((x, acc) -> x - (force acc), 0, LazyList.take(10, LazyList.from(0))) == -5

    @test
    def foldRight05(): Bool =
        LazyList.foldRight((x, acc) -> (force acc) - x, 0, LazyList.take(10, LazyList.from(0))) == -45

    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def exists01(): Bool =
        not LazyList.exists(x -> x == 0, LazyList.empty())

    @test
    def exists02(): Bool =
        LazyList.exists(x -> x == 0, LazyCons(0, lazy Empty))

    @test
    def exists03(): Bool =
        LazyList.exists(x -> x == 42, LazyList.from(42))

    @test
    def exists04(): Bool =
        LazyList.exists(x -> x == 10, LazyList.repeat(10))

    /////////////////////////////////////////////////////////////////////////////
    // forall                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forall01(): Bool =
        not LazyList.forall(x -> x > 2, LazyList.repeat(1))

    @test
    def forall02(): Bool =
        not LazyList.forall(x -> x == 9, LazyList.from(10))

    @test
    def forall03(): Bool =
        LazyList.forall(x -> x >= 7, LazyList.take(10, LazyList.from(7)))

    @test
    def forall04(): Bool =
        LazyList.forall(x -> x == 42, LazyList.take(5, LazyList.repeat(42)))

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map01(): Bool = {
        let xs = LazyList.map(x -> x * 2, LazyList.take(10, LazyList.from(0)));
        xs == LazyList.unfold(x -> if (x == 10) None else Some ((x * 2, x + 1)), 0)
    }

    @test
    def map02(): Bool =
        LazyList.map(x -> x + 1, LazyCons(1, lazy Empty)) == LazyCons(2, lazy Empty)

    @test
    def map03(): Bool = {
        let xs = LazyList.take(2, LazyList.map(x -> x + "z", LazyList.repeat("x")));
        xs == LazyCons("xz", lazy LazyCons("xz", lazy Empty))
    }

    @test
    def map04(): Bool = {
        let xs = LazyList.map(x -> x + 3, LazyList.take(3, LazyList.from(1)));
        xs == LazyCons(4, lazy LazyCons(5, lazy LazyCons(6, lazy Empty)))
    }

    /////////////////////////////////////////////////////////////////////////////
    // filter                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filter01(): Bool = {
        let xs = LazyList.filter(x -> x >= 2, LazyList.take(10, LazyList.from(0)));
        xs == LazyList.take(8, LazyList.from(2))
    }

    @test
    def filter02(): Bool =
        let xs = LazyList.take(3, LazyList.filter(x -> x % 2 == 0, LazyList.from(0)));
        xs == LazyCons(0, lazy LazyCons(2, lazy LazyCons(4, lazy Empty)))

    @test
    def filter03(): Bool =
        LazyList.filter(x -> x == '.', LazyList.take(5, LazyList.repeat(':'))) == LazyList.empty()

    @test
    def filter04(): Bool =
        LazyList.filter(x -> x > 14, LazyList.take(5, LazyList.from(10))) == LazyList.empty()

    /////////////////////////////////////////////////////////////////////////////
    // unfold                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def unfold01(): Bool =
        LazyList.unfold(x -> if (x == 5) None else Some((x + 1, x + 1)), 0) == LazyList.take(5, LazyList.from(1))

    @test
    def unfold02(): Bool =
        let xs = LazyList.unfold(x -> if (x == 100) None else Some((Char.fromInt32(x), x + 1)), 97);
        xs == LazyCons('a', lazy LazyCons('b', lazy LazyCons('c', lazy Empty)))

    @test
    def unfold03(): Bool =
        let xs = LazyList.reverse(LazyList.take(10, LazyList.from(0)));
        LazyList.unfold(x -> if (x == 0) None else Some((x - 1, x - 1)), 10) == xs

    /////////////////////////////////////////////////////////////////////////////
    // zipWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWith01(): Bool = {
        let xs = LazyList.take(3, LazyList.repeat(1));
        let ys = LazyList.zipWith((x, y) -> (x + 1, y + 1), xs, xs);
        ys == LazyCons((2, 2), lazy LazyCons((2, 2), lazy LazyCons((2, 2), lazy Empty)))
    }

    @test
    def zipWith02(): Bool = {
        let xs = LazyList.take(3, LazyList.repeat(1));
        LazyList.zipWith((x, y) -> (x + 1, y + 1), xs, LazyList.empty()) == LazyList.empty()
    }

    @test
    def zipWith03(): Bool = {
        let xs = LazyList.take(3, LazyList.repeat(1));
        LazyList.zipWith((x, y) -> (x + 1, y + 1), LazyList.empty(), xs) == LazyList.empty()
    }

    @test
    def zipWith04(): Bool = {
        let xs = LazyList.take(4, LazyList.from(5));
        let ys = LazyList.take(4, LazyList.from(11));
        let zs = LazyCons((6, 10), lazy LazyCons((7, 11), lazy LazyCons((8, 12), lazy LazyCons((9, 13), lazy Empty))));
        LazyList.zipWith((x, y) -> (x + 1, y - 1), xs, ys) == zs
    }

    /////////////////////////////////////////////////////////////////////////////
    // zip                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zip01(): Bool = {
        let xs = LazyList.take(2, LazyList.zip(LazyList.repeat(4), LazyList.repeat(7)));
        LazyCons((4, 7), lazy LazyCons((4, 7), lazy Empty)) == xs
    }

    @test
    def zip02(): Bool = {
        let xs = LazyList.take(1, LazyList.zip(LazyList.repeat('a'), LazyList.repeat('b')));
        LazyCons(('a', 'b'), lazy Empty) == xs
    }

    @test
    def zip03(): Bool = {
        let xs = LazyList.take(3, LazyList.repeat(1));
        LazyList.zip(xs, LazyList.empty()) == LazyList.empty()
    }

    @test
    def zip04(): Bool = {
        let xs = LazyList.take(3, LazyList.repeat(1));
        LazyList.zip(LazyList.empty(), xs) == LazyList.empty()
    }

}