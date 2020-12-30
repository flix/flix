use LazyList.LazyList;
use LazyList.LazyList.{Empty, LazyCons};

namespace TestLazyList {

    /////////////////////////////////////////////////////////////////////////////
    // repeat                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def repeat01(): Bool =
        let xs = LazyList.take(1, LazyList.repeat(1));
        LazyList.length(xs) == 1 and LazyList.head(xs) == Some(1)

    @test
    def repeat02(): Bool =
        let xs = LazyList.take(1, LazyList.repeat('a'));
        LazyList.length(xs) == 1 and LazyList.head(xs) == Some('a')

    @test
    def repeat03(): Bool =
        let xs = LazyList.take(1, LazyList.repeat("Hello, World!"));
        LazyList.length(xs) == 1 and LazyList.head(xs) == Some("Hello, World!")

    @test
    def repeat04(): Bool =
        let xs = LazyList.take(2, LazyList.repeat(42));
        LazyList.length(xs) == 2 and LazyList.head(xs) == Some(42) and LazyList.last(xs) == Some(42)

    @test
    def repeat05(): Bool = {
        let xs = LazyList.take(0, LazyList.repeat(42));
        LazyList.isEmpty(xs)
    }

    /////////////////////////////////////////////////////////////////////////////
    // from                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def from01(): Bool = {
        let xs = LazyList.take(0, LazyList.from(0));
        LazyList.isEmpty(xs)
    }

    @test
    def from02(): Bool =
        let xs = LazyList.take(1, LazyList.from(0));
        LazyList.length(xs) == 1 and LazyList.head(xs) == Some(0)

    @test
    def from03(): Bool =
        let xs = LazyList.take(2, LazyList.from(107));
        LazyList.length(xs) == 2 and LazyList.head(xs) == Some(107) and LazyList.last(xs) == Some(108)

    @test
    def from04(): Bool = {
        let xs = LazyList.take(3, LazyList.from(42));
        LazyList.nth(0, xs) == Some(42) and LazyList.nth(1, xs) == Some(43) and LazyList.nth(2, xs) == Some(44)
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
        LazyList.head(LazyList.empty(): LazyList[Unit]) == None

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
        LazyList.last(LazyList.empty(): LazyList[Unit]) == None

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
    // nth                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def nth01(): Bool = LazyList.nth(0, LazyList.empty(): LazyList[Unit]) == None

    @test
    def nth02(): Bool = LazyList.nth(1, LazyList.empty(): LazyList[Unit]) == None

    @test
    def nth03(): Bool = {
        let xs = LazyList.take(10, LazyList.from(1));
        LazyList.nth(0, xs) == Some(1)
    }

    @test
    def nth04(): Bool = {
        let xs = LazyList.take(10, LazyList.from(1));
        LazyList.nth(5, xs) == Some(6)
    }

    @test
    def nth05(): Bool = {
        let xs = LazyList.take(10, LazyList.from(1));
        LazyList.nth(9, xs) == Some(10)
    }

    /////////////////////////////////////////////////////////////////////////////
    // append                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def append01(): Bool =
        LazyList.isEmpty(LazyList.append(LazyList.empty(), LazyList.empty()))

    @test
    def append02(): Bool = {
        let xs = LazyList.take(4, LazyList.from(1));
        let ys = LazyList.append(xs, LazyList.empty());
        LazyList.last(xs) == LazyList.last(ys)
    }

    @test
    def append03(): Bool = {
        let xs = LazyList.take(4, LazyList.from(1));
        let ys = LazyList.append(LazyList.empty(), xs);
        LazyList.last(xs) == LazyList.last(ys)
    }

    @test
    def append04(): Bool = {
        let xs = LazyList.take(4, LazyList.from(1));
        let ys = LazyList.take(9, LazyList.from(5));
        let zs = LazyList.append(xs, ys);
        LazyList.head(zs) == Some(1) and LazyList.last(zs) == Some(13)
    }

    @test
    def append05(): Bool = {
        let xs = LazyList.append(LazyCons(42, lazy Empty), LazyCons(107, lazy Empty));
        LazyList.foldLeft((acc, x) -> acc + x, 0, xs) == 149
    }

    /////////////////////////////////////////////////////////////////////////////
    // reverse                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reverse01(): Bool =
        LazyList.isEmpty(LazyList.reverse(LazyList.empty()))

    @test
    def reverse02(): Bool = {
        let xs = LazyList.reverse(LazyCons(42, lazy Empty));
        LazyList.head(xs) == Some(42)
    }

    @test
    def reverse03(): Bool = {
        let xs = LazyList.reverse(LazyCons(1, lazy LazyCons(2, lazy Empty)));
        LazyList.head(xs) == Some(2) and LazyList.last(xs) == Some(1)
    }

    @test
    def reverse04(): Bool = {
        let xs = LazyList.reverse(LazyList.take(7, LazyList.from(0)));
        LazyList.head(xs) == Some(6) and LazyList.last(xs) == Some(0)
    }

    @test
    def reverse05(): Bool = {
        let xs = LazyList.reverse(LazyList.unfold(x -> if (x == 42) None else Some((x - 1, x - 1)), 55));
        LazyList.head(xs) == Some(42) and LazyList.last(xs) == Some(54)
    }

    /////////////////////////////////////////////////////////////////////////////
    // take                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def take01(): Bool =
        LazyList.isEmpty(LazyList.take(0, LazyList.take(5, LazyList.from(10))))

    @test
    def take02(): Bool =
        LazyList.isEmpty(LazyList.take(0, LazyList.repeat(4)))

    @test
    def take03(): Bool = {
        let xs = LazyList.take(1, LazyList.repeat(7));
        LazyList.length(xs) == 1 and LazyList.head(xs) == Some(7)
    }

    @test
    def take04(): Bool = {
        let xs = LazyList.take(2, LazyList.from(42));
        LazyList.length(xs) == 2 and LazyList.last(xs) == Some(43)
    }

    /////////////////////////////////////////////////////////////////////////////
    // drop                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def drop01(): Bool =
        LazyList.isEmpty(LazyList.drop(0, LazyList.empty()))

    @test
    def drop02(): Bool =
        LazyList.isEmpty(LazyList.drop(42, LazyList.empty()))

    @test
    def drop03(): Bool =
        LazyList.length(LazyList.drop(7, LazyList.take(10, LazyList.repeat(3)))) == 3

    @test
    def drop04(): Bool = {
        let xs = LazyList.drop(9, LazyList.take(10, LazyList.repeat(3)));
        LazyList.length(xs) == 1 and LazyList.head(xs) == Some(3)
    }

    @test
    def drop05(): Bool = {
        let xs = LazyList.drop(1, LazyList.take(5, LazyList.repeat(3)));
        LazyList.length(xs) == 4 and LazyList.head(xs) == Some(3)
    }

    /////////////////////////////////////////////////////////////////////////////
    // tail                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def tail01(): Bool =
        LazyList.isEmpty(LazyList.tail(LazyList.empty()))

    @test
    def tail02(): Bool =
        LazyList.isEmpty(LazyList.tail(LazyCons(42, lazy Empty)))

    @test
    def tail03(): Bool = {
        let xs = LazyList.tail(LazyList.take(4, LazyList.from(10)));
        LazyList.length(xs) == 3 and LazyList.head(xs) == Some(11)
    }

    @test
    def tail04(): Bool = {
        let xs = LazyList.tail(LazyList.take(10, LazyList.from(0)));
        LazyList.length(xs) == 9 and LazyList.head(xs) == Some(1)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toList01(): Bool =
        LazyList.toList(LazyList.empty(): LazyList[Unit]) == Nil

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
        LazyList.length(xs) == 10 and LazyList.nth(9, xs) == Some(18)
    }

    @test
    def map02(): Bool = {
        let xs = LazyList.map(x -> x + 1, LazyCons(1, lazy Empty));
        LazyList.head(xs) == Some(2)
    }

    @test
    def map03(): Bool = {
        let xs = LazyList.take(2, LazyList.map(x -> x + "z", LazyList.repeat("x")));
        LazyList.head(xs) == Some("xz")
    }

    @test
    def map04(): Bool = {
        let xs = LazyList.map(x -> x + 3, LazyList.take(3, LazyList.from(1)));
        LazyList.last(xs) == Some(6)
    }

    /////////////////////////////////////////////////////////////////////////////
    // filter                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filter01(): Bool = {
        let xs = LazyList.filter(x -> x >= 2, LazyList.take(10, LazyList.from(0)));
        LazyList.head(xs) == Some(2) and LazyList.last(xs) == Some(9)
    }

    @test
    def filter02(): Bool = {
        let xs = LazyList.take(3, LazyList.filter(x -> x % 2 == 0, LazyList.from(0)));
        LazyList.last(xs) == Some(4)
    }

    @test
    def filter03(): Bool = {
        let xs = LazyList.filter(x -> x == '.', LazyList.take(5, LazyList.repeat(':')));
        LazyList.isEmpty(xs)
    }

    @test
    def filter04(): Bool = {
        let xs = LazyList.filter(x -> x > 14, LazyList.take(5, LazyList.from(10)));
        LazyList.isEmpty(xs)
    }

    /////////////////////////////////////////////////////////////////////////////
    // unfold                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def unfold01(): Bool = {
        let xs = LazyList.unfold(x -> if (x == 5) None else Some((x + 1, x + 1)), 0);
        LazyList.last(xs) == Some(5)
    }

    @test
    def unfold02(): Bool = {
        let xs = LazyList.unfold(x -> if (x == 100) None else Some((Char.fromInt32(x), x + 1)), 97);
        LazyList.head(xs) == Some('a') and LazyList.last(xs) == Some('c')
    }

    @test
    def unfold03(): Bool = {
        let xs = LazyList.unfold(x -> if (x == 0) None else Some((x - 1, x - 1)), 10);
        LazyList.head(xs) == Some(9)
    }

    /////////////////////////////////////////////////////////////////////////////
    // zipWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWith01(): Bool = {
        let xs = LazyList.take(3, LazyList.repeat(1));
        let ys = LazyList.zipWith((x, y) -> (x + 1, y + 1), xs, xs);
        LazyList.length(ys) == 3 and LazyList.head(ys) == Some((2, 2))
    }

    @test
    def zipWith02(): Bool = {
        let xs = LazyList.take(3, LazyList.repeat(1));
        let ys = LazyList.zipWith((x, y) -> (x + 1, y + 1), xs, LazyList.empty());
        LazyList.isEmpty(ys)
    }

    @test
    def zipWith03(): Bool = {
        let xs = LazyList.take(3, LazyList.repeat(1));
        let ys = LazyList.zipWith((x, y) -> (x + 1, y + 1), LazyList.empty(), xs);
        LazyList.isEmpty(ys)
    }

    @test
    def zipWith04(): Bool = {
        let xs = LazyList.take(4, LazyList.from(5));
        let ys = LazyList.take(4, LazyList.from(11));
        let zs = LazyList.zipWith((x, y) -> (x + 1, y - 1), xs, ys);
        LazyList.head(zs) == Some((6, 10)) and LazyList.last(zs) == Some((9, 13))
    }

    /////////////////////////////////////////////////////////////////////////////
    // zip                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zip01(): Bool = {
        let xs = LazyList.take(2, LazyList.zip(LazyList.repeat(4), LazyList.repeat(7)));
        LazyList.head(xs) == Some((4, 7))
    }

    @test
    def zip02(): Bool = {
        let xs = LazyList.take(1, LazyList.zip(LazyList.repeat('a'), LazyList.repeat('b')));
        LazyList.head(xs) == Some(('a', 'b'))
    }

    @test
    def zip03(): Bool = {
        let xs = LazyList.take(3, LazyList.repeat(1));
        let ys = LazyList.zip(xs, LazyList.empty());
        LazyList.isEmpty(ys)
    }

    @test
    def zip04(): Bool = {
        let xs = LazyList.take(3, LazyList.repeat(1));
        let ys = LazyList.zip(LazyList.empty(), xs);
        LazyList.isEmpty(ys)
    }

}