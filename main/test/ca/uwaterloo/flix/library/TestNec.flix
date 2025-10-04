mod TestNec {

    use Assert.{assertEq, assertNeq, assertTrue, assertFalse, fail};
    use Hash.hash

    use Nec.ViewLeft.{OneLeft, SomeLeft}
    use Nec.ViewRight.{OneRight, SomeRight}

    use Nec.{cons, singleton, snoc}

    def necOf2(x: a, y: a): Nec[a] = cons(x, singleton(y))

    def necOf3(w: a, x: a, y: a): Nec[a] = cons(w, cons(x, singleton(y)))

    def necOf4(v: a, w: a, x: a, y: a): Nec[a] = cons(v, cons(w, cons(x, singleton(y))))

    def necOf5(u: a, v: a, w: a, x: a, y: a): Nec[a] = cons(u, cons(v, cons(w, cons(x, singleton(y)))))

    def necOf6(t: a, u: a, v: a, w: a, x: a, y: a): Nec[a] = cons(t, cons(u, cons(v, cons(w, cons(x, singleton(y))))))

    def necOf7(s: a, t: a, u: a, v: a, w: a, x: a, y: a): Nec[a] = cons(s, cons(t, cons(u, cons(v, cons(w, cons(x, singleton(y)))))))

    def necOf8(r: a, s: a, t: a, u: a, v: a, w: a, x: a, y: a): Nec[a] = cons(r, cons(s, cons(t, cons(u, cons(v, cons(w, cons(x, singleton(y))))))))

    /////////////////////////////////////////////////////////////////////////////
    // equals (==)                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def equals01(): Unit \ Assert = assertEq(expected = singleton(1), singleton(1))

    @Test
    def equals02(): Unit \ Assert = assertEq(expected = necOf2(1, 2), cons(1, singleton(2)))

    @Test
    def equals03(): Unit \ Assert = assertEq(expected = necOf3(1, 2, 3), snoc(cons(1, singleton(2)), 3))

    @Test
    def equals04(): Unit \ Assert = assertEq(expected = necOf4(1, 2, 3, 4), snoc(cons(1, cons(2, singleton(3))), 4))

    @Test
    def equals05(): Unit \ Assert = assertEq(expected = necOf5(1, 2, 3, 4, 5), snoc(snoc(cons(1, cons(2, singleton(3))), 4), 5))

    @Test
    def equals06(): Unit \ Assert = assertEq(expected = cons(1, necOf5(2, 3, 4, 5, 6)), snoc(necOf5(1, 2, 3, 4, 5), 6))

    /////////////////////////////////////////////////////////////////////////////
    // not equals (!=)                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def notEquals01(): Unit \ Assert = assertNeq(unexpected = singleton(1), singleton(2))

    @Test
    def notEquals02(): Unit \ Assert = assertNeq(unexpected = necOf2(1, 1), necOf2(1, 2))

    @Test
    def notEquals03(): Unit \ Assert = assertNeq(unexpected = necOf3(1, 2, 3), necOf2(1, 2))

    @Test
    def notEquals04(): Unit \ Assert = assertNeq(unexpected = necOf3(1, 2, 3), necOf3(1, 2, 2))

    /////////////////////////////////////////////////////////////////////////////
    // compare                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testCompare01(): Unit \ Assert = assertEq(expected = Comparison.EqualTo, Order.compare(singleton(1), singleton(1)))

    @Test
    def testCompare02(): Unit \ Assert = assertEq(expected = Comparison.LessThan, Order.compare(singleton(1), singleton(2)))

    @Test
    def testCompare03(): Unit \ Assert = assertEq(expected = Comparison.GreaterThan, Order.compare(singleton(2), singleton(1)))

    @Test
    def testCompare04(): Unit \ Assert = assertEq(expected = Comparison.LessThan, Order.compare(singleton(1), necOf2(1, 2)))

    @Test
    def testCompare05(): Unit \ Assert = assertEq(expected = Comparison.GreaterThan, Order.compare(necOf2(1, 2), singleton(1)))

    @Test
    def testCompare06(): Unit \ Assert = assertEq(expected = Comparison.GreaterThan, Order.compare(singleton(1), necOf2(0,0)))

    @Test
    def testCompare07(): Unit \ Assert = assertEq(expected = Comparison.LessThan, Order.compare(necOf2(0, 0), singleton(1)))

    @Test
    def testCompare08(): Unit \ Assert = assertEq(expected = Comparison.GreaterThan, Order.compare(necOf2(1, 2), necOf2(1, 1)))

    @Test
    def testCompare09(): Unit \ Assert = assertEq(expected = Comparison.LessThan, Order.compare(necOf2(1, 2), necOf2(1, 3)))

    @Test
    def testCompare10(): Unit \ Assert = assertEq(expected = Comparison.EqualTo, Order.compare(necOf4(1, 2, 3, 4), necOf4(1, 2, 3, 4)))

    /////////////////////////////////////////////////////////////////////////////
    // singleton                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def singleton01(): Unit \ Assert = assertEq(expected = 1 :: Nil, Nec.singleton(1) |> Nec.toList)

    @Test
    def singleton02(): Unit \ Assert = assertEq(expected = 2 :: Nil, Nec.singleton(2) |> Nec.toList)

    @Test
    def singleton03(): Unit \ Assert = assertEq(expected = 'a' :: Nil, Nec.singleton('a') |> Nec.toList)

    /////////////////////////////////////////////////////////////////////////////
    // isSingleton                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isSingleton01(): Unit \ Assert = assertTrue(Nec.isSingleton(singleton(1)))

    @Test
    def isSingleton02(): Unit \ Assert = assertFalse(Nec.isSingleton(necOf2(1, 2)))

    @Test
    def isSingleton03(): Unit \ Assert = assertFalse(Nec.isSingleton(necOf3(1, 2, 3)))


    /////////////////////////////////////////////////////////////////////////////
    // cons                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def cons01(): Unit \ Assert = assertEq(expected = necOf2(1, 2), Nec.cons(1, singleton(2)))

    @Test
    def cons02(): Unit \ Assert = assertEq(expected = necOf3(1, 2, 3), Nec.cons(1, Nec.cons(2, singleton(3))))

    @Test
    def cons03(): Unit \ Assert = assertEq(expected = necOf4(1, 2, 3, 4), Nec.cons(1, Nec.cons(2, Nec.cons(3, singleton(4)))))

    /////////////////////////////////////////////////////////////////////////////
    // snoc                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def snoc01(): Unit \ Assert = assertEq(expected = necOf2(1, 2), Nec.snoc(singleton(1), 2))

    @Test
    def snoc02(): Unit \ Assert = assertEq(expected = necOf3(1, 2, 3), Nec.snoc(Nec.snoc(singleton(1), 2), 3))

    @Test
    def snoc03(): Unit \ Assert = assertEq(expected = necOf4(1, 2, 3, 4), Nec.snoc(Nec.snoc(Nec.snoc(singleton(1), 2), 3), 4))

    /////////////////////////////////////////////////////////////////////////////
    // head                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def head01(): Unit \ Assert = assertEq(expected = 123, Nec.head(singleton(123)))

    @Test
    def head02(): Unit \ Assert = assertEq(expected = 123, Nec.head(necOf2(123, 1)))

    @Test
    def head03(): Unit \ Assert = assertEq(expected = 123, Nec.head(necOf3(123, 1, 2)))

    /////////////////////////////////////////////////////////////////////////////
    // last                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def last01(): Unit \ Assert = assertEq(expected = 123, Nec.last(singleton(123)))

    @Test
    def last02(): Unit \ Assert = assertEq(expected = 1, Nec.last(necOf2(123, 1)))

    @Test
    def last03(): Unit \ Assert = assertEq(expected = 2, Nec.last(necOf3(123, 1, 2)))

    /////////////////////////////////////////////////////////////////////////////
    // init                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def init01(): Unit \ Assert = assertEq(expected = Nil, Nec.init(singleton(1)))

    @Test
    def init02(): Unit \ Assert = assertEq(expected = 1 :: Nil, Nec.init(necOf2(1, 2)))

    @Test
    def init03(): Unit \ Assert = assertEq(expected = 1 :: 2 :: Nil, Nec.init(necOf3(1, 2, 3)))

    /////////////////////////////////////////////////////////////////////////////
    // tail                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tail01(): Unit \ Assert = assertEq(expected = Nil, Nec.tail(singleton(1)))

    @Test
    def tail02(): Unit \ Assert = assertEq(expected = 2 :: Nil, Nec.tail(necOf2(1, 2)))

    @Test
    def tail03(): Unit \ Assert = assertEq(expected = 2 :: 3 :: Nil, Nec.tail(necOf3(1, 2, 3)))

    /////////////////////////////////////////////////////////////////////////////
    // length                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def length01(): Unit \ Assert = assertEq(expected = 1, Nec.length(singleton(1)))

    @Test
    def length02(): Unit \ Assert = assertEq(expected = 2, Nec.length(necOf2(1, 2)))

    @Test
    def length03(): Unit \ Assert = assertEq(expected = 3, Nec.length(necOf3(1, 2, 3)))

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def size01(): Unit \ Assert = assertEq(expected = 1, Nec.size(singleton(1)))

    @Test
    def size02(): Unit \ Assert = assertEq(expected = 2, Nec.size(necOf2(1, 2)))

    @Test
    def size03(): Unit \ Assert = assertEq(expected = 3, Nec.size(necOf3(1, 2, 3)))

    /////////////////////////////////////////////////////////////////////////////
    // append                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def append01(): Unit \ Assert = assertEq(expected = necOf2(1, 2), Nec.append(singleton(1), singleton(2)))

    @Test
    def append02(): Unit \ Assert = assertEq(expected = necOf3(1, 2, 3), Nec.append(necOf2(1, 2), singleton(3)))

    @Test
    def append03(): Unit \ Assert = assertEq(expected = necOf3(1, 2, 3), Nec.append(singleton(1), necOf2(2, 3)))

    @Test
    def append04(): Unit \ Assert = assertEq(expected =  necOf4(1, 2, 3, 4), Nec.append(necOf2(1, 2), necOf2(3, 4)))

    @Test
    def append05(): Unit \ Assert = assertEq(expected = necOf6(1, 2, 3, 4, 5, 6), Nec.append(necOf3(1, 2, 3), necOf3(4, 5, 6)))

    /////////////////////////////////////////////////////////////////////////////
    // viewLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def viewLeft01(): Unit \ Assert = assertTrue(Nec.viewLeft(singleton(1)) == OneLeft(1))

    @Test
    def viewLeft02(): Unit \ Assert = assertTrue(Nec.viewLeft(necOf2(1, 2)) == SomeLeft(1, singleton(2)))

    @Test
    def viewLeft03(): Unit \ Assert = assertTrue(Nec.viewLeft(necOf3(1, 2, 3)) == SomeLeft(1, necOf2(2, 3)))

    /////////////////////////////////////////////////////////////////////////////
    // viewRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def viewRight01(): Unit \ Assert = assertTrue(Nec.viewRight(singleton(1)) == OneRight(1))

    @Test
    def viewRight02(): Unit \ Assert = assertTrue(Nec.viewRight(necOf2(1, 2)) == SomeRight(singleton(1), 2))

    @Test
    def viewRight03(): Unit \ Assert = assertTrue(Nec.viewRight(necOf3(1, 2, 3)) == SomeRight(necOf2(1, 2), 3))

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def memberOf01(): Unit \ Assert = assertFalse(Nec.memberOf(0, singleton(1)))

    @Test
    def memberOf02(): Unit \ Assert = assertFalse(Nec.memberOf(0, necOf2(1, 2)))

    @Test
    def memberOf03(): Unit \ Assert = assertFalse(Nec.memberOf(0, necOf3(1, 2, 3)))

    @Test
    def memberOf04(): Unit \ Assert = assertTrue(Nec.memberOf(1, necOf3(1, 2, 3)))

    @Test
    def memberOf05(): Unit \ Assert = assertTrue(Nec.memberOf(2, necOf3(1, 2, 3)))

    @Test
    def memberOf06(): Unit \ Assert = assertTrue(Nec.memberOf(3, necOf3(1, 2, 3)))

    /////////////////////////////////////////////////////////////////////////////
    // minimum                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def minimum01(): Unit \ Assert = assertEq(expected = 1, Nec.minimum(singleton(1)))

    @Test
    def minimum02(): Unit \ Assert = assertEq(expected = 1, Nec.minimum(necOf3(1, 2, 3)))

    @Test
    def minimum03(): Unit \ Assert = assertEq(expected = 0, Nec.minimum(necOf4(1, 2, 0, 3)))

    /////////////////////////////////////////////////////////////////////////////
    // minimumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def minimumBy01(): Unit \ Assert = assertEq(expected = 1, Nec.minimumBy((x, y) -> x <=> y, singleton(1)))

    @Test
    def minimumBy02(): Unit \ Assert = assertEq(expected = 1, Nec.minimumBy((x, y) -> x <=> y, necOf3(1, 2, 3)))

    @Test
    def minimumBy03(): Unit \ Assert = assertEq(expected = 0, Nec.minimumBy((x, y) -> x <=> y, necOf4(1, 2, 0, 3)))

    @Test
    def minimumBy04(): Unit \ Assert = assertEq(expected = 3, Nec.minimumBy((x, y) -> x <=> y, necOf6(9, 5, 4, 6, 7, 3)))

    @Test
    def minimumBy05(): Unit \ Assert = assertEq(expected = 2, Nec.minimumBy((x, y) -> x <=> y, necOf6(9, 5, 4, 2, 7, 10)))

    /////////////////////////////////////////////////////////////////////////////
    // maximum                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def maximum01(): Unit \ Assert = assertEq(expected = 1, Nec.maximum(singleton(1)))

    @Test
    def maximum02(): Unit \ Assert = assertEq(expected = 3, Nec.maximum(necOf3(1, 2, 3)))

    @Test
    def maximum03(): Unit \ Assert = assertEq(expected = 3, Nec.maximum(necOf4(1, 2, 0, 3)))

    /////////////////////////////////////////////////////////////////////////////
    // maximumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def maximumBy01(): Unit \ Assert = assertEq(expected = 1, Nec.maximumBy((x, y) -> x <=> y, singleton(1)))

    @Test
    def maximumBy02(): Unit \ Assert = assertEq(expected = 3, Nec.maximumBy((x, y) -> x <=> y, necOf3(1, 2, 3)))

    @Test
    def maximumBy03(): Unit \ Assert = assertEq(expected = 3, Nec.maximumBy((x, y) -> x <=> y, necOf4(1, 2, 0, 3)))

    @Test
    def maximumBy04(): Unit \ Assert = assertEq(expected = 9, Nec.maximumBy((x, y) -> x <=> y, necOf6(9, 5, 4, 6, 7, 3)))

    @Test
    def maximumBy05(): Unit \ Assert = assertEq(expected = 10, Nec.maximumBy((x, y) -> x <=> y, necOf6(3, 5, 4, 9, 7, 10)))

    /////////////////////////////////////////////////////////////////////////////
    // indexOf                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indexOf01(): Unit \ Assert = assertEq(expected = None, Nec.indexOf(0, singleton(1)))

    @Test
    def indexOf02(): Unit \ Assert = assertEq(expected = Some(0), Nec.indexOf(1, singleton(1)))

    @Test
    def indexOf03(): Unit \ Assert = assertEq(expected = None, Nec.indexOf(0, necOf2(1, 2)))

    @Test
    def indexOf04(): Unit \ Assert = assertEq(expected = Some(0), Nec.indexOf(1, necOf2(1, 2)))

    @Test
    def indexOf05(): Unit \ Assert = assertEq(expected = Some(1), Nec.indexOf(2, necOf2(1, 2)))

    @Test
    def indexOf06(): Unit \ Assert = assertEq(expected = Some(2), Nec.indexOf(3, necOf3(1, 2, 3)))

    /////////////////////////////////////////////////////////////////////////////
    // find                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def find01(): Unit \ Assert = assertEq(expected = None, Nec.find(x -> x == 0, singleton(1)))

    @Test
    def find02(): Unit \ Assert = assertEq(expected = None, Nec.find(x -> x == 0, necOf2(1, 2)))

    @Test
    def find03(): Unit \ Assert = assertEq(expected = None, Nec.find(x -> x == 0, necOf3(1, 2, 3)))

    @Test
    def find04(): Unit \ Assert = assertEq(expected = Some(1), Nec.find(x -> x == 1, necOf3(1, 2, 3)))

    @Test
    def find05(): Unit \ Assert = assertEq(expected = Some(2), Nec.find(x -> x == 2, necOf3(1, 2, 3)))

    @Test
    def find06(): Unit \ Assert = assertEq(expected = Some(3), Nec.find(x -> x == 3, necOf3(1, 2, 3)))

    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findLeft01(): Unit \ Assert = assertEq(expected = None, Nec.findLeft(x -> x == 0, singleton(1)))

    @Test
    def findLeft02(): Unit \ Assert = assertEq(expected = None, Nec.findLeft(x -> x == 0, necOf2(1, 2)))

    @Test
    def findLeft03(): Unit \ Assert = assertEq(expected = None, Nec.findLeft(x -> x == 0, necOf3(1, 2, 3)))

    @Test
    def findLeft04(): Unit \ Assert = assertEq(expected = Some(1), Nec.findLeft(x -> x == 1, necOf3(1, 2, 3)))

    @Test
    def findLeft05(): Unit \ Assert = assertEq(expected = Some(2), Nec.findLeft(x -> x == 2, necOf3(1, 2, 3)))

    @Test
    def findLeft06(): Unit \ Assert = assertEq(expected = Some(3), Nec.findLeft(x -> x == 3, necOf3(1, 2, 3)))

    /////////////////////////////////////////////////////////////////////////////
    // findRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findRight01(): Unit \ Assert = assertEq(expected = None, Nec.findRight(x -> x == 0, singleton(1)))

    @Test
    def findRight02(): Unit \ Assert = assertEq(expected = None, Nec.findRight(x -> x == 0, necOf2(1, 2)))

    @Test
    def findRight03(): Unit \ Assert = assertEq(expected = None, Nec.findRight(x -> x == 0, necOf3(1, 2, 3)))

    @Test
    def findRight04(): Unit \ Assert = assertEq(expected = Some(1), Nec.findRight(x -> x == 1, necOf3(1, 2, 3)))

    @Test
    def findRight05(): Unit \ Assert = assertEq(expected = Some(2), Nec.findRight(x -> x == 2, necOf3(1, 2, 3)))

    @Test
    def findRight06(): Unit \ Assert = assertEq(expected = Some(3), Nec.findRight(x -> x == 3, necOf3(1, 2, 3)))

    /////////////////////////////////////////////////////////////////////////////
    // range                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def range01(): Unit \ Assert = assertEq(expected = None, Nec.range(1, 0))

    @Test
    def range02(): Unit \ Assert = assertEq(expected = None, Nec.range(1, 1))

    @Test
    def range03(): Unit \ Assert = assertEq(expected = Some(singleton(1)), Nec.range(1, 2))

    @Test
    def range04(): Unit \ Assert = assertEq(expected = Some(necOf2(1, 2)), Nec.range(1, 3))

    @Test
    def range05(): Unit \ Assert = assertEq(expected = Some(necOf3(1, 2, 3)), Nec.range(1, 4))

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map01(): Unit \ Assert = assertEq(expected = singleton(2), Nec.map(x -> x + 1, singleton(1)))

    @Test
    def map02(): Unit \ Assert = assertEq(expected = necOf2(2, 3), Nec.map(x -> x + 1, necOf2(1, 2)))

    @Test
    def map03(): Unit \ Assert = assertEq(expected = necOf3(2, 3, 4), Nec.map(x -> x + 1, necOf3(1, 2, 3)))

    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapWithIndex01(): Unit \ Assert = assertEq(expected = singleton(0), Nec.mapWithIndex((i, _) -> i, singleton(21)))

    @Test
    def mapWithIndex02(): Unit \ Assert = assertEq(expected = necOf2(0, 1), Nec.mapWithIndex((i, _) -> i, necOf2(21, 42)))

    @Test
    def mapWithIndex03(): Unit \ Assert = assertEq(expected = necOf3(0, 1, 2), Nec.mapWithIndex((i, _) -> i, necOf3(21, 42, 84)))

    /////////////////////////////////////////////////////////////////////////////
    // ap                                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def ap01(): Unit \ Assert = assertEq(expected = singleton(6), Nec.ap(singleton(x -> x+1), singleton(5)))

    @Test
    def ap02(): Unit \ Assert = assertEq(expected = necOf2(1, 6), Nec.ap(singleton(x -> x+1), necOf2(0, 5)))

    @Test
    def ap03(): Unit \ Assert = assertEq(expected = necOf4(1, 5, 0, 8), Nec.ap(necOf2((x -> x+1), (x -> x*2)), necOf2(0, 4)))

    @Test
    def ap04(): Unit \ Assert = assertEq(expected = necOf6(1, 5, 9, 0, 8, 16), Nec.ap(necOf2((x -> x+1), (x -> x*2)), necOf3(0, 4, 8)))

    @Test
    def ap05(): Unit \ Assert = assertEq(expected = necOf8(1, 5, 9, 17, 0, 8, 16, 32), Nec.ap(necOf2((x -> x+1), (x -> x*2)), necOf4(0, 4, 8, 16)))

    /////////////////////////////////////////////////////////////////////////////
    // flatMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def flatMap01(): Unit \ Assert = assertEq(expected = singleton(2), Nec.flatMap(x -> singleton(x + 1), singleton(1)))

    @Test
    def flatMap02(): Unit \ Assert = assertEq(expected = necOf2(2, 3), Nec.flatMap(x -> singleton(x + 1), necOf2(1, 2)))

    @Test
    def flatMap03(): Unit \ Assert = assertEq(expected = necOf3(2, 3, 4), Nec.flatMap(x -> singleton(x + 1), necOf3(1, 2, 3)))

    @Test
    def flatMap04(): Unit \ Assert = assertEq(expected = necOf2(1, 2), Nec.flatMap(x -> necOf2(x, (x+1)), singleton(1)))

    @Test
    def flatMap05(): Unit \ Assert = assertEq(expected = necOf4(1, 2, 4, 5), Nec.flatMap(x -> necOf2(x, (x+1)), necOf2(1, 4)))

    @Test
    def flatMap06(): Unit \ Assert = assertEq(expected = necOf6(1, 2, 3, 4, 5, 6), Nec.flatMap(x -> necOf2(x, (x+1)), necOf3(1, 3, 5)))

    /////////////////////////////////////////////////////////////////////////////
    // reverse                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reverse01(): Unit \ Assert = assertEq(expected = singleton(1), Nec.reverse(singleton(1)))

    @Test
    def reverse02(): Unit \ Assert = assertEq(expected = necOf2(2, 1), Nec.reverse(necOf2(1, 2)))

    @Test
    def reverse03(): Unit \ Assert = assertEq(expected = necOf3(3, 2, 1), Nec.reverse(necOf3(1, 2, 3)))

    @Test
    def reverse04(): Unit \ Assert = assertEq(expected = necOf4(4, 3, 2, 1), Nec.reverse(necOf4(1, 2, 3, 4)))

    /////////////////////////////////////////////////////////////////////////////
    // replace                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def replace01(): Unit \ Assert = assertEq(expected = singleton(42), Nec.replace(src =  1, dst =  42, singleton(1)))

    @Test
    def replace02(): Unit \ Assert = assertEq(expected = singleton(1), Nec.replace(src =  7, dst =  42, singleton(1)))

    @Test
    def replace03(): Unit \ Assert = assertEq(expected = necOf2(1, 42), Nec.replace(src =  2, dst =  42, necOf2(1, 2)))

    @Test
    def replace04(): Unit \ Assert = assertEq(expected = necOf2(1, 2), Nec.replace(src =  7, dst =  42, necOf2(1, 2)))

    /////////////////////////////////////////////////////////////////////////////
    // permutations                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def permutations01(): Unit \ Assert = assertEq(expected = singleton(1 :: Nil), Nec.permutations(singleton(1)))

    @Test
    def permutations02(): Unit \ Assert = assertEq(expected = necOf2((1 :: 2 :: Nil), (2 :: 1 :: Nil)), Nec.permutations(necOf2(1, 2)))

    @Test
    def permutations03(): Unit \ Assert = assertEq(expected = necOf6((1 :: 2 :: 3 :: Nil),
                                                                                (1 :: 3 :: 2 :: Nil),
                                                                                (2 :: 1 :: 3 :: Nil),
                                                                                (2 :: 3 :: 1 :: Nil),
                                                                                (3 :: 1 :: 2 :: Nil),
                                                                                (3 :: 2 :: 1 :: Nil)), Nec.permutations(necOf3(1, 2, 3)))

    /////////////////////////////////////////////////////////////////////////////
    // subsequences                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def subsequences01(): Unit \ Assert = assertEq(expected = necOf2(1 :: Nil, (Nil)), Nec.subsequences(singleton(1)))

    @Test
    def subsequences02(): Unit \ Assert = assertEq(expected = necOf4((1 :: 2 :: Nil),
                                                                            (1 :: Nil),
                                                                            (2 :: Nil),
                                                                            (Nil)), Nec.subsequences(necOf2(1, 2)))

    @Test
    def subsequences03(): Unit \ Assert = assertEq(expected = necOf8((1 :: 2 :: 3 :: Nil),
                                                                            (1 :: 2 :: Nil),
                                                                            (1 :: 3 :: Nil),
                                                                            (1 :: Nil),
                                                                            (2 :: 3 :: Nil),
                                                                            (2 :: Nil),
                                                                            (3 :: Nil),
                                                                            (Nil)), Nec.subsequences(necOf3(1, 2, 3)))

    /////////////////////////////////////////////////////////////////////////////
    // intersperse                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def intersperse01(): Unit \ Assert = assertEq(expected = singleton(1), Nec.intersperse(7, singleton(1)))

    @Test
    def intersperse02(): Unit \ Assert = assertEq(expected = necOf3(1, 7, 2), Nec.intersperse(7, necOf2(1, 2)))

    @Test
    def intersperse03(): Unit \ Assert = assertEq(expected = necOf5(1, 7, 2, 7, 3), Nec.intersperse(7, necOf3(1, 2, 3)))

    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def count01(): Unit \ Assert = assertEq(expected = 1, Nec.count(x -> x == 1, singleton(1)))

    @Test
    def count02(): Unit \ Assert = assertEq(expected = 0, Nec.count(x -> x == 2, singleton(1)))

    @Test
    def count03(): Unit \ Assert = assertEq(expected = 2, Nec.count(x -> x == 2, necOf4(1, 2, 1, 2)))

    @Test
    def count04(): Unit \ Assert = assertEq(expected = 3, Nec.count(x -> x == 2, necOf4(2, 2, 1, 2)))

    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sum01(): Unit \ Assert = assertEq(expected = 1, Nec.sum(singleton(1)))

    @Test
    def sum02(): Unit \ Assert = assertEq(expected = 6, Nec.sum(necOf3(1, 2, 3)))

    @Test
    def sum03(): Unit \ Assert = assertEq(expected = 3, Nec.sum(necOf4(1, 2, 3, -3)))

    @Test
    def sum04(): Unit \ Assert = assertEq(expected = -10, Nec.sum(necOf4(-1, -2, -3, -4)))

    @Test
    def sum05(): Unit \ Assert = assertEq(expected = 0, Nec.sum(necOf2(10, -10)))

    @Test
    def sum06(): Unit \ Assert = match Nec.range(1, 101) {
        case Some(n1) => assertEq(expected = 5050, Nec.sum(n1))
        case _        => unreachable!()
    }

    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumWith01(): Unit \ Assert = assertEq(expected = 2, Nec.sumWith(x -> x + 1, singleton(1)))

    @Test
    def sumWith02(): Unit \ Assert = assertEq(expected = 9, Nec.sumWith(x -> x + 1, necOf3(1, 2, 3)))

    @Test
    def sumWith03(): Unit \ Assert = assertEq(expected = 7, Nec.sumWith(x -> x + 1, necOf4(1, 2, 3, -3)))

    @Test
    def sumWith04(): Unit \ Assert = assertEq(expected = -6, Nec.sumWith(x -> x + 1, necOf4(-1, -2, -3, -4)))

    @Test
    def sumWith05(): Unit \ Assert = assertEq(expected = 2, Nec.sumWith(x -> x + 1, necOf2(10, -10)))


    /////////////////////////////////////////////////////////////////////////////
    // flatten                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def flatten01(): Unit \ Assert = assertEq(expected = singleton(1), Nec.flatten(singleton(singleton(1))))

    @Test
    def flatten02(): Unit \ Assert = assertEq(expected = necOf2(1, 2), Nec.flatten(necOf2(singleton(1), singleton(2))))

    @Test
    def flatten03(): Unit \ Assert = assertEq(expected = necOf3(1, 2, 3), Nec.flatten(necOf3(singleton(1), singleton(2), singleton(3))))

    @Test
    def flatten04(): Unit \ Assert = assertEq(expected = necOf5(1, 2, 3, 4, 5), Nec.flatten(necOf3(singleton(1), necOf2(2, 3), necOf2(4, 5))))

    @Test
    def flatten05(): Unit \ Assert = assertEq(expected = necOf7(1, 2, 3, 4, 5, 6, 7), Nec.flatten(necOf3(necOf3(1, 2, 3), necOf2(4, 5),  necOf2(6, 7))))
    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def exists01(): Unit \ Assert = assertTrue(Nec.exists(x -> x == 1, singleton(1)))

    @Test
    def exists02(): Unit \ Assert = assertFalse(Nec.exists(x -> x == 2, singleton(1)))

    @Test
    def exists03(): Unit \ Assert = assertTrue(Nec.exists(x -> x == 1, necOf2(1, 2)))

    @Test
    def exists04(): Unit \ Assert = assertTrue(Nec.exists(x -> x == 2, necOf2(1, 2)))

    /////////////////////////////////////////////////////////////////////////////
    // forAll                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forAll01(): Unit \ Assert = assertTrue(Nec.forAll(x -> x == 1, singleton(1)))

    @Test
    def forAll02(): Unit \ Assert = assertFalse(Nec.forAll(x -> x == 2, singleton(1)))

    @Test
    def forAll03(): Unit \ Assert = assertTrue(Nec.forAll(x -> x == 1, necOf2(1, 1)))

    @Test
    def forAll04(): Unit \ Assert = assertFalse(Nec.forAll(x -> x == 1, necOf2(1, 2)))

    @Test
    def forAll05(): Unit \ Assert = assertFalse(Nec.forAll(x -> x == 1, necOf2(2, 1)))

    /////////////////////////////////////////////////////////////////////////////
    // filter                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filter01(): Unit \ Assert = assertEq(expected = 1 :: Nil, Nec.filter(x -> x > 0, singleton(1)))

    @Test
    def filter02(): Unit \ Assert = assertEq(expected = Nil, Nec.filter(x -> x > 1, singleton(1)))

    @Test
    def filter03(): Unit \ Assert = assertEq(expected = 1 :: 2 :: Nil, Nec.filter(x -> x > 0, necOf2(1, 2)))

    @Test
    def filter04(): Unit \ Assert = assertEq(expected = 2 :: Nil, Nec.filter(x -> x > 1, necOf2(1, 2)))

    /////////////////////////////////////////////////////////////////////////////
    // fold                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fold01(): Unit \ Assert = assertEq(expected = "a", Nec.fold(singleton("a")))

    @Test
    def fold02(): Unit \ Assert = assertEq(expected = (), Nec.fold(singleton(())))

    @Test
    def fold03(): Unit \ Assert = assertEq(expected = "abc", Nec.fold(necOf3("a", "b", "c")))

    @Test
    def fold04(): Unit \ Assert = assertEq(expected = ("abc", ()), Nec.fold(necOf3(("a", ()), ("b", ()), ("c", ()))))

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldLeft01(): Unit \ Assert = assertEq(expected = -1, Nec.foldLeft((acc, x) -> acc - x, 0, singleton(1)))

    @Test
    def foldLeft02(): Unit \ Assert = assertEq(expected = -3, Nec.foldLeft((acc, x) -> acc - x, 0, necOf2(1, 2)))

    @Test
    def foldLeft03(): Unit \ Assert = assertEq(expected = -6, Nec.foldLeft((acc, x) -> acc - x, 0, necOf3(1, 2, 3)))

    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRight01(): Unit \ Assert = assertEq(expected = -1, Nec.foldRight((x, acc) -> acc - x, 0, singleton(1)))

    @Test
    def foldRight02(): Unit \ Assert = assertEq(expected = -3, Nec.foldRight((x, acc) -> acc - x, 0, necOf2(1, 2)))

    @Test
    def foldRight03(): Unit \ Assert = assertEq(expected = -6, Nec.foldRight((x, acc) -> acc - x, 0, necOf3(1, 2, 3)))

    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithCont                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRightWithCont01(): Unit \ Assert = assertEq(expected = -1, Nec.foldRightWithCont((x, k) -> k() - x, 0, singleton(1)))

    @Test
    def foldRightWithCont02(): Unit \ Assert = assertEq(expected = -3, Nec.foldRightWithCont((x, k) -> k() - x, 0, necOf2(1, 2)))

    @Test
    def foldRightWithCont03(): Unit \ Assert = assertEq(expected = -6, Nec.foldRightWithCont((x, k) -> k() - x, 0, necOf3(1, 2, 3)))


    /////////////////////////////////////////////////////////////////////////////
    // foldMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldMap01(): Unit \ Assert = assertEq(expected = 6, Nec.foldMap(x -> 2 * x, necOf2(1, 2)))

    @Test
    def foldMap02(): Unit \ Assert = assertEq(expected = "b", Nec.foldMap(x -> if (x == "a") "b" else x, singleton("a")))

    @Test
    def foldMap03(): Unit \ Assert = assertEq(expected = "abb", Nec.foldMap(x -> if (x == "c") "b" else x, necOf3("a", "b", "c")))

    @Test
    def foldMap04(): Unit \ Assert = assertEq(expected = "123", Nec.foldMap(Int32.toString, necOf3(1, 2, 3)))


    /////////////////////////////////////////////////////////////////////////////
    // filterMap                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterMap01(): Unit \ Assert = assertEq(expected = Nil, Nec.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, singleton(1)))

    @Test
    def filterMap02(): Unit \ Assert = assertEq(expected = 1 :: Nil, Nec.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, singleton(2)))

    @Test
    def filterMap03(): Unit \ Assert = assertEq(expected = Nil, Nec.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, necOf2(1, 3)))

    @Test
    def filterMap04(): Unit \ Assert = assertEq(expected = 2 :: Nil, Nec.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, necOf2(1, 4)))

    @Test
    def filterMap05(): Unit \ Assert = assertEq(expected = 3 :: Nil, Nec.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, necOf2(6, -1)))

    @Test
    def filterMap07(): Unit \ Assert = assertEq(expected = 4 :: 3 :: Nil, Nec.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, necOf2(8, 6)))

    @Test
    def filterMap08(): Unit \ Assert = assertEq(expected = 0 :: 1 :: 2 :: 5 :: Nil, Nec.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, necOf7(0, 1, 2, 3, 4, 5, 10)))

    /////////////////////////////////////////////////////////////////////////////
    // findMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findMap01(): Unit \ Assert = assertEq(expected = None, Nec.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, singleton(1)))

    @Test
    def findMap02(): Unit \ Assert = assertEq(expected = Some(1), Nec.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, singleton(2)))

    @Test
    def findMap03(): Unit \ Assert = assertEq(expected = None, Nec.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, necOf2(1, 3)))

    @Test
    def findMap04(): Unit \ Assert = assertEq(expected = Some(2), Nec.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, necOf2(1, 4)))

    @Test
    def findMap05(): Unit \ Assert = assertEq(expected = Some(3), Nec.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, necOf2(6, -1)))

    @Test
    def findMap06(): Unit \ Assert = assertEq(expected = Some(4), Nec.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, necOf2(8, 6)))

    @Test
    def findMap07(): Unit \ Assert = assertEq(expected = Some(0), Nec.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, necOf7(0, 1, 2, 3, 4, 5, 10)))

    /////////////////////////////////////////////////////////////////////////////
    // zip                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zip01(): Unit \ Assert = assertEq(expected = singleton((1, 1)), Nec.zip(singleton(1), singleton(1)))

    @Test
    def zip02(): Unit \ Assert = assertEq(expected = singleton((1, 1)), Nec.zip(necOf2(1, 2), singleton(1)))

    @Test
    def zip03(): Unit \ Assert = assertEq(expected = singleton((1, 1)), Nec.zip(singleton(1), necOf2(1, 2)))

    @Test
    def zip04(): Unit \ Assert = assertEq(expected = necOf2((1, 1), (2, 2)), Nec.zip(necOf2(1, 2), necOf2(1, 2)))

    /////////////////////////////////////////////////////////////////////////////
    // zipWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWith01(): Unit \ Assert = assertEq(expected = singleton(2), Nec.zipWith((x, y) -> x + y, singleton(1), singleton(1)))

    @Test
    def zipWith02(): Unit \ Assert = assertEq(expected = singleton(2), Nec.zipWith((x, y) -> x + y, necOf2(1, 2), singleton(1)))

    @Test
    def zipWith03(): Unit \ Assert = assertEq(expected = singleton(2), Nec.zipWith((x, y) -> x + y, singleton(1), necOf2(1, 2)))

    @Test
    def zipWith04(): Unit \ Assert = assertEq(expected = necOf2(2, 4), Nec.zipWith((x, y) -> x + y, necOf2(1, 2), necOf2(1, 2)))

    /////////////////////////////////////////////////////////////////////////////
    // unzip                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unzip01(): Unit \ Assert = assertEq(expected = (singleton(1), singleton(1)), Nec.unzip(singleton((1, 1))))

    @Test
    def unzip02(): Unit \ Assert = assertEq(expected = (necOf2(1, 2), necOf2(1, 2)), Nec.unzip(necOf2((1, 1), (2, 2))))

    @Test
    def unzip03(): Unit \ Assert = assertEq(expected = (necOf3(1, 2, 3), necOf3(1, 2, 3)), Nec.unzip(necOf3((1, 1), (2, 2), (3, 3))))

    /////////////////////////////////////////////////////////////////////////////
    // zipWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWithIndex01(): Unit \ Assert = assertEq(expected = singleton((0, 1)), Nec.zipWithIndex(singleton(1)))

    @Test
    def zipWithIndex02(): Unit \ Assert = assertEq(expected = necOf2((0, 1), (1, 2)), Nec.zipWithIndex(necOf2(1, 2)))

    @Test
    def zipWithIndex03(): Unit \ Assert = assertEq(expected = necOf2((0, 2), (1, 1)), Nec.zipWithIndex(necOf2(2, 1)))

    @Test
    def zipWithIndex04(): Unit \ Assert = assertEq(expected = necOf3((0, 4), (1, 3), (2, 6)), Nec.zipWithIndex(necOf3(4, 3, 6)))

    @Test
    def zipWithIndex05(): Unit \ Assert = assertEq(expected = necOf4((0, 1), (1, 2), (2, 3), (3, 4)), Nec.zipWithIndex(necOf4(1, 2, 3, 4)))

    @Test
    def zipWithIndex06(): Unit \ Assert = assertEq(expected = necOf8((0, 8), (1, 7), (2, 6), (3, 5), (4, 4), (5, 3), (6, 2), (7, 1)), Nec.zipWithIndex(necOf8(8, 7, 6, 5, 4, 3, 2, 1)))

    /////////////////////////////////////////////////////////////////////////////
    // zipWithA                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWithA01(): Unit \ Assert = assertEq(expected = Identity.Identity(singleton(2)), Nec.zipWithA((x, y) -> Identity.Identity(x + y), singleton(1), singleton(1)))

    @Test
    def zipWithA02(): Unit \ Assert = assertEq(expected = Identity.Identity(singleton(2)), Nec.zipWithA((x, y) -> Identity.Identity(x + y), necOf2(1, 2), singleton(1)))

    @Test
    def zipWithA03(): Unit \ Assert = assertEq(expected = Identity.Identity(singleton(2)), Nec.zipWithA((x, y) -> Identity.Identity(x + y), singleton(1), necOf2(1, 2)))

    @Test
    def zipWithA04(): Unit \ Assert = assertEq(expected = Identity.Identity(necOf2(2, 4)), Nec.zipWithA((x, y) -> Identity.Identity(x + y), necOf2(1, 2), necOf2(1, 2)))

    /////////////////////////////////////////////////////////////////////////////
    // mapAccumLeft                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapAccumLeft01(): Unit \ Assert = assertEq(expected = (11, singleton(11)), Nec.mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, singleton(1)))

    @Test
    def mapAccumLeft02(): Unit \ Assert = assertEq(expected = (12, necOf2(11, 13)), Nec.mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, necOf2(1,2)))

    @Test
    def mapAccumLeft03(): Unit \ Assert = assertEq(expected = (13, necOf3(11, 13, 15)), Nec.mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, necOf3(1, 2, 3)))

    /////////////////////////////////////////////////////////////////////////////
    // mapAccumRight                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapAccumRight01(): Unit \ Assert = assertEq(expected = (11, singleton(11)), Nec.mapAccumRight((acc, x) -> (acc+1, x+acc), 10, singleton(1)))

    @Test
    def mapAccumRight02(): Unit \ Assert = assertEq(expected = (12, necOf2(12, 12)), Nec.mapAccumRight((acc, x) -> (acc+1, x+acc), 10, necOf2(1, 2)))

    @Test
    def mapAccumRight03(): Unit \ Assert = assertEq(expected = (13, necOf3(13, 13, 13)), Nec.mapAccumRight((acc, x) -> (acc+1, x+acc), 10, necOf3(1, 2, 3)))

    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEach01(): Unit \ Assert = region rc {
        let ri = Ref.fresh(rc, 21);
        Nec.forEach(x -> Ref.put(x, ri), singleton(1));
        assertEq(expected = 1, Ref.get(ri))
    }

    @Test
    def forEach02(): Unit \ Assert = region rc {
        let ri = Ref.fresh(rc, 21);
        Nec.forEach(x -> Ref.put(x, ri), necOf3(1, 2, 3));
        assertEq(expected = 3, Ref.get(ri))
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEachWithIndex                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEachWithIndex01(): Unit \ Assert = region rc {
        let ri = Ref.fresh(rc, 21);
        Nec.forEachWithIndex((i, _) -> Ref.put(i, ri), singleton(1));
        assertEq(expected = 0, Ref.get(ri))
    }

    @Test
    def forEachWithIndex02(): Unit \ Assert = region rc {
        let ri = Ref.fresh(rc, 21);
        Nec.forEachWithIndex((i, _) -> Ref.put(i, ri), necOf3(1, 2, 3));
        assertEq(expected = 2, Ref.get(ri))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toList01(): Unit \ Assert = assertEq(expected = 1 :: Nil, Nec.toList(singleton(1)))

    @Test
    def toList02(): Unit \ Assert = assertEq(expected = 1 :: 2 :: Nil, Nec.toList(necOf2(1, 2)))

    @Test
    def toList03(): Unit \ Assert = assertEq(expected = 1 :: 2 :: 3 :: Nil, Nec.toList(necOf3(1, 2, 3)))


    /////////////////////////////////////////////////////////////////////////////
    // toMutDeque                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMutDeque01(): Unit \ Assert = region rc {
        let c = singleton(1);
        let d1 = Nec.toMutDeque(rc, c);

        let d2 = MutDeque.empty(rc);
        MutDeque.pushBack(1, d2);

        assertTrue(d1 `MutDeque.sameElements` d2)
    }

    @Test
    def toMutDeque02(): Unit \ Assert = region rc {
        let c = necOf2(1, 2);
        let d1 = Nec.toMutDeque(rc, c);

        let d2 = MutDeque.empty(rc);
        MutDeque.pushFront(1, d2);
        MutDeque.pushBack(2, d2);

        assertTrue(d1 `MutDeque.sameElements` d2)
    }

    @Test
    def toMutDeque03(): Unit \ Assert = region rc {
        let c = necOf3('a', 'b', 'c');
        let d1 = Nec.toMutDeque(rc, c);

        let d2 = MutDeque.empty(rc);
        MutDeque.pushFront('b', d2);
        MutDeque.pushBack('c', d2);
        MutDeque.pushFront('a', d2);

        assertTrue(d1 `MutDeque.sameElements` d2)
    }


    /////////////////////////////////////////////////////////////////////////////
    // toMutList                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMutList01(): Unit \ Assert = region rc {
        let m1 = Nec.toMutList(rc, singleton(1));
        let m2 = MutList.range(rc, 1, 2);
        assertTrue(MutList.sameElements(m1, m2))
    }

    @Test
    def toMutList02(): Unit \ Assert = region rc {
        let m1 = Nec.toMutList(rc, necOf2(1, 2));
        let m2 = MutList.range(rc, 1, 3);
        assertTrue(MutList.sameElements(m1, m2))
    }

    @Test
    def toMutList03(): Unit \ Assert = region rc {
        let m1 = Nec.toMutList(rc, necOf3(1, 2, 3));
        let m2 = MutList.range(rc, 1, 4);
        assertTrue(MutList.sameElements(m1, m2))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toSet01(): Unit \ Assert = assertEq(expected = Set#{1}, Nec.toSet(singleton(1)))

    @Test
    def toSet02(): Unit \ Assert = assertEq(expected = Set#{1, 2}, Nec.toSet(necOf2(1, 2)))

    @Test
    def toSet03(): Unit \ Assert = assertEq(expected = Set#{1}, Nec.toSet(necOf2(1, 1)))

    @Test
    def toSet04(): Unit \ Assert = assertEq(expected = Set#{1, 2, 3}, Nec.toSet(necOf3(1, 2, 3)))

    @Test
    def toSet05(): Unit \ Assert = assertEq(expected = Set#{1, 2}, Nec.toSet(necOf3(1, 2, 1)))

    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMap01(): Unit \ Assert = assertEq(expected = Map#{1 => 0, 3 => 0, 6 => 0, 7 => 0}, Nec.toMap(necOf4((7, 0), (1, 0), (3, 0), (6, 0))))

    @Test
    def toMap02(): Unit \ Assert = assertEq(expected = Map#{1 => 2, 3 => 6, 6 => 12, 7 => 14}, Nec.toMap(necOf4((7, 14), (1, 2), (3, 6), (6, 12))))

    @Test
    def toMap03(): Unit \ Assert = assertEq(expected = Map#{1 => 11, 3 => 13, 6 => 16, 7 => 17}, Nec.toMap(necOf4((7, 17), (1, 11), (3, 13), (6, 16))))

    @Test
    def toMap04(): Unit \ Assert = assertEq(expected = Map#{1 => 11, 3 => 13, 6 => 16, 7 => 17}, Nec.toMap(necOf4((7, 17), (3, 13), (6, 16), (1, 11))))

    /////////////////////////////////////////////////////////////////////////////
    // toMapWith                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMapWith01(): Unit \ Assert = assertEq(expected = Map#{1 => 0, 3 => 0, 6 => 0, 7 => 0}, Nec.toMapWith(x -> 0 * x, necOf4(7, 1, 3, 6)))

    @Test
    def toMapWith02(): Unit \ Assert = assertEq(expected = Map#{1 => 2, 3 => 6, 6 => 12, 7 => 14}, Nec.toMapWith(x -> 2 * x, necOf4(7, 1, 3, 6)))

    @Test
    def toMapWith03(): Unit \ Assert = assertEq(expected = Map#{1 => 11, 3 => 13, 6 => 16, 7 => 17}, Nec.toMapWith(x -> x + 10, necOf4(7, 1, 3, 6)))

    @Test
    def toMapWith04(): Unit \ Assert = assertEq(expected = Map#{1 => 11, 3 => 13, 6 => 16, 7 => 17}, Nec.toMapWith(x -> x + 10, necOf4(7, 3, 6, 1)))

    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toArray01(): Unit \ Assert = region rc {
        let a = Nec.toArray(rc, singleton(1));
        assertTrue(Array.sameElements(a, Array#{1} @ rc))
    }

    @Test
    def toArray02(): Unit \ Assert = region rc {
        let a = Nec.toArray(rc, necOf2(1, 2));
        assertTrue(Array.sameElements(a, Array#{1, 2} @ rc))
    }

    @Test
    def toArray03(): Unit \ Assert = region rc {
        let a = Nec.toArray(rc, necOf3(1, 2, 3));
        assertTrue(Array.sameElements(a, Array#{1, 2, 3} @ rc))
    }

    @Test
    def toArray04(): Unit \ Assert = region rc {
        let a = Nec.toArray(rc, necOf3(3, 2, 3));
        assertTrue(Array.sameElements(a, Array#{3, 2, 3} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toVector                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toVector01(): Unit \ Assert =
        let c = singleton(1);
        assertEq(expected = Vector#{1}, Nec.toVector(c))

    @Test
    def toVector02(): Unit \ Assert =
        let c = necOf2(1, 2);
        assertEq(expected = Vector#{1, 2}, Nec.toVector(c))

    @Test
    def toVector03(): Unit \ Assert =
        let c = necOf3(1, 2, 3);
        assertEq(expected = Vector#{1, 2, 3}, Nec.toVector(c))

    @Test
    def toVector04(): Unit \ Assert =
        let c = necOf3(3, 2, 3);
        assertEq(expected = Vector#{3, 2, 3}, Nec.toVector(c))

    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iterator01(): Unit \ Assert = region rc {
        assertEq(expected = Some(necOf3(1, 2, 3)), necOf3(1, 2, 3) |> Nec.iterator(rc) |> Iterator.toNec)
    }

    @Test
    def iterator02(): Unit \ Assert = region rc {
        match Nec.range(-100, 100) {
            case Some(nec) => assertEq(expected = List.range(-100, 100), nec |> Nec.iterator(rc) |> Iterator.toList)
            case None      => unreachable!()
        }
    }

    /////////////////////////////////////////////////////////////////////////////
    // The following tests for sort functions correspond to those in TestList  //
    /////////////////////////////////////////////////////////////////////////////
    // sortWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    def cmp(x: Int32, y: Int32): Comparison =
        if (x < y) Comparison.LessThan
        else if (x == y) Comparison.EqualTo
        else Comparison.GreaterThan

    @Test
    def sortWith02(): Unit \ Assert = assertEq(expected = singleton(0), Nec.sortWith(cmp, singleton(0)))

    @Test
    def sortWith03(): Unit \ Assert = assertEq(expected = necOf2(0, 1), Nec.sortWith(cmp, necOf2(0, 1)))

    @Test
    def sortWith04(): Unit \ Assert = assertEq(expected = necOf2(0, 1), Nec.sortWith(cmp, necOf2(1, 0)))

    @Test
    def sortWith05(): Unit \ Assert = assertEq(expected = necOf2(1, 1), Nec.sortWith(cmp, necOf2(1, 1)))

    @Test
    def sortWith06(): Unit \ Assert = assertEq(expected = necOf6(0, 1, 2, 3, 4, 5), Nec.sortWith(cmp, necOf6(0, 1, 2, 3, 4, 5)))

    @Test
    def sortWith07(): Unit \ Assert = assertEq(expected = necOf6(0, 1, 2, 3, 4, 5), Nec.sortWith(cmp, necOf6(5, 4, 3, 2, 1, 0)))

    @Test
    def sortWith08(): Unit \ Assert = assertEq(expected = necOf6(0, 1, 2, 3, 4, 5), Nec.sortWith(cmp, necOf6(5, 3, 0, 4, 1, 2)))

    @Test
    def sortWith09(): Unit \ Assert = assertEq(expected = necOf6(0, 1, 2, 2, 3, 4), Nec.sortWith(cmp, necOf6(2, 3, 0, 4, 1, 2)))

    @Test
    def sortWith10(): Unit \ Assert = assertEq(expected = necOf6(5, 4, 3, 2, 1, 0), Nec.sortWith(flip(cmp), necOf6(0, 1, 2, 3, 4, 5)))

    @Test
    def sortWith11(): Unit \ Assert = assertEq(expected = necOf6(5, 4, 3, 2, 1, 0), Nec.sortWith(flip(cmp), necOf6(5, 4, 3, 2, 1, 0)))

    @Test
    def sortWith12(): Unit \ Assert = assertEq(expected = necOf6(5, 4, 3, 2, 1, 0), Nec.sortWith(flip(cmp), necOf6(5, 3, 0, 4, 1, 2)))

    @Test
    def sortWith13(): Unit \ Assert = assertEq(expected = necOf6(4, 3, 2, 2, 1, 0), Nec.sortWith(flip(cmp), necOf6(2, 3, 0, 4, 1, 2)))

    /////////////////////////////////////////////////////////////////////////////
    // sort                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    def testSortVsSortWith(xs: Nec[Int32]) : Bool =
        Nec.sort(xs) == Nec.sortWith(cmp, xs)

    @Test
    def sort02(): Unit \ Assert = assertTrue(testSortVsSortWith(singleton(0)))

    @Test
    def sort03(): Unit \ Assert = assertTrue(testSortVsSortWith(necOf2(0, 1)))

    @Test
    def sort04(): Unit \ Assert = assertTrue(testSortVsSortWith(necOf2(1, 0)))

    @Test
    def sort05(): Unit \ Assert = assertTrue(testSortVsSortWith(necOf2(1, 1)))

    @Test
    def sort06(): Unit \ Assert = assertTrue(testSortVsSortWith(necOf6(0, 1, 2, 3, 4, 5)))

    @Test
    def sort07(): Unit \ Assert = assertTrue(testSortVsSortWith(necOf6(5, 4, 3, 2, 1, 0)))

    @Test
    def sort08(): Unit \ Assert = assertTrue(testSortVsSortWith(necOf6(5, 3, 0, 4, 1, 2)))

    @Test
    def sort09(): Unit \ Assert = assertTrue(testSortVsSortWith(necOf6(2, 3, 0, 4, 1, 2)))

    /////////////////////////////////////////////////////////////////////////////
    // sortBy                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    def testSortByVsSort(a: Nec[Int32]) : Bool =
        (Nec.sortBy(identity, a) == Nec.sort(a)) and
        (Nec.sortBy(x -> 4*x+7, a) == Nec.sort(a)) and
        (Nec.sortBy(x -> -x, a) == Nec.sortWith(flip(cmp),a))

    @Test
    def sortBy02(): Unit \ Assert = assertTrue(testSortByVsSort(singleton(0)))

    @Test
    def sortBy03(): Unit \ Assert = assertTrue(testSortByVsSort(necOf2(0, 1)))

    @Test
    def sortBy04(): Unit \ Assert = assertTrue(testSortByVsSort(necOf2(1, 0)))

    @Test
    def sortBy05(): Unit \ Assert = assertTrue(testSortByVsSort(necOf2(1, 1)))

    @Test
    def sortBy06(): Unit \ Assert = assertTrue(testSortByVsSort(necOf6(0, 1, 2, 3, 4, 5)))

    @Test
    def sortBy07(): Unit \ Assert = assertTrue(testSortByVsSort(necOf6(5, 4, 3, 2, 1, 0)))

    @Test
    def sortBy08(): Unit \ Assert = assertTrue(testSortByVsSort(necOf6(5, 3, 0, 4, 1, 2)))

    @Test
    def sortBy09(): Unit \ Assert = assertTrue(testSortByVsSort(necOf6(2, 3, 0, 4, 1, 2)))

    /////////////////////////////////////////////////////////////////////////////
    // sequence                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sequence01(): Unit \ Assert =
        let xs: Nec[Option[Int32]] = singleton(None);
        assertEq(expected = None, Nec.sequence(xs))

    @Test
    def sequence02(): Unit \ Assert = assertEq(expected = Some(singleton(1)), Nec.sequence(singleton(Some(1))))

    @Test
    def sequence03(): Unit \ Assert = assertEq(expected = Some(necOf2(1, 2)), Nec.sequence(necOf2(Some(1), Some(2))))

    @Test
    def sequence04(): Unit \ Assert = assertEq(expected = Some(necOf3(1, 2, 3)), Nec.sequence(necOf3(Some(1), Some(2), Some(3))))

    @Test
    def sequence05(): Unit \ Assert =
        let l = Nec.singleton(Identity.Identity(1));
        assertEq(expected = Identity.Identity(Nec.singleton(1)), Nec.sequence(l))

    @Test
    def sequence06(): Unit \ Assert =
        let l = necOf2(Identity.Identity(1), Identity.Identity(2));
        assertEq(expected = Identity.Identity(necOf2(1, 2)), Nec.sequence(l))

    @Test
    def sequence07(): Unit \ Assert =
        let l = necOf3(Identity.Identity(1), Identity.Identity(2), Identity.Identity(3));
        assertEq(expected = Identity.Identity(necOf3(1, 2, 3)), Nec.sequence(l))

    /////////////////////////////////////////////////////////////////////////////
    // traverse                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def traverse01(): Unit \ Assert = assertEq(expected = Some(singleton(2)), Nec.traverse(x -> Some(x+1), singleton(1)))

    @Test
    def traverse02(): Unit \ Assert = assertEq(expected = Some(necOf2(2, 3)), Nec.traverse(x -> Some(x+1), necOf2(1, 2)))

    @Test
    def traverse03(): Unit \ Assert = assertEq(expected = None, Nec.traverse(_ -> (None: Option[Int32]), singleton(1)))

    @Test
    def traverse04(): Unit \ Assert = assertEq(expected = None, Nec.traverse(_ -> (None: Option[Int32]), necOf2(1, 2)))

    @Test
    def traverse05(): Unit \ Assert = region rc {
        let st = Ref.fresh(rc, 0);
        let l = Nec.singleton(1);
        let ans = Nec.traverse(x -> {Ref.put(x, st); Identity.Identity(x)}, l);
        assertEq(expected = Identity.Identity(Nec.singleton(1)), ans);
        assertEq(expected = 1, Ref.get(st))
    }

    @Test
    def traverse06(): Unit \ Assert = region rc {
        let st = Ref.fresh(rc, 0);
        let l = necOf2(1, 2);
        let ans = Nec.traverse(x -> {Ref.put(x, st); Identity.Identity(x)}, l);
        assertEq(expected = Identity.Identity(necOf2(1, 2)), ans);
        assertEq(expected = 2, Ref.get(st))
    }

    @Test
    def traverse07(): Unit \ Assert = region rc {
        let st = Ref.fresh(rc, 0);
        let l = necOf3(1, 2, 3);
        let ans = Nec.traverse(x -> {Ref.put(x, st); Identity.Identity(x)}, l);
        assertEq(expected = Identity.Identity(necOf3(1, 2, 3)), ans);
        assertEq(expected = 3, Ref.get(st))
    }

    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def join01(): Unit \ Assert = assertEq(expected = "1", Nec.join(",", singleton(1)))

    @Test
    def join02(): Unit \ Assert = assertEq(expected = "1,2,3", Nec.join(",", necOf3(1, 2, 3)))

    @Test
    def join03(): Unit \ Assert = assertEq(expected = "1,2,3", Nec.join(",", necOf3("1", "2", "3")))

    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def joinWith01(): Unit \ Assert = assertEq(expected = "2", Nec.joinWith(x -> "${x + 1}", ",", singleton(1)))

    @Test
    def joinWith02(): Unit \ Assert = assertEq(expected = "2,3,4", Nec.joinWith(x -> "${x + 1}", ",", necOf3(1, 2, 3)))

    @Test
    def joinWith03(): Unit \ Assert = assertEq(expected = "11,22,33", Nec.joinWith(x -> x + x, ",", necOf3("1", "2", "3")))

    /////////////////////////////////////////////////////////////////////////////
    // reduce                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduce01(): Unit \ Assert = assertEq(expected = "a", Nec.reduce(singleton("a")))

    @Test
    def reduce02(): Unit \ Assert = assertEq(expected = (), Nec.reduce(singleton(())))

    @Test
    def reduce03(): Unit \ Assert = assertEq(expected = "abc", Nec.reduce(necOf3("a", "b", "c")))

    /////////////////////////////////////////////////////////////////////////////
    // reduceLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceLeft01(): Unit \ Assert = assertEq(expected = 1, Nec.reduceLeft((acc, x) -> acc - x, singleton(1)))

    @Test
    def reduceLeft02(): Unit \ Assert = assertEq(expected = -1, Nec.reduceLeft((acc, x) -> acc - x, necOf2(1, 2)))

    @Test
    def reduceLeft03(): Unit \ Assert = assertEq(expected = -4, Nec.reduceLeft((acc, x) -> acc - x, necOf3(1, 2, 3)))

    /////////////////////////////////////////////////////////////////////////////
    // reduceRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceRight01(): Unit \ Assert = assertEq(expected = 1, Nec.reduceRight((x, acc) -> acc - x, singleton(1)))

    @Test
    def reduceRight02(): Unit \ Assert = assertEq(expected = 1, Nec.reduceRight((x, acc) -> acc - x, necOf2(1, 2)))

    @Test
    def reduceRight03(): Unit \ Assert = assertEq(expected = 0, Nec.reduceRight((x, acc) -> acc - x, necOf3(1, 2, 3)))

    /////////////////////////////////////////////////////////////////////////////
    // reduceLeftTo                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceLeftTo01(): Unit \ Assert = assertEq(expected = 1, Nec.reduceLeftTo((acc, x) -> acc - x, identity, singleton(1)))

    @Test
    def reduceLeftTo02(): Unit \ Assert = assertEq(expected = -1, Nec.reduceLeftTo((acc, x) -> acc - x, identity, necOf2(1, 2)))

    @Test
    def reduceLeftTo03(): Unit \ Assert = assertEq(expected = -4, Nec.reduceLeftTo((acc, x) -> acc - x, identity, necOf3(1, 2, 3)))

    /////////////////////////////////////////////////////////////////////////////
    // reduceRightTo                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceRightTo01(): Unit \ Assert = assertEq(expected = 1, Nec.reduceRightTo((x, acc) -> acc - x, identity, singleton(1)))

    @Test
    def reduceRightTo02(): Unit \ Assert = assertEq(expected = 1, Nec.reduceRightTo((x, acc) -> acc - x, identity, necOf2(1, 2)))

    @Test
    def reduceRightTo03(): Unit \ Assert = assertEq(expected = 0, Nec.reduceRightTo((x, acc) -> acc - x, identity, necOf3(1, 2, 3)))

    /////////////////////////////////////////////////////////////////////////////
    // hash                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def hash01(): Unit \ Assert = assertEq(expected = hash(singleton(1)), hash(singleton(1)))

    @Test
    def hash02(): Unit \ Assert = assertEq(expected = hash(necOf3(1, 2, 3)), hash(necOf3(1, 2, 3)))

    @Test
    def hash03(): Unit \ Assert = assertNeq(unexpected = hash(necOf3(1, 3, 2)), hash(necOf3(1, 2, 3)))

    @Test
    def hash04(): Unit \ Assert = assertNeq(unexpected = hash(singleton(1)), hash(singleton(2)))

    @Test
    def hash05(): Unit \ Assert = assertNeq(unexpected = hash(necOf3("c", "b", "a")), hash(necOf3("c", "a", "b")))

    /////////////////////////////////////////////////////////////////////////////
    // dropWhileLeft                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dropWhileLeft01(): Unit \ Assert = assertEq(expected = 1 :: Nil, Nec.dropWhileLeft(i -> i > 3, singleton(1)))

    @Test
    def dropWhileLeft02(): Unit \ Assert = assertEq(expected = Nil, Nec.dropWhileLeft(i -> i > 3, singleton(4)))

    @Test
    def dropWhileLeft03(): Unit \ Assert = assertEq(expected = 1 :: 2 :: Nil, Nec.dropWhileLeft(i -> i > 3, necOf2(1, 2)))

    @Test
    def dropWhileLeft04(): Unit \ Assert = assertEq(expected = 1 :: 5 :: Nil, Nec.dropWhileLeft(i -> i > 3, necOf2(1, 5)))

    @Test
    def dropWhileLeft05(): Unit \ Assert = assertEq(expected = 1 :: Nil, Nec.dropWhileLeft(i -> i > 3, necOf2(5, 1)))

    @Test
    def dropWhileLeft06(): Unit \ Assert = assertEq(expected = Nil, Nec.dropWhileLeft(i -> i > 3, necOf2(5, 8)))

    @Test
    def dropWhileLeft07(): Unit \ Assert = assertEq(expected = -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: Nil, Nec.dropWhileLeft(i -> i > 3, necOf8(4, 6, -3, 11, -5, 1, 2, 16)))

    /////////////////////////////////////////////////////////////////////////////
    // dropWhileRight                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dropWhileRight01(): Unit \ Assert = assertEq(expected = 1 :: Nil, Nec.dropWhileRight(i -> i > 3, singleton(1)))

    @Test
    def dropWhileRight02(): Unit \ Assert = assertEq(expected = Nil, Nec.dropWhileRight(i -> i > 3, singleton(4)))

    @Test
    def dropWhileRight03(): Unit \ Assert = assertEq(expected = 1 :: 2 :: Nil, Nec.dropWhileRight(i -> i > 3, necOf2(1, 2)))

    @Test
    def dropWhileRight04(): Unit \ Assert = assertEq(expected = 1 :: Nil, Nec.dropWhileRight(i -> i > 3, necOf2(1, 5)))

    @Test
    def dropWhileRight05(): Unit \ Assert = assertEq(expected = 5 :: 1 :: Nil, Nec.dropWhileRight(i -> i > 3, necOf2(5, 1)))

    @Test
    def dropWhileRight06(): Unit \ Assert = assertEq(expected = Nil, Nec.dropWhileRight(i -> i > 3, necOf2(5, 8)))

    @Test
    def dropWhileRight07(): Unit \ Assert = assertEq(expected = 4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: Nil, Nec.dropWhileRight(i -> i > 3, necOf8(4, 6, -3, 11, -5, 1, 2, 16)))

    /////////////////////////////////////////////////////////////////////////////
    // takeWhileLeft                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def takeWhileLeft01(): Unit \ Assert = assertEq(expected = Nil, Nec.takeWhileLeft(i -> i > 3, singleton(1)))

    @Test
    def takeWhileLeft02(): Unit \ Assert = assertEq(expected = 4 :: Nil, Nec.takeWhileLeft(i -> i > 3, singleton(4)))

    @Test
    def takeWhileLeft03(): Unit \ Assert = assertEq(expected = Nil, Nec.takeWhileLeft(i -> i > 3, necOf2(1, 2)))

    @Test
    def takeWhileLeft04(): Unit \ Assert = assertEq(expected = Nil, Nec.takeWhileLeft(i -> i > 3, necOf2(1, 5)))

    @Test
    def takeWhileLeft05(): Unit \ Assert = assertEq(expected = 5 :: Nil, Nec.takeWhileLeft(i -> i > 3, necOf2(5, 1)))

    @Test
    def takeWhileLeft06(): Unit \ Assert = assertEq(expected = 5 :: 8 :: Nil, Nec.takeWhileLeft(i -> i > 3, necOf2(5, 8)))

    @Test
    def takeWhileLeft08(): Unit \ Assert = assertEq(expected = 4 :: 6 :: Nil, Nec.takeWhileLeft(i -> i > 3, necOf8(4, 6, -3, 11, -5, 1, 2, 16)))

    /////////////////////////////////////////////////////////////////////////////
    // takeWhileRight                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def takeWhileRight01(): Unit \ Assert = assertEq(expected = Nil, Nec.takeWhileRight(i -> i > 3, singleton(1)))

    @Test
    def takeWhileRight02(): Unit \ Assert = assertEq(expected = 4 :: Nil, Nec.takeWhileRight(i -> i > 3, singleton(4)))

    @Test
    def takeWhileRight03(): Unit \ Assert = assertEq(expected = Nil, Nec.takeWhileRight(i -> i > 3, necOf2(1, 2)))

    @Test
    def takeWhileRight04(): Unit \ Assert = assertEq(expected = 5 :: Nil, Nec.takeWhileRight(i -> i > 3, necOf2(1, 5)))

    @Test
    def takeWhileRight05(): Unit \ Assert = assertEq(expected = Nil, Nec.takeWhileRight(i -> i > 3, necOf2(5, 1)))

    @Test
    def takeWhileRight06(): Unit \ Assert = assertEq(expected = 5 :: 8 :: Nil, Nec.takeWhileRight(i -> i > 3, necOf2(5, 8)))

    @Test
    def takeWhileRight07(): Unit \ Assert = assertEq(expected = 16 :: Nil, Nec.takeWhileRight(i -> i > 3, necOf8(4, 6, -3, 11, -5, 1, 2, 16)))

    /////////////////////////////////////////////////////////////////////////////
    // shuffle                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def shuffle01(): Unit \ Assert + NonDet =
        let c = Nec.singleton(1) |> Nec.shuffle;
        match c {
            case Some(cc) => assertEq(expected = 1, Nec.length(cc)); assertEq(expected = Set#{1}, Nec.toSet(cc))
            case _       => fail("shuffle returned None")
        }

    @Test
    def shuffle02(): Unit \ Assert + NonDet =
        let c = necOf3(1, 2, 3) |> Nec.shuffle;
        match c {
            case Some(cc) => assertEq(expected = 3, Nec.length(cc)); assertEq(expected = Set#{1, 2, 3}, Nec.toSet(cc))
            case _       => fail("shuffle returned None")
        }

    @Test
    def shuffle03(): Unit \ Assert + NonDet =
        let c = necOf8(1, 2, 3, 4, 5, 6, 7, 8) |> Nec.shuffle;
        match c {
            case Some(cc) => assertEq(expected = 8, Nec.length(cc)); assertEq(expected = Set#{1, 2, 3, 4, 5, 6, 7, 8}, Nec.toSet(cc))
            case _       => fail("shuffle returned None")
        }

}
