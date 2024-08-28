/*
 *  Copyright 2022 Stephen Tetley
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

mod TestNec {

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

    @test
    def equals01(): Bool = singleton(1) == singleton(1)

    @test
    def equals02(): Bool = necOf2(1, 2) == cons(1, singleton(2))

    @test
    def equals03(): Bool = necOf3(1, 2, 3) == snoc(cons(1, singleton(2)), 3)

    @test
    def equals04(): Bool = necOf4(1, 2, 3, 4) == snoc(cons(1, cons(2, singleton(3))), 4)

    @test
    def equals05(): Bool = necOf5(1, 2, 3, 4, 5) == snoc(snoc(cons(1, cons(2, singleton(3))), 4), 5)

    @test
    def equals06(): Bool = cons(1, necOf5(2, 3, 4, 5, 6)) == snoc(necOf5(1, 2, 3, 4, 5), 6)

    /////////////////////////////////////////////////////////////////////////////
    // not equals (!=)                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def notEquals01(): Bool = singleton(1) != singleton(2)

    @test
    def notEquals02(): Bool = necOf2(1, 1) != necOf2(1, 2)

    @test
    def notEquals03(): Bool = necOf3(1, 2, 3) != necOf2(1, 2)

    @test
    def notEquals04(): Bool = necOf3(1, 2, 3) != necOf3(1, 2, 2)

    /////////////////////////////////////////////////////////////////////////////
    // compare                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testCompare01(): Bool = Order.compare(singleton(1), singleton(1)) == Comparison.EqualTo

    @test
    def testCompare02(): Bool = Order.compare(singleton(1), singleton(2)) == Comparison.LessThan

    @test
    def testCompare03(): Bool = Order.compare(singleton(2), singleton(1)) == Comparison.GreaterThan

    @test
    def testCompare04(): Bool = Order.compare(singleton(1), necOf2(1, 2)) == Comparison.LessThan

    @test
    def testCompare05(): Bool = Order.compare(necOf2(1, 2), singleton(1)) == Comparison.GreaterThan

    @test
    def testCompare06(): Bool = Order.compare(singleton(1), necOf2(0,0)) == Comparison.GreaterThan

    @test
    def testCompare07(): Bool = Order.compare(necOf2(0, 0), singleton(1)) == Comparison.LessThan

    @test
    def testCompare08(): Bool = Order.compare(necOf2(1, 2), necOf2(1, 1)) == Comparison.GreaterThan

    @test
    def testCompare09(): Bool = Order.compare(necOf2(1, 2), necOf2(1, 3)) == Comparison.LessThan

    @test
    def testCompare10(): Bool = Order.compare(necOf4(1, 2, 3, 4), necOf4(1, 2, 3, 4)) == Comparison.EqualTo

    /////////////////////////////////////////////////////////////////////////////
    // singleton                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def singleton01(): Bool = Nec.singleton(1) |> Nec.toList == 1 :: Nil

    @test
    def singleton02(): Bool = Nec.singleton(2) |> Nec.toList == 2 :: Nil

    @test
    def singleton03(): Bool = Nec.singleton('a') |> Nec.toList == 'a' :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // isSingleton                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isSingleton01(): Bool = Nec.isSingleton(singleton(1)) == true

    @test
    def isSingleton02(): Bool = Nec.isSingleton(necOf2(1, 2)) == false

    @test
    def isSingleton03(): Bool = Nec.isSingleton(necOf3(1, 2, 3)) == false


    /////////////////////////////////////////////////////////////////////////////
    // cons                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def cons01(): Bool = Nec.cons(1, singleton(2)) == necOf2(1, 2)

    @test
    def cons02(): Bool = Nec.cons(1, Nec.cons(2, singleton(3))) == necOf3(1, 2, 3)

    @test
    def cons03(): Bool = Nec.cons(1, Nec.cons(2, Nec.cons(3, singleton(4)))) == necOf4(1, 2, 3, 4)

    /////////////////////////////////////////////////////////////////////////////
    // snoc                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def snoc01(): Bool = Nec.snoc(singleton(1), 2) == necOf2(1, 2)

    @test
    def snoc02(): Bool = Nec.snoc(Nec.snoc(singleton(1), 2), 3) == necOf3(1, 2, 3)

    @test
    def snoc03(): Bool = Nec.snoc(Nec.snoc(Nec.snoc(singleton(1), 2), 3), 4) == necOf4(1, 2, 3, 4)

    /////////////////////////////////////////////////////////////////////////////
    // head                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def head01(): Bool = Nec.head(singleton(123)) == 123

    @test
    def head02(): Bool = Nec.head(necOf2(123, 1)) == 123

    @test
    def head03(): Bool = Nec.head(necOf3(123, 1, 2)) == 123

    /////////////////////////////////////////////////////////////////////////////
    // last                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def last01(): Bool = Nec.last(singleton(123)) == 123

    @test
    def last02(): Bool = Nec.last(necOf2(123, 1)) == 1

    @test
    def last03(): Bool = Nec.last(necOf3(123, 1, 2)) == 2

    /////////////////////////////////////////////////////////////////////////////
    // init                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def init01(): Bool = Nec.init(singleton(1)) == Nil

    @test
    def init02(): Bool = Nec.init(necOf2(1, 2)) == 1 :: Nil

    @test
    def init03(): Bool = Nec.init(necOf3(1, 2, 3)) == 1 :: 2 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // tail                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def tail01(): Bool = Nec.tail(singleton(1)) == Nil

    @test
    def tail02(): Bool = Nec.tail(necOf2(1, 2)) == 2 :: Nil

    @test
    def tail03(): Bool = Nec.tail(necOf3(1, 2, 3)) == 2 :: 3 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // length                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def length01(): Bool = Nec.length(singleton(1)) == 1

    @test
    def length02(): Bool = Nec.length(necOf2(1, 2)) == 2

    @test
    def length03(): Bool = Nec.length(necOf3(1, 2, 3)) == 3

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def size01(): Bool = Nec.size(singleton(1)) == 1

    @test
    def size02(): Bool = Nec.size(necOf2(1, 2)) == 2

    @test
    def size03(): Bool = Nec.size(necOf3(1, 2, 3)) == 3

    /////////////////////////////////////////////////////////////////////////////
    // append                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def append01(): Bool = Nec.append(singleton(1), singleton(2)) == necOf2(1, 2)

    @test
    def append02(): Bool = Nec.append(necOf2(1, 2), singleton(3)) == necOf3(1, 2, 3)

    @test
    def append03(): Bool = Nec.append(singleton(1), necOf2(2, 3)) == necOf3(1, 2, 3)

    @test
    def append04(): Bool = Nec.append(necOf2(1, 2), necOf2(3, 4)) ==  necOf4(1, 2, 3, 4)

    @test
    def append05(): Bool = Nec.append(necOf3(1, 2, 3), necOf3(4, 5, 6)) == necOf6(1, 2, 3, 4, 5, 6)

    /////////////////////////////////////////////////////////////////////////////
    // viewLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def viewLeft01(): Bool = Nec.viewLeft(singleton(1)) == OneLeft(1)

    @test
    def viewLeft02(): Bool = Nec.viewLeft(necOf2(1, 2)) == SomeLeft(1, singleton(2))

    @test
    def viewLeft03(): Bool = Nec.viewLeft(necOf3(1, 2, 3)) == SomeLeft(1, necOf2(2, 3))

    /////////////////////////////////////////////////////////////////////////////
    // viewRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def viewRight01(): Bool = Nec.viewRight(singleton(1)) == OneRight(1)

    @test
    def viewRight02(): Bool = Nec.viewRight(necOf2(1, 2)) == SomeRight(singleton(1), 2)

    @test
    def viewRight03(): Bool = Nec.viewRight(necOf3(1, 2, 3)) == SomeRight(necOf2(1, 2), 3)

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def memberOf01(): Bool = Nec.memberOf(0, singleton(1)) == false

    @test
    def memberOf02(): Bool = Nec.memberOf(0, necOf2(1, 2)) == false

    @test
    def memberOf03(): Bool = Nec.memberOf(0, necOf3(1, 2, 3)) == false

    @test
    def memberOf04(): Bool = Nec.memberOf(1, necOf3(1, 2, 3))

    @test
    def memberOf05(): Bool = Nec.memberOf(2, necOf3(1, 2, 3))

    @test
    def memberOf06(): Bool = Nec.memberOf(3, necOf3(1, 2, 3))

    /////////////////////////////////////////////////////////////////////////////
    // minimum                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def minimum01(): Bool = Nec.minimum(singleton(1)) == 1

    @test
    def minimum02(): Bool = Nec.minimum(necOf3(1, 2, 3)) == 1

    @test
    def minimum03(): Bool = Nec.minimum(necOf4(1, 2, 0, 3)) == 0

    /////////////////////////////////////////////////////////////////////////////
    // minimumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def minimumBy01(): Bool = Nec.minimumBy((x, y) -> x <=> y, singleton(1)) == 1

    @test
    def minimumBy02(): Bool = Nec.minimumBy((x, y) -> x <=> y, necOf3(1, 2, 3)) == 1

    @test
    def minimumBy03(): Bool = Nec.minimumBy((x, y) -> x <=> y, necOf4(1, 2, 0, 3)) == 0

    @test
    def minimumBy04(): Bool = Nec.minimumBy((x, y) -> x <=> y, necOf6(9, 5, 4, 6, 7, 3)) == 3

    @test
    def minimumBy05(): Bool = Nec.minimumBy((x, y) -> x <=> y, necOf6(9, 5, 4, 2, 7, 10)) == 2

    /////////////////////////////////////////////////////////////////////////////
    // maximum                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def maximum01(): Bool = Nec.maximum(singleton(1)) == 1

    @test
    def maximum02(): Bool = Nec.maximum(necOf3(1, 2, 3)) == 3

    @test
    def maximum03(): Bool = Nec.maximum(necOf4(1, 2, 0, 3)) == 3

    /////////////////////////////////////////////////////////////////////////////
    // maximumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def maximumBy01(): Bool = Nec.maximumBy((x, y) -> x <=> y, singleton(1)) == 1

    @test
    def maximumBy02(): Bool = Nec.maximumBy((x, y) -> x <=> y, necOf3(1, 2, 3)) == 3

    @test
    def maximumBy03(): Bool = Nec.maximumBy((x, y) -> x <=> y, necOf4(1, 2, 0, 3)) == 3

    @test
    def maximumBy04(): Bool = Nec.maximumBy((x, y) -> x <=> y, necOf6(9, 5, 4, 6, 7, 3)) == 9

    @test
    def maximumBy05(): Bool = Nec.maximumBy((x, y) -> x <=> y, necOf6(3, 5, 4, 9, 7, 10)) == 10

    /////////////////////////////////////////////////////////////////////////////
    // indexOf                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def indexOf01(): Bool = Nec.indexOf(0, singleton(1)) == None

    @test
    def indexOf02(): Bool = Nec.indexOf(1, singleton(1)) == Some(0)

    @test
    def indexOf03(): Bool = Nec.indexOf(0, necOf2(1, 2)) == None

    @test
    def indexOf04(): Bool = Nec.indexOf(1, necOf2(1, 2)) == Some(0)

    @test
    def indexOf05(): Bool = Nec.indexOf(2, necOf2(1, 2)) == Some(1)

    @test
    def indexOf06(): Bool = Nec.indexOf(3, necOf3(1, 2, 3)) == Some(2)

    /////////////////////////////////////////////////////////////////////////////
    // find                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def find01(): Bool = Nec.find(x -> x == 0, singleton(1)) == None

    @test
    def find02(): Bool = Nec.find(x -> x == 0, necOf2(1, 2)) == None

    @test
    def find03(): Bool = Nec.find(x -> x == 0, necOf3(1, 2, 3)) == None

    @test
    def find04(): Bool = Nec.find(x -> x == 1, necOf3(1, 2, 3)) == Some(1)

    @test
    def find05(): Bool = Nec.find(x -> x == 2, necOf3(1, 2, 3)) == Some(2)

    @test
    def find06(): Bool = Nec.find(x -> x == 3, necOf3(1, 2, 3)) == Some(3)

    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findLeft01(): Bool = Nec.findLeft(x -> x == 0, singleton(1)) == None

    @test
    def findLeft02(): Bool = Nec.findLeft(x -> x == 0, necOf2(1, 2)) == None

    @test
    def findLeft03(): Bool = Nec.findLeft(x -> x == 0, necOf3(1, 2, 3)) == None

    @test
    def findLeft04(): Bool = Nec.findLeft(x -> x == 1, necOf3(1, 2, 3)) == Some(1)

    @test
    def findLeft05(): Bool = Nec.findLeft(x -> x == 2, necOf3(1, 2, 3)) == Some(2)

    @test
    def findLeft06(): Bool = Nec.findLeft(x -> x == 3, necOf3(1, 2, 3)) == Some(3)

    /////////////////////////////////////////////////////////////////////////////
    // findRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findRight01(): Bool = Nec.findRight(x -> x == 0, singleton(1)) == None

    @test
    def findRight02(): Bool = Nec.findRight(x -> x == 0, necOf2(1, 2)) == None

    @test
    def findRight03(): Bool = Nec.findRight(x -> x == 0, necOf3(1, 2, 3)) == None

    @test
    def findRight04(): Bool = Nec.findRight(x -> x == 1, necOf3(1, 2, 3)) == Some(1)

    @test
    def findRight05(): Bool = Nec.findRight(x -> x == 2, necOf3(1, 2, 3)) == Some(2)

    @test
    def findRight06(): Bool = Nec.findRight(x -> x == 3, necOf3(1, 2, 3)) == Some(3)

    /////////////////////////////////////////////////////////////////////////////
    // range                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def range01(): Bool = Nec.range(1, 0) == None

    @test
    def range02(): Bool = Nec.range(1, 1) == None

    @test
    def range03(): Bool = Nec.range(1, 2) == Some(singleton(1))

    @test
    def range04(): Bool = Nec.range(1, 3) == Some(necOf2(1, 2))

    @test
    def range05(): Bool = Nec.range(1, 4) == Some(necOf3(1, 2, 3))

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map01(): Bool = Nec.map(x -> x + 1, singleton(1)) == singleton(2)

    @test
    def map02(): Bool = Nec.map(x -> x + 1, necOf2(1, 2)) == necOf2(2, 3)

    @test
    def map03(): Bool = Nec.map(x -> x + 1, necOf3(1, 2, 3)) == necOf3(2, 3, 4)

    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapWithIndex01(): Bool = Nec.mapWithIndex((i, _) -> i, singleton(21)) == singleton(0)

    @test
    def mapWithIndex02(): Bool = Nec.mapWithIndex((i, _) -> i, necOf2(21, 42)) == necOf2(0, 1)

    @test
    def mapWithIndex03(): Bool = Nec.mapWithIndex((i, _) -> i, necOf3(21, 42, 84)) == necOf3(0, 1, 2)

    /////////////////////////////////////////////////////////////////////////////
    // ap                                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def ap01(): Bool = Nec.ap(singleton(x -> x+1), singleton(5)) == singleton(6)

    @test
    def ap02(): Bool = Nec.ap(singleton(x -> x+1), necOf2(0, 5)) == necOf2(1, 6)

    @test
    def ap03(): Bool = Nec.ap(necOf2((x -> x+1), (x -> x*2)), necOf2(0, 4)) == necOf4(1, 5, 0, 8)

    @test
    def ap04(): Bool = Nec.ap(necOf2((x -> x+1), (x -> x*2)), necOf3(0, 4, 8)) == necOf6(1, 5, 9, 0, 8, 16)

    @test
    def ap05(): Bool = Nec.ap(necOf2((x -> x+1), (x -> x*2)), necOf4(0, 4, 8, 16)) == necOf8(1, 5, 9, 17, 0, 8, 16, 32)

    /////////////////////////////////////////////////////////////////////////////
    // flatMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatMap01(): Bool = Nec.flatMap(x -> singleton(x + 1), singleton(1)) == singleton(2)

    @test
    def flatMap02(): Bool = Nec.flatMap(x -> singleton(x + 1), necOf2(1, 2)) == necOf2(2, 3)

    @test
    def flatMap03(): Bool = Nec.flatMap(x -> singleton(x + 1), necOf3(1, 2, 3)) == necOf3(2, 3, 4)

    @test
    def flatMap04(): Bool = Nec.flatMap(x -> necOf2(x, (x+1)), singleton(1)) == necOf2(1, 2)

    @test
    def flatMap05(): Bool = Nec.flatMap(x -> necOf2(x, (x+1)), necOf2(1, 4)) == necOf4(1, 2, 4, 5)

    @test
    def flatMap06(): Bool = Nec.flatMap(x -> necOf2(x, (x+1)), necOf3(1, 3, 5)) == necOf6(1, 2, 3, 4, 5, 6)

    /////////////////////////////////////////////////////////////////////////////
    // reverse                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reverse01(): Bool = Nec.reverse(singleton(1)) == singleton(1)

    @test
    def reverse02(): Bool = Nec.reverse(necOf2(1, 2)) == necOf2(2, 1)

    @test
    def reverse03(): Bool = Nec.reverse(necOf3(1, 2, 3)) == necOf3(3, 2, 1)

    @test
    def reverse04(): Bool = Nec.reverse(necOf4(1, 2, 3, 4)) == necOf4(4, 3, 2, 1)

    /////////////////////////////////////////////////////////////////////////////
    // replace                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def replace01(): Bool = Nec.replace(src =  1, dst =  42, singleton(1)) == singleton(42)

    @test
    def replace02(): Bool = Nec.replace(src =  7, dst =  42, singleton(1)) == singleton(1)

    @test
    def replace03(): Bool = Nec.replace(src =  2, dst =  42, necOf2(1, 2)) == necOf2(1, 42)

    @test
    def replace04(): Bool = Nec.replace(src =  7, dst =  42, necOf2(1, 2)) == necOf2(1, 2)

    /////////////////////////////////////////////////////////////////////////////
    // permutations                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def permutations01(): Bool = Nec.permutations(singleton(1)) == singleton(1 :: Nil)

    @test
    def permutations02(): Bool = Nec.permutations(necOf2(1, 2)) == necOf2((1 :: 2 :: Nil), (2 :: 1 :: Nil))

    @test
    def permutations03(): Bool = Nec.permutations(necOf3(1, 2, 3)) == necOf6((1 :: 2 :: 3 :: Nil),
                                                                                (1 :: 3 :: 2 :: Nil),
                                                                                (2 :: 1 :: 3 :: Nil),
                                                                                (2 :: 3 :: 1 :: Nil),
                                                                                (3 :: 1 :: 2 :: Nil),
                                                                                (3 :: 2 :: 1 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // subsequences                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def subsequences01(): Bool = Nec.subsequences(singleton(1)) == necOf2(1 :: Nil, (Nil))

    @test
    def subsequences02(): Bool = Nec.subsequences(necOf2(1, 2)) == necOf4((1 :: 2 :: Nil),
                                                                            (1 :: Nil),
                                                                            (2 :: Nil),
                                                                            (Nil))

    @test
    def subsequences03(): Bool = Nec.subsequences(necOf3(1, 2, 3)) == necOf8((1 :: 2 :: 3 :: Nil),
                                                                            (1 :: 2 :: Nil),
                                                                            (1 :: 3 :: Nil),
                                                                            (1 :: Nil),
                                                                            (2 :: 3 :: Nil),
                                                                            (2 :: Nil),
                                                                            (3 :: Nil),
                                                                            (Nil))

    /////////////////////////////////////////////////////////////////////////////
    // intersperse                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def intersperse01(): Bool = Nec.intersperse(7, singleton(1)) == singleton(1)

    @test
    def intersperse02(): Bool = Nec.intersperse(7, necOf2(1, 2)) == necOf3(1, 7, 2)

    @test
    def intersperse03(): Bool = Nec.intersperse(7, necOf3(1, 2, 3)) == necOf5(1, 7, 2, 7, 3)

    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def count01(): Bool = Nec.count(x -> x == 1, singleton(1)) == 1

    @test
    def count02(): Bool = Nec.count(x -> x == 2, singleton(1)) == 0

    @test
    def count03(): Bool = Nec.count(x -> x == 2, necOf4(1, 2, 1, 2)) == 2

    @test
    def count04(): Bool = Nec.count(x -> x == 2, necOf4(2, 2, 1, 2)) == 3

    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sum01(): Bool =
        Nec.sum(singleton(1)) == 1

    @test
    def sum02(): Bool =
        Nec.sum(necOf3(1, 2, 3)) == 6

    @test
    def sum03(): Bool =
        Nec.sum(necOf4(1, 2, 3, -3)) == 3

    @test
    def sum04(): Bool =
        Nec.sum(necOf4(-1, -2, -3, -4)) == -10

    @test
    def sum05(): Bool =
        Nec.sum(necOf2(10, -10)) == 0

    @test
    def sum06(): Bool = match Nec.range(1, 101) {
        case Some(n1) => Nec.sum(n1) == 5050
        case _        => unreachable!()
    }

    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumWith01(): Bool =
        Nec.sumWith(x -> x + 1, singleton(1)) == 2

    @test
    def sumWith02(): Bool =
        Nec.sumWith(x -> x + 1, necOf3(1, 2, 3)) == 9

    @test
    def sumWith03(): Bool =
        Nec.sumWith(x -> x + 1, necOf4(1, 2, 3, -3)) == 7

    @test
    def sumWith04(): Bool =
        Nec.sumWith(x -> x + 1, necOf4(-1, -2, -3, -4)) == -6

    @test
    def sumWith05(): Bool =
        Nec.sumWith(x -> x + 1, necOf2(10, -10)) == 2


    /////////////////////////////////////////////////////////////////////////////
    // flatten                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatten01(): Bool = Nec.flatten(singleton(singleton(1))) == singleton(1)

    @test
    def flatten02(): Bool = Nec.flatten(necOf2(singleton(1), singleton(2))) == necOf2(1, 2)

    @test
    def flatten03(): Bool = Nec.flatten(necOf3(singleton(1), singleton(2), singleton(3))) == necOf3(1, 2, 3)

    @test
    def flatten04(): Bool = Nec.flatten(necOf3(singleton(1), necOf2(2, 3), necOf2(4, 5))) == necOf5(1, 2, 3, 4, 5)

    @test
    def flatten05(): Bool = Nec.flatten(necOf3(necOf3(1, 2, 3), necOf2(4, 5),  necOf2(6, 7))) == necOf7(1, 2, 3, 4, 5, 6, 7)
    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def exists01(): Bool = Nec.exists(x -> x == 1, singleton(1)) == true

    @test
    def exists02(): Bool = Nec.exists(x -> x == 2, singleton(1)) == false

    @test
    def exists03(): Bool = Nec.exists(x -> x == 1, necOf2(1, 2)) == true

    @test
    def exists04(): Bool = Nec.exists(x -> x == 2, necOf2(1, 2)) == true

    /////////////////////////////////////////////////////////////////////////////
    // forAll                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forAll01(): Bool = Nec.forAll(x -> x == 1, singleton(1)) == true

    @test
    def forAll02(): Bool = Nec.forAll(x -> x == 2, singleton(1)) == false

    @test
    def forAll03(): Bool = Nec.forAll(x -> x == 1, necOf2(1, 1)) == true

    @test
    def forAll04(): Bool = Nec.forAll(x -> x == 1, necOf2(1, 2)) == false

    @test
    def forAll05(): Bool = Nec.forAll(x -> x == 1, necOf2(2, 1)) == false

    /////////////////////////////////////////////////////////////////////////////
    // filter                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filter01(): Bool = Nec.filter(x -> x > 0, singleton(1)) == 1 :: Nil

    @test
    def filter02(): Bool = Nec.filter(x -> x > 1, singleton(1)) == Nil

    @test
    def filter03(): Bool = Nec.filter(x -> x > 0, necOf2(1, 2)) == 1 :: 2 :: Nil

    @test
    def filter04(): Bool = Nec.filter(x -> x > 1, necOf2(1, 2)) == 2 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // fold                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def fold01(): Bool = Nec.fold(singleton("a")) == "a"

    @test
    def fold02(): Bool = Nec.fold(singleton(())) == ()

    @test
    def fold03(): Bool = Nec.fold(necOf3("a", "b", "c")) == "abc"

    @test
    def fold04(): Bool = Nec.fold(necOf3(("a", ()), ("b", ()), ("c", ()))) == ("abc", ())

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeft01(): Bool = Nec.foldLeft((acc, x) -> acc - x, 0, singleton(1)) == -1

    @test
    def foldLeft02(): Bool = Nec.foldLeft((acc, x) -> acc - x, 0, necOf2(1, 2)) == -3

    @test
    def foldLeft03(): Bool = Nec.foldLeft((acc, x) -> acc - x, 0, necOf3(1, 2, 3)) == -6

    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRight01(): Bool = Nec.foldRight((x, acc) -> acc - x, 0, singleton(1)) == -1

    @test
    def foldRight02(): Bool = Nec.foldRight((x, acc) -> acc - x, 0, necOf2(1, 2)) == -3

    @test
    def foldRight03(): Bool = Nec.foldRight((x, acc) -> acc - x, 0, necOf3(1, 2, 3)) == -6

    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithCont                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRightWithCont01(): Bool = Nec.foldRightWithCont((x, k) -> k() - x, 0, singleton(1)) == -1

    @test
    def foldRightWithCont02(): Bool = Nec.foldRightWithCont((x, k) -> k() - x, 0, necOf2(1, 2)) == -3

    @test
    def foldRightWithCont03(): Bool = Nec.foldRightWithCont((x, k) -> k() - x, 0, necOf3(1, 2, 3)) == -6


    /////////////////////////////////////////////////////////////////////////////
    // foldMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldMap01(): Bool =
        Nec.foldMap(x -> 2 * x, necOf2(1, 2)) == 6

    @test
    def foldMap02(): Bool =
        Nec.foldMap(x -> if(x == "a") "b" else x, singleton("a")) == "b"

    @test
    def foldMap03(): Bool =
        Nec.foldMap(x -> if(x == "c") "b" else x, necOf3("a", "b", "c")) == "abb"

    @test
    def foldMap04(): Bool =
        Nec.foldMap(Int32.toString, necOf3(1, 2, 3)) == "123"


    /////////////////////////////////////////////////////////////////////////////
    // filterMap                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterMap01(): Bool = Nec.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, singleton(1)) == Nil

    @test
    def filterMap02(): Bool = Nec.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, singleton(2)) == 1 :: Nil

    @test
    def filterMap03(): Bool = Nec.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, necOf2(1, 3)) == Nil

    @test
    def filterMap04(): Bool = Nec.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, necOf2(1, 4)) == 2 :: Nil

    @test
    def filterMap05(): Bool = Nec.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, necOf2(6, -1)) == 3 :: Nil

    @test
    def filterMap07(): Bool = Nec.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, necOf2(8, 6)) == 4 :: 3 :: Nil

    @test
    def filterMap08(): Bool = Nec.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, necOf7(0, 1, 2, 3, 4, 5, 10)) ==
                            0 :: 1 :: 2 :: 5 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // findMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findMap01(): Bool = Nec.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, singleton(1)) == None

    @test
    def findMap02(): Bool = Nec.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, singleton(2)) == Some(1)

    @test
    def findMap03(): Bool = Nec.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, necOf2(1, 3)) == None

    @test
    def findMap04(): Bool = Nec.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, necOf2(1, 4)) == Some(2)

    @test
    def findMap05(): Bool = Nec.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, necOf2(6, -1)) == Some(3)

    @test
    def findMap06(): Bool = Nec.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, necOf2(8, 6)) == Some(4)

    @test
    def findMap07(): Bool = Nec.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i/2) else None, necOf7(0, 1, 2, 3, 4, 5, 10)) == Some(0)

    /////////////////////////////////////////////////////////////////////////////
    // zip                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zip01(): Bool = Nec.zip(singleton(1), singleton(1)) == singleton((1, 1))

    @test
    def zip02(): Bool = Nec.zip(necOf2(1, 2), singleton(1)) == singleton((1, 1))

    @test
    def zip03(): Bool = Nec.zip(singleton(1), necOf2(1, 2)) == singleton((1, 1))

    @test
    def zip04(): Bool = Nec.zip(necOf2(1, 2), necOf2(1, 2)) == necOf2((1, 1), (2, 2))

    /////////////////////////////////////////////////////////////////////////////
    // zipWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWith01(): Bool = Nec.zipWith((x, y) -> x + y, singleton(1), singleton(1)) == singleton(2)

    @test
    def zipWith02(): Bool = Nec.zipWith((x, y) -> x + y, necOf2(1, 2), singleton(1)) == singleton(2)

    @test
    def zipWith03(): Bool = Nec.zipWith((x, y) -> x + y, singleton(1), necOf2(1, 2)) == singleton(2)

    @test
    def zipWith04(): Bool = Nec.zipWith((x, y) -> x + y, necOf2(1, 2), necOf2(1, 2)) == necOf2(2, 4)

    /////////////////////////////////////////////////////////////////////////////
    // unzip                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def unzip01(): Bool = Nec.unzip(singleton((1, 1))) == (singleton(1), singleton(1))

    @test
    def unzip02(): Bool = Nec.unzip(necOf2((1, 1), (2, 2))) == (necOf2(1, 2), necOf2(1, 2))

    @test
    def unzip03(): Bool = Nec.unzip(necOf3((1, 1), (2, 2), (3, 3))) == (necOf3(1, 2, 3), necOf3(1, 2, 3))

    /////////////////////////////////////////////////////////////////////////////
    // zipWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWithIndex01(): Bool = Nec.zipWithIndex(singleton(1)) == singleton((0, 1))

    @test
    def zipWithIndex02(): Bool = Nec.zipWithIndex(necOf2(1, 2)) == necOf2((0, 1), (1, 2))

    @test
    def zipWithIndex03(): Bool = Nec.zipWithIndex(necOf2(2, 1)) == necOf2((0, 2), (1, 1))

    @test
    def zipWithIndex04(): Bool = Nec.zipWithIndex(necOf3(4, 3, 6)) == necOf3((0, 4), (1, 3), (2, 6))

    @test
    def zipWithIndex05(): Bool = Nec.zipWithIndex(necOf4(1, 2, 3, 4)) == necOf4((0, 1), (1, 2), (2, 3), (3, 4))

    @test
    def zipWithIndex06(): Bool =
        Nec.zipWithIndex(necOf8(8, 7, 6, 5, 4, 3, 2, 1)) == necOf8((0, 8), (1, 7), (2, 6), (3, 5), (4, 4), (5, 3), (6, 2), (7, 1))

    /////////////////////////////////////////////////////////////////////////////
    // zipWithA                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWithA01(): Bool = Nec.zipWithA((x, y) -> Identity.Identity(x + y), singleton(1), singleton(1)) == Identity.Identity(singleton(2))

    @test
    def zipWithA02(): Bool = Nec.zipWithA((x, y) -> Identity.Identity(x + y), necOf2(1, 2), singleton(1)) == Identity.Identity(singleton(2))

    @test
    def zipWithA03(): Bool = Nec.zipWithA((x, y) -> Identity.Identity(x + y), singleton(1), necOf2(1, 2)) == Identity.Identity(singleton(2))

    @test
    def zipWithA04(): Bool = Nec.zipWithA((x, y) -> Identity.Identity(x + y), necOf2(1, 2), necOf2(1, 2)) == Identity.Identity(necOf2(2, 4))

    /////////////////////////////////////////////////////////////////////////////
    // mapAccumLeft                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapAccumLeft01(): Bool = Nec.mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, singleton(1)) == (11, singleton(11))

    @test
    def mapAccumLeft02(): Bool = Nec.mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, necOf2(1,2)) == (12, necOf2(11, 13))

    @test
    def mapAccumLeft03(): Bool = Nec.mapAccumLeft((acc, x) -> (acc+1, x+acc), 10, necOf3(1, 2, 3)) == (13, necOf3(11, 13, 15))

    /////////////////////////////////////////////////////////////////////////////
    // mapAccumRight                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapAccumRight01(): Bool = Nec.mapAccumRight((acc, x) -> (acc+1, x+acc), 10, singleton(1)) == (11, singleton(11))

    @test
    def mapAccumRight02(): Bool = Nec.mapAccumRight((acc, x) -> (acc+1, x+acc), 10, necOf2(1, 2)) == (12, necOf2(12, 12))

    @test
    def mapAccumRight03(): Bool = Nec.mapAccumRight((acc, x) -> (acc+1, x+acc), 10, necOf3(1, 2, 3)) == (13, necOf3(13, 13, 13))

    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forEach01(): Bool = region rc {
        let ri = Ref.fresh(rc, 21);
        Nec.forEach(x -> Ref.put(x, ri), singleton(1));
        1 == Ref.get(ri)
    }

    @test
    def forEach02(): Bool = region rc {
        let ri = Ref.fresh(rc, 21);
        Nec.forEach(x -> Ref.put(x, ri), necOf3(1, 2, 3));
        3 == Ref.get(ri)
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEachWithIndex                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forEachWithIndex01(): Bool = region rc {
        let ri = Ref.fresh(rc, 21);
        Nec.forEachWithIndex((i, _) -> Ref.put(i, ri), singleton(1));
        0 == Ref.get(ri)
    }

    @test
    def forEachWithIndex02(): Bool = region rc {
        let ri = Ref.fresh(rc, 21);
        Nec.forEachWithIndex((i, _) -> Ref.put(i, ri), necOf3(1, 2, 3));
        2 == Ref.get(ri)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toList01(): Bool = Nec.toList(singleton(1)) == 1 :: Nil

    @test
    def toList02(): Bool = Nec.toList(necOf2(1, 2)) == 1 :: 2 :: Nil

    @test
    def toList03(): Bool = Nec.toList(necOf3(1, 2, 3)) == 1 :: 2 :: 3 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // toMutDeque                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMutDeque01(): Bool = region rc {
        let c = singleton(1);
        let d1 = Nec.toMutDeque(rc, c);

        let d2 = MutDeque.empty(rc);
        MutDeque.pushBack(1, d2);

        d1 `MutDeque.sameElements` d2
    }

    @test
    def toMutDeque02(): Bool = region rc {
        let c = necOf2(1, 2);
        let d1 = Nec.toMutDeque(rc, c);

        let d2 = MutDeque.empty(rc);
        MutDeque.pushFront(1, d2);
        MutDeque.pushBack(2, d2);

        d1 `MutDeque.sameElements` d2
    }

    @test
    def toMutDeque03(): Bool = region rc {
        let c = necOf3('a', 'b', 'c');
        let d1 = Nec.toMutDeque(rc, c);

        let d2 = MutDeque.empty(rc);
        MutDeque.pushFront('b', d2);
        MutDeque.pushBack('c', d2);
        MutDeque.pushFront('a', d2);

        d1 `MutDeque.sameElements` d2
    }


    /////////////////////////////////////////////////////////////////////////////
    // toMutList                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMutList01(): Bool = region rc {
        let m1 = Nec.toMutList(rc, singleton(1));
        let m2 = MutList.range(rc, 1, 2);
        MutList.sameElements(m1, m2)
    }

    @test
    def toMutList02(): Bool = region rc {
        let m1 = Nec.toMutList(rc, necOf2(1, 2));
        let m2 = MutList.range(rc, 1, 3);
        MutList.sameElements(m1, m2)
    }

    @test
    def toMutList03(): Bool = region rc {
        let m1 = Nec.toMutList(rc, necOf3(1, 2, 3));
        let m2 = MutList.range(rc, 1, 4);
        MutList.sameElements(m1, m2)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toSet01(): Bool = Nec.toSet(singleton(1)) == Set#{1}

    @test
    def toSet02(): Bool = Nec.toSet(necOf2(1, 2)) == Set#{1, 2}

    @test
    def toSet03(): Bool = Nec.toSet(necOf2(1, 1)) == Set#{1}

    @test
    def toSet04(): Bool = Nec.toSet(necOf3(1, 2, 3)) == Set#{1, 2, 3}

    @test
    def toSet05(): Bool = Nec.toSet(necOf3(1, 2, 1)) == Set#{1, 2}

    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMap01(): Bool =
        Nec.toMap(necOf4((7, 0), (1, 0), (3, 0), (6, 0))) == Map#{1 => 0, 3 => 0, 6 => 0, 7 => 0}

    @test
    def toMap02(): Bool =
        Nec.toMap(necOf4((7, 14), (1, 2), (3, 6), (6, 12))) == Map#{1 => 2, 3 => 6, 6 => 12, 7 => 14}

    @test
    def toMap03(): Bool =
        Nec.toMap(necOf4((7, 17), (1, 11), (3, 13), (6, 16))) == Map#{1 => 11, 3 => 13, 6 => 16, 7 => 17}

    @test
    def toMap04(): Bool =
        Nec.toMap(necOf4((7, 17), (3, 13), (6, 16), (1, 11))) == Map#{1 => 11, 3 => 13, 6 => 16, 7 => 17}

    /////////////////////////////////////////////////////////////////////////////
    // toMapWith                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMapWith01(): Bool =
        Nec.toMapWith(x -> 0 * x, necOf4(7, 1, 3, 6)) == Map#{1 => 0, 3 => 0, 6 => 0, 7 => 0}

    @test
    def toMapWith02(): Bool =
        Nec.toMapWith(x -> 2 * x, necOf4(7, 1, 3, 6)) == Map#{1 => 2, 3 => 6, 6 => 12, 7 => 14}

    @test
    def toMapWith03(): Bool =
        Nec.toMapWith(x -> x + 10, necOf4(7, 1, 3, 6)) == Map#{1 => 11, 3 => 13, 6 => 16, 7 => 17}

    @test
    def toMapWith04(): Bool =
        Nec.toMapWith(x -> x + 10, necOf4(7, 3, 6, 1)) == Map#{1 => 11, 3 => 13, 6 => 16, 7 => 17}

    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toArray01(): Bool = region rc {
        let a = Nec.toArray(rc, singleton(1));
        Array.sameElements(a, Array#{1} @ rc)
    }

    @test
    def toArray02(): Bool = region rc {
        let a = Nec.toArray(rc, necOf2(1, 2));
        Array.sameElements(a, Array#{1, 2} @ rc)
    }

    @test
    def toArray03(): Bool = region rc {
        let a = Nec.toArray(rc, necOf3(1, 2, 3));
        Array.sameElements(a, Array#{1, 2, 3} @ rc)
    }

    @test
    def toArray04(): Bool = region rc {
        let a = Nec.toArray(rc, necOf3(3, 2, 3));
        Array.sameElements(a, Array#{3, 2, 3} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toVector                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toVector01(): Bool =
        let c = singleton(1);
        Nec.toVector(c) == Vector#{1}

    @test
    def toVector02(): Bool =
        let c = necOf2(1, 2);
        Nec.toVector(c) == Vector#{1, 2}

    @test
    def toVector03(): Bool =
        let c = necOf3(1, 2, 3);
        Nec.toVector(c) == Vector#{1, 2, 3}

    @test
    def toVector04(): Bool =
        let c = necOf3(3, 2, 3);
        Nec.toVector(c) == Vector#{3, 2, 3}

    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def iterator01(): Bool = region rc {
        necOf3(1, 2, 3) |> Nec.iterator(rc) |> Iterator.toNec == Some(necOf3(1, 2, 3))
    }

    @test
    def iterator02(): Bool = region rc {
        match Nec.range(-100, 100) {
            case Some(nec) => nec |> Nec.iterator(rc) |> Iterator.toList == List.range(-100, 100)
            case None      => unreachable!()
        }
    }

    /////////////////////////////////////////////////////////////////////////////
    // enumerator                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def enumerator01(): Bool = region rc {
        necOf3(1, 2, 3) |> Nec.enumerator(rc) |> Iterator.toList == (0, 1) :: (1, 2) :: (2, 3) :: Nil
    }

    @test
    def enumerator02(): Bool = region rc {
        match Nec.range(-100, 100) {
            case Some(nec) => nec |> Nec.enumerator(rc) |> Iterator.toList == List.mapWithIndex((ix, i) -> (ix, i), (List.range(-100, 100)))
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

    @test
    def sortWith02(): Bool =
        Nec.sortWith(cmp, singleton(0)) == singleton(0)

    @test
    def sortWith03(): Bool =
        Nec.sortWith(cmp, necOf2(0, 1)) == necOf2(0, 1)

    @test
    def sortWith04(): Bool =
        Nec.sortWith(cmp, necOf2(1, 0)) == necOf2(0, 1)

    @test
    def sortWith05(): Bool =
        Nec.sortWith(cmp, necOf2(1, 1)) == necOf2(1, 1)

    @test
    def sortWith06(): Bool =
        Nec.sortWith(cmp, necOf6(0, 1, 2, 3, 4, 5)) == necOf6(0, 1, 2, 3, 4, 5)

    @test
    def sortWith07(): Bool =
        Nec.sortWith(cmp, necOf6(5, 4, 3, 2, 1, 0)) == necOf6(0, 1, 2, 3, 4, 5)

    @test
    def sortWith08(): Bool =
        Nec.sortWith(cmp, necOf6(5, 3, 0, 4, 1, 2)) == necOf6(0, 1, 2, 3, 4, 5)

    @test
    def sortWith09(): Bool =
        Nec.sortWith(cmp, necOf6(2, 3, 0, 4, 1, 2)) == necOf6(0, 1, 2, 2, 3, 4)

    @test
    def sortWith10(): Bool =
        Nec.sortWith(flip(cmp), necOf6(0, 1, 2, 3, 4, 5)) == necOf6(5, 4, 3, 2, 1, 0)

    @test
    def sortWith11(): Bool =
        Nec.sortWith(flip(cmp), necOf6(5, 4, 3, 2, 1, 0)) == necOf6(5, 4, 3, 2, 1, 0)

    @test
    def sortWith12(): Bool =
        Nec.sortWith(flip(cmp), necOf6(5, 3, 0, 4, 1, 2)) == necOf6(5, 4, 3, 2, 1, 0)

    @test
    def sortWith13(): Bool =
        Nec.sortWith(flip(cmp), necOf6(2, 3, 0, 4, 1, 2)) == necOf6(4, 3, 2, 2, 1, 0)

    /////////////////////////////////////////////////////////////////////////////
    // sort                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    def testSortVsSortWith(xs: Nec[Int32]) : Bool =
        Nec.sort(xs) == Nec.sortWith(cmp, xs)

    @test
    def sort02(): Bool = testSortVsSortWith(singleton(0))

    @test
    def sort03(): Bool = testSortVsSortWith(necOf2(0, 1))

    @test
    def sort04(): Bool = testSortVsSortWith(necOf2(1, 0))

    @test
    def sort05(): Bool = testSortVsSortWith(necOf2(1, 1))

    @test
    def sort06(): Bool = testSortVsSortWith(necOf6(0, 1, 2, 3, 4, 5))

    @test
    def sort07(): Bool = testSortVsSortWith(necOf6(5, 4, 3, 2, 1, 0))

    @test
    def sort08(): Bool = testSortVsSortWith(necOf6(5, 3, 0, 4, 1, 2))

    @test
    def sort09(): Bool = testSortVsSortWith(necOf6(2, 3, 0, 4, 1, 2))

    /////////////////////////////////////////////////////////////////////////////
    // sortBy                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    def testSortByVsSort(a: Nec[Int32]) : Bool =
        (Nec.sortBy(identity, a) == Nec.sort(a)) and
        (Nec.sortBy(x -> 4*x+7, a) == Nec.sort(a)) and
        (Nec.sortBy(x -> -x, a) == Nec.sortWith(flip(cmp),a))

    @test
    def sortBy02(): Bool = testSortByVsSort(singleton(0))

    @test
    def sortBy03(): Bool = testSortByVsSort(necOf2(0, 1))

    @test
    def sortBy04(): Bool = testSortByVsSort(necOf2(1, 0))

    @test
    def sortBy05(): Bool = testSortByVsSort(necOf2(1, 1))

    @test
    def sortBy06(): Bool = testSortByVsSort(necOf6(0, 1, 2, 3, 4, 5))

    @test
    def sortBy07(): Bool = testSortByVsSort(necOf6(5, 4, 3, 2, 1, 0))

    @test
    def sortBy08(): Bool = testSortByVsSort(necOf6(5, 3, 0, 4, 1, 2))

    @test
    def sortBy09(): Bool = testSortByVsSort(necOf6(2, 3, 0, 4, 1, 2))

    /////////////////////////////////////////////////////////////////////////////
    // sequence                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sequence01(): Bool =
        let xs: Nec[Option[Int32]] = singleton(None);
        Nec.sequence(xs) == None

    @test
    def sequence02(): Bool = Nec.sequence(singleton(Some(1))) == Some(singleton(1))

    @test
    def sequence03(): Bool = Nec.sequence(necOf2(Some(1), Some(2))) == Some(necOf2(1, 2))

    @test
    def sequence04(): Bool = Nec.sequence(necOf3(Some(1), Some(2), Some(3))) == Some(necOf3(1, 2, 3))

    @test
    def sequence05(): Bool =
        let l = Nec.singleton(Identity.Identity(1));
        Nec.sequence(l) == Identity.Identity(Nec.singleton(1))

    @test
    def sequence06(): Bool =
        let l = necOf2(Identity.Identity(1), Identity.Identity(2));
        Nec.sequence(l) == Identity.Identity(necOf2(1, 2))

    @test
    def sequence07(): Bool =
        let l = necOf3(Identity.Identity(1), Identity.Identity(2), Identity.Identity(3));
        Nec.sequence(l) == Identity.Identity(necOf3(1, 2, 3))

    /////////////////////////////////////////////////////////////////////////////
    // traverse                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def traverse01(): Bool = Nec.traverse(x -> Some(x+1), singleton(1)) == Some(singleton(2))

    @test
    def traverse02(): Bool = Nec.traverse(x -> Some(x+1), necOf2(1, 2)) == Some(necOf2(2, 3))

    @test
    def traverse03(): Bool = Nec.traverse(_ -> (None: Option[Int32]), singleton(1)) == None

    @test
    def traverse04(): Bool = Nec.traverse(_ -> (None: Option[Int32]), necOf2(1, 2)) == None

    @test
    def traverse05(): Bool = region rc {
        let st = Ref.fresh(rc, 0);
        let l = Nec.singleton(1);
        let ans = Nec.traverse(x -> {Ref.put(x, st); Identity.Identity(x)}, l);
        ans == Identity.Identity(Nec.singleton(1)) and Ref.get(st) == 1
    }

    @test
    def traverse06(): Bool = region rc {
        let st = Ref.fresh(rc, 0);
        let l = necOf2(1, 2);
        let ans = Nec.traverse(x -> {Ref.put(x, st); Identity.Identity(x)}, l);
        ans == Identity.Identity(necOf2(1, 2)) and Ref.get(st) == 2
    }

    @test
    def traverse07(): Bool = region rc {
        let st = Ref.fresh(rc, 0);
        let l = necOf3(1, 2, 3);
        let ans = Nec.traverse(x -> {Ref.put(x, st); Identity.Identity(x)}, l);
        ans == Identity.Identity(necOf3(1, 2, 3)) and Ref.get(st) == 3
    }

    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def join01(): Bool =
        Nec.join(",", singleton(1)) == "1"

    @test
    def join02(): Bool =
        Nec.join(",", necOf3(1, 2, 3)) == "1,2,3"

    @test
    def join03(): Bool =
        Nec.join(",", necOf3("1", "2", "3")) == "1,2,3"

    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def joinWith01(): Bool =
        Nec.joinWith(x -> "${x + 1}", ",", singleton(1)) == "2"

    @test
    def joinWith02(): Bool =
        Nec.joinWith(x -> "${x + 1}", ",", necOf3(1, 2, 3)) == "2,3,4"

    @test
    def joinWith03(): Bool =
        Nec.joinWith(x -> x + x, ",", necOf3("1", "2", "3")) == "11,22,33"

    /////////////////////////////////////////////////////////////////////////////
    // reduce                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduce01(): Bool = Nec.reduce(singleton("a")) == "a"

    @test
    def reduce02(): Bool = Nec.reduce(singleton(())) == ()

    @test
    def reduce03(): Bool = Nec.reduce(necOf3("a", "b", "c")) == "abc"

    /////////////////////////////////////////////////////////////////////////////
    // reduceLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceLeft01(): Bool = Nec.reduceLeft((acc, x) -> acc - x, singleton(1)) == 1

    @test
    def reduceLeft02(): Bool = Nec.reduceLeft((acc, x) -> acc - x, necOf2(1, 2)) == -1

    @test
    def reduceLeft03(): Bool = Nec.reduceLeft((acc, x) -> acc - x, necOf3(1, 2, 3)) == -4

    /////////////////////////////////////////////////////////////////////////////
    // reduceRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceRight01(): Bool = Nec.reduceRight((x, acc) -> acc - x, singleton(1)) == 1

    @test
    def reduceRight02(): Bool = Nec.reduceRight((x, acc) -> acc - x, necOf2(1, 2)) == 1

    @test
    def reduceRight03(): Bool = Nec.reduceRight((x, acc) -> acc - x, necOf3(1, 2, 3)) == 0

    /////////////////////////////////////////////////////////////////////////////
    // reduceLeftTo                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceLeftTo01(): Bool = Nec.reduceLeftTo((acc, x) -> acc - x, identity, singleton(1)) == 1

    @test
    def reduceLeftTo02(): Bool = Nec.reduceLeftTo((acc, x) -> acc - x, identity, necOf2(1, 2)) == -1

    @test
    def reduceLeftTo03(): Bool = Nec.reduceLeftTo((acc, x) -> acc - x, identity, necOf3(1, 2, 3)) == -4

    /////////////////////////////////////////////////////////////////////////////
    // reduceRightTo                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceRightTo01(): Bool = Nec.reduceRightTo((x, acc) -> acc - x, identity, singleton(1)) == 1

    @test
    def reduceRightTo02(): Bool = Nec.reduceRightTo((x, acc) -> acc - x, identity, necOf2(1, 2)) == 1

    @test
    def reduceRightTo03(): Bool = Nec.reduceRightTo((x, acc) -> acc - x, identity, necOf3(1, 2, 3)) == 0

    /////////////////////////////////////////////////////////////////////////////
    // hash                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def hash01(): Bool = hash(singleton(1)) == hash(singleton(1))

    @test
    def hash02(): Bool = hash(necOf3(1, 2, 3)) == hash(necOf3(1, 2, 3))

    @test
    def hash03(): Bool = hash(necOf3(1, 3, 2)) != hash(necOf3(1, 2, 3))

    @test
    def hash04(): Bool = hash(singleton(1)) != hash(singleton(2))

    @test
    def hash05(): Bool = hash(necOf3("c", "b", "a")) != hash(necOf3("c", "a", "b"))

    /////////////////////////////////////////////////////////////////////////////
    // dropWhileLeft                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropWhileLeft01(): Bool = Nec.dropWhileLeft(i -> i > 3, singleton(1)) == 1 :: Nil

    @test
    def dropWhileLeft02(): Bool = Nec.dropWhileLeft(i -> i > 3, singleton(4)) == Nil

    @test
    def dropWhileLeft03(): Bool = Nec.dropWhileLeft(i -> i > 3, necOf2(1, 2)) == 1 :: 2 :: Nil

    @test
    def dropWhileLeft04(): Bool = Nec.dropWhileLeft(i -> i > 3, necOf2(1, 5)) == 1 :: 5 :: Nil

    @test
    def dropWhileLeft05(): Bool = Nec.dropWhileLeft(i -> i > 3, necOf2(5, 1)) == 1 :: Nil

    @test
    def dropWhileLeft06(): Bool = Nec.dropWhileLeft(i -> i > 3, necOf2(5, 8)) == Nil

    @test
    def dropWhileLeft07(): Bool = Nec.dropWhileLeft(i -> i > 3, necOf8(4, 6, -3, 11, -5, 1, 2, 16)) ==
                            -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // dropWhileRight                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropWhileRight01(): Bool = Nec.dropWhileRight(i -> i > 3, singleton(1)) == 1 :: Nil

    @test
    def dropWhileRight02(): Bool = Nec.dropWhileRight(i -> i > 3, singleton(4)) == Nil

    @test
    def dropWhileRight03(): Bool = Nec.dropWhileRight(i -> i > 3, necOf2(1, 2)) == 1 :: 2 :: Nil

    @test
    def dropWhileRight04(): Bool = Nec.dropWhileRight(i -> i > 3, necOf2(1, 5)) == 1 :: Nil

    @test
    def dropWhileRight05(): Bool = Nec.dropWhileRight(i -> i > 3, necOf2(5, 1)) == 5 :: 1 :: Nil

    @test
    def dropWhileRight06(): Bool = Nec.dropWhileRight(i -> i > 3, necOf2(5, 8)) == Nil

    @test
    def dropWhileRight07(): Bool = Nec.dropWhileRight(i -> i > 3, necOf8(4, 6, -3, 11, -5, 1, 2, 16)) ==
                            4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // takeWhileLeft                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeWhileLeft01(): Bool = Nec.takeWhileLeft(i -> i > 3, singleton(1)) == Nil

    @test
    def takeWhileLeft02(): Bool = Nec.takeWhileLeft(i -> i > 3, singleton(4)) == 4 :: Nil

    @test
    def takeWhileLeft03(): Bool = Nec.takeWhileLeft(i -> i > 3, necOf2(1, 2)) == Nil

    @test
    def takeWhileLeft04(): Bool = Nec.takeWhileLeft(i -> i > 3, necOf2(1, 5)) == Nil

    @test
    def takeWhileLeft05(): Bool = Nec.takeWhileLeft(i -> i > 3, necOf2(5, 1)) == 5 :: Nil

    @test
    def takeWhileLeft06(): Bool = Nec.takeWhileLeft(i -> i > 3, necOf2(5, 8)) == 5 :: 8 :: Nil

    @test
    def takeWhileLeft08(): Bool = Nec.takeWhileLeft(i -> i > 3, necOf8(4, 6, -3, 11, -5, 1, 2, 16)) == 4 :: 6 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // takeWhileRight                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeWhileRight01(): Bool = Nec.takeWhileRight(i -> i > 3, singleton(1)) == Nil

    @test
    def takeWhileRight02(): Bool = Nec.takeWhileRight(i -> i > 3, singleton(4)) == 4 :: Nil

    @test
    def takeWhileRight03(): Bool = Nec.takeWhileRight(i -> i > 3, necOf2(1, 2)) == Nil

    @test
    def takeWhileRight04(): Bool = Nec.takeWhileRight(i -> i > 3, necOf2(1, 5)) == 5 :: Nil

    @test
    def takeWhileRight05(): Bool = Nec.takeWhileRight(i -> i > 3, necOf2(5, 1)) == Nil

    @test
    def takeWhileRight06(): Bool = Nec.takeWhileRight(i -> i > 3, necOf2(5, 8)) == 5 :: 8 :: Nil

    @test
    def takeWhileRight07(): Bool = Nec.takeWhileRight(i -> i > 3, necOf8(4, 6, -3, 11, -5, 1, 2, 16)) == 16 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // shuffle                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def shuffle01(): Bool \ IO =
        let c = Nec.singleton(1) |> Nec.shuffle(Random.fresh());
        match c {
            case Some(cc) => Nec.length(cc) == 1 and Nec.toSet(cc) == Set#{1}
            case _       => false
        }

    @test
    def shuffle02(): Bool \ IO =
        let c = necOf3(1, 2, 3) |> Nec.shuffle(Random.fresh());
        match c {
            case Some(cc) => Nec.length(cc) == 3 and Nec.toSet(cc) == Set#{1, 2, 3}
            case _       => false
        }

    @test
    def shuffle03(): Bool \ IO =
        let c = necOf8(1, 2, 3, 4, 5, 6, 7, 8) |> Nec.shuffle(Random.fresh());
        match c {
            case Some(cc) => Nec.length(cc) == 8 and Nec.toSet(cc) == Set#{1, 2, 3, 4, 5, 6, 7, 8}
            case _       => false
        }

}
