/*
 *  Copyright 2021 Stephen Tetley
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

namespace TestFoldable {

    use Applicative.{point, *>};

    /// To test foldLeftM, foldRightM, foreachM we want a monad with
    /// "side-effects" so we can log direction of travel.

    opaque type Logger[i, a] = (Chain[i], a)

    def tell(x: i): Logger[i, Unit] = Logger((Chain.singleton(x), ()))

    def runLogger(ma: Logger[i, a]): (List[i], a) =
        let Logger((w, a)) = ma;
        (Chain.toList(w), a)


    instance Functor[Logger[i]] {
        pub def map(f: a -> b & ef, ma: Logger[i, a]): Logger[i, b] & ef =
            let Logger((w, a)) = ma;
            Logger((w, f(a)))
    }

    instance Applicative[Logger[i]] {
        pub def point(x: a): Logger[i, a] = Logger(Chain.empty(), x)

        pub def ap(mf: Logger[i, a -> b & ef], ma: Logger[i, a]): Logger[i, b] & ef =
            let Logger((w1, f)) = mf;
            let Logger((w2, a)) = ma;
            Logger((Chain.append(w1, w2), f(a)))
        }


    instance Monad[Logger[i]] {
        pub def flatMap(f: a -> Logger[i, b] & ef, ma: Logger[i, a]): Logger[i, b] & ef =
            let Logger((w1, a)) = ma;
            let Logger((w2, b)) = f(a);
            Logger((Chain.append(w1, w2), b))
    }


    /////////////////////////////////////////////////////////////////////////////
    // head                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def head01(): Bool =
        Foldable.head(Nil: List[String]) == None

    @test
    def head02(): Bool =
        Foldable.head("a" :: Nil) == Some("a")

    @test
    def head03(): Bool =
        Foldable.head("a" :: "b" :: Nil) == Some("a")

    /////////////////////////////////////////////////////////////////////////////
    // last                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def last01(): Bool =
        Foldable.last(Nil: List[String]) == None

    @test
    def last02(): Bool =
        Foldable.last("a" :: Nil) == Some("a")

    @test
    def last03(): Bool =
        Foldable.last("a" :: "b" :: Nil) == Some("b")

    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findLeft01(): Bool =
        Foldable.findLeft(x -> x mod 2 == 0, Nil: List[Int32]) == None

    @test
    def findLeft02(): Bool =
        Foldable.findLeft(x -> x mod 2 == 0, 1 :: Nil) == None

    @test
    def findLeft03(): Bool =
        Foldable.findLeft(x -> x mod 2 == 0, 1 :: 2 :: Nil) == Some(2)

    @test
    def findLeft04(): Bool =
        Foldable.findLeft(x -> x mod 2 == 0, 1 :: 2 :: 3 :: 4 :: Nil) == Some(2)

    /////////////////////////////////////////////////////////////////////////////
    // fold                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def fold01(): Bool =
        Foldable.fold(Nil: List[String]) == ""

    @test
    def fold02(): Bool =
        Foldable.fold("a" :: Nil) == "a"

    @test
    def fold03(): Bool =
        Foldable.fold("a" :: "b" :: Nil) == "ab"

    /////////////////////////////////////////////////////////////////////////////
    // foldLeftM                                                                //
    /////////////////////////////////////////////////////////////////////////////

    def foldLeftStep(acc: Int32, a: Int32): Logger[Int32, Int32] =
        tell(a) *> point(acc+a)

    @test
    def foldLeftM01(): Bool =
        let xs: List[Int32] = Nil;
        runLogger(Foldable.foldLeftM(foldLeftStep, 0, xs)) == (Nil, 0)

    @test
    def foldLeftM02(): Bool =
        let xs = 1 :: 2 :: Nil;
        runLogger(Foldable.foldLeftM(foldLeftStep, 0, xs)) == (1 :: 2 :: Nil, 3)

    @test
    def foldLeftM03(): Bool =
        let xs = 1 :: 2 :: 3 :: 4 :: 5 :: Nil;
        runLogger(Foldable.foldLeftM(foldLeftStep, 0, xs)) == (1 :: 2 :: 3 :: 4 :: 5 :: Nil, 15)

    /////////////////////////////////////////////////////////////////////////////
    // foldRightM                                                              //
    /////////////////////////////////////////////////////////////////////////////

    def foldRightStep(a: Int32, acc: Int32): Logger[Int32, Int32] =
        tell(a) *> point(acc+a)

    @test
    def foldRightM01(): Bool =
        let xs: List[Int32] = Nil;
        runLogger(Foldable.foldRightM(foldRightStep, 0, xs)) == (Nil, 0)

    @test
    def foldRightM02(): Bool =
        let xs = 1 :: 2 :: Nil;
        runLogger(Foldable.foldRightM(foldRightStep, 0, xs)) == (2 :: 1 :: Nil, 3)

    @test
    def foldRightM03(): Bool =
        let xs = 1 :: 2 :: 3 :: 4 :: 5 :: Nil;
        runLogger(Foldable.foldRightM(foldRightStep, 0, xs)) == (5 :: 4 :: 3 :: 2 :: 1 :: Nil, 15)

    /////////////////////////////////////////////////////////////////////////////
    // foreachM                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foreachM01(): Bool =
        let xs: List[Int32] = Nil;
        runLogger(Foldable.foreachM(tell, xs)) == (Nil, ())

    @test
    def foreachM02(): Bool =
        let xs = 1 :: 2 :: Nil;
        runLogger(Foldable.foreachM(tell, xs)) == (1 :: 2 :: Nil, ())

    @test
    def foreachM03(): Bool =
        let xs = 1 :: 2 :: 3 :: 4 :: 5 :: Nil;
        runLogger(Foldable.foreachM(tell, xs)) == (1 :: 2 :: 3 :: 4 :: 5 :: Nil, ())

    /////////////////////////////////////////////////////////////////////////////
    // toMapWith                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMapWith01(): Bool =
        Foldable.toMapWith(x -> 0 * x, List.toChain(Nil)) == Map#{}

    @test
    def toMapWith02(): Bool =
        Foldable.toMapWith(x -> 0 * x, List.toChain(1 :: 2 :: Nil)) == Map#{1 => 0, 2 => 0}

    @test
    def toMapWith03(): Bool =
        Foldable.toMapWith(x -> 2 * x, List.toChain(1 :: 2 :: Nil)) == Map#{1 => 2, 2 => 4}

    @test
    def toMapWith04(): Bool =
        Foldable.toMapWith(x -> x + 10, List.toChain(1 :: 2 :: Nil)) == Map#{1 => 11, 2 => 12}

    @test
    def toMapWith05(): Bool =
        Foldable.toMapWith(x -> x + "b", List.toChain("aa" :: "a" :: Nil)) == Map#{"a" => "ab", "aa" => "aab"}


    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def join01(): Bool =
        Foldable.join(",", Nil: List[Int32]) == ""

    @test
    def join02(): Bool =
        Foldable.join(",", 1 :: Nil) == "1"

    @test
    def join03(): Bool =
        Foldable.join(",", 1 :: 2 :: 3 :: Nil) == "1,2,3"

    @test
    def join04(): Bool =
        Foldable.join(",", "1" :: "2" :: "3" :: Nil) == "1,2,3"


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def joinWith01(): Bool =
        Foldable.joinWith(x -> "${x + 1}", ",", Nil: List[Int32]) == ""

    @test
    def joinWith02(): Bool =
        Foldable.joinWith(x -> "${x + 1}", ",", 1 :: Nil) == "2"

    @test
    def joinWith03(): Bool =
        Foldable.joinWith(x -> "${x + 1}", ",", 1 :: 2 :: 3 :: Nil) == "2,3,4"

    @test
    def joinWith04(): Bool =
        Foldable.joinWith(x -> x + x, ",", "1" :: "2" :: "3" :: Nil) == "11,22,33"


    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toArray01(): Bool & Impure =
        Array.sameElements(Foldable.toArray(Nel(1, Nil)), [1])

    @test
    def toArray02(): Bool & Impure =
        Array.sameElements(Foldable.toArray(Nel(1, 2 :: 3 :: Nil)), [1, 2, 3])

    @test
    def toArray03(): Bool & Impure =
        Array.sameElements(Foldable.toArray(Nel(1, 1 :: 2 :: 3 :: Nil)), [1, 1, 2, 3])

}
