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

mod TestFoldable {

    use Applicative.{point, *>}

    /// To test foldLeftM, foldRightM, foreachM we want a monad with
    /// "side-effects" so we can log direction of travel.

    enum Logger[i, a](Chain[i], a)

    def tell(x: i): Logger[i, Unit] = Logger.Logger((Chain.singleton(x), ()))

    def runLogger(ma: Logger[i, a]): (List[i], a) =
        let Logger.Logger((w, a)) = ma;
        (Chain.toList(w), a)


    instance Functor[Logger[i]] {
        pub def map(f: a -> b \ ef, ma: Logger[i, a]): Logger[i, b] \ ef =
            let Logger.Logger((w, a)) = ma;
            Logger.Logger((w, f(a)))
    }

    instance Applicative[Logger[i]] {
        pub def point(x: a): Logger[i, a] = Logger.Logger(Chain.empty(), x)

        pub def ap(mf: Logger[i, a -> b \ ef], ma: Logger[i, a]): Logger[i, b] \ ef =
            let Logger.Logger((w1, f)) = mf;
            let Logger.Logger((w2, a)) = ma;
            Logger.Logger((Chain.append(w1, w2), f(a)))
        }


    instance Monad[Logger[i]] {
        pub def flatMap(f: a -> Logger[i, b] \ ef, ma: Logger[i, a]): Logger[i, b] \ ef =
            let Logger.Logger((w1, a)) = ma;
            let Logger.Logger((w2, b)) = f(a);
            Logger.Logger((Chain.append(w1, w2), b))
    }


    /////////////////////////////////////////////////////////////////////////////
    // head                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def head01(): Bool =
        Foldable.head((Nil: List[String])) == None

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
        Foldable.last((Nil: List[String])) == None

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
        Foldable.findLeft(x -> x `Int32.remainder` 2 == 0, (Nil: List[Int32])) == None

    @test
    def findLeft02(): Bool =
        Foldable.findLeft(x -> x `Int32.remainder` 2 == 0, 1 :: Nil) == None

    @test
    def findLeft03(): Bool =
        Foldable.findLeft(x -> x `Int32.remainder` 2 == 0, 1 :: 2 :: Nil) == Some(2)

    @test
    def findLeft04(): Bool =
        Foldable.findLeft(x -> x `Int32.remainder` 2 == 0, 1 :: 2 :: 3 :: 4 :: Nil) == Some(2)

    /////////////////////////////////////////////////////////////////////////////
    // foldMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldMap01(): Bool =
        Foldable.foldMap(x -> 2 * x, Nil) == 0

    @test
    def foldMap02(): Bool =
        Foldable.foldMap(x -> 2 * x, 1 :: 2 :: Nil) == 6

    @test
    def foldMap03(): Bool =
        Foldable.foldMap(x -> if (x == "a") "b" else x, "a" :: Nil) == "b"

    @test
    def foldMap04(): Bool =
        Foldable.foldMap(x -> if (x == "c") "b" else x, "a" :: "b" :: "c":: Nil) == "abb"

    @test
    def foldMap05(): Bool =
        Foldable.foldMap(Int32.toString, 1 :: 2 :: 3 :: Nil) == "123"

    /////////////////////////////////////////////////////////////////////////////
    // fold                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def fold01(): Bool =
        Foldable.fold((Nil: List[String])) == ""

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
    // length                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def length01(): Bool =
        Foldable.length((Nil: List[String])) == 0

    @test
    def length02(): Bool =
        Foldable.length("a" :: "b" :: Nil) == 2

    @test
    def length03(): Bool =
        Foldable.length(Some(5) :: None :: Some(18) :: None :: Nil) == 4

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def size01(): Bool =
        Foldable.size((Nil: List[String])) == 0

    @test
    def size02(): Bool =
        Foldable.size("a" :: "b" :: Nil) == 2

    @test
    def size03(): Bool =
        Foldable.size(Some(5) :: None :: Some(18) :: None :: Nil) == 4

    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def count01(): Bool =
        let f = x -> x == "a";
        Foldable.count(f, Nil) == 0

    @test
    def count02(): Bool =
        let xs = "a" :: "b" :: "a" :: "c" :: "a" :: Nil;
        let f = x -> x == "a";
        Foldable.count(f, xs) == 3

    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sum01(): Bool =
        Foldable.sum(None) == 0

    @test
    def sum02(): Bool =
        let num = Some(8);
        Foldable.sum(num) == 8

    @test
    def sum03(): Bool =
        let xs = 1 :: 2 :: 3 :: 4 :: 5 :: Nil;
        Foldable.sum(xs) == 15

    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumWith01(): Bool =
        let f = x -> x + 1;
        Foldable.sumWith(f, Nil) == 0

    @test
    def sumWith02(): Bool =
        let f = x -> x + 1;
        Foldable.sumWith(f, 1 :: 2 :: 3 :: 4 :: Nil) == 14

    @test
    def sumWith03(): Bool =
        let f = x -> match x {
            case None    => 0
            case Some(v) => 2 * v
        };
        Foldable.sumWith(f, Some(Some(5))) == 10

    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def exists01(): Bool =
        let f = x -> x == 3;
        Foldable.exists(f, Nil) == false

    @test
    def exists02(): Bool =
        let f = x -> x == "d";
        Foldable.exists(f, "a" :: "b" :: "c" :: "d" :: "e" :: Nil) == true

    @test
    def exists03(): Bool =
        let f = x -> x == "f";
        Foldable.exists(f, "a" :: "b" :: "c" :: "d" :: "e" :: Nil) == false

    /////////////////////////////////////////////////////////////////////////////
    // forAll                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forAll01(): Bool =
        let f = x -> x == 3;
        Foldable.forAll(f, Nil) == true

    @test
    def forAll02(): Bool =
        let f = x -> x == "a";
        Foldable.forAll(f, "a" :: "a" :: "a" :: "a" :: "a" :: Nil) == true

    @test
    def forAll03(): Bool =
        let f = x -> x == "a";
        Foldable.forAll(f, "a" :: "a" :: "a" :: "a" :: "b" :: Nil) == false

    /////////////////////////////////////////////////////////////////////////////
    // forEachM                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forEachM01(): Bool =
        let xs: List[Int32] = Nil;
        runLogger(Foldable.forEachM(tell, xs)) == (Nil, ())

    @test
    def forEachM02(): Bool =
        let xs = 1 :: 2 :: Nil;
        runLogger(Foldable.forEachM(tell, xs)) == (1 :: 2 :: Nil, ())

    @test
    def forEachM03(): Bool =
        let xs = 1 :: 2 :: 3 :: 4 :: 5 :: Nil;
        runLogger(Foldable.forEachM(tell, xs)) == (1 :: 2 :: 3 :: 4 :: 5 :: Nil, ())

    /////////////////////////////////////////////////////////////////////////////
    // toChain                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toChain01(): Bool =
        Foldable.toChain((Nil: List[Int32])) == (Chain.empty(): Chain[Int32])

    @test
    def toChain02(): Bool =
        Foldable.toChain(1 :: 2 :: Nil) == List.toChain(1 :: 2 :: Nil)

    @test
    def toChain03(): Bool =
        Foldable.toChain(Set#{0, 1, 2, 3, 4}) == List.toChain(0 :: 1 :: 2 :: 3 :: 4 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // toDelayList                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toDelayList01(): Bool =
        Foldable.toDelayList((Nil: List[Int32])) == (DelayList.empty(): DelayList[Int32])

    @test
    def toDelayList02(): Bool =
        Foldable.toDelayList(1 :: 2 :: Nil) == List.toDelayList(1 :: 2 :: Nil)

    @test
    def toDelayList03(): Bool =
        Foldable.toDelayList(Set#{0, 1, 2, 3, 4}) == List.toDelayList(0 :: 1 :: 2 :: 3 :: 4 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toArray01(): Bool = region rc {
        Array.sameElements(Foldable.toArray(rc, Nel.Nel(1, Nil)), Array#{1} @ rc)
    }

    @test
    def toArray02(): Bool = region rc {
        Array.sameElements(Foldable.toArray(rc, Nel.Nel(1, 2 :: 3 :: Nil)), Array#{1, 2, 3} @ rc)
    }

    @test
    def toArray03(): Bool = region rc {
        Array.sameElements(Foldable.toArray(rc, Nel.Nel(1, 1 :: 2 :: 3 :: Nil)), Array#{1, 1, 2, 3} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toVector                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toVector01(): Bool =
        Foldable.toVector(Nel.Nel(1, Nil)) == Vector#{1}

    @test
    def toVector02(): Bool =
        Foldable.toVector(Nel.Nel(1, 2 :: 3 :: Nil)) == Vector#{1, 2, 3}

    @test
    def toVector03(): Bool =
        Foldable.toVector(Nel.Nel(1, 1 :: 2 :: 3 :: Nil)) == Vector#{1, 1, 2, 3}

    /////////////////////////////////////////////////////////////////////////////
    // toMutDeque                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMutDeque01(): Bool = region rc {
        let l = (Nil: List[Unit]);
        let d = Foldable.toMutDeque(rc, l);

        l == MutDeque.toList(d)
    }

    @test
    def toMutDeque02(): Bool = region rc {
        let l = 1 :: 2 :: Nil;
        let d = Foldable.toMutDeque(rc, l);

        l == MutDeque.toList(d)
    }

    @test
    def toMutDeque03(): Bool = region rc {
        let l = 3 :: 1 :: 2 :: Nil;
        let d = Foldable.toMutDeque(rc, l);

        l == MutDeque.toList(d)
    }

    @test
    def toMutDeque04(): Bool = region rc {
        let l = 'a' :: 'b' :: 'c' :: Nil;
        let d = Foldable.toMutDeque(rc, l);

        l == MutDeque.toList(d)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toMutSet                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMutSet01(): Bool = region rc {
        let s1 = (Set#{}: Set[Int32]);
        let s2 = Foldable.toMutSet(rc, s1);

        s1 == MutSet.toSet(s2)
    }

    @test
    def toMutSet02(): Bool = region rc {
        let s1 = Set#{1, 2};
        let s2 = Foldable.toMutSet(rc, s1);

        s1 == MutSet.toSet(s2)
    }

    @test
    def toMutSet03(): Bool = region rc {
        let s1 = Set#{1, 2, 3};
        let s2 = Foldable.toMutSet(rc, s1);

        s1 == MutSet.toSet(s2)
    }

    @test
    def toMutSet04(): Bool = region rc {
        let s1 = Set#{'a', 'b', 'c'};
        let s2 = Foldable.toMutSet(rc, s1);

        s1 == MutSet.toSet(s2)
    }

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
    // toDelayMap                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toDelayMap01(): Bool =
        let l = (Nil: List[(Int32, Int32)]);
        let d1 = Foldable.toDelayMap(List.toChain(l));
        (d1 |> DelayMap.toList) == l

    @test
    def toDelayMap02(): Bool =
        let l = (1, 2) :: (2, 3) :: (3, 4) :: Nil;
        let d1 = Foldable.toDelayMap(List.toChain(l));
        (d1 |> DelayMap.toList) == l

    @test
    def toDelayMap03(): Bool =
        let s = Set#{(0, 2), (1, 4), (2, 6), (3, 8), (4, 10)};
        let d1 = Foldable.toDelayMap(s);
        (d1 |> DelayMap.toSet) == s

    /////////////////////////////////////////////////////////////////////////////
    // toMutMap                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMutMap01(): Bool = region rc {
        let l = (Nil: List[(Unit, Unit)]);
        let m = Foldable.toMutMap(rc, l);

        l == MutMap.toList(m)
    }

    @test
    def toMutMap02(): Bool = region rc {
        let l = (1, 2) :: (3, 4) :: Nil;
        let m = Foldable.toMutMap(rc, l);

        l == MutMap.toList(m)
    }

    @test
    def toMutMap03(): Bool = region rc {
        let l = (1, 4) :: (2, 3) :: (3, 4) :: Nil;
        let m = Foldable.toMutMap(rc, l);

        l == MutMap.toList(m)
    }

    @test
    def toMutMap04(): Bool = region rc {
        let l = (1, 'a') :: (2, 'b') :: (3, 'c') :: Nil;
        let m = Foldable.toMutMap(rc, l);

        l == MutMap.toList(m)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toNec                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toNec01(): Bool =
        Foldable.toNec((Nil: List[Int32])) == None

    @test
    def toNec02(): Bool =
        Foldable.toNec(1 :: 2 :: Nil) == Some(Nec.cons(1, Nec.singleton(2)))

    @test
    def toNec03(): Bool =
        Foldable.toNec(Set#{0, 1, 2, 3, 4}) == List.toNec(0 :: 1 :: 2 :: 3 :: 4 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // toNel                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toNel01(): Bool =
        Foldable.toNel((Nil: List[Int32])) == None

    @test
    def toNel02(): Bool =
        Foldable.toNel(1 :: 2 :: Nil) == Some(Nel.cons(1, Nel.singleton(2)))

    @test
    def toNel03(): Bool =
        Foldable.toNel(Set#{0, 1, 2, 3, 4}) == List.toNel(0 :: 1 :: 2 :: 3 :: 4 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def join01(): Bool =
        Foldable.join(",", (Nil: List[Int32])) == ""

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
        Foldable.joinWith(x -> "${x + 1}", ",", (Nil: List[Int32])) == ""

    @test
    def joinWith02(): Bool =
        Foldable.joinWith(x -> "${x + 1}", ",", 1 :: Nil) == "2"

    @test
    def joinWith03(): Bool =
        Foldable.joinWith(x -> "${x + 1}", ",", 1 :: 2 :: 3 :: Nil) == "2,3,4"

    @test
    def joinWith04(): Bool =
        Foldable.joinWith(x -> x + x, ",", "1" :: "2" :: "3" :: Nil) == "11,22,33"

}
