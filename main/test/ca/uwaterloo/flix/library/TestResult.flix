/*
 * Copyright 2017 Liam Palmer
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

mod TestResult {

    use Hash.hash
    use Validation.{Success, Failure}


    /////////////////////////////////////////////////////////////////////////////
    // Functor.map                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def functor_map01(): Bool = Functor.map(i -> i == 2, Err(-1ii)) == Err(-1ii)

    @test
    def functor_map02(): Bool = (Functor.map(i -> i == 2, Ok(1)): Result[Unit, Bool]) == Ok(false)

    @test
    def functor_map03(): Bool = (Functor.map(i -> i == 2, Ok(2)): Result[Unit, Bool]) == Ok(true)

    /////////////////////////////////////////////////////////////////////////////
    // Monad.flatMap                                                           //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def monad_flatMap01(): Bool = Monad.flatMap(i -> if (i == 2) Ok(i) else Err(false), Err(true)) == Err(true)

    @test
    def monad_flatMap02(): Bool = Monad.flatMap(i -> if (i == 2) Ok(i) else Err(false), Ok(1)) == Err(false)

    @test
    def monad_flatMap03(): Bool = Monad.flatMap(i -> if (i == 2) Ok(2*i) else Err(false), Ok(2)) == Ok(4)

    /////////////////////////////////////////////////////////////////////////////
    // Applicative.ap                                                          //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def applicative_ap01(): Bool = (Applicative.ap(Ok(x -> x + 1), Ok(123)): Result[String, Int32]) == Ok(124)

    @test
    def applicative_ap02(): Bool = Applicative.ap(Ok(x -> x + 1), Err("b")) == Err("b")

    @test
    def applicative_ap03(): Bool = (Applicative.ap(Err("a"), Ok(123)): Result[String, Int32]) == Err("a")

    @test
    def applicative_ap04(): Bool = (Applicative.ap(Err("a"), Err("b")): Result[String, Int32]) == Err("a")

    /////////////////////////////////////////////////////////////////////////////
    // isOk                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def isOk01(): Bool = Result.isOk(Ok(1)) == true

    @test
    def isOk02(): Bool = Result.isOk(Err(1)) == false

    /////////////////////////////////////////////////////////////////////////////
    // isErr                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def isErr01(): Bool = Result.isErr(Ok(1)) == false

    @test
    def isErr02(): Bool = Result.isErr(Err(1)) == true

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def size01(): Bool = Result.size(Ok(1)) == 1

    @test
    def size02(): Bool = Result.size(Err(1)) == 0

    /////////////////////////////////////////////////////////////////////////////
    // getWithDefault                                                          //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def getWithDefault01(): Bool = Result.getWithDefault(2, Ok(1)) == 1

    @test
    def getWithDefault02(): Bool = Result.getWithDefault(2, Err(false)) == 2

    /////////////////////////////////////////////////////////////////////////////
    // withDefault                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def withDefault01(): Bool = (Result.withDefault({default = Ok(2)}, Ok(1)): Result[Unit, Int32]) == Ok(1)

    @test
    def withDefault02(): Bool = Result.withDefault({default = Err(false)}, Ok(1)) == Ok(1)

    @test
    def withDefault03(): Bool = (Result.withDefault({default = Ok(2)}, Err(false)): Result[Unit, Int32]) == Ok(2)

    @test
    def withDefault04(): Bool = (Result.withDefault({default = Err(true)}, Err(false)): Result[Bool, Unit]) == Err(true)

    /////////////////////////////////////////////////////////////////////////////
    // replace                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def replace01(): Bool = Result.replace(src =  3, dst =  4, Err(false)) == Err(false)

    @test
    def replace02(): Bool = (Result.replace(src =  3, dst =  4, (Ok(2): Result[Unit, Int32]))) == Ok(2)

    @test
    def replace03(): Bool = (Result.replace(src =  3, dst =  4, (Ok(3): Result[Unit, Int32]))) == Ok(4)

    @test
    def replace04(): Bool = (Result.replace(src =  3, dst =  4, (Ok(4): Result[Unit, Int32]))) == Ok(4)

    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def exists01(): Bool = Result.exists(i -> i == 2, Err(false)) == false

    @test
    def exists02(): Bool = Result.exists(i -> i == 2, Ok(1)) == false

    @test
    def exists03(): Bool = Result.exists(i -> i == 2, Ok(2)) == true

    /////////////////////////////////////////////////////////////////////////////
    // forAll                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def forAll01(): Bool = Result.forAll(i -> i == 2, Err(false)) == true

    @test
    def forAll02(): Bool = Result.forAll(i -> i == 2, Ok(1)) == false

    @test
    def forAll03(): Bool = Result.forAll(i -> i == 2, Ok(2)) == true

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def map01(): Bool = Result.map(i -> i == 2, Err(-1ii)) == Err(-1ii)

    @test
    def map02(): Bool = (Result.map(i -> i == 2, Ok(1)): Result[Unit, Bool]) == Ok(false)

    @test
    def map03(): Bool = (Result.map(i -> i == 2, Ok(2)): Result[Unit, Bool]) == Ok(true)

    /////////////////////////////////////////////////////////////////////////////
    // mapErr                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def mapErr01(): Bool = Result.mapErr(i -> i == 2, Ok(1)) == Ok(1)

    @test
    def mapErr02(): Bool = (Result.mapErr(i -> i == 2, Err(1)): Result[Bool, Unit]) == Err(false)

    @test
    def mapErr03(): Bool = (Result.mapErr(i -> i == 2, Err(2)): Result[Bool, Unit]) == Err(true)

    /////////////////////////////////////////////////////////////////////////////
    // flatMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def flatMap01(): Bool = Result.flatMap(i -> if (i == 2) Ok(i) else Err(false), Err(true)) == Err(true)

    @test
    def flatMap02(): Bool = Result.flatMap(i -> if (i == 2) Ok(i) else Err(false), Ok(1)) == Err(false)

    @test
    def flatMap03(): Bool = Result.flatMap(i -> if (i == 2) Ok(2*i) else Err(false), Ok(2)) == Ok(4)


    /////////////////////////////////////////////////////////////////////////////
    // flatten                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def flatten01(): Bool = {
        let err: Result[Int32, Unit] = Err(42);
        Result.flatten(Ok(err)) == Err(42)
    }

    @test
    def flatten02(): Bool = {
        let err: Result[Int32, Result[Int32, Int32]] = Err(42);
        Result.flatten(err) == Err(42)
    }

    @test
    def flatten03(): Bool = {
        let err: Result[Int32, Result[Int32, Int32]] = Ok(Ok(42));
        Result.flatten(err) == Ok(42)
    }

    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def count01(): Bool = Result.count(i -> i == 2, Err(false)) == 0

    @test
    def count02(): Bool = Result.count(i -> i == 2, Ok(1)) == 0

    @test
    def count03(): Bool = Result.count(i -> i == 2, Ok(2)) == 1


    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sum01(): Bool = Result.sum(Err(false)) == 0

    @test
    def sum02(): Bool = Result.sum(Ok(1)) == 1

    @test
    def sum03(): Bool = Result.sum(Ok(-2)) == -2


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumWith01(): Bool = Result.sumWith(x -> x + 1, Err(false)) == 0

    @test
    def sumWith02(): Bool = Result.sumWith(x -> x + 1, Ok(1)) == 2

    @test
    def sumWith03(): Bool = Result.sumWith(x -> x + 1, Ok(-2)) == -1


    /////////////////////////////////////////////////////////////////////////////
    // find                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def find01(): Bool = Result.find(i -> i == 2, Err(false)) == None

    @test
    def find02(): Bool = Result.find(i -> i == 2, Ok(1)) == None

    @test
    def find03(): Bool = Result.find(i -> i == 2, Ok(2)) == Some(2)

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def foldLeft01(): Bool = Result.foldLeft((b, i) -> if (i == 2 and b) true else false, false, Err(-1ii)) == false

    @test
    def foldLeft02(): Bool = Result.foldLeft((b, i) -> if (i == 2 and b) true else false, false, Ok(1)) == false

    @test
    def foldLeft03(): Bool = Result.foldLeft((b, i) -> if (i == 2 and b) true else false, true, Ok(1)) == false

    @test
    def foldLeft04(): Bool = Result.foldLeft((b, i) -> if (i == 2 and b) true else false, false, Ok(2)) == false

    @test
    def foldLeft05(): Bool = Result.foldLeft((b, i) -> if (i == 2 and b) true else false, true, Ok(2)) == true

    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def foldRight01(): Bool = Result.foldRight((i, acc) -> if (i == 2 and acc) true else false, false, Err(-1ii)) == false

    @test
    def foldRight02(): Bool = Result.foldRight((i, acc) -> if (i == 2 and acc) true else false, false, Ok(1)) == false

    @test
    def foldRight03(): Bool = Result.foldRight((i, acc) -> if (i == 2 and acc) true else false, true, Ok(1)) == false

    @test
    def foldRight04(): Bool = Result.foldRight((i, acc) -> if (i == 2 and acc) true else false, false, Ok(2)) == false

    @test
    def foldRight05(): Bool = Result.foldRight((i, acc) -> if (i == 2 and acc) true else false, true, Ok(2)) == true

    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithCont                                                       //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def foldRightWithCont01(): Bool = Result.foldRightWithCont((i, k) -> if (i == 2 and k()) true else false, false, Err(-1ii)) == false

    @test
    def foldRightWithCont02(): Bool = Result.foldRightWithCont((i, k) -> if (i == 2 and k()) true else false, false, Ok(1)) == false

    @test
    def foldRightWithCont03(): Bool = Result.foldRightWithCont((i, k) -> if (i == 2 and k()) true else false, true, Ok(1)) == false

    @test
    def foldRightWithCont04(): Bool = Result.foldRightWithCont((i, k) -> if (i == 2 and k()) true else false, false, Ok(2)) == false

    @test
    def foldRightWithCont05(): Bool = Result.foldRightWithCont((i, k) -> if (i == 2 and k()) true else false, true, Ok(2)) == true


    /////////////////////////////////////////////////////////////////////////////
    // sequence                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def sequence01(): Bool = (Result.sequence(Nil): Result[Unit, List[Unit]]) == Ok(Nil)

    @test
    def sequence02(): Bool = (Result.sequence(Ok(1) :: Nil): Result[Unit, List[Int32]]) == Ok(1 :: Nil)

    @test
    def sequence03(): Bool = (Result.sequence(Ok(1) :: Ok(2) :: Nil): Result[Unit, List[Int32]]) == Ok(1 :: 2 :: Nil)

    @test
    def sequence04(): Bool = (Result.sequence(Ok(1) :: Ok(2) :: Ok(3) :: Nil): Result[Unit, List[Int32]]) == Ok(1 :: 2 :: 3 :: Nil)

    @test
    def sequence05(): Bool = Result.sequence(Err("one") :: Ok(2) :: Ok(3) :: Nil) == Err("one")

    @test
    def sequence06(): Bool = Result.sequence(Ok(1) :: Ok(2) :: Err("three") :: Nil) == Err("three")

    @test
    def sequence07(): Bool = Result.sequence(Ok(1) :: Err("two") :: Ok(3) :: Nil) == Err("two")

    /////////////////////////////////////////////////////////////////////////////
    // traverse                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def traverse01(): Bool = (Result.traverse(x -> Ok(x + 1), Nil): Result[Unit, List[Int32]]) == Ok(Nil)

    @test
    def traverse02(): Bool = (Result.traverse(x -> Ok(x + 1), 1 :: Nil): Result[Unit, List[Int32]]) == Ok(2 :: Nil)

    @test
    def traverse03(): Bool = (Result.traverse(x -> Ok(x + 1), 1 :: 2 :: Nil): Result[Unit, List[Int32]]) == Ok(2 :: 3 :: Nil)

    @test
    def traverse04(): Bool = (Result.traverse(x -> Ok(x + 1), 1 :: 2 :: 3 :: Nil): Result[Unit, List[Int32]]) == Ok(2 :: 3 :: 4 :: Nil)

    @test
    def traverse05(): Bool = Result.traverse(x -> if (x == 1) Err("one") else Ok(x), 1 :: 2 :: 3 :: Nil) == Err("one")

    @test
    def traverse06(): Bool = Result.traverse(x -> if (x == 3) Err("three") else Ok(x), 1 :: 2 :: 3 :: Nil) == Err("three")

    @test
    def traverse07(): Bool = Result.traverse(x -> if (x == 2) Err("two") else Ok(x), 1 :: 2 :: 3 :: Nil) == Err("two")

    @test
    def traverse08(): Bool = (Result.traverse(_ -> Ok(42), 1 :: 2 :: 3 :: Nil): Result[Unit, List[Int32]]) == Ok(42 :: 42 :: 42 :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // traverseX                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def traverseX01(): Bool = (Result.traverseX(x -> Ok(x + 1), Nil): Result[String, Unit]) == Ok()

    @test
    def traverseX02(): Bool = (Result.traverseX(x -> Ok(x + 1), 1 :: Nil): Result[String, Unit]) == Ok()

    @test
    def traverseX03(): Bool = (Result.traverseX(x -> Ok(x + 1), 1 :: 2 :: Nil): Result[String, Unit]) == Ok()

    @test
    def traverseX04(): Bool = (Result.traverseX(x -> Ok(x + 1), 1 :: 2 :: 3 :: Nil): Result[String, Unit]) == Ok()

    @test
    def traverseX05(): Bool = Result.traverseX(x -> if (x == 1) Err("one") else Ok(x), 1 :: 2 :: 3 :: Nil) == Err("one")

    @test
    def traverseX06(): Bool = Result.traverseX(x -> if (x == 3) Err("three") else Ok(x), 1 :: 2 :: 3 :: Nil) == Err("three")

    @test
    def traverseX07(): Bool = Result.traverseX(x -> if (x == 2) Err("two") else Ok(x), 1 :: 2 :: 3 :: Nil) == Err("two")

    @test
    def traverseX08(): Bool = (Result.traverseX(_ -> Ok(42), 1 :: 2 :: 3 :: Nil): Result[String, Unit]) == Ok()

    /////////////////////////////////////////////////////////////////////////////
    // foldLeftM                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def foldLeftM01(): Bool = Result.foldLeftM((ac, i) -> if (i < 0) Err(-1) else Ok(Int32.max(i, ac)), 0, Nil) == Ok(0)

    @test
    def foldLeftM02(): Bool = Result.foldLeftM((ac, i) -> if (i < 0) Err(-1) else Ok(Int32.max(i, ac)), 0, 1 :: Nil) == Ok(1)

    @test
    def foldLeftM03(): Bool = Result.foldLeftM((ac, i) -> if (i < 0) Err(-1) else Ok(Int32.max(i, ac)), 0, 1 :: 2 :: Nil) == Ok(2)

    @test
    def foldLeftM04(): Bool = Result.foldLeftM((ac, i) -> if (i < 0) Err(-1) else Ok(Int32.max(i, ac)), 0, 1 :: 2 :: 3 :: Nil) == Ok(3)

    @test
    def foldLeftM05(): Bool = Result.foldLeftM((ac, i) -> if (i < 0) Err(-1) else Ok(Int32.max(i, ac)), 0, -1 :: 2 :: 3 :: Nil) == Err(-1)

    @test
    def foldLeftM06(): Bool = Result.foldLeftM((ac, i) -> if (i < 0) Err(-1) else Ok(Int32.max(i, ac)), 0, 1 :: -2 :: 3 :: Nil) == Err(-1)

    @test
    def foldLeftM07(): Bool = Result.foldLeftM((ac, i) -> if (i < 0) Err(-1) else Ok(Int32.max(i, ac)), 0, 1 :: 2 :: -3 :: Nil) == Err(-1)

    /////////////////////////////////////////////////////////////////////////////
    // foldRightM                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def foldRightM01(): Bool = Result.foldRightM((i, ac) -> if (i < 0) Err(-1) else Ok(Int32.max(i, ac)), 0, Nil) == Ok(0)

    @test
    def foldRightM02(): Bool = Result.foldRightM((i, ac) -> if (i < 0) Err(-1) else Ok(Int32.max(i, ac)), 0, 1 :: Nil) == Ok(1)

    @test
    def foldRightM03(): Bool = Result.foldRightM((i, ac) -> if (i < 0) Err(-1) else Ok(Int32.max(i, ac)), 0, 1 :: 2 :: Nil) == Ok(2)

    @test
    def foldRightM04(): Bool = Result.foldRightM((i, ac) -> if (i < 0) Err(-1) else Ok(Int32.max(i, ac)), 0, 1 :: 2 :: 3 :: Nil) == Ok(3)

    @test
    def foldRightM05(): Bool = Result.foldRightM((i, ac) -> if (i < 0) Err(-1) else Ok(Int32.max(i, ac)), 0, -1 :: 2 :: 3 :: Nil) == Err(-1)

    @test
    def foldRightM06(): Bool = Result.foldRightM((i, ac) -> if (i < 0) Err(-1) else Ok(Int32.max(i, ac)), 0, 1 :: -2 :: 3 :: Nil) == Err(-1)

    @test
    def foldRightM07(): Bool = Result.foldRightM((i, ac) -> if (i < 0) Err(-1) else Ok(Int32.max(i, ac)), 0, 1 :: 2 :: -3 :: Nil) == Err(-1)

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def toList01(): Bool = Result.toList((Err(-1ii): Result[BigInt, Unit])) == Nil

    @test
    def toList02(): Bool = Result.toList(Ok(1)) == 1 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def toSet01(): Bool = Result.toSet((Err(-1ii): Result[BigInt, Unit])) == Set#{}

    @test
    def toSet02(): Bool = Result.toSet(Ok(1)) == Set#{1}

    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def toMap01(): Bool = Result.toMap((Err(-1ii): Result[BigInt, (Unit, Unit)])) == Map#{}

    @test
    def toMap02(): Bool = Result.toMap(Ok((1, true))) == Map#{1 => true}

    /////////////////////////////////////////////////////////////////////////////
    // toMapWith                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMapWith01(): Bool =
        Result.toMapWith(x -> 0 * x, Err(1)) == Map#{}

    @test
    def toMapWith02(): Bool =
        Result.toMapWith(x -> 0 * x, Ok(1)) == Map#{1 => 0}

    @test
    def toMapWith03(): Bool =
        Result.toMapWith(x -> 2 * x, Ok(1)) == Map#{1 => 2}

    @test
    def toMapWith04(): Bool =
        Result.toMapWith(x -> x + 10, Ok(1)) == Map#{1 => 11}

    @test
    def toMapWith05(): Bool =
        Result.toMapWith(x -> x + "b", Ok("a")) == Map#{"a" => "ab"}

    /////////////////////////////////////////////////////////////////////////////
    // toOption                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def toOption01(): Bool = Result.toOption((Err(-1ii): Result[BigInt, Unit])) == None

    @test
    def toOption02(): Bool = Result.toOption(Ok(1)) == Some(1)

    /////////////////////////////////////////////////////////////////////////////
    // toValidation                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @test
    pub def toValidation01(): Bool =
        Result.toValidation((Err(1): Result[Int32, Unit])) == Failure(Nec.singleton(1))

    @test
    pub def toValidation02(): Bool =
        Result.toValidation((Ok("s"): Result[Unit, String])) == Success("s")

    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forEach01(): Bool = region rc {
        let ri = Ref.fresh(rc, 21);
        Result.forEach(x -> Ref.put(x, ri), (Ok(42): Result[Int32, Int32]));
        42 == deref ri
    }

    @test
    def forEach02(): Bool = region rc {
        let ri = Ref.fresh(rc, 21);
        Result.forEach(x -> Ref.put(x, ri), (Err(42): Result[Int32, Int32]));
        21 == deref ri
    }

    /////////////////////////////////////////////////////////////////////////////
    // ap                                                                      //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def ap01(): Bool = Result.ap(Ok(x -> x + 1), (Ok(123): Result[String, Int32])) == Ok(124)

    @test
    def ap02(): Bool = Result.ap(Ok(x -> x + 1), Err("b")) == Err("b")

    @test
    def ap03(): Bool = (Result.ap(Err("a"), Ok(123)): Result[String, Int32]) == Err("a")

    @test
    def ap04(): Bool = (Result.ap(Err("a"), Err("b")): Result[String, Int32]) == Err("a")

    /////////////////////////////////////////////////////////////////////////////
    // map2                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map201(): Bool = (Result.map2((x1,x2) -> x1 + x2, Ok(123), Ok(1)): Result[String, Int32]) == Ok(124)

    @test
    def map202(): Bool = Result.map2((x1,x2) -> x1 + x2, Ok(123), Err("e1")) == Err("e1")

    @test
    def map203(): Bool = Result.map2((x1,x2) -> x1 + x2, Err("e1"), Ok(1)) == Err("e1")

    @test
    def map204(): Bool = Result.map2((x1,x2) -> x1 + x2, Err("e1"), (Err("e2"): Result[String, Int32])) == Err("e1")

    /////////////////////////////////////////////////////////////////////////////
    // map3                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map301(): Bool = (Result.map3((x1,x2,x3) -> x1 + x2 + x3, Ok(123), Ok(1), Ok(2)): Result[String, Int32]) == Ok(126)

    @test
    def map302(): Bool = Result.map3((x1,x2,x3) -> x1 + x2 + x3, Ok(123), Ok(1), Err("e1")) == Err("e1")

    @test
    def map303(): Bool = Result.map3((x1,x2,x3) -> x1 + x2 + x3, Ok(123), Err("e1"), Ok(2)) == Err("e1")

    @test
    def map304(): Bool = Result.map3((x1,x2,x3) -> x1 + x2 + x3, Err("e1"), Ok(1), Ok(2)) == Err("e1")

    @test
    def map305(): Bool = Result.map3((x1,x2,x3) -> x1 + x2 + x3, Ok(123), Err("e1"), Err("e2")) == Err("e1")

    @test
    def map306(): Bool = Result.map3((x1,x2,x3) -> x1 + x2 + x3, Err("e1"), Ok(1), Err("e2")) == Err("e1")

    @test
    def map307(): Bool = Result.map3((x1,x2,x3) -> x1 + x2 + x3, Err("e1"), Err("e2"), (Err("e3"): Result[String, Int32])) == Err("e1")

    /////////////////////////////////////////////////////////////////////////////
    // map4                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map401(): Bool = (Result.map4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Ok(123), Ok(1), Ok(2), Ok(3)): Result[String, Int32]) == Ok(129)

    @test
    def map402(): Bool = Result.map4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Ok(123), Ok(1), Ok(2), Err("e1")) == Err("e1")

    @test
    def map403(): Bool = Result.map4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Ok(123), Ok(1), Err("e1"), Ok(3)) == Err("e1")

    @test
    def map404(): Bool = Result.map4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Ok(123), Err("e1"), Ok(2), Ok(3)) == Err("e1")

    @test
    def map405(): Bool = Result.map4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Err("e1"), Ok(1), Ok(2), Ok(3)) == Err("e1")

    @test
    def map406(): Bool = Result.map4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Err("e1"), Err("e2"), Err("e3"), (Err("e4"): Result[String, Int32])) == Err("e1")

    /////////////////////////////////////////////////////////////////////////////
    // map5                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map501(): Bool = (Result.map5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4)): Result[String, Int32]) == Ok(133)

    @test
    def map502(): Bool = Result.map5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Ok(123), Ok(1), Ok(2), Ok(3), Err("e1")) == Err("e1")

    @test
    def map503(): Bool = Result.map5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Ok(123), Ok(1), Ok(2), Err("e1"), Ok(4)) == Err("e1")

    @test
    def map504(): Bool = Result.map5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Ok(123), Ok(1), Err("e1"), Ok(3), Ok(4)) == Err("e1")

    @test
    def map505(): Bool = Result.map5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Ok(123), Err("e1"), Ok(2), Ok(3), Ok(4)) == Err("e1")

    @test
    def map506(): Bool = Result.map5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Err("e1"), Ok(1), Ok(2), Ok(3), Ok(4)) == Err("e1")

    @test
    def map507(): Bool = Result.map5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Err("e1"), Err("e2"), Err("e3"), Err("e4"), (Err("e5"): Result[String, Int32])) == Err("e1")

    /////////////////////////////////////////////////////////////////////////////
    // map6                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map601(): Bool = (Result.map6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5)): Result[String, Int32]) == Ok(138)

    @test
    def map602(): Bool = Result.map6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Err("e1")) == Err("e1")

    @test
    def map603(): Bool = Result.map6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Ok(123), Ok(1), Ok(2), Ok(3), Err("e1"), Ok(5)) == Err("e1")

    @test
    def map604(): Bool = Result.map6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Ok(123), Ok(1), Ok(2), Err("e1"), Ok(4), Ok(5)) == Err("e1")

    @test
    def map605(): Bool = Result.map6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Ok(123), Ok(1), Err("e1"), Ok(3), Ok(4), Ok(5)) == Err("e1")

    @test
    def map606(): Bool = Result.map6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Ok(123), Err("e1"), Ok(2), Ok(3), Ok(4), Ok(5)) == Err("e1")

    @test
    def map607(): Bool = Result.map6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Err("e1"), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5)) == Err("e1")

    @test
    def map608(): Bool = Result.map6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Err("e1"), Err("e2"), Err("e3"), Err("e4"), Err("e5"), (Err("e6"): Result[String, Int32])) == Err("e1")

    /////////////////////////////////////////////////////////////////////////////
    // map7                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map701(): Bool = (Result.map7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6)): Result[String, Int32]) == Ok(144)

    @test
    def map702(): Bool = Result.map7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Err("e1")) == Err("e1")

    @test
    def map703(): Bool = Result.map7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Err("e1"), Ok(6)) == Err("e1")

    @test
    def map704(): Bool = Result.map7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Ok(123), Ok(1), Ok(2), Ok(3), Err("e1"), Ok(5), Ok(6)) == Err("e1")

    @test
    def map705(): Bool = Result.map7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Ok(123), Ok(1), Ok(2), Err("e1"), Ok(4), Ok(5), Ok(6)) == Err("e1")

    @test
    def map706(): Bool = Result.map7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Ok(123), Ok(1), Err("e1"), Ok(3), Ok(4), Ok(5), Ok(6)) == Err("e1")

    @test
    def map707(): Bool = Result.map7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Ok(123), Err("e1"), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6)) == Err("e1")

    @test
    def map708(): Bool = Result.map7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Err("e1"), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6)) == Err("e1")

    @test
    def map709(): Bool = Result.map7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Err("e1"), Err("e2"), Err("e3"), Err("e4"), Err("e5"), Err("e6"), (Err("e7"): Result[String, Int32])) == Err("e1")

    /////////////////////////////////////////////////////////////////////////////
    // map8                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map801(): Bool = (Result.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6), Ok(7)): Result[String, Int32]) == Ok(151)

    @test
    def map802(): Bool = Result.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6), Err("e1")) == Err("e1")

    @test
    def map803(): Bool = Result.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Err("e1"), Ok(7)) == Err("e1")

    @test
    def map804(): Bool = Result.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Err("e1"), Ok(6), Ok(7)) == Err("e1")

    @test
    def map805(): Bool = Result.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Ok(123), Ok(1), Ok(2), Ok(3), Err("e1"), Ok(5), Ok(6), Ok(7)) == Err("e1")

    @test
    def map806(): Bool = Result.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Ok(123), Ok(1), Ok(2), Err("e1"), Ok(4), Ok(5), Ok(6), Ok(7)) == Err("e1")

    @test
    def map807(): Bool = Result.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Ok(123), Ok(1), Err("e1"), Ok(3), Ok(4), Ok(5), Ok(6), Ok(7)) == Err("e1")

    @test
    def map808(): Bool = Result.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Ok(123), Err("e1"), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6), Ok(7)) == Err("e1")

    @test
    def map809(): Bool = Result.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Err("e1"), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6), Ok(7)) == Err("e1")

    @test
    def map810(): Bool = Result.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Err("e1"), Err("e2"), Err("e3"), Err("e4"), Err("e5"), Err("e6"), Err("e7"), (Err("e8"): Result[String, Int32])) == Err("e1")

    /////////////////////////////////////////////////////////////////////////////
    // map9                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map901(): Bool = (Result.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6), Ok(7), Ok(8)): Result[String, Int32]) == Ok(159)

    @test
    def map902(): Bool = Result.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6), Ok(7), Err("e1")) == Err("e1")

    @test
    def map903(): Bool = Result.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6), Err("e1"), Ok(8)) == Err("e1")

    @test
    def map904(): Bool = Result.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Err("e1"), Ok(7), Ok(8)) == Err("e1")

    @test
    def map905(): Bool = Result.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Err("e1"), Ok(6), Ok(7), Ok(8)) == Err("e1")

    @test
    def map906(): Bool = Result.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Ok(123), Ok(1), Ok(2), Ok(3), Err("e1"), Ok(5), Ok(6), Ok(7), Ok(8)) == Err("e1")

    @test
    def map907(): Bool = Result.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Ok(123), Ok(1), Ok(2), Err("e1"), Ok(4), Ok(5), Ok(6), Ok(7), Ok(8)) == Err("e1")

    @test
    def map908(): Bool = Result.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Ok(123), Ok(1), Err("e1"), Ok(3), Ok(4), Ok(5), Ok(6), Ok(7), Ok(8)) == Err("e1")

    @test
    def map909(): Bool = Result.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Ok(123), Err("e1"), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6), Ok(7), Ok(8)) == Err("e1")

    @test
    def map910(): Bool = Result.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Err("e1"), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6), Ok(7), Ok(8)) == Err("e1")

    @test
    def map911(): Bool = Result.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Err("e1"), Err("e2"), Err("e3"), Err("e4"), Err("e5"), Err("e6"), Err("e7"), Err("e8"), (Err("e9"): Result[String, Int32])) == Err("e1")

    /////////////////////////////////////////////////////////////////////////////
    // map10                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map1001(): Bool = (Result.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6), Ok(7), Ok(8), Ok(9)): Result[String, Int32]) == Ok(168)

    @test
    def map1002(): Bool = Result.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6), Ok(7), Ok(8), Err("e1")) == Err("e1")

    @test
    def map1003(): Bool = Result.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6), Ok(7), Err("e1"), Ok(9)) == Err("e1")

    @test
    def map1004(): Bool = Result.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6), Err("e1"), Ok(8), Ok(9)) == Err("e1")

    @test
    def map1005(): Bool = Result.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Err("e1"), Ok(7), Ok(8), Ok(9)) == Err("e1")

    @test
    def map1006(): Bool = Result.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Err("e1"), Ok(6), Ok(7), Ok(8), Ok(9)) == Err("e1")

    @test
    def map1007(): Bool = Result.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Ok(123), Ok(1), Ok(2), Ok(3), Err("e1"), Ok(5), Ok(6), Ok(7), Ok(8), Ok(9)) == Err("e1")

    @test
    def map1008(): Bool = Result.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Ok(123), Ok(1), Ok(2), Err("e1"), Ok(4), Ok(5), Ok(6), Ok(7), Ok(8), Ok(9)) == Err("e1")

    @test
    def map1009(): Bool = Result.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Ok(123), Ok(1), Err("e1"), Ok(3), Ok(4), Ok(5), Ok(6), Ok(7), Ok(8), Ok(9)) == Err("e1")

    @test
    def map1010(): Bool = Result.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Ok(123), Err("e1"), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6), Ok(7), Ok(8), Ok(9)) == Err("e1")

    @test
    def map1011(): Bool = Result.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Err("e1"), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6), Ok(7), Ok(8), Ok(9)) == Err("e1")

    @test
    def map1012(): Bool = Result.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Err("e1"), Err("e2"), Err("e3"), Err("e4"), Err("e5"), Err("e6"), Err("e7"), Err("e8"), Err("e9"), (Err("e10"): Result[String, Int32])) == Err("e1")

    /////////////////////////////////////////////////////////////////////////////
    // hash                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def hash01(): Bool = hash((Err(()): Result[Unit, Unit])) == hash((Err(()): Result[Unit, Unit]))

    @test
    def hash02(): Bool = hash((Ok(()): Result[Unit, Unit])) != hash((Err(()): Result[Unit, Unit]))

    @test
    def hash03(): Bool = hash((Ok("Success"): Result[Unit, String])) == hash((Ok("Success"): Result[Unit, String]))

    @test
    def hash04(): Bool = hash((Ok("Success"): Result[String, String])) != hash((Err("Error"): Result[String, String]))

    @test
    def hash05(): Bool = hash((Err(13i8): Result[Int8, Unit])) != hash((Err(255i16): Result[Int16, Unit]))


    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def iterator01(): Bool = region rc {
        (Err(1): Result[Int32, Int32]) |> Result.iterator(rc) |> Iterator.toList == Nil
    }

    @test
    def iterator02(): Bool = region rc {
        Ok(1) |> Result.iterator(rc) |> Iterator.toList == 1 :: Nil
    }

    @test
    def iterator03(): Bool = region rc {
        Ok(1 :: 2 :: 3 :: Nil) |> Result.iterator(rc) |> Iterator.toList == (1 :: 2 :: 3 :: Nil) :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // tryCatch                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def tryCatch01(): Bool = Result.tryCatch(_ -> 1 / 1) == Ok(1)

    @test
    def tryCatch02(): Bool = region rc {
        let arr = Array#{0} @ rc;
        Result.tryCatch(_ -> Array.get(10, arr)) |> Result.isErr
    }

}
