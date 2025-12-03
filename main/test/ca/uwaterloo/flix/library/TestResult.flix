mod TestResult {

    use Assert.{assertEq, assertNeq, assertTrue, assertFalse}
    use Validation.{Success, Failure}

    /////////////////////////////////////////////////////////////////////////////
    // Functor.map                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def functor_map01(): Unit \ Assert = assertEq(expected = Err(-1ii), Functor.map(i -> i == 2, Err(-1ii)))

    @Test
    def functor_map02(): Unit \ Assert = assertEq(expected = Ok(false), (Functor.map(i -> i == 2, Ok(1)): Result[Unit, Bool]))

    @Test
    def functor_map03(): Unit \ Assert = assertEq(expected = Ok(true), (Functor.map(i -> i == 2, Ok(2)): Result[Unit, Bool]))

    /////////////////////////////////////////////////////////////////////////////
    // Monad.flatMap                                                           //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def monad_flatMap01(): Unit \ Assert = assertEq(expected = Err(true), Monad.flatMap(i -> if (i == 2) Ok(i) else Err(false), Err(true)))

    @Test
    def monad_flatMap02(): Unit \ Assert = assertEq(expected = Err(false), Monad.flatMap(i -> if (i == 2) Ok(i) else Err(false), Ok(1)))

    @Test
    def monad_flatMap03(): Unit \ Assert = assertEq(expected = Ok(4), Monad.flatMap(i -> if (i == 2) Ok(2*i) else Err(false), Ok(2)))

    /////////////////////////////////////////////////////////////////////////////
    // Applicative.ap                                                          //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def applicative_ap01(): Unit \ Assert = assertEq(expected = Ok(124), (Applicative.ap(Ok(x -> x + 1), Ok(123)): Result[String, Int32]))

    @Test
    def applicative_ap02(): Unit \ Assert = assertEq(expected = Err("b"), Applicative.ap(Ok(x -> x + 1), Err("b")))

    @Test
    def applicative_ap03(): Unit \ Assert = assertEq(expected = Err("a"), (Applicative.ap(Err("a"), Ok(123)): Result[String, Int32]))

    @Test
    def applicative_ap04(): Unit \ Assert = assertEq(expected = Err("a"), (Applicative.ap(Err("a"), Err("b")): Result[String, Int32]))

    /////////////////////////////////////////////////////////////////////////////
    // isOk                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def isOk01(): Unit \ Assert = assertTrue(Result.isOk(Ok(1)))

    @Test
    def isOk02(): Unit \ Assert = assertFalse(Result.isOk(Err(1)))

    /////////////////////////////////////////////////////////////////////////////
    // isErr                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def isErr01(): Unit \ Assert = assertFalse(Result.isErr(Ok(1)))

    @Test
    def isErr02(): Unit \ Assert = assertTrue(Result.isErr(Err(1)))

    /////////////////////////////////////////////////////////////////////////////
    // getWithDefault                                                          //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def getWithDefault01(): Unit \ Assert = assertEq(expected = 1, Result.getWithDefault(2, Ok(1)))

    @Test
    def getWithDefault02(): Unit \ Assert = assertEq(expected = 2, Result.getWithDefault(2, Err(false)))

    /////////////////////////////////////////////////////////////////////////////
    // withDefault                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def withDefault01(): Unit \ Assert = assertEq(expected = Ok(1), (Result.withDefault({default = Ok(2)}, Ok(1)): Result[Unit, Int32]))

    @Test
    def withDefault02(): Unit \ Assert = assertEq(expected = Ok(1), Result.withDefault({default = Err(false)}, Ok(1)))

    @Test
    def withDefault03(): Unit \ Assert = assertEq(expected = Ok(2), (Result.withDefault({default = Ok(2)}, Err(false)): Result[Unit, Int32]))

    @Test
    def withDefault04(): Unit \ Assert = assertEq(expected = Err(true), (Result.withDefault({default = Err(true)}, Err(false)): Result[Bool, Unit]))

    /////////////////////////////////////////////////////////////////////////////
    // replace                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def replace01(): Unit \ Assert = assertEq(expected = Err(false), Result.replace(src =  3, dst =  4, Err(false)))

    @Test
    def replace02(): Unit \ Assert = assertEq(expected = Ok(2), (Result.replace(src =  3, dst =  4, (Ok(2): Result[Unit, Int32]))))

    @Test
    def replace03(): Unit \ Assert = assertEq(expected = Ok(4), (Result.replace(src =  3, dst =  4, (Ok(3): Result[Unit, Int32]))))

    @Test
    def replace04(): Unit \ Assert = assertEq(expected = Ok(4), (Result.replace(src =  3, dst =  4, (Ok(4): Result[Unit, Int32]))))

    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def exists01(): Unit \ Assert = assertFalse(Result.exists(i -> i == 2, Err(false)))

    @Test
    def exists02(): Unit \ Assert = assertFalse(Result.exists(i -> i == 2, Ok(1)))

    @Test
    def exists03(): Unit \ Assert = assertTrue(Result.exists(i -> i == 2, Ok(2)))

    /////////////////////////////////////////////////////////////////////////////
    // forAll                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def forAll01(): Unit \ Assert = assertTrue(Result.forAll(i -> i == 2, Err(false)))

    @Test
    def forAll02(): Unit \ Assert = assertFalse(Result.forAll(i -> i == 2, Ok(1)))

    @Test
    def forAll03(): Unit \ Assert = assertTrue(Result.forAll(i -> i == 2, Ok(2)))

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def map01(): Unit \ Assert = assertEq(expected = Err(-1ii), Result.map(i -> i == 2, Err(-1ii)))

    @Test
    def map02(): Unit \ Assert = assertEq(expected = Ok(false), (Result.map(i -> i == 2, Ok(1)): Result[Unit, Bool]))

    @Test
    def map03(): Unit \ Assert = assertEq(expected = Ok(true), (Result.map(i -> i == 2, Ok(2)): Result[Unit, Bool]))

    /////////////////////////////////////////////////////////////////////////////
    // mapErr                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def mapErr01(): Unit \ Assert = assertEq(expected = Ok(1), Result.mapErr(i -> i == 2, Ok(1)))

    @Test
    def mapErr02(): Unit \ Assert = assertEq(expected = Err(false), (Result.mapErr(i -> i == 2, Err(1)): Result[Bool, Unit]))

    @Test
    def mapErr03(): Unit \ Assert = assertEq(expected = Err(true), (Result.mapErr(i -> i == 2, Err(2)): Result[Bool, Unit]))

    /////////////////////////////////////////////////////////////////////////////
    // flatMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def flatMap01(): Unit \ Assert = assertEq(expected = Err(true), Result.flatMap(i -> if (i == 2) Ok(i) else Err(false), Err(true)))

    @Test
    def flatMap02(): Unit \ Assert = assertEq(expected = Err(false), Result.flatMap(i -> if (i == 2) Ok(i) else Err(false), Ok(1)))

    @Test
    def flatMap03(): Unit \ Assert = assertEq(expected = Ok(4), Result.flatMap(i -> if (i == 2) Ok(2*i) else Err(false), Ok(2)))


    /////////////////////////////////////////////////////////////////////////////
    // flatten                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def flatten01(): Unit \ Assert = {
        let err: Result[Int32, Unit] = Err(42);
        assertEq(expected = Err(42), Result.flatten(Ok(err)))
    }

    @Test
    def flatten02(): Unit \ Assert = {
        let err: Result[Int32, Result[Int32, Int32]] = Err(42);
        assertEq(expected = Err(42), Result.flatten(err))
    }

    @Test
    def flatten03(): Unit \ Assert = {
        let err: Result[Int32, Result[Int32, Int32]] = Ok(Ok(42));
        assertEq(expected = Ok(42), Result.flatten(err))
    }

    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def count01(): Unit \ Assert = assertEq(expected = 0, Result.count(i -> i == 2, Err(false)))

    @Test
    def count02(): Unit \ Assert = assertEq(expected = 0, Result.count(i -> i == 2, Ok(1)))

    @Test
    def count03(): Unit \ Assert = assertEq(expected = 1, Result.count(i -> i == 2, Ok(2)))


    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sum01(): Unit \ Assert = assertEq(expected = 0, Result.sum(Err(false)))

    @Test
    def sum02(): Unit \ Assert = assertEq(expected = 1, Result.sum(Ok(1)))

    @Test
    def sum03(): Unit \ Assert = assertEq(expected = -2, Result.sum(Ok(-2)))


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumWith01(): Unit \ Assert = assertEq(expected = 0, Result.sumWith(x -> x + 1, Err(false)))

    @Test
    def sumWith02(): Unit \ Assert = assertEq(expected = 2, Result.sumWith(x -> x + 1, Ok(1)))

    @Test
    def sumWith03(): Unit \ Assert = assertEq(expected = -1, Result.sumWith(x -> x + 1, Ok(-2)))


    /////////////////////////////////////////////////////////////////////////////
    // find                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def find01(): Unit \ Assert = assertEq(expected = None, Result.find(i -> i == 2, Err(false)))

    @Test
    def find02(): Unit \ Assert = assertEq(expected = None, Result.find(i -> i == 2, Ok(1)))

    @Test
    def find03(): Unit \ Assert = assertEq(expected = Some(2), Result.find(i -> i == 2, Ok(2)))

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def foldLeft01(): Unit \ Assert = assertFalse(Result.foldLeft((b, i) -> if (i == 2 and b) true else false, false, Err(-1ii)))

    @Test
    def foldLeft02(): Unit \ Assert = assertFalse(Result.foldLeft((b, i) -> if (i == 2 and b) true else false, false, Ok(1)))

    @Test
    def foldLeft03(): Unit \ Assert = assertFalse(Result.foldLeft((b, i) -> if (i == 2 and b) true else false, true, Ok(1)))

    @Test
    def foldLeft04(): Unit \ Assert = assertFalse(Result.foldLeft((b, i) -> if (i == 2 and b) true else false, false, Ok(2)))

    @Test
    def foldLeft05(): Unit \ Assert = assertTrue(Result.foldLeft((b, i) -> if (i == 2 and b) true else false, true, Ok(2)))

    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def foldRight01(): Unit \ Assert = assertFalse(Result.foldRight((i, acc) -> if (i == 2 and acc) true else false, false, Err(-1ii)))

    @Test
    def foldRight02(): Unit \ Assert = assertFalse(Result.foldRight((i, acc) -> if (i == 2 and acc) true else false, false, Ok(1)))

    @Test
    def foldRight03(): Unit \ Assert = assertFalse(Result.foldRight((i, acc) -> if (i == 2 and acc) true else false, true, Ok(1)))

    @Test
    def foldRight04(): Unit \ Assert = assertFalse(Result.foldRight((i, acc) -> if (i == 2 and acc) true else false, false, Ok(2)))

    @Test
    def foldRight05(): Unit \ Assert = assertTrue(Result.foldRight((i, acc) -> if (i == 2 and acc) true else false, true, Ok(2)))

    /////////////////////////////////////////////////////////////////////////////
    // sequence                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def sequence01(): Unit \ Assert = assertEq(expected = Ok(Nil), (Result.sequence(Nil): Result[Unit, List[Unit]]))

    @Test
    def sequence02(): Unit \ Assert = assertEq(expected = Ok(1 :: Nil), (Result.sequence(Ok(1) :: Nil): Result[Unit, List[Int32]]))

    @Test
    def sequence03(): Unit \ Assert = assertEq(expected = Ok(1 :: 2 :: Nil), (Result.sequence(Ok(1) :: Ok(2) :: Nil): Result[Unit, List[Int32]]))

    @Test
    def sequence04(): Unit \ Assert = assertEq(expected = Ok(1 :: 2 :: 3 :: Nil), (Result.sequence(Ok(1) :: Ok(2) :: Ok(3) :: Nil): Result[Unit, List[Int32]]))

    @Test
    def sequence05(): Unit \ Assert = assertEq(expected = Err("one"), Result.sequence(Err("one") :: Ok(2) :: Ok(3) :: Nil))

    @Test
    def sequence06(): Unit \ Assert = assertEq(expected = Err("three"), Result.sequence(Ok(1) :: Ok(2) :: Err("three") :: Nil))

    @Test
    def sequence07(): Unit \ Assert = assertEq(expected = Err("two"), Result.sequence(Ok(1) :: Err("two") :: Ok(3) :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // traverse                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def traverse01(): Unit \ Assert = assertEq(expected = Ok(Nil), (Result.traverse(x -> Ok(x + 1), Nil): Result[Unit, List[Int32]]))

    @Test
    def traverse02(): Unit \ Assert = assertEq(expected = Ok(2 :: Nil), (Result.traverse(x -> Ok(x + 1), 1 :: Nil): Result[Unit, List[Int32]]))

    @Test
    def traverse03(): Unit \ Assert = assertEq(expected = Ok(2 :: 3 :: Nil), (Result.traverse(x -> Ok(x + 1), 1 :: 2 :: Nil): Result[Unit, List[Int32]]))

    @Test
    def traverse04(): Unit \ Assert = assertEq(expected = Ok(2 :: 3 :: 4 :: Nil), (Result.traverse(x -> Ok(x + 1), 1 :: 2 :: 3 :: Nil): Result[Unit, List[Int32]]))

    @Test
    def traverse05(): Unit \ Assert = assertEq(expected = Err("one"), Result.traverse(x -> if (x == 1) Err("one") else Ok(x), 1 :: 2 :: 3 :: Nil))

    @Test
    def traverse06(): Unit \ Assert = assertEq(expected = Err("three"), Result.traverse(x -> if (x == 3) Err("three") else Ok(x), 1 :: 2 :: 3 :: Nil))

    @Test
    def traverse07(): Unit \ Assert = assertEq(expected = Err("two"), Result.traverse(x -> if (x == 2) Err("two") else Ok(x), 1 :: 2 :: 3 :: Nil))

    @Test
    def traverse08(): Unit \ Assert = assertEq(expected = Ok(42 :: 42 :: 42 :: Nil), (Result.traverse(_ -> Ok(42), 1 :: 2 :: 3 :: Nil): Result[Unit, List[Int32]]))

    /////////////////////////////////////////////////////////////////////////////
    // traverseX                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def traverseX01(): Unit \ Assert = assertEq(expected = Ok(), (Result.traverseX(x -> Ok(x + 1), Nil): Result[String, Unit]))

    @Test
    def traverseX02(): Unit \ Assert = assertEq(expected = Ok(), (Result.traverseX(x -> Ok(x + 1), 1 :: Nil): Result[String, Unit]))

    @Test
    def traverseX03(): Unit \ Assert = assertEq(expected = Ok(), (Result.traverseX(x -> Ok(x + 1), 1 :: 2 :: Nil): Result[String, Unit]))

    @Test
    def traverseX04(): Unit \ Assert = assertEq(expected = Ok(), (Result.traverseX(x -> Ok(x + 1), 1 :: 2 :: 3 :: Nil): Result[String, Unit]))

    @Test
    def traverseX05(): Unit \ Assert = assertEq(expected = Err("one"), Result.traverseX(x -> if (x == 1) Err("one") else Ok(x), 1 :: 2 :: 3 :: Nil))

    @Test
    def traverseX06(): Unit \ Assert = assertEq(expected = Err("three"), Result.traverseX(x -> if (x == 3) Err("three") else Ok(x), 1 :: 2 :: 3 :: Nil))

    @Test
    def traverseX07(): Unit \ Assert = assertEq(expected = Err("two"), Result.traverseX(x -> if (x == 2) Err("two") else Ok(x), 1 :: 2 :: 3 :: Nil))

    @Test
    def traverseX08(): Unit \ Assert = assertEq(expected = Ok(), (Result.traverseX(_ -> Ok(42), 1 :: 2 :: 3 :: Nil): Result[String, Unit]))

    /////////////////////////////////////////////////////////////////////////////
    // foldLeftM                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def foldLeftM01(): Unit \ Assert = assertEq(expected = Ok(0), Result.foldLeftM((ac, i) -> if (i < 0) Err(-1) else Ok(Int32.max(i, ac)), 0, Nil))

    @Test
    def foldLeftM02(): Unit \ Assert = assertEq(expected = Ok(1), Result.foldLeftM((ac, i) -> if (i < 0) Err(-1) else Ok(Int32.max(i, ac)), 0, 1 :: Nil))

    @Test
    def foldLeftM03(): Unit \ Assert = assertEq(expected = Ok(2), Result.foldLeftM((ac, i) -> if (i < 0) Err(-1) else Ok(Int32.max(i, ac)), 0, 1 :: 2 :: Nil))

    @Test
    def foldLeftM04(): Unit \ Assert = assertEq(expected = Ok(3), Result.foldLeftM((ac, i) -> if (i < 0) Err(-1) else Ok(Int32.max(i, ac)), 0, 1 :: 2 :: 3 :: Nil))

    @Test
    def foldLeftM05(): Unit \ Assert = assertEq(expected = Err(-1), Result.foldLeftM((ac, i) -> if (i < 0) Err(-1) else Ok(Int32.max(i, ac)), 0, -1 :: 2 :: 3 :: Nil))

    @Test
    def foldLeftM06(): Unit \ Assert = assertEq(expected = Err(-1), Result.foldLeftM((ac, i) -> if (i < 0) Err(-1) else Ok(Int32.max(i, ac)), 0, 1 :: -2 :: 3 :: Nil))

    @Test
    def foldLeftM07(): Unit \ Assert = assertEq(expected = Err(-1), Result.foldLeftM((ac, i) -> if (i < 0) Err(-1) else Ok(Int32.max(i, ac)), 0, 1 :: 2 :: -3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // foldRightM                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def foldRightM01(): Unit \ Assert = assertEq(expected = Ok(0), Result.foldRightM((i, ac) -> if (i < 0) Err(-1) else Ok(Int32.max(i, ac)), 0, Nil))

    @Test
    def foldRightM02(): Unit \ Assert = assertEq(expected = Ok(1), Result.foldRightM((i, ac) -> if (i < 0) Err(-1) else Ok(Int32.max(i, ac)), 0, 1 :: Nil))

    @Test
    def foldRightM03(): Unit \ Assert = assertEq(expected = Ok(2), Result.foldRightM((i, ac) -> if (i < 0) Err(-1) else Ok(Int32.max(i, ac)), 0, 1 :: 2 :: Nil))

    @Test
    def foldRightM04(): Unit \ Assert = assertEq(expected = Ok(3), Result.foldRightM((i, ac) -> if (i < 0) Err(-1) else Ok(Int32.max(i, ac)), 0, 1 :: 2 :: 3 :: Nil))

    @Test
    def foldRightM05(): Unit \ Assert = assertEq(expected = Err(-1), Result.foldRightM((i, ac) -> if (i < 0) Err(-1) else Ok(Int32.max(i, ac)), 0, -1 :: 2 :: 3 :: Nil))

    @Test
    def foldRightM06(): Unit \ Assert = assertEq(expected = Err(-1), Result.foldRightM((i, ac) -> if (i < 0) Err(-1) else Ok(Int32.max(i, ac)), 0, 1 :: -2 :: 3 :: Nil))

    @Test
    def foldRightM07(): Unit \ Assert = assertEq(expected = Err(-1), Result.foldRightM((i, ac) -> if (i < 0) Err(-1) else Ok(Int32.max(i, ac)), 0, 1 :: 2 :: -3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toList01(): Unit \ Assert = assertEq(expected = Nil, Result.toList((Err(-1ii): Result[BigInt, Unit])))

    @Test
    def toList02(): Unit \ Assert = assertEq(expected = 1 :: Nil, Result.toList(Ok(1)))

    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toSet01(): Unit \ Assert = assertEq(expected = Set#{}, Result.toSet((Err(-1ii): Result[BigInt, Unit])))

    @Test
    def toSet02(): Unit \ Assert = assertEq(expected = Set#{1}, Result.toSet(Ok(1)))

    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toMap01(): Unit \ Assert = assertEq(expected = Map#{}, Result.toMap((Err(-1ii): Result[BigInt, (Unit, Unit)])))

    @Test
    def toMap02(): Unit \ Assert = assertEq(expected = Map#{1 => true}, Result.toMap(Ok((1, true))))

    /////////////////////////////////////////////////////////////////////////////
    // toMapWith                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMapWith01(): Unit \ Assert =
        assertEq(expected = Map#{}, Result.toMapWith(x -> 0 * x, Err(1)))

    @Test
    def toMapWith02(): Unit \ Assert =
        assertEq(expected = Map#{1 => 0}, Result.toMapWith(x -> 0 * x, Ok(1)))

    @Test
    def toMapWith03(): Unit \ Assert =
        assertEq(expected = Map#{1 => 2}, Result.toMapWith(x -> 2 * x, Ok(1)))

    @Test
    def toMapWith04(): Unit \ Assert =
        assertEq(expected = Map#{1 => 11}, Result.toMapWith(x -> x + 10, Ok(1)))

    @Test
    def toMapWith05(): Unit \ Assert =
        assertEq(expected = Map#{"a" => "ab"}, Result.toMapWith(x -> x + "b", Ok("a")))

    /////////////////////////////////////////////////////////////////////////////
    // toOption                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toOption01(): Unit \ Assert = assertEq(expected = None, Result.toOption((Err(-1ii): Result[BigInt, Unit])))

    @Test
    def toOption02(): Unit \ Assert = assertEq(expected = Some(1), Result.toOption(Ok(1)))

    /////////////////////////////////////////////////////////////////////////////
    // toValidation                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    pub def toValidation01(): Unit \ Assert =
        assertEq(expected = Failure(Nec.singleton(1)), Result.toValidation((Err(1): Result[Int32, Unit])))

    @Test
    pub def toValidation02(): Unit \ Assert =
        assertEq(expected = Success("s"), Result.toValidation((Ok("s"): Result[Unit, String])))

    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEach01(): Unit \ Assert = region rc {
        let ri = Ref.fresh(rc, 21);
        Result.forEach(x -> Ref.put(x, ri), (Ok(42): Result[Int32, Int32]));
        assertEq(expected = 42, Ref.get(ri))
    }

    @Test
    def forEach02(): Unit \ Assert = region rc {
        let ri = Ref.fresh(rc, 21);
        Result.forEach(x -> Ref.put(x, ri), (Err(42): Result[Int32, Int32]));
        assertEq(expected = 21, Ref.get(ri))
    }

    /////////////////////////////////////////////////////////////////////////////
    // ap                                                                      //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def ap01(): Unit \ Assert = assertEq(expected = Ok(124), Result.ap(Ok(x -> x + 1), (Ok(123): Result[String, Int32])))

    @Test
    def ap02(): Unit \ Assert = assertEq(expected = Err("b"), Result.ap(Ok(x -> x + 1), Err("b")))

    @Test
    def ap03(): Unit \ Assert = assertEq(expected = Err("a"), (Result.ap(Err("a"), Ok(123)): Result[String, Int32]))

    @Test
    def ap04(): Unit \ Assert = assertEq(expected = Err("a"), (Result.ap(Err("a"), Err("b")): Result[String, Int32]))

    /////////////////////////////////////////////////////////////////////////////
    // map2                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map201(): Unit \ Assert = assertEq(expected = Ok(124), (Result.map2((x1,x2) -> x1 + x2, Ok(123), Ok(1)): Result[String, Int32]))

    @Test
    def map202(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map2((x1,x2) -> x1 + x2, Ok(123), Err("e1")))

    @Test
    def map203(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map2((x1,x2) -> x1 + x2, Err("e1"), Ok(1)))

    @Test
    def map204(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map2((x1,x2) -> x1 + x2, Err("e1"), (Err("e2"): Result[String, Int32])))

    /////////////////////////////////////////////////////////////////////////////
    // map3                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map301(): Unit \ Assert = assertEq(expected = Ok(126), (Result.map3((x1,x2,x3) -> x1 + x2 + x3, Ok(123), Ok(1), Ok(2)): Result[String, Int32]))

    @Test
    def map302(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map3((x1,x2,x3) -> x1 + x2 + x3, Ok(123), Ok(1), Err("e1")))

    @Test
    def map303(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map3((x1,x2,x3) -> x1 + x2 + x3, Ok(123), Err("e1"), Ok(2)))

    @Test
    def map304(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map3((x1,x2,x3) -> x1 + x2 + x3, Err("e1"), Ok(1), Ok(2)))

    @Test
    def map305(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map3((x1,x2,x3) -> x1 + x2 + x3, Ok(123), Err("e1"), Err("e2")))

    @Test
    def map306(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map3((x1,x2,x3) -> x1 + x2 + x3, Err("e1"), Ok(1), Err("e2")))

    @Test
    def map307(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map3((x1,x2,x3) -> x1 + x2 + x3, Err("e1"), Err("e2"), (Err("e3"): Result[String, Int32])))

    /////////////////////////////////////////////////////////////////////////////
    // map4                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map401(): Unit \ Assert = assertEq(expected = Ok(129), (Result.map4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Ok(123), Ok(1), Ok(2), Ok(3)): Result[String, Int32]))

    @Test
    def map402(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Ok(123), Ok(1), Ok(2), Err("e1")))

    @Test
    def map403(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Ok(123), Ok(1), Err("e1"), Ok(3)))

    @Test
    def map404(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Ok(123), Err("e1"), Ok(2), Ok(3)))

    @Test
    def map405(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Err("e1"), Ok(1), Ok(2), Ok(3)))

    @Test
    def map406(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Err("e1"), Err("e2"), Err("e3"), (Err("e4"): Result[String, Int32])))

    /////////////////////////////////////////////////////////////////////////////
    // map5                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map501(): Unit \ Assert = assertEq(expected = Ok(133), (Result.map5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4)): Result[String, Int32]))

    @Test
    def map502(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Ok(123), Ok(1), Ok(2), Ok(3), Err("e1")))

    @Test
    def map503(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Ok(123), Ok(1), Ok(2), Err("e1"), Ok(4)))

    @Test
    def map504(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Ok(123), Ok(1), Err("e1"), Ok(3), Ok(4)))

    @Test
    def map505(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Ok(123), Err("e1"), Ok(2), Ok(3), Ok(4)))

    @Test
    def map506(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Err("e1"), Ok(1), Ok(2), Ok(3), Ok(4)))

    @Test
    def map507(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Err("e1"), Err("e2"), Err("e3"), Err("e4"), (Err("e5"): Result[String, Int32])))

    /////////////////////////////////////////////////////////////////////////////
    // map6                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map601(): Unit \ Assert = assertEq(expected = Ok(138), (Result.map6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5)): Result[String, Int32]))

    @Test
    def map602(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Err("e1")))

    @Test
    def map603(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Ok(123), Ok(1), Ok(2), Ok(3), Err("e1"), Ok(5)))

    @Test
    def map604(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Ok(123), Ok(1), Ok(2), Err("e1"), Ok(4), Ok(5)))

    @Test
    def map605(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Ok(123), Ok(1), Err("e1"), Ok(3), Ok(4), Ok(5)))

    @Test
    def map606(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Ok(123), Err("e1"), Ok(2), Ok(3), Ok(4), Ok(5)))

    @Test
    def map607(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Err("e1"), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5)))

    @Test
    def map608(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Err("e1"), Err("e2"), Err("e3"), Err("e4"), Err("e5"), (Err("e6"): Result[String, Int32])))

    /////////////////////////////////////////////////////////////////////////////
    // map7                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map701(): Unit \ Assert = assertEq(expected = Ok(144), (Result.map7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6)): Result[String, Int32]))

    @Test
    def map702(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Err("e1")))

    @Test
    def map703(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Err("e1"), Ok(6)))

    @Test
    def map704(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Ok(123), Ok(1), Ok(2), Ok(3), Err("e1"), Ok(5), Ok(6)))

    @Test
    def map705(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Ok(123), Ok(1), Ok(2), Err("e1"), Ok(4), Ok(5), Ok(6)))

    @Test
    def map706(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Ok(123), Ok(1), Err("e1"), Ok(3), Ok(4), Ok(5), Ok(6)))

    @Test
    def map707(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Ok(123), Err("e1"), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6)))

    @Test
    def map708(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Err("e1"), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6)))

    @Test
    def map709(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Err("e1"), Err("e2"), Err("e3"), Err("e4"), Err("e5"), Err("e6"), (Err("e7"): Result[String, Int32])))

    /////////////////////////////////////////////////////////////////////////////
    // map8                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map801(): Unit \ Assert = assertEq(expected = Ok(151), (Result.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6), Ok(7)): Result[String, Int32]))

    @Test
    def map802(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6), Err("e1")))

    @Test
    def map803(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Err("e1"), Ok(7)))

    @Test
    def map804(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Err("e1"), Ok(6), Ok(7)))

    @Test
    def map805(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Ok(123), Ok(1), Ok(2), Ok(3), Err("e1"), Ok(5), Ok(6), Ok(7)))

    @Test
    def map806(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Ok(123), Ok(1), Ok(2), Err("e1"), Ok(4), Ok(5), Ok(6), Ok(7)))

    @Test
    def map807(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Ok(123), Ok(1), Err("e1"), Ok(3), Ok(4), Ok(5), Ok(6), Ok(7)))

    @Test
    def map808(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Ok(123), Err("e1"), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6), Ok(7)))

    @Test
    def map809(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Err("e1"), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6), Ok(7)))

    @Test
    def map810(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Err("e1"), Err("e2"), Err("e3"), Err("e4"), Err("e5"), Err("e6"), Err("e7"), (Err("e8"): Result[String, Int32])))

    /////////////////////////////////////////////////////////////////////////////
    // map9                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map901(): Unit \ Assert = assertEq(expected = Ok(159), (Result.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6), Ok(7), Ok(8)): Result[String, Int32]))

    @Test
    def map902(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6), Ok(7), Err("e1")))

    @Test
    def map903(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6), Err("e1"), Ok(8)))

    @Test
    def map904(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Err("e1"), Ok(7), Ok(8)))

    @Test
    def map905(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Err("e1"), Ok(6), Ok(7), Ok(8)))

    @Test
    def map906(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Ok(123), Ok(1), Ok(2), Ok(3), Err("e1"), Ok(5), Ok(6), Ok(7), Ok(8)))

    @Test
    def map907(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Ok(123), Ok(1), Ok(2), Err("e1"), Ok(4), Ok(5), Ok(6), Ok(7), Ok(8)))

    @Test
    def map908(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Ok(123), Ok(1), Err("e1"), Ok(3), Ok(4), Ok(5), Ok(6), Ok(7), Ok(8)))

    @Test
    def map909(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Ok(123), Err("e1"), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6), Ok(7), Ok(8)))

    @Test
    def map910(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Err("e1"), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6), Ok(7), Ok(8)))

    @Test
    def map911(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Err("e1"), Err("e2"), Err("e3"), Err("e4"), Err("e5"), Err("e6"), Err("e7"), Err("e8"), (Err("e9"): Result[String, Int32])))

    /////////////////////////////////////////////////////////////////////////////
    // map10                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map1001(): Unit \ Assert = assertEq(expected = Ok(168), (Result.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6), Ok(7), Ok(8), Ok(9)): Result[String, Int32]))

    @Test
    def map1002(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6), Ok(7), Ok(8), Err("e1")))

    @Test
    def map1003(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6), Ok(7), Err("e1"), Ok(9)))

    @Test
    def map1004(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6), Err("e1"), Ok(8), Ok(9)))

    @Test
    def map1005(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Err("e1"), Ok(7), Ok(8), Ok(9)))

    @Test
    def map1006(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Ok(123), Ok(1), Ok(2), Ok(3), Ok(4), Err("e1"), Ok(6), Ok(7), Ok(8), Ok(9)))

    @Test
    def map1007(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Ok(123), Ok(1), Ok(2), Ok(3), Err("e1"), Ok(5), Ok(6), Ok(7), Ok(8), Ok(9)))

    @Test
    def map1008(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Ok(123), Ok(1), Ok(2), Err("e1"), Ok(4), Ok(5), Ok(6), Ok(7), Ok(8), Ok(9)))

    @Test
    def map1009(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Ok(123), Ok(1), Err("e1"), Ok(3), Ok(4), Ok(5), Ok(6), Ok(7), Ok(8), Ok(9)))

    @Test
    def map1010(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Ok(123), Err("e1"), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6), Ok(7), Ok(8), Ok(9)))

    @Test
    def map1011(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Err("e1"), Ok(1), Ok(2), Ok(3), Ok(4), Ok(5), Ok(6), Ok(7), Ok(8), Ok(9)))

    @Test
    def map1012(): Unit \ Assert = assertEq(expected = Err("e1"), Result.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Err("e1"), Err("e2"), Err("e3"), Err("e4"), Err("e5"), Err("e6"), Err("e7"), Err("e8"), Err("e9"), (Err("e10"): Result[String, Int32])))

    /////////////////////////////////////////////////////////////////////////////
    // hash                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def hash01(): Unit \ Assert = assertEq(expected = true, Hash.hash((Err(()): Result[Unit, Unit])) == Hash.hash((Err(()): Result[Unit, Unit])))

    @Test
    def hash02(): Unit \ Assert = assertEq(expected = true, Hash.hash((Ok(()): Result[Unit, Unit])) != Hash.hash((Err(()): Result[Unit, Unit])))

    @Test
    def hash03(): Unit \ Assert = assertEq(expected = true, Hash.hash((Ok("Success"): Result[Unit, String])) == Hash.hash((Ok("Success"): Result[Unit, String])))

    @Test
    def hash04(): Unit \ Assert = assertEq(expected = true, Hash.hash((Ok("Success"): Result[String, String])) != Hash.hash((Err("Error"): Result[String, String])))

    @Test
    def hash05(): Unit \ Assert = assertEq(expected = true, Hash.hash((Err(13i8): Result[Int8, Unit])) != Hash.hash((Err(255i16): Result[Int16, Unit])))


    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iterator01(): Unit \ Assert = region rc {
        assertEq(expected = Nil, (Err(1): Result[Int32, Int32]) |> Result.iterator(rc) |> Iterator.toList)
    }

    @Test
    def iterator02(): Unit \ Assert = region rc {
        assertEq(expected = 1 :: Nil, Ok(1) |> Result.iterator(rc) |> Iterator.toList)
    }

    @Test
    def iterator03(): Unit \ Assert = region rc {
        assertEq(expected = (1 :: 2 :: 3 :: Nil) :: Nil, Ok(1 :: 2 :: 3 :: Nil) |> Result.iterator(rc) |> Iterator.toList)
    }

    /////////////////////////////////////////////////////////////////////////////
    // tryCatch                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tryCatch01(): Unit \ Assert = assertEq(expected = Ok(1), Result.tryCatch(_ -> 1 / 1))

    @Test
    def tryCatch02(): Unit \ Assert = region rc {
        let arr = Array#{0} @ rc;
        assertEq(expected = true, Result.tryCatch(_ -> Array.get(10, arr)) |> Result.isErr)
    }

}
