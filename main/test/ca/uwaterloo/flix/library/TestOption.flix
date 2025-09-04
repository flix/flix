mod TestOption {

    use Hash.hash;
    use Assert.{assertEq, assertNeq, assertTrue, assertFalse, assertSome, assertNone};

    /////////////////////////////////////////////////////////////////////////////
    // empty                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def empty01(): Unit \ Assert = assertEq(expected = (None: Option[Int32]), Option.empty())

    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isEmpty01(): Unit \ Assert = assertTrue(Option.isEmpty(None))

    @Test
    def isEmpty02(): Unit \ Assert = assertFalse(Option.isEmpty(Some(0)))

    @Test
    def isEmpty03(): Unit \ Assert = assertFalse(Option.isEmpty(Some(22ii)))

    /////////////////////////////////////////////////////////////////////////////
    // nonEmpty                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def nonEmpty01(): Unit \ Assert = assertNone(None)

    @Test
    def nonEmpty02(): Unit \ Assert = assertSome(Some(0))

    @Test
    def nonEmpty03(): Unit \ Assert = assertSome(Some(22ii))

    /////////////////////////////////////////////////////////////////////////////
    // getWithDefault                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def getWithDefault01(): Unit \ Assert = assertEq(expected = 32, Option.getWithDefault(32, None))

    @Test
    def getWithDefault02(): Unit \ Assert = assertEq(expected = 30, Option.getWithDefault(32, Some(30)))

    @Test
    def getWithDefault03(): Unit \ Assert = assertEq(expected = 32ii, Option.getWithDefault(32ii, None))

    @Test
    def getWithDefault04(): Unit \ Assert = assertEq(expected = 30ii, Option.getWithDefault(32ii, Some(30ii)))

    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def exists01(): Unit \ Assert = assertFalse(Option.exists(i -> i == 2, None))

    @Test
    def exists02(): Unit \ Assert = assertFalse(Option.exists(i -> i == 2, Some(1)))

    @Test
    def exists03(): Unit \ Assert = assertTrue(Option.exists(i -> i == 2, Some(2)))

    /////////////////////////////////////////////////////////////////////////////
    // forAll                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forAll01(): Unit \ Assert = assertTrue(Option.forAll(i -> i == 2, None))

    @Test
    def forAll02(): Unit \ Assert = assertFalse(Option.forAll(i -> i == 2, Some(1)))

    @Test
    def forAll03(): Unit \ Assert = assertTrue(Option.forAll(i -> i == 2, Some(2)))

    /////////////////////////////////////////////////////////////////////////////
    // filter                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filter01(): Unit \ Assert = assertNone(Option.filter(i -> i == 2, None))

    @Test
    def filter02(): Unit \ Assert = assertNone(Option.filter(i -> i == 2, Some(1)))

    @Test
    def filter03(): Unit \ Assert = assertEq(expected = Some(2), Option.filter(i -> i == 2, Some(2)))

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map01(): Unit \ Assert = assertNone(Option.map(i -> 2*i, None))

    @Test
    def map02(): Unit \ Assert = assertEq(expected = Some(2), Option.map(i -> 2*i, Some(1)))

    @Test
    def map03(): Unit \ Assert = assertEq(expected = Some(4), Option.map(i -> 2*i, Some(2)))

    @Test
    def map04(): Unit \ Assert = region rc {
        let n = Ref.fresh(rc, 2);
        discard Option.map(i -> Ref.put(i, n), Some(4));
        assertEq(expected = 4, Ref.get(n))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toList01(): Unit \ Assert = assertEq(expected = Nil, Option.toList((None: Option[Unit])))

    @Test
    def toList02(): Unit \ Assert = assertEq(expected = 1 :: Nil, Option.toList(Some(1)))

    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toSet01(): Unit \ Assert = assertEq(expected = Set#{}, Option.toSet((None: Option[Unit])))

    @Test
    def toSet02(): Unit \ Assert = assertEq(expected = Set#{1}, Option.toSet(Some(1)))

    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMap01(): Unit \ Assert = assertEq(expected = Map#{}, Option.toMap((None: Option[(Unit, Unit)])))

    @Test
    def toMap02(): Unit \ Assert = assertEq(expected = Map#{1 => true}, Option.toMap(Some((1, true))))

    /////////////////////////////////////////////////////////////////////////////
    // toMapWith                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMapWith01(): Unit \ Assert =
        assertEq(expected = Map#{}, Option.toMapWith(x -> 0 * x, None))

    @Test
    def toMapWith02(): Unit \ Assert =
        assertEq(expected = Map#{1 => 0}, Option.toMapWith(x -> 0 * x, Some(1)))

    @Test
    def toMapWith03(): Unit \ Assert =
        assertEq(expected = Map#{1 => 2}, Option.toMapWith(x -> 2 * x, Some(1)))

    @Test
    def toMapWith04(): Unit \ Assert =
        assertEq(expected = Map#{1 => 11}, Option.toMapWith(x -> x + 10, Some(1)))

    @Test
    def toMapWith05(): Unit \ Assert =
        assertEq(expected = Map#{"a" => "ab"}, Option.toMapWith(x -> x + "b", Some("a")))

    /////////////////////////////////////////////////////////////////////////////
    // toOk                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toOk01(): Unit \ Assert = assertEq(expected = Err("err"), Option.toOk("err", (None: Option[Unit])))

    @Test
    def toOk02(): Unit \ Assert = assertEq(expected = Ok(1), Option.toOk("err", Some(1)))

    /////////////////////////////////////////////////////////////////////////////
    // toErr                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toErr01(): Unit \ Assert = assertEq(expected = Ok("default"), Option.toErr("default", (None: Option[Unit])))

    @Test
    def toErr02(): Unit \ Assert = assertEq(expected = Err("err"), Option.toErr("default", Some("err")))

    /////////////////////////////////////////////////////////////////////////////
    // toSuccess                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toSuccess01(): Unit \ Assert = assertEq(expected = Validation.Failure(Nec.singleton("err")), Option.toSuccess("err", (None: Option[Unit])))

    @Test
    def toSuccess02(): Unit \ Assert = assertEq(expected = Validation.Success(1), Option.toSuccess("err", Some(1)))

    /////////////////////////////////////////////////////////////////////////////
    // toFailure                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toFailure01(): Unit \ Assert = assertEq(expected = Validation.Success("default"), Option.toFailure("default", (None: Option[Unit])))

    @Test
    def toFailure02(): Unit \ Assert = assertEq(expected = Validation.Failure(Nec.singleton("err")), Option.toFailure("default", Some("err")))

    /////////////////////////////////////////////////////////////////////////////
    // withDefault                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def withDefault01(): Unit \ Assert = assertEq(expected = (None: Option[Int32]), Option.withDefault(default = None, None))

    @Test
    def withDefault02(): Unit \ Assert = assertEq(expected = Some(1), Option.withDefault(default = Some(1), None))

    @Test
    def withDefault03(): Unit \ Assert = assertEq(expected = Some(1), Option.withDefault(default = None, Some(1)))

    @Test
    def withDefault04(): Unit \ Assert = assertEq(expected = Some(1), Option.withDefault(default = Some(2), Some(1)))

    /////////////////////////////////////////////////////////////////////////////
    // replace                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def replace01(): Unit \ Assert = assertNone(Option.replace(src = 3, dst = 4, None))

    @Test
    def replace02(): Unit \ Assert = assertEq(expected = Some(2), Option.replace(src = 3, dst = 4, Some(2)))

    @Test
    def replace03(): Unit \ Assert = assertEq(expected = Some(4), Option.replace(src = 3, dst = 4, Some(3)))

    @Test
    def replace04(): Unit \ Assert = assertEq(expected = Some(4), Option.replace(src = 3, dst = 4, Some(4)))

    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def count01(): Unit \ Assert = assertEq(expected = 0, Option.count(i -> i == 2, None))

    @Test
    def count02(): Unit \ Assert = assertEq(expected = 0, Option.count(i -> i == 2, Some(1)))

    @Test
    def count03(): Unit \ Assert = assertEq(expected = 1, Option.count(i -> i == 2, Some(2)))


    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sum01(): Unit \ Assert = assertEq(expected = 0, Option.sum(None))

    @Test
    def sum02(): Unit \ Assert = assertEq(expected = 1, Option.sum(Some(1)))

    @Test
    def sum03(): Unit \ Assert = assertEq(expected = -2, Option.sum(Some(-2)))


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumWith01(): Unit \ Assert = assertEq(expected = 0, Option.sumWith(x -> x + 1, None))

    @Test
    def sumWith02(): Unit \ Assert = assertEq(expected = 2, Option.sumWith(x -> x + 1, Some(1)))

    @Test
    def sumWith03(): Unit \ Assert = assertEq(expected = -1, Option.sumWith(x -> x + 1, Some(-2)))


    /////////////////////////////////////////////////////////////////////////////
    // find                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def find01(): Unit \ Assert = assertNone(Option.find(i -> i == 2, None))

    @Test
    def find02(): Unit \ Assert = assertNone(Option.find(i -> i == 2, Some(1)))

    @Test
    def find03(): Unit \ Assert = assertEq(expected = Some(2), Option.find(i -> i == 2, Some(2)))

    /////////////////////////////////////////////////////////////////////////////
    // flatten                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def flatten01(): Unit \ Assert = assertNone((Option.flatten(None): Option[Unit]))

    @Test
    def flatten02(): Unit \ Assert = assertNone(Option.flatten(Some((None: Option[Unit]))))

    @Test
    def flatten03(): Unit \ Assert = assertEq(expected = Some(2), Option.flatten(Some(Some(2))))

    /////////////////////////////////////////////////////////////////////////////
    // fold                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fold01(): Unit \ Assert = assertEq(expected = (), Option.fold((None: Option[Unit])))

    @Test
    def fold02(): Unit \ Assert = assertEq(expected = "a", Option.fold(Some("a")))

    @Test
    def fold03(): Unit \ Assert = assertEq(expected = (), Option.fold(Some(())))

    @Test
    def fold04(): Unit \ Assert = assertEq(expected = ("a", "b", "c"), Option.fold(Some(("a", "b", "c"))))

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def foldLeft01(): Unit \ Assert = assertFalse(Option.foldLeft((b, i) -> if (i == 2 and b) true else false, false, None))

    @Test
    def foldLeft02(): Unit \ Assert = assertFalse(Option.foldLeft((b, i) -> if (i == 2 and b) true else false, false, Some(1)))

    @Test
    def foldLeft03(): Unit \ Assert = assertFalse(Option.foldLeft((b, i) -> if (i == 2 and b) true else false, false, Some(2)))

    @Test
    def foldLeft04(): Unit \ Assert = assertFalse(Option.foldLeft((b, i) -> if (i == 2 and b) true else false, true, Some(1)))

    @Test
    def foldLeft05(): Unit \ Assert = assertTrue(Option.foldLeft((b, i) -> if (i == 2 and b) true else false, true, Some(2)))

    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def foldRight01(): Unit \ Assert = assertFalse(Option.foldRight((i, acc) -> if (i == 2 and acc) true else false, None, false))

    @Test
    def foldRight02(): Unit \ Assert = assertFalse(Option.foldRight((i, acc) -> if (i == 2 and acc) true else false, Some(1), false))

    @Test
    def foldRight03(): Unit \ Assert = assertFalse(Option.foldRight((i, acc) -> if (i == 2 and acc) true else false, Some(2), false))

    @Test
    def foldRight04(): Unit \ Assert = assertFalse(Option.foldRight((i, acc) -> if (i == 2 and acc) true else false, Some(1), true))

    @Test
    def foldRight05(): Unit \ Assert = assertTrue(Option.foldRight((i, acc) -> if (i == 2 and acc) true else false, Some(2), true))

    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithCont                                                       //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def foldRightWithCont01(): Unit \ Assert = assertFalse(Option.foldRightWithCont((i, k) -> if (i == 2 and k()) true else false, None, false))

    @Test
    def foldRightWithCont02(): Unit \ Assert = assertFalse(Option.foldRightWithCont((i, k) -> if (i == 2 and k()) true else false, Some(1), false))

    @Test
    def foldRightWithCont03(): Unit \ Assert = assertFalse(Option.foldRightWithCont((i, k) -> if (i == 2 and k()) true else false, Some(2), false))

    @Test
    def foldRightWithCont04(): Unit \ Assert = assertFalse(Option.foldRightWithCont((i, k) -> if (i == 2 and k()) true else false, Some(1), true))

    @Test
    def foldRightWithCont05(): Unit \ Assert = assertTrue(Option.foldRightWithCont((i, k) -> if (i == 2 and k()) true else false, Some(2), true))

    /////////////////////////////////////////////////////////////////////////////
    // sequence                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def sequence01(): Unit \ Assert = assertEq(expected = Some(Nil), (Option.sequence(Nil): Option[List[Unit]]))

    @Test
    def sequence02(): Unit \ Assert = assertEq(expected = Some(1 :: Nil), Option.sequence(Some(1) :: Nil))

    @Test
    def sequence03(): Unit \ Assert = assertEq(expected = Some(1 :: 2 :: Nil), Option.sequence(Some(1) :: Some(2) :: Nil))

    @Test
    def sequence04(): Unit \ Assert = assertEq(expected = Some(1 :: 2 :: 3 :: Nil), Option.sequence(Some(1) :: Some(2) :: Some(3) :: Nil))

    @Test
    def sequence05(): Unit \ Assert = assertNone(Option.sequence(None :: Some(2) :: Some(3) :: Nil))

    @Test
    def sequence06(): Unit \ Assert = assertNone(Option.sequence(Some(1) :: Some(2) :: None :: Nil))

    @Test
    def sequence07(): Unit \ Assert = assertNone(Option.sequence(Some(1) :: None :: Some(3) :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // traverse                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def traverse01(): Unit \ Assert = assertEq(expected = Some(Nil), Option.traverse(x -> Some(x + 1), Nil))

    @Test
    def traverse02(): Unit \ Assert = assertEq(expected = Some(2 :: Nil), Option.traverse(x -> Some(x + 1), 1 :: Nil))

    @Test
    def traverse03(): Unit \ Assert = assertEq(expected = Some(2 :: 3 :: Nil), Option.traverse(x -> Some(x + 1), 1 :: 2 :: Nil))

    @Test
    def traverse04(): Unit \ Assert = assertEq(expected = Some(2 :: 3 :: 4 :: Nil), Option.traverse(x -> Some(x + 1), 1 :: 2 :: 3 :: Nil))

    @Test
    def traverse05(): Unit \ Assert = assertNone(Option.traverse(x -> if (x == 1) None else Some(x), 1 :: 2 :: 3 :: Nil))

    @Test
    def traverse06(): Unit \ Assert = assertNone(Option.traverse(x -> if (x == 3) None else Some(x), 1 :: 2 :: 3 :: Nil))

    @Test
    def traverse07(): Unit \ Assert = assertNone(Option.traverse(x -> if (x == 2) None else Some(x), 1 :: 2 :: 3 :: Nil))

    @Test
    def traverse08(): Unit \ Assert = assertEq(expected = Some(42 :: 42 :: 42 :: Nil), Option.traverse(_ -> Some(42), 1 :: 2 :: 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // traverseX                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def traverseX01(): Unit \ Assert = assertEq(expected = Some(), Option.traverseX(x -> Some(x + 1), Nil))

    @Test
    def traverseX02(): Unit \ Assert = assertEq(expected = Some(), Option.traverseX(x -> Some(x + 1), 1 :: Nil))

    @Test
    def traverseX03(): Unit \ Assert = assertEq(expected = Some(), Option.traverseX(x -> Some(x + 1), 1 :: 2 :: Nil))

    @Test
    def traverseX04(): Unit \ Assert = assertEq(expected = Some(), Option.traverseX(x -> Some(x + 1), 1 :: 2 :: 3 :: Nil))

    @Test
    def traverseX05(): Unit \ Assert = assertNone(Option.traverseX(x -> if (x == 1) None else Some(x), 1 :: 2 :: 3 :: Nil))

    @Test
    def traverseX06(): Unit \ Assert = assertNone(Option.traverseX(x -> if (x == 3) None else Some(x), 1 :: 2 :: 3 :: Nil))

    @Test
    def traverseX07(): Unit \ Assert = assertNone(Option.traverseX(x -> if (x == 2) None else Some(x), 1 :: 2 :: 3 :: Nil))

    @Test
    def traverseX08(): Unit \ Assert = assertEq(expected = Some(), Option.traverseX(_ -> Some(42), 1 :: 2 :: 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // foldLeftM                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def foldLeftM01(): Unit \ Assert = assertEq(expected = Some(0), Option.foldLeftM((ac, i) -> if (i < 0) None else Some(Int32.max(i, ac)), 0, Nil))

    @Test
    def foldLeftM02(): Unit \ Assert = assertEq(expected = Some(1), Option.foldLeftM((ac, i) -> if (i < 0) None else Some(Int32.max(i, ac)), 0, 1 :: Nil))

    @Test
    def foldLeftM03(): Unit \ Assert = assertEq(expected = Some(2), Option.foldLeftM((ac, i) -> if (i < 0) None else Some(Int32.max(i, ac)), 0, 1 :: 2 :: Nil))

    @Test
    def foldLeftM04(): Unit \ Assert = assertEq(expected = Some(3), Option.foldLeftM((ac, i) -> if (i < 0) None else Some(Int32.max(i, ac)), 0, 1 :: 2 :: 3 :: Nil))

    @Test
    def foldLeftM05(): Unit \ Assert = assertNone(Option.foldLeftM((ac, i) -> if (i < 0) None else Some(Int32.max(i, ac)), 0, -1 :: 2 :: 3 :: Nil))

    @Test
    def foldLeftM06(): Unit \ Assert = assertNone(Option.foldLeftM((ac, i) -> if (i < 0) None else Some(Int32.max(i, ac)), 0, 1 :: -2 :: 3 :: Nil))

    @Test
    def foldLeftM07(): Unit \ Assert = assertNone(Option.foldLeftM((ac, i) -> if (i < 0) None else Some(Int32.max(i, ac)), 0, 1 :: 2 :: -3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // foldRightM                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def foldRightM01(): Unit \ Assert = assertEq(expected = Some(0), Option.foldRightM((i, ac) -> if (i < 0) None else Some(Int32.max(i, ac)), 0, Nil))

    @Test
    def foldRightM02(): Unit \ Assert = assertEq(expected = Some(1), Option.foldRightM((i, ac) -> if (i < 0) None else Some(Int32.max(i, ac)), 0, 1 :: Nil))

    @Test
    def foldRightM03(): Unit \ Assert = assertEq(expected = Some(2), Option.foldRightM((i, ac) -> if (i < 0) None else Some(Int32.max(i, ac)), 0, 1 :: 2 :: Nil))

    @Test
    def foldRightM04(): Unit \ Assert = assertEq(expected = Some(3), Option.foldRightM((i, ac) -> if (i < 0) None else Some(Int32.max(i, ac)), 0, 1 :: 2 :: 3 :: Nil))

    @Test
    def foldRightM05(): Unit \ Assert = assertNone(Option.foldRightM((i, ac) -> if (i < 0) None else Some(Int32.max(i, ac)), 0, -1 :: 2 :: 3 :: Nil))

    @Test
    def foldRightM06(): Unit \ Assert = assertNone(Option.foldRightM((i, ac) -> if (i < 0) None else Some(Int32.max(i, ac)), 0, 1 :: -2 :: 3 :: Nil))

    @Test
    def foldRightM07(): Unit \ Assert = assertNone(Option.foldRightM((i, ac) -> if (i < 0) None else Some(Int32.max(i, ac)), 0, 1 :: 2 :: -3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // zip                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def zip01(): Unit \ Assert = assertNone(Option.zip((None: Option[Unit]), (None: Option[Unit])))

    @Test
    def zip02(): Unit \ Assert = assertNone(Option.zip(Some(1), (None: Option[Unit])))

    @Test
    def zip03(): Unit \ Assert = assertNone(Option.zip((None: Option[Unit]), Some(true)))

    @Test
    def zip04(): Unit \ Assert = assertEq(expected = Some((1, true)), Option.zip(Some(1), Some(true)))

    /////////////////////////////////////////////////////////////////////////////
    // unzip                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def unzip01(): Unit \ Assert = assertEq(expected = (None, None), Option.unzip((None: Option[(Unit, Unit)])))

    @Test
    def unzip02(): Unit \ Assert = assertEq(expected = (Some(1), Some(true)), Option.unzip(Some((1, true))))

    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEach01(): Unit \ Assert = region rc {
        let ri = Ref.fresh(rc, 21);
        Option.forEach(x -> Ref.put(x, ri), None);
        assertEq(expected = 21, Ref.get(ri))
    }

    @Test
    def forEach02(): Unit \ Assert = region rc {
        let ri = Ref.fresh(rc, 21);
        Option.forEach(x -> Ref.put(x, ri), Some(42));
        assertEq(expected = 42, Ref.get(ri))
    }

    /////////////////////////////////////////////////////////////////////////////
    // point                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def point01(): Unit \ Assert = assertEq(expected = Some(5), Option.point(5))

    /////////////////////////////////////////////////////////////////////////////
    // ap                                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def ap01(): Unit \ Assert = assertEq(expected = Some(124), Option.ap(Some(x -> x + 1), Some(123)))

    @Test
    def ap02(): Unit \ Assert = assertNone(Option.ap(Some(x -> x + 1), (None: Option[Int32])))

    @Test
    def ap03(): Unit \ Assert = assertNone((Option.ap(None, Some(123)): Option[Int32]))

    @Test
    def ap04(): Unit \ Assert = assertNone((Option.ap(None, None): Option[Unit]))

    /////////////////////////////////////////////////////////////////////////////
    // flatMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def flatMap01(): Unit \ Assert = assertEq(expected = Some(124), Option.flatMap(x -> Some(x + 1), Some(123)))

    @Test
    def flatMap02(): Unit \ Assert = assertNone(Option.flatMap(x -> Some(x + 1), (None: Option[Int32])))

    @Test
    def flatMap03(): Unit \ Assert = assertNone((Option.flatMap(_x -> None, Some(123)): Option[Int32]))

    @Test
    def flatMap04(): Unit \ Assert = assertNone((Option.flatMap(_x -> None, None): Option[Unit]))

    @Test
    def flatMap05(): Unit \ Assert = assertNone(Option.flatMap(i -> if (i == 1) None else Some(2*i), None))

    @Test
    def flatMap06(): Unit \ Assert = assertNone(Option.flatMap(i -> if (i == 1) None else Some(2*i), Some(1)))

    @Test
    def flatMap07(): Unit \ Assert = assertEq(expected = Some(4), Option.flatMap(i -> if (i == 1) None else Some(2*i), Some(2)))

    /////////////////////////////////////////////////////////////////////////////
    // map2                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map201(): Unit \ Assert = assertEq(expected = Some(124), Option.map2((x1,x2) -> x1 + x2, Some(123), Some(1)))

    @Test
    def map202(): Unit \ Assert = assertNone(Option.map2((x1,x2) -> x1 + x2, Some(123), None))

    @Test
    def map203(): Unit \ Assert = assertNone(Option.map2((x1,x2) -> x1 + x2, None, Some(1)))

    @Test
    def map204(): Unit \ Assert = assertNone(Option.map2((x1,x2) -> x1 + x2, None, (None: Option[Int32])))

    /////////////////////////////////////////////////////////////////////////////
    // map3                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map301(): Unit \ Assert = assertEq(expected = Some(126), Option.map3((x1,x2,x3) -> x1 + x2 + x3, Some(123), Some(1), Some(2)))

    @Test
    def map302(): Unit \ Assert = assertNone(Option.map3((x1,x2,x3) -> x1 + x2 + x3, Some(123), Some(1), None))

    @Test
    def map303(): Unit \ Assert = assertNone(Option.map3((x1,x2,x3) -> x1 + x2 + x3, Some(123), None, Some(2)))

    @Test
    def map304(): Unit \ Assert = assertNone(Option.map3((x1,x2,x3) -> x1 + x2 + x3, None, Some(1), Some(2)))

    @Test
    def map305(): Unit \ Assert = assertNone(Option.map3((x1,x2,x3) -> x1 + x2 + x3, Some(123), None, None))

    @Test
    def map306(): Unit \ Assert = assertNone(Option.map3((x1,x2,x3) -> x1 + x2 + x3, None, Some(1), None))

    @Test
    def map307(): Unit \ Assert = assertNone(Option.map3((x1,x2,x3) -> x1 + x2 + x3, None, None, (None: Option[Int32])))

    /////////////////////////////////////////////////////////////////////////////
    // map4                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map401(): Unit \ Assert = assertEq(expected = Some(129), Option.map4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Some(123), Some(1), Some(2), Some(3)))

    @Test
    def map402(): Unit \ Assert = assertNone(Option.map4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Some(123), Some(1), Some(2), None))

    @Test
    def map403(): Unit \ Assert = assertNone(Option.map4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Some(123), Some(1), None, Some(3)))

    @Test
    def map404(): Unit \ Assert = assertNone(Option.map4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, Some(123), None, Some(2), Some(3)))

    @Test
    def map405(): Unit \ Assert = assertNone(Option.map4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, None, Some(1), Some(2), Some(3)))

    @Test
    def map406(): Unit \ Assert = assertNone(Option.map4((x1,x2,x3,x4) -> x1 + x2 + x3 + x4, None, None, None, (None: Option[Int32])))

    /////////////////////////////////////////////////////////////////////////////
    // map5                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map501(): Unit \ Assert = assertEq(expected = Some(133), Option.map5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Some(123), Some(1), Some(2), Some(3), Some(4)))

    @Test
    def map502(): Unit \ Assert = assertNone(Option.map5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Some(123), Some(1), Some(2), Some(3), None))

    @Test
    def map503(): Unit \ Assert = assertNone(Option.map5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Some(123), Some(1), Some(2), None, Some(4)))

    @Test
    def map504(): Unit \ Assert = assertNone(Option.map5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Some(123), Some(1), None, Some(3), Some(4)))

    @Test
    def map505(): Unit \ Assert = assertNone(Option.map5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, Some(123), None, Some(2), Some(3), Some(4)))

    @Test
    def map506(): Unit \ Assert = assertNone(Option.map5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, None, Some(1), Some(2), Some(3), Some(4)))

    @Test
    def map507(): Unit \ Assert = assertNone(Option.map5((x1,x2,x3,x4,x5) -> x1 + x2 + x3 + x4 + x5, None, None, None, None, (None: Option[Int32])))

    /////////////////////////////////////////////////////////////////////////////
    // map6                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map601(): Unit \ Assert = assertEq(expected = Some(138), Option.map6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Some(123), Some(1), Some(2), Some(3), Some(4), Some(5)))

    @Test
    def map602(): Unit \ Assert = assertNone(Option.map6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Some(123), Some(1), Some(2), Some(3), Some(4), None))

    @Test
    def map603(): Unit \ Assert = assertNone(Option.map6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Some(123), Some(1), Some(2), Some(3), None, Some(5)))

    @Test
    def map604(): Unit \ Assert = assertNone(Option.map6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Some(123), Some(1), Some(2), None, Some(4), Some(5)))

    @Test
    def map605(): Unit \ Assert = assertNone(Option.map6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Some(123), Some(1), None, Some(3), Some(4), Some(5)))

    @Test
    def map606(): Unit \ Assert = assertNone(Option.map6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, Some(123), None, Some(2), Some(3), Some(4), Some(5)))

    @Test
    def map607(): Unit \ Assert = assertNone(Option.map6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, None, Some(1), Some(2), Some(3), Some(4), Some(5)))

    @Test
    def map608(): Unit \ Assert = assertNone(Option.map6((x1,x2,x3,x4,x5,x6) -> x1 + x2 + x3 + x4 + x5 + x6, None, None, None, None, None, (None: Option[Int32])))

    /////////////////////////////////////////////////////////////////////////////
    // map7                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map701(): Unit \ Assert = assertEq(expected = Some(144), Option.map7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Some(123), Some(1), Some(2), Some(3), Some(4), Some(5), Some(6)))

    @Test
    def map702(): Unit \ Assert = assertNone(Option.map7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Some(123), Some(1), Some(2), Some(3), Some(4), Some(5), None))

    @Test
    def map703(): Unit \ Assert = assertNone(Option.map7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Some(123), Some(1), Some(2), Some(3), Some(4), None, Some(6)))

    @Test
    def map704(): Unit \ Assert = assertNone(Option.map7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Some(123), Some(1), Some(2), Some(3), None, Some(5), Some(6)))

    @Test
    def map705(): Unit \ Assert = assertNone(Option.map7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Some(123), Some(1), Some(2), None, Some(4), Some(5), Some(6)))

    @Test
    def map706(): Unit \ Assert = assertNone(Option.map7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Some(123), Some(1), None, Some(3), Some(4), Some(5), Some(6)))

    @Test
    def map707(): Unit \ Assert = assertNone(Option.map7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, Some(123), None, Some(2), Some(3), Some(4), Some(5), Some(6)))

    @Test
    def map708(): Unit \ Assert = assertNone(Option.map7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, None, Some(1), Some(2), Some(3), Some(4), Some(5), Some(6)))

    @Test
    def map709(): Unit \ Assert = assertNone(Option.map7((x1,x2,x3,x4,x5,x6,x7) -> x1 + x2 + x3 + x4 + x5 + x6 + x7, None, None, None, None, None, None, (None: Option[Int32])))

    /////////////////////////////////////////////////////////////////////////////
    // map8                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map801(): Unit \ Assert = assertEq(expected = Some(151), Option.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Some(123), Some(1), Some(2), Some(3), Some(4), Some(5), Some(6), Some(7)))

    @Test
    def map802(): Unit \ Assert = assertNone(Option.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Some(123), Some(1), Some(2), Some(3), Some(4), Some(5), Some(6), None))

    @Test
    def map803(): Unit \ Assert = assertNone(Option.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Some(123), Some(1), Some(2), Some(3), Some(4), Some(5), None, Some(7)))

    @Test
    def map804(): Unit \ Assert = assertNone(Option.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Some(123), Some(1), Some(2), Some(3), Some(4), None, Some(6), Some(7)))

    @Test
    def map805(): Unit \ Assert = assertNone(Option.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Some(123), Some(1), Some(2), Some(3), None, Some(5), Some(6), Some(7)))

    @Test
    def map806(): Unit \ Assert = assertNone(Option.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Some(123), Some(1), Some(2), None, Some(4), Some(5), Some(6), Some(7)))

    @Test
    def map807(): Unit \ Assert = assertNone(Option.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Some(123), Some(1), None, Some(3), Some(4), Some(5), Some(6), Some(7)))

    @Test
    def map808(): Unit \ Assert = assertNone(Option.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, Some(123), None, Some(2), Some(3), Some(4), Some(5), Some(6), Some(7)))

    @Test
    def map809(): Unit \ Assert = assertNone(Option.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, None, Some(1), Some(2), Some(3), Some(4), Some(5), Some(6), Some(7)))

    @Test
    def map810(): Unit \ Assert = assertNone(Option.map8((x1,x2,x3,x4,x5,x6,x7,x8) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, None, None, None, None, None, None, None, (None: Option[Int32])))

    /////////////////////////////////////////////////////////////////////////////
    // map9                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map901(): Unit \ Assert = assertEq(expected = Some(159), Option.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Some(123), Some(1), Some(2), Some(3), Some(4), Some(5), Some(6), Some(7), Some(8)))

    @Test
    def map902(): Unit \ Assert = assertNone(Option.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Some(123), Some(1), Some(2), Some(3), Some(4), Some(5), Some(6), Some(7), None))

    @Test
    def map903(): Unit \ Assert = assertNone(Option.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Some(123), Some(1), Some(2), Some(3), Some(4), Some(5), Some(6), None, Some(8)))

    @Test
    def map904(): Unit \ Assert = assertNone(Option.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Some(123), Some(1), Some(2), Some(3), Some(4), Some(5), None, Some(7), Some(8)))

    @Test
    def map905(): Unit \ Assert = assertNone(Option.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Some(123), Some(1), Some(2), Some(3), Some(4), None, Some(6), Some(7), Some(8)))

    @Test
    def map906(): Unit \ Assert = assertNone(Option.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Some(123), Some(1), Some(2), Some(3), None, Some(5), Some(6), Some(7), Some(8)))

    @Test
    def map907(): Unit \ Assert = assertNone(Option.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Some(123), Some(1), Some(2), None, Some(4), Some(5), Some(6), Some(7), Some(8)))

    @Test
    def map908(): Unit \ Assert = assertNone(Option.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Some(123), Some(1), None, Some(3), Some(4), Some(5), Some(6), Some(7), Some(8)))

    @Test
    def map909(): Unit \ Assert = assertNone(Option.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, Some(123), None, Some(2), Some(3), Some(4), Some(5), Some(6), Some(7), Some(8)))

    @Test
    def map910(): Unit \ Assert = assertNone(Option.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, None, Some(1), Some(2), Some(3), Some(4), Some(5), Some(6), Some(7), Some(8)))

    @Test
    def map911(): Unit \ Assert = assertNone(Option.map9((x1,x2,x3,x4,x5,x6,x7,x8,x9) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9, None, None, None, None, None, None, None, None, (None: Option[Int32])))

    /////////////////////////////////////////////////////////////////////////////
    // map10                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map1001(): Unit \ Assert = assertEq(expected = Some(168), Option.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Some(123), Some(1), Some(2), Some(3), Some(4), Some(5), Some(6), Some(7), Some(8), Some(9)))

    @Test
    def map1002(): Unit \ Assert = assertNone(Option.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Some(123), Some(1), Some(2), Some(3), Some(4), Some(5), Some(6), Some(7), Some(8), None))

    @Test
    def map1003(): Unit \ Assert = assertNone(Option.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Some(123), Some(1), Some(2), Some(3), Some(4), Some(5), Some(6), Some(7), None, Some(9)))

    @Test
    def map1004(): Unit \ Assert = assertNone(Option.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Some(123), Some(1), Some(2), Some(3), Some(4), Some(5), Some(6), None, Some(8), Some(9)))

    @Test
    def map1005(): Unit \ Assert = assertNone(Option.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Some(123), Some(1), Some(2), Some(3), Some(4), Some(5), None, Some(7), Some(8), Some(9)))

    @Test
    def map1006(): Unit \ Assert = assertNone(Option.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Some(123), Some(1), Some(2), Some(3), Some(4), None, Some(6), Some(7), Some(8), Some(9)))

    @Test
    def map1007(): Unit \ Assert = assertNone(Option.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Some(123), Some(1), Some(2), Some(3), None, Some(5), Some(6), Some(7), Some(8), Some(9)))

    @Test
    def map1008(): Unit \ Assert = assertNone(Option.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Some(123), Some(1), Some(2), None, Some(4), Some(5), Some(6), Some(7), Some(8), Some(9)))

    @Test
    def map1009(): Unit \ Assert = assertNone(Option.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Some(123), Some(1), None, Some(3), Some(4), Some(5), Some(6), Some(7), Some(8), Some(9)))

    @Test
    def map1010(): Unit \ Assert = assertNone(Option.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, Some(123), None, Some(2), Some(3), Some(4), Some(5), Some(6), Some(7), Some(8), Some(9)))

    @Test
    def map1011(): Unit \ Assert = assertNone(Option.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, None, Some(1), Some(2), Some(3), Some(4), Some(5), Some(6), Some(7), Some(8), Some(9)))

    @Test
    def map1012(): Unit \ Assert = assertNone(Option.map10((x1,x2,x3,x4,x5,x6,x7,x8,x9,x10) -> x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, None, None, None, None, None, None, None, None, None, (None: Option[Int32])))

    /////////////////////////////////////////////////////////////////////////////
    // hash                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def hash01(): Unit \ Assert = assertEq(expected = hash((None: Option[Unit])), hash((None: Option[Unit])))

    @Test
    def hash02(): Unit \ Assert = assertEq(expected = hash(Some(1)), hash(Some(1)))

    @Test
    def hash03(): Unit \ Assert = assertEq(expected = hash(Some((1, 1))), hash(Some((1, 1))))

    @Test
    def hash04(): Unit \ Assert = assertTrue(hash(Some((1, 2))) != hash(Some((2, 1))))

    @Test
    def hash05(): Unit \ Assert = assertTrue(hash((None: Option[Unit])) != hash(Some((()))))


    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iterator01(): Unit \ Assert = region rc {
        assertEq(expected = Nil, (None: Option[Int32]) |> Option.iterator(rc) |> Iterator.toList)
    }

    @Test
    def iterator02(): Unit \ Assert = region rc {
        assertEq(expected = 1 :: Nil, Some(1) |> Option.iterator(rc) |> Iterator.toList)
    }

    @Test
    def iterator03(): Unit \ Assert = region rc {
        assertEq(expected = (1 :: 2 :: 3 :: Nil) :: Nil, Some(1 :: 2 :: 3 :: Nil) |> Option.iterator(rc) |> Iterator.toList)
    }

}
