mod Test.Exp.ReifyType {

    //////////////////////////////////////
    // Emulating reifyType              //
    //////////////////////////////////////

    @Test
    def typeMatchUnit01(): Bool = typematch () {
        case _: Unit => true
        case _: _ => false
    }

    @Test
    def typeMatchUnit02(): Bool = typematch () {
        case x: Unit => x == ()
        case _: _ => false
    }

    @Test
    def typeMatchBool01(): Bool = typematch true {
        case _: Bool => true
        case _: _ => false
    }

    @Test
    def typeMatchBool02(): Bool = typematch true {
        case x: Bool => x == true
        case _: _ => false
    }

    @Test
    def typeMatchChar01(): Bool = typematch 'a' {
        case _: Char => true
        case _: _ => false
    }

    @Test
    def typeMatchChar02(): Bool = typematch 'a' {
        case x: Char => x == 'a'
        case _: _ => false
    }

    @Test
    def typeMatchFloat32_01(): Bool = typematch 0.123f32 {
        case _: Float32 => true
        case _: _ => false
    }

    @Test
    def typeMatchFloat32_02(): Bool = typematch 0.123f32 {
        case x: Float32 => x == 0.123f32
        case _: _ => false
    }

    @Test
    def typeMatchFloat64_01(): Bool = typematch 0.123f64 {
        case _: Float64 => true
        case _: _ => false
    }

    @Test
    def typeMatchFloat64_02(): Bool = typematch 0.123f64 {
        case x: Float64 => x == 0.123f64
        case _: _ => false
    }

    @Test
    def typeMatchInt8_01(): Bool = typematch 123i8 {
        case _: Int8 => true
        case _: _ => false
    }

    @Test
    def typeMatchInt8_02(): Bool = typematch 123i8 {
        case x: Int8 => x == 123i8
        case _: _ => false
    }

    @Test
    def typeMatchInt16_01(): Bool = typematch 123i16 {
        case _: Int16 => true
        case _: _ => false
    }

    @Test
    def typeMatchInt16_02(): Bool = typematch 123i16 {
        case x: Int16 => x == 123i16
        case _: _ => false
    }

    @Test
    def typeMatchInt32_01(): Bool = typematch 123i32 {
        case _: Int32 => true
        case _: _ => false
    }

    @Test
    def typeMatchInt32_02(): Bool = typematch 123i32 {
        case x: Int32 => x == 123i32
        case _: _ => false
    }

    @Test
    def typeMatchInt64_01(): Bool = typematch 123i64 {
        case _: Int64 => true
        case _: _ => false
    }

    @Test
    def typeMatchInt64_02(): Bool = typematch 123i64 {
        case x: Int64 => x == 123i64
        case _: _ => false
    }

    @Test
    def typeMatchBigInt01(): Bool = typematch 123ii {
        case _: BigInt => true
        case _: _ => false
    }

    @Test
    def typeMatchBigInt02(): Bool = typematch 123ii {
        case x: BigInt => x == 123ii
        case _: _ => false
    }

    @Test
    def typeMatchString01(): Bool = typematch "hello" {
        case _: String => true
        case _: _ => false
    }

    @Test
    def typeMatchString02(): Bool = typematch "hello" {
        case x: String => x == "hello"
        case _: _ => false
    }

    @Test
    def typeMatchArrayUnit01(): Bool = region rc {
        typematch Array#{()} @ rc {
            case _: Array[Unit, rc] => true
            case _: _ => false
        }
    }

    @Test
    def typeMatchArrayUnit02(): Bool = region rc {
        typematch Array#{()} @ rc {
            case x: Array[Unit, rc] => Array.sameElements(x, Array#{()} @ rc)
            case _: _ => false
        }
    }

    @Test
    def typeMatchArrayBool01(): Bool = region rc {
        typematch Array#{true} @ rc {
            case _: Array[Bool, rc] => true
            case _: _ => false
        }
    }

    @Test
    def typeMatchArrayBool02(): Bool = region rc {
        typematch Array#{true} @ rc {
            case x: Array[Bool, rc] => Array.sameElements(x, Array#{true} @ rc)
            case _: _ => false
        }
    }

    @Test
    def typeMatchArrayChar01(): Bool = region rc {
        typematch Array#{'a'} @ rc {
            case _: Array[Char, rc] => true
            case _: _ => false
        }
    }

    @Test
    def typeMatchArrayChar02(): Bool = region rc {
        typematch Array#{'a'} @ rc {
            case x: Array[Char, rc] => Array.sameElements(x, Array#{'a'} @ rc)
            case _: _ => false
        }
    }

    @Test
    def typeMatchArrayFloat32_01(): Bool = region rc {
        typematch Array#{0.123f32} @ rc {
            case _: Array[Float32, rc] => true
            case _: _ => false
        }
    }

    @Test
    def typeMatchArrayFloat32_02(): Bool = region rc {
        typematch Array#{0.123f32} @ rc {
            case x: Array[Float32, rc] => Array.sameElements(x, Array#{0.123f32} @ rc)
            case _: _ => false
        }
    }

    @Test
    def typeMatchArrayFloat64_01(): Bool = region rc {
        typematch Array#{0.123f64} @ rc {
            case _: Array[Float64, rc] => true
            case _: _ => false
        }
    }

    @Test
    def typeMatchArrayFloat64_02(): Bool = region rc {
        typematch Array#{0.123f64} @ rc {
            case x: Array[Float64, rc] => Array.sameElements(x, Array#{0.123f64} @ rc)
            case _: _ => false
        }
    }

    @Test
    def typeMatchArrayInt8_01(): Bool = region rc {
        typematch Array#{123i8} @ rc {
            case _: Array[Int8, rc] => true
            case _: _ => false
        }
    }

    @Test
    def typeMatchArrayInt8_02(): Bool = region rc {
        typematch Array#{123i8} @ rc {
            case x: Array[Int8, rc] => Array.sameElements(x, Array#{123i8} @ rc)
            case _: _ => false
        }
    }

    @Test
    def typeMatchArrayInt16_01(): Bool = region rc {
        typematch Array#{123i16} @ rc {
            case _: Array[Int16, rc] => true
            case _: _ => false
        }
    }

    @Test
    def typeMatchArrayInt16_02(): Bool = region rc {
        typematch Array#{123i16} @ rc {
            case x: Array[Int16, rc] => Array.sameElements(x, Array#{123i16} @ rc)
            case _: _ => false
        }
    }

    @Test
    def typeMatchArrayInt32_01(): Bool = region rc {
        typematch Array#{123i32} @ rc {
            case _: Array[Int32, rc] => true
            case _: _ => false
        }
    }

    @Test
    def typeMatchArrayInt32_02(): Bool = region rc {
        typematch Array#{123i32} @ rc {
            case x: Array[Int32, rc] => Array.sameElements(x, Array#{123i32} @ rc)
            case _: _ => false
        }
    }

    @Test
    def typeMatchArrayInt64_01(): Bool = region rc {
        typematch Array#{123i64} @ rc {
            case _: Array[Int64, rc] => true
            case _: _ => false
        }
    }

    @Test
    def typeMatchArrayInt64_02(): Bool = region rc {
        typematch Array#{123i64} @ rc {
            case x: Array[Int64, rc] => Array.sameElements(x, Array#{123i64} @ rc)
            case _: _ => false
        }
    }

    @Test
    def typeMatchArrayBigInt01(): Bool = region rc {
        typematch Array#{123ii} @ rc {
            case _: Array[BigInt, rc] => true
            case _: _ => false
        }
    }

    @Test
    def typeMatchArrayBigInt02(): Bool = region rc {
        typematch Array#{123ii} @ rc {
            case x: Array[BigInt, rc] => Array.sameElements(x, Array#{123ii} @ rc)
            case _: _ => false
        }
    }

    @Test
    def typeMatchArrayString01(): Bool = region rc {
        typematch Array#{"hello"} @ rc {
            case _: Array[String, rc] => true
            case _: _ => false
        }
    }

    @Test
    def typeMatchArrayString02(): Bool = region rc {
        typematch Array#{"hello"} @ rc {
            case x: Array[String, rc] => Array.sameElements(x, Array#{"hello"} @ rc)
            case _: _ => false
        }
    }

    //////////////////////////////////////
    // Emulating reifyEff               //
    //////////////////////////////////////

    @Test
    def typeMatchEff01(): Bool = hof(_ -> 123)

    @Test
    def typeMatchEff02(): Bool = not hof(_ -> unchecked_cast(123 as _ \ IO))

    @Test
    def typeMatchEff03(): Bool = not hof(_ -> unchecked_cast(123 as _ \ IO))

    @Test
    def typeMatchEff04(): Bool = hof((x -> x) >> (y -> y))

    @Test
    def typeMatchEff05(): Bool = not hof((x -> x) >> (y -> unchecked_cast(y as _ \ IO)))

    @Test
    def typeMatchEff06(): Bool = not hof((x -> unchecked_cast(x as _ \ IO)) >> (y -> y))

    @Test
    def typeMatchEff07(): Bool = not hof((x -> unchecked_cast(x as _ \ IO)) >> (y -> unchecked_cast(y as _ \ IO)))

    @Test
    def typeMatchEff08(): Bool = not hof((x -> x) >> (y -> unchecked_cast(y as _ \ IO)))

    @Test
    def typeMatchEff09(): Bool = not hof((x -> unchecked_cast(x as _ \ IO)) >> (y -> y))

    @Test
    def typeMatchEff10(): Bool = not hof((x -> unchecked_cast(x as _ \ IO)) >> (y -> unchecked_cast(y as _ \ IO)))

    @Test
    def typeMatchEff11(): Bool = not hof((x -> unchecked_cast(x as _ \ IO)) >> (y -> unchecked_cast(y as _ \ IO)))

    @Test
    def typeMatchEff12(): Bool = not hof((x -> unchecked_cast(x as _ \ IO)) >> (y -> unchecked_cast(y as _ \ IO)))

    def hof(f: a -> b \ ef): Bool = typematch f {
        case _: _ -> _ \ {} => true
        case _: _ => false
    }

    def >>(f: a -> b \ ef1, g: b -> c \ ef2): a -> c \ { ef1, ef2 } = x -> g(f(x))

    pub eff E

    //////////////////////////////////////
    // Reflecting on input              //
    //////////////////////////////////////

    def reflectBool(x: a): Bool = typematch x {
      case r: Bool => r == true or r == false
      case _: _ => false
    }

    @Test
    def polyTypeMatchBool01(): Bool = reflectBool(true)

    @Test
    def polyTypeMatchBool02(): Bool = reflectBool(false)

    @Test
    def polyTypeMatchBool03(): Bool = not reflectBool(123i32)

    @Test
    def polyTypeMatchBool04(): Bool = not reflectBool("abc")

    def reflectChar(x: a): Bool = typematch x {
      case r: Char => r <= 'a' or r > 'a'
      case _: _ => false
    }

    @Test
    def polyTypeMatchChar01(): Bool = reflectChar('a')

    @Test
    def polyTypeMatchChar02(): Bool = reflectChar('\u1234')

    @Test
    def polyTypeMatchChar03(): Bool = not reflectChar(123i32)

    @Test
    def polyTypeMatchChar04(): Bool = not reflectChar("abc")

    def reflectFloat32(x: a): Bool = typematch x {
      case r: Float32 => r <= 0.0f32 or r > 0.0f32
      case _: _ => false
    }

    @Test
    def polyTypeMatchFloat32_01(): Bool = reflectFloat32(123.0f32)

    @Test
    def polyTypeMatchFloat32_02(): Bool = reflectFloat32(-123.0f32)

    @Test
    def polyTypeMatchFloat32_03(): Bool = not reflectFloat32(123i32)

    @Test
    def polyTypeMatchFloat32_04(): Bool = not reflectFloat32("abc")

    def reflectFloat64(x: a): Bool = typematch x {
      case r: Float64 => r <= 0.0f64 or r > 0.0f64
      case _: _ => false
    }

    @Test
    def polyTypeMatchFloat64_01(): Bool = reflectFloat64(123.0f64)

    @Test
    def polyTypeMatchFloat64_02(): Bool = reflectFloat64(-123.0f64)

    @Test
    def polyTypeMatchFloat64_03(): Bool = not reflectFloat64(123i64)

    @Test
    def polyTypeMatchFloat64_04(): Bool = not reflectFloat64("abc")

    def reflectInt8(x: a): Bool = typematch x {
      case r: Int8 => r <= 0i8 or r > 0i8
      case _: _ => false
    }

    @Test
    def polyTypeMatchInt8_01(): Bool = reflectInt8(123i8)

    @Test
    def polyTypeMatchInt8_02(): Bool = reflectInt8(-123i8)

    @Test
    def polyTypeMatchInt8_03(): Bool = not reflectInt8(123i64)

    @Test
    def polyTypeMatchInt8_04(): Bool = not reflectInt8("abc")

    def reflectInt16(x: a): Bool = typematch x {
      case r: Int16 => r <= 0i16 or r > 0i16
      case _: _ => false
    }

    @Test
    def polyTypeMatchInt16_01(): Bool = reflectInt16(123i16)

    @Test
    def polyTypeMatchInt16_02(): Bool = reflectInt16(-123i16)

    @Test
    def polyTypeMatchInt16_03(): Bool = not reflectInt16(123i64)

    @Test
    def polyTypeMatchInt16_04(): Bool = not reflectInt16("abc")

    def reflectInt32(x: a): Bool = typematch x {
      case r: Int32 => r <= 0i32 or r > 0i32
      case _: _ => false
    }

    @Test
    def polyTypeMatchInt32_01(): Bool = reflectInt32(123i32)

    @Test
    def polyTypeMatchInt32_02(): Bool = reflectInt32(-123i32)

    @Test
    def polyTypeMatchInt32_03(): Bool = not reflectInt32(123i64)

    @Test
    def polyTypeMatchInt32_04(): Bool = not reflectInt32("abc")

    def reflectInt64(x: a): Bool = typematch x {
      case r: Int64 => r <= 0i64 or r > 0i64
      case _: _ => false
    }

    @Test
    def polyTypeMatchInt64_01(): Bool = reflectInt64(123i64)

    @Test
    def polyTypeMatchInt64_02(): Bool = reflectInt64(-123i64)

    @Test
    def polyTypeMatchInt64_03(): Bool = not reflectInt64(123ii)

    @Test
    def polyTypeMatchInt64_04(): Bool = not reflectInt64("abc")

    def reflectBigInt(x: a): Bool = typematch x {
      case r: BigInt => r <= 0ii or r > 0ii
      case _: _ => false
    }

    @Test
    def polyTypeMatchBigInt_01(): Bool = reflectBigInt(123ii)

    @Test
    def polyTypeMatchBigInt_02(): Bool = reflectBigInt(-123ii)

    @Test
    def polyTypeMatchBigInt_03(): Bool = not reflectBigInt(123i64)

    @Test
    def polyTypeMatchBigInt_04(): Bool = not reflectBigInt("abc")

    def reflectString(x: a): Bool = typematch x {
      case r: String => r <= "hi" or r > "hi"
      case _: _ => false
    }

    @Test
    def polyTypeMatchString_01(): Bool = reflectString("hi")

    @Test
    def polyTypeMatchString_02(): Bool = reflectString("\u1234")

    @Test
    def polyTypeMatchString_03(): Bool = not reflectString(123i64)

    @Test
    def polyTypeMatchString_04(): Bool = not reflectString('a')

    //////////////////////////////////////
    // Complex tests                    //
    //////////////////////////////////////

    def sameTypes(x: a, _: b): Bool = typematch x {
        case _: b => true
        case _: _ => false
    }

    @Test
    def sameTypes01(): Bool = sameTypes(123, 456)

    @Test
    def sameTypes02(): Bool = sameTypes(123 :: Nil, 456 :: Nil)

    @Test
    def sameTypes03(): Bool = not sameTypes(123, 456.0)

    @Test
    def sameTypes04(): Bool = not sameTypes(123 :: Nil, 456.0 :: Nil)

    @Test
    def sameTypes05(): Bool = not sameTypes(123 :: Nil, Nil)

    def hasListSomewhere(x: a): Bool = typematch x {
        case _: List[_] => true
        case _: _[List] => true
        case _: _[List[_]] => true
        case _: _[List, _] => true
        case _: _[List[_], _] => true
        case _: _ => false
    }

    @Test
    def hasListSomewhere01(): Bool = hasListSomewhere(Nil)

    @Test
    def hasListSomewhere02(): Bool = hasListSomewhere(Some(Nil))

    @Test
    def hasListSomewhere03(): Bool = not hasListSomewhere(123)

    @Test
    def hasListSomewhere04(): Bool = not hasListSomewhere(None)

    def isPredicateNotFromInt32(f: a): Bool = typematch f {
        case _: Int32 -> _ => false
        case _: _ -> Bool => true
        case _: _ => false
    }

    @Test
    def isPredicateNotFromInt32_01(): Bool = isPredicateNotFromInt32(_ -> true)

    @Test
    def isPredicateNotFromInt32_02(): Bool = isPredicateNotFromInt32(x -> not x)

    @Test
    def isPredicateNotFromInt32_03(): Bool = not isPredicateNotFromInt32("hi")

    @Test
    def isPredicateNotFromInt32_04(): Bool = not isPredicateNotFromInt32(x -> x == 123)

    def isAppliedType(x: a): Bool = typematch x {
        case _: _[_] => true
        case _: _ => false
    }

    @Test
    def isAppliedType01(): Bool = isAppliedType(Nil)

    @Test
    def isAppliedType02(): Bool = isAppliedType(Map#{})

    @Test
    def isAppliedType03(): Bool = not isAppliedType(123)

    def isEmptyRecord(x: {| r}): Bool = typematch x {
        case _: {} => true
        case _: _ => false
    }

    @Test
    def isEmptyRecord01(): Bool = isEmptyRecord({})

    @Test
    def isEmptyRecord02(): Bool = not isEmptyRecord({age = 123})

    ///
    /// Applies the pure function if one is pure.
    /// If both or neither is pure, applies `f1`.
    ///
    def applyThePureOne(f1: a -> b \ ef1, f2: a -> b \ ef2, x: a): b \ ef1 = typematch f1 {
        case f: a -> b \ {} => f(x)
        case _: _ => typematch f2 {
            case f: a -> b \ {} => f(x)
            case _: _ => f1(x)
        }
    }

    @Test
    def applyThePureOne01(): Bool = {
        let f1 = _ -> true;
        let f2 = _ -> false;
        applyThePureOne(f1, f2, 123)
    }

    @Test
    def applyThePureOne02(): Bool \ IO = {
        let f1 = _ -> checked_ecast(false);
        let f2 = _ -> true;
        applyThePureOne(f1, f2, 123)
    }

    @Test
    def applyThePureOne03(): Bool = {
        let f1 = _ -> true;
        let f2 = _ -> checked_ecast(false);
        applyThePureOne(f1, f2, 123)
    }

    @Test
    def applyThePureOne05(): Bool \ IO = {
        let f1 = _ -> unchecked_cast(true as _ \ IO);
        let f2 = _ -> unchecked_cast(false as _ \ IO);
        applyThePureOne(f1, f2, 123)
    }

    @Test
    def typeMatchFallThrough01(): Bool = typematch "hello" {
        case x: Int8 => x == 123i8
        case x: Int64 => x == 123i64
        case _: _ => true
    }

    @Test
    def typeMatchPolymorphic01(): Bool = typematch Nil {
        case _: List[Unit] => false
        case _: List[_] => true
        case _: _ => true
    }

    @Test
    def typeMatchPolymorphic02(): Bool = typematch (Nil: List[String]) {
        case _: List[Unit] => false
        case _: List[String] => true
        case _: _ => false
    }

    //////////////////////////////////////
    // Recursive tests                  //
    //////////////////////////////////////

    def pairTreeSize(x: a): Int32 = typematch x {
        case pair: (_, _) => 1 + pairTreeSize(fst(pair)) + pairTreeSize(snd(pair))
        case _: _ => 1
    }

    @Test
    def testPairTreeSize01(): Bool = pairTreeSize("hi") == 1

    @Test
    def testPairTreeSize02(): Bool = pairTreeSize(("hi", "there")) == 3

    @Test
    def testPairTreeSize03(): Bool = pairTreeSize(("hi", ("there", "pal"))) == 5

    def magicList(n: Int32): List[a] = typematch (Proxy.Proxy: Proxy[a]) {
        case _: Proxy[Int32] =>
            if (n > 0) {
                unchecked_cast((n :: magicList(n - 1)) as List[a])
            } else {
                Nil
            }
        case _: _ => Nil
    }

    @Test
    def testMagicList01(): Bool = magicList(3) == 3 :: 2 :: 1 :: Nil

    @Test
    def testMagicList02(): Bool = magicList(3) |> List.isEmpty

    //////////////////////////////////////
    // Record tests                     //
    //////////////////////////////////////

    def hasCookie(x: {| _}): Bool = typematch x {
        case _: {cookie = _ | _} => true
        case _: _ => false
    }

    @Test
    def testCookie01(): Bool = hasCookie({cookie = "woohoo"})

    @Test
    def testCookie02(): Bool = hasCookie({cookie = 123})

    @Test
    def testCookie03(): Bool = not hasCookie({})

    @Test
    def testCookie04(): Bool = not hasCookie({blah = 'h'})

    //////////////////////////////////////
    // Regression tests                 //
    //////////////////////////////////////

    // see https://github.com/flix/flix/issues/6004
    def reg1(x: a): String =
        typematch x {
            case _: Array[_, _] =>
                "Array"
            case _: _ =>
                "Not Array"
        }

    def reg2(x: a): String \ Impure =
        typematch x {
            case y: Array[_, _] =>
                checked_ecast(Array.get(0, y));
                "Array"
            case _: _ =>
                "Not Array"
        }

    @Test
     def testReg1(): Bool \ Impure = reg1(Array#{1, 2, 3} @ Static) == "Array"

    @Test
     def testReg2(): Bool \ Impure = reg2(Array#{1, 2, 3} @ Static) == "Array"
}
