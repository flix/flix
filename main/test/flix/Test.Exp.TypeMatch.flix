mod Test.Exp.ReifyType {

    //////////////////////////////////////
    // Emulating reifyType              //
    //////////////////////////////////////

    @Test
    def matchTypeUnit01(): Bool = typematch () {
        case _: Unit => true
        case _: _ => false
    }

    @Test
    def matchTypeUnit02(): Bool = typematch () {
        case x: Unit => x == ()
        case _: _ => false
    }

    @Test
    def matchTypeBool01(): Bool = typematch true {
        case _: Bool => true
        case _: _ => false
    }

    @Test
    def matchTypeBool02(): Bool = typematch true {
        case x: Bool => x == true
        case _: _ => false
    }

    @Test
    def matchTypeChar01(): Bool = typematch 'a' {
        case _: Char => true
        case _: _ => false
    }

    @Test
    def matchTypeChar02(): Bool = typematch 'a' {
        case x: Char => x == 'a'
        case _: _ => false
    }

    @Test
    def matchTypeFloat32_01(): Bool = typematch 0.123f32 {
        case _: Float32 => true
        case _: _ => false
    }

    @Test
    def matchTypeFloat32_02(): Bool = typematch 0.123f32 {
        case x: Float32 => x == 0.123f32
        case _: _ => false
    }

    @Test
    def matchTypeFloat64_01(): Bool = typematch 0.123f64 {
        case _: Float64 => true
        case _: _ => false
    }

    @Test
    def matchTypeFloat64_02(): Bool = typematch 0.123f64 {
        case x: Float64 => x == 0.123f64
        case _: _ => false
    }

    @Test
    def matchTypeInt8_01(): Bool = typematch 123i8 {
        case _: Int8 => true
        case _: _ => false
    }

    @Test
    def matchTypeInt8_02(): Bool = typematch 123i8 {
        case x: Int8 => x == 123i8
        case _: _ => false
    }

    @Test
    def matchTypeInt16_01(): Bool = typematch 123i16 {
        case _: Int16 => true
        case _: _ => false
    }

    @Test
    def matchTypeInt16_02(): Bool = typematch 123i16 {
        case x: Int16 => x == 123i16
        case _: _ => false
    }

    @Test
    def matchTypeInt32_01(): Bool = typematch 123i32 {
        case _: Int32 => true
        case _: _ => false
    }

    @Test
    def matchTypeInt32_02(): Bool = typematch 123i32 {
        case x: Int32 => x == 123i32
        case _: _ => false
    }

    @Test
    def matchTypeInt64_01(): Bool = typematch 123i64 {
        case _: Int64 => true
        case _: _ => false
    }

    @Test
    def matchTypeInt64_02(): Bool = typematch 123i64 {
        case x: Int64 => x == 123i64
        case _: _ => false
    }

    @Test
    def matchTypeBigInt01(): Bool = typematch 123ii {
        case _: BigInt => true
        case _: _ => false
    }

    @Test
    def matchTypeBigInt02(): Bool = typematch 123ii {
        case x: BigInt => x == 123ii
        case _: _ => false
    }

    @Test
    def matchTypeString01(): Bool = typematch "hello" {
        case _: String => true
        case _: _ => false
    }

    @Test
    def matchTypeString02(): Bool = typematch "hello" {
        case x: String => x == "hello"
        case _: _ => false
    }

    @Test
    def matchTypeArrayUnit01(): Bool = region r {
        typematch Array#{()} @ r {
            case _: Array[Unit, r] => true
            case _: _ => false
        }
    }

    @Test
    def matchTypeArrayUnit02(): Bool = region r {
        typematch Array#{()} @ r {
            case x: Array[Unit, r] => Array.sameElements(x, Array#{()} @ r)
            case _: _ => false
        }
    }

    @Test
    def matchTypeArrayBool01(): Bool = region r {
        typematch Array#{true} @ r {
            case _: Array[Bool, r] => true
            case _: _ => false
        }
    }

    @Test
    def matchTypeArrayBool02(): Bool = region r {
        typematch Array#{true} @ r {
            case x: Array[Bool, r] => Array.sameElements(x, Array#{true} @ r)
            case _: _ => false
        }
    }

    @Test
    def matchTypeArrayChar01(): Bool = region r {
        typematch Array#{'a'} @ r {
            case _: Array[Char, r] => true
            case _: _ => false
        }
    }

    @Test
    def matchTypeArrayChar02(): Bool = region r {
        typematch Array#{'a'} @ r {
            case x: Array[Char, r] => Array.sameElements(x, Array#{'a'} @ r)
            case _: _ => false
        }
    }

    @Test
    def matchTypeArrayFloat32_01(): Bool = region r {
        typematch Array#{0.123f32} @ r {
            case _: Array[Float32, r] => true
            case _: _ => false
        }
    }

    @Test
    def matchTypeArrayFloat32_02(): Bool = region r {
        typematch Array#{0.123f32} @ r {
            case x: Array[Float32, r] => Array.sameElements(x, Array#{0.123f32} @ r)
            case _: _ => false
        }
    }

    @Test
    def matchTypeArrayFloat64_01(): Bool = region r {
        typematch Array#{0.123f64} @ r {
            case _: Array[Float64, r] => true
            case _: _ => false
        }
    }

    @Test
    def matchTypeArrayFloat64_02(): Bool = region r {
        typematch Array#{0.123f64} @ r {
            case x: Array[Float64, r] => Array.sameElements(x, Array#{0.123f64} @ r)
            case _: _ => false
        }
    }

    @Test
    def matchTypeArrayInt8_01(): Bool = region r {
        typematch Array#{123i8} @ r {
            case _: Array[Int8, r] => true
            case _: _ => false
        }
    }

    @Test
    def matchTypeArrayInt8_02(): Bool = region r {
        typematch Array#{123i8} @ r {
            case x: Array[Int8, r] => Array.sameElements(x, Array#{123i8} @ r)
            case _: _ => false
        }
    }

    @Test
    def matchTypeArrayInt16_01(): Bool = region r {
        typematch Array#{123i16} @ r {
            case _: Array[Int16, r] => true
            case _: _ => false
        }
    }

    @Test
    def matchTypeArrayInt16_02(): Bool = region r {
        typematch Array#{123i16} @ r {
            case x: Array[Int16, r] => Array.sameElements(x, Array#{123i16} @ r)
            case _: _ => false
        }
    }

    @Test
    def matchTypeArrayInt32_01(): Bool = region r {
        typematch Array#{123i32} @ r {
            case _: Array[Int32, r] => true
            case _: _ => false
        }
    }

    @Test
    def matchTypeArrayInt32_02(): Bool = region r {
        typematch Array#{123i32} @ r {
            case x: Array[Int32, r] => Array.sameElements(x, Array#{123i32} @ r)
            case _: _ => false
        }
    }

    @Test
    def matchTypeArrayInt64_01(): Bool = region r {
        typematch Array#{123i64} @ r {
            case _: Array[Int64, r] => true
            case _: _ => false
        }
    }

    @Test
    def matchTypeArrayInt64_02(): Bool = region r {
        typematch Array#{123i64} @ r {
            case x: Array[Int64, r] => Array.sameElements(x, Array#{123i64} @ r)
            case _: _ => false
        }
    }

    @Test
    def matchTypeArrayBigInt01(): Bool = region r {
        typematch Array#{123ii} @ r {
            case _: Array[BigInt, r] => true
            case _: _ => false
        }
    }

    @Test
    def matchTypeArrayBigInt02(): Bool = region r {
        typematch Array#{123ii} @ r {
            case x: Array[BigInt, r] => Array.sameElements(x, Array#{123ii} @ r)
            case _: _ => false
        }
    }

    @Test
    def matchTypeArrayString01(): Bool = region r {
        typematch Array#{"hello"} @ r {
            case _: Array[String, r] => true
            case _: _ => false
        }
    }

    @Test
    def matchTypeArrayString02(): Bool = region r {
        typematch Array#{"hello"} @ r {
            case x: Array[String, r] => Array.sameElements(x, Array#{"hello"} @ r)
            case _: _ => false
        }
    }

    //////////////////////////////////////
    // Emulating reifyEff               //
    //////////////////////////////////////

    @Test
    def matchTypeEff01(): Bool = hof(_ -> 123)

    @Test
    def matchTypeEff02(): Bool = not hof(_ -> unchecked_cast(123 as _ \ IO))

    @Test
    def matchTypeEff03(): Bool = not hof(_ -> unchecked_cast(123 as _ \ IO))

    @Test
    def matchTypeEff04(): Bool = hof((x -> x) >> (y -> y))

    @Test
    def matchTypeEff05(): Bool = not hof((x -> x) >> (y -> unchecked_cast(y as _ \ IO)))

    @Test
    def matchTypeEff06(): Bool = not hof((x -> unchecked_cast(x as _ \ IO)) >> (y -> y))

    @Test
    def matchTypeEff07(): Bool = not hof((x -> unchecked_cast(x as _ \ IO)) >> (y -> unchecked_cast(y as _ \ IO)))

    @Test
    def matchTypeEff08(): Bool = not hof((x -> x) >> (y -> unchecked_cast(y as _ \ IO)))

    @Test
    def matchTypeEff09(): Bool = not hof((x -> unchecked_cast(x as _ \ IO)) >> (y -> y))

    @Test
    def matchTypeEff10(): Bool = not hof((x -> unchecked_cast(x as _ \ IO)) >> (y -> unchecked_cast(y as _ \ IO)))

    @Test
    def matchTypeEff11(): Bool = not hof((x -> unchecked_cast(x as _ \ IO)) >> (y -> unchecked_cast(y as _ \ IO)))

    @Test
    def matchTypeEff12(): Bool = not hof((x -> unchecked_cast(x as _ \ IO)) >> (y -> unchecked_cast(y as _ \ IO)))

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
    def polyMatchTypeBool01(): Bool = reflectBool(true)

    @Test
    def polyMatchTypeBool02(): Bool = reflectBool(false)

    @Test
    def polyMatchTypeBool03(): Bool = not reflectBool(123i32)

    @Test
    def polyMatchTypeBool04(): Bool = not reflectBool("abc")

    def reflectChar(x: a): Bool = typematch x {
      case r: Char => r <= 'a' or r > 'a'
      case _: _ => false
    }

    @Test
    def polyMatchTypeChar01(): Bool = reflectChar('a')

    @Test
    def polyMatchTypeChar02(): Bool = reflectChar('\u1234')

    @Test
    def polyMatchTypeChar03(): Bool = not reflectChar(123i32)

    @Test
    def polyMatchTypeChar04(): Bool = not reflectChar("abc")

    def reflectFloat32(x: a): Bool = typematch x {
      case r: Float32 => r <= 0.0f32 or r > 0.0f32
      case _: _ => false
    }

    @Test
    def polyMatchTypeFloat32_01(): Bool = reflectFloat32(123.0f32)

    @Test
    def polyMatchTypeFloat32_02(): Bool = reflectFloat32(-123.0f32)

    @Test
    def polyMatchTypeFloat32_03(): Bool = not reflectFloat32(123i32)

    @Test
    def polyMatchTypeFloat32_04(): Bool = not reflectFloat32("abc")

    def reflectFloat64(x: a): Bool = typematch x {
      case r: Float64 => r <= 0.0f64 or r > 0.0f64
      case _: _ => false
    }

    @Test
    def polyMatchTypeFloat64_01(): Bool = reflectFloat64(123.0f64)

    @Test
    def polyMatchTypeFloat64_02(): Bool = reflectFloat64(-123.0f64)

    @Test
    def polyMatchTypeFloat64_03(): Bool = not reflectFloat64(123i64)

    @Test
    def polyMatchTypeFloat64_04(): Bool = not reflectFloat64("abc")

    def reflectInt8(x: a): Bool = typematch x {
      case r: Int8 => r <= 0i8 or r > 0i8
      case _: _ => false
    }

    @Test
    def polyMatchTypeInt8_01(): Bool = reflectInt8(123i8)

    @Test
    def polyMatchTypeInt8_02(): Bool = reflectInt8(-123i8)

    @Test
    def polyMatchTypeInt8_03(): Bool = not reflectInt8(123i64)

    @Test
    def polyMatchTypeInt8_04(): Bool = not reflectInt8("abc")

    def reflectInt16(x: a): Bool = typematch x {
      case r: Int16 => r <= 0i16 or r > 0i16
      case _: _ => false
    }

    @Test
    def polyMatchTypeInt16_01(): Bool = reflectInt16(123i16)

    @Test
    def polyMatchTypeInt16_02(): Bool = reflectInt16(-123i16)

    @Test
    def polyMatchTypeInt16_03(): Bool = not reflectInt16(123i64)

    @Test
    def polyMatchTypeInt16_04(): Bool = not reflectInt16("abc")

    def reflectInt32(x: a): Bool = typematch x {
      case r: Int32 => r <= 0i32 or r > 0i32
      case _: _ => false
    }

    @Test
    def polyMatchTypeInt32_01(): Bool = reflectInt32(123i32)

    @Test
    def polyMatchTypeInt32_02(): Bool = reflectInt32(-123i32)

    @Test
    def polyMatchTypeInt32_03(): Bool = not reflectInt32(123i64)

    @Test
    def polyMatchTypeInt32_04(): Bool = not reflectInt32("abc")

    def reflectInt64(x: a): Bool = typematch x {
      case r: Int64 => r <= 0i64 or r > 0i64
      case _: _ => false
    }

    @Test
    def polyMatchTypeInt64_01(): Bool = reflectInt64(123i64)

    @Test
    def polyMatchTypeInt64_02(): Bool = reflectInt64(-123i64)

    @Test
    def polyMatchTypeInt64_03(): Bool = not reflectInt64(123ii)

    @Test
    def polyMatchTypeInt64_04(): Bool = not reflectInt64("abc")

    def reflectBigInt(x: a): Bool = typematch x {
      case r: BigInt => r <= 0ii or r > 0ii
      case _: _ => false
    }

    @Test
    def polyMatchTypeBigInt_01(): Bool = reflectBigInt(123ii)

    @Test
    def polyMatchTypeBigInt_02(): Bool = reflectBigInt(-123ii)

    @Test
    def polyMatchTypeBigInt_03(): Bool = not reflectBigInt(123i64)

    @Test
    def polyMatchTypeBigInt_04(): Bool = not reflectBigInt("abc")

    def reflectString(x: a): Bool = typematch x {
      case r: String => r <= "hi" or r > "hi"
      case _: _ => false
    }

    @Test
    def polyMatchTypeString_01(): Bool = reflectString("hi")

    @Test
    def polyMatchTypeString_02(): Bool = reflectString("\u1234")

    @Test
    def polyMatchTypeString_03(): Bool = not reflectString(123i64)

    @Test
    def polyMatchTypeString_04(): Bool = not reflectString('a')

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
    def matchTypeFallThrough01(): Bool = typematch "hello" {
        case x: Int8 => x == 123i8
        case x: Int64 => x == 123i64
        case _: _ => true
    }

    @Test
    def matchTypePolymorphic01(): Bool = typematch Nil {
        case _: List[Unit] => false
        case _: List[_] => true
        case _: _ => true
    }

    @Test
    def matchTypePolymorphic02(): Bool = typematch (Nil: List[String]) {
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
}
