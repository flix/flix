mod Test.Exp.TypeMatch {

    use Assert.assertTrue

    //////////////////////////////////////
    // Emulating type reflection        //
    //////////////////////////////////////

    @Test
    def typeMatchUnit01(): Unit \ Assert = assertTrue(typematch () {
        case _: Unit => true
        case _: _ => false
    })

    @Test
    def typeMatchUnit02(): Unit \ Assert = assertTrue(typematch () {
        case x: Unit => x == ()
        case _: _ => false
    })

    @Test
    def typeMatchBool01(): Unit \ Assert = assertTrue(typematch true {
        case _: Bool => true
        case _: _ => false
    })

    @Test
    def typeMatchBool02(): Unit \ Assert = assertTrue(typematch true {
        case x: Bool => x == true
        case _: _ => false
    })

    @Test
    def typeMatchChar01(): Unit \ Assert = assertTrue(typematch 'a' {
        case _: Char => true
        case _: _ => false
    })

    @Test
    def typeMatchChar02(): Unit \ Assert = assertTrue(typematch 'a' {
        case x: Char => x == 'a'
        case _: _ => false
    })

    @Test
    def typeMatchFloat32_01(): Unit \ Assert = assertTrue(typematch 0.123f32 {
        case _: Float32 => true
        case _: _ => false
    })

    @Test
    def typeMatchFloat32_02(): Unit \ Assert = assertTrue(typematch 0.123f32 {
        case x: Float32 => x == 0.123f32
        case _: _ => false
    })

    @Test
    def typeMatchFloat64_01(): Unit \ Assert = assertTrue(typematch 0.123f64 {
        case _: Float64 => true
        case _: _ => false
    })

    @Test
    def typeMatchFloat64_02(): Unit \ Assert = assertTrue(typematch 0.123f64 {
        case x: Float64 => x == 0.123f64
        case _: _ => false
    })

    @Test
    def typeMatchInt8_01(): Unit \ Assert = assertTrue(typematch 123i8 {
        case _: Int8 => true
        case _: _ => false
    })

    @Test
    def typeMatchInt8_02(): Unit \ Assert = assertTrue(typematch 123i8 {
        case x: Int8 => x == 123i8
        case _: _ => false
    })

    @Test
    def typeMatchInt16_01(): Unit \ Assert = assertTrue(typematch 123i16 {
        case _: Int16 => true
        case _: _ => false
    })

    @Test
    def typeMatchInt16_02(): Unit \ Assert = assertTrue(typematch 123i16 {
        case x: Int16 => x == 123i16
        case _: _ => false
    })

    @Test
    def typeMatchInt32_01(): Unit \ Assert = assertTrue(typematch 123i32 {
        case _: Int32 => true
        case _: _ => false
    })

    @Test
    def typeMatchInt32_02(): Unit \ Assert = assertTrue(typematch 123i32 {
        case x: Int32 => x == 123i32
        case _: _ => false
    })

    @Test
    def typeMatchInt64_01(): Unit \ Assert = assertTrue(typematch 123i64 {
        case _: Int64 => true
        case _: _ => false
    })

    @Test
    def typeMatchInt64_02(): Unit \ Assert = assertTrue(typematch 123i64 {
        case x: Int64 => x == 123i64
        case _: _ => false
    })

    @Test
    def typeMatchBigInt01(): Unit \ Assert = assertTrue(typematch 123ii {
        case _: BigInt => true
        case _: _ => false
    })

    @Test
    def typeMatchBigInt02(): Unit \ Assert = assertTrue(typematch 123ii {
        case x: BigInt => x == 123ii
        case _: _ => false
    })

    @Test
    def typeMatchString01(): Unit \ Assert = assertTrue(typematch "hello" {
        case _: String => true
        case _: _ => false
    })

    @Test
    def typeMatchString02(): Unit \ Assert = assertTrue(typematch "hello" {
        case x: String => x == "hello"
        case _: _ => false
    })

    @Test
    def typeMatchArrayUnit01(): Unit \ Assert = region rc {
        assertTrue(typematch Array#{()} @ rc {
            case _: Array[Unit, rc] => true
            case _: _ => false
        })
    }

    @Test
    def typeMatchArrayUnit02(): Unit \ Assert = region rc {
        assertTrue(typematch Array#{()} @ rc {
            case x: Array[Unit, rc] => Array.sameElements(x, Array#{()} @ rc)
            case _: _ => false
        })
    }

    @Test
    def typeMatchArrayBool01(): Unit \ Assert = region rc {
        assertTrue(typematch Array#{true} @ rc {
            case _: Array[Bool, rc] => true
            case _: _ => false
        })
    }

    @Test
    def typeMatchArrayBool02(): Unit \ Assert = region rc {
        assertTrue(typematch Array#{true} @ rc {
            case x: Array[Bool, rc] => Array.sameElements(x, Array#{true} @ rc)
            case _: _ => false
        })
    }

    @Test
    def typeMatchArrayChar01(): Unit \ Assert = region rc {
        assertTrue(typematch Array#{'a'} @ rc {
            case _: Array[Char, rc] => true
            case _: _ => false
        })
    }

    @Test
    def typeMatchArrayChar02(): Unit \ Assert = region rc {
        assertTrue(typematch Array#{'a'} @ rc {
            case x: Array[Char, rc] => Array.sameElements(x, Array#{'a'} @ rc)
            case _: _ => false
        })
    }

    @Test
    def typeMatchArrayFloat32_01(): Unit \ Assert = region rc {
        assertTrue(typematch Array#{0.123f32} @ rc {
            case _: Array[Float32, rc] => true
            case _: _ => false
        })
    }

    @Test
    def typeMatchArrayFloat32_02(): Unit \ Assert = region rc {
        assertTrue(typematch Array#{0.123f32} @ rc {
            case x: Array[Float32, rc] => Array.sameElements(x, Array#{0.123f32} @ rc)
            case _: _ => false
        })
    }

    @Test
    def typeMatchArrayFloat64_01(): Unit \ Assert = region rc {
        assertTrue(typematch Array#{0.123f64} @ rc {
            case _: Array[Float64, rc] => true
            case _: _ => false
        })
    }

    @Test
    def typeMatchArrayFloat64_02(): Unit \ Assert = region rc {
        assertTrue(typematch Array#{0.123f64} @ rc {
            case x: Array[Float64, rc] => Array.sameElements(x, Array#{0.123f64} @ rc)
            case _: _ => false
        })
    }

    @Test
    def typeMatchArrayInt8_01(): Unit \ Assert = region rc {
        assertTrue(typematch Array#{123i8} @ rc {
            case _: Array[Int8, rc] => true
            case _: _ => false
        })
    }

    @Test
    def typeMatchArrayInt8_02(): Unit \ Assert = region rc {
        assertTrue(typematch Array#{123i8} @ rc {
            case x: Array[Int8, rc] => Array.sameElements(x, Array#{123i8} @ rc)
            case _: _ => false
        })
    }

    @Test
    def typeMatchArrayInt16_01(): Unit \ Assert = region rc {
        assertTrue(typematch Array#{123i16} @ rc {
            case _: Array[Int16, rc] => true
            case _: _ => false
        })
    }

    @Test
    def typeMatchArrayInt16_02(): Unit \ Assert = region rc {
        assertTrue(typematch Array#{123i16} @ rc {
            case x: Array[Int16, rc] => Array.sameElements(x, Array#{123i16} @ rc)
            case _: _ => false
        })
    }

    @Test
    def typeMatchArrayInt32_01(): Unit \ Assert = region rc {
        assertTrue(typematch Array#{123i32} @ rc {
            case _: Array[Int32, rc] => true
            case _: _ => false
        })
    }

    @Test
    def typeMatchArrayInt32_02(): Unit \ Assert = region rc {
        assertTrue(typematch Array#{123i32} @ rc {
            case x: Array[Int32, rc] => Array.sameElements(x, Array#{123i32} @ rc)
            case _: _ => false
        })
    }

    @Test
    def typeMatchArrayInt64_01(): Unit \ Assert = region rc {
        assertTrue(typematch Array#{123i64} @ rc {
            case _: Array[Int64, rc] => true
            case _: _ => false
        })
    }

    @Test
    def typeMatchArrayInt64_02(): Unit \ Assert = region rc {
        assertTrue(typematch Array#{123i64} @ rc {
            case x: Array[Int64, rc] => Array.sameElements(x, Array#{123i64} @ rc)
            case _: _ => false
        })
    }

    @Test
    def typeMatchArrayBigInt01(): Unit \ Assert = region rc {
        assertTrue(typematch Array#{123ii} @ rc {
            case _: Array[BigInt, rc] => true
            case _: _ => false
        })
    }

    @Test
    def typeMatchArrayBigInt02(): Unit \ Assert = region rc {
        assertTrue(typematch Array#{123ii} @ rc {
            case x: Array[BigInt, rc] => Array.sameElements(x, Array#{123ii} @ rc)
            case _: _ => false
        })
    }

    @Test
    def typeMatchArrayString01(): Unit \ Assert = region rc {
        assertTrue(typematch Array#{"hello"} @ rc {
            case _: Array[String, rc] => true
            case _: _ => false
        })
    }

    @Test
    def typeMatchArrayString02(): Unit \ Assert = region rc {
        assertTrue(typematch Array#{"hello"} @ rc {
            case x: Array[String, rc] => Array.sameElements(x, Array#{"hello"} @ rc)
            case _: _ => false
        })
    }

    //////////////////////////////////////
    // Emulating effect reflection      //
    //////////////////////////////////////

    @Test
    def typeMatchEff01(): Unit \ Assert = assertTrue(hof(_ -> 123))

    @Test
    def typeMatchEff02(): Unit \ Assert = assertTrue(not hof(_ -> unchecked_cast(123 as _ \ IO)))

    @Test
    def typeMatchEff03(): Unit \ Assert = assertTrue(not hof(_ -> unchecked_cast(123 as _ \ IO)))

    @Test
    def typeMatchEff04(): Unit \ Assert = assertTrue(hof((x -> x) >> (y -> y)))

    @Test
    def typeMatchEff05(): Unit \ Assert = assertTrue(not hof((x -> x) >> (y -> unchecked_cast(y as _ \ IO))))

    @Test
    def typeMatchEff06(): Unit \ Assert = assertTrue(not hof((x -> unchecked_cast(x as _ \ IO)) >> (y -> y)))

    @Test
    def typeMatchEff07(): Unit \ Assert = assertTrue(not hof((x -> unchecked_cast(x as _ \ IO)) >> (y -> unchecked_cast(y as _ \ IO))))

    @Test
    def typeMatchEff08(): Unit \ Assert = assertTrue(not hof((x -> x) >> (y -> unchecked_cast(y as _ \ IO))))

    @Test
    def typeMatchEff09(): Unit \ Assert = assertTrue(not hof((x -> unchecked_cast(x as _ \ IO)) >> (y -> y)))

    @Test
    def typeMatchEff10(): Unit \ Assert = assertTrue(not hof((x -> unchecked_cast(x as _ \ IO)) >> (y -> unchecked_cast(y as _ \ IO))))

    @Test
    def typeMatchEff11(): Unit \ Assert = assertTrue(not hof((x -> unchecked_cast(x as _ \ IO)) >> (y -> unchecked_cast(y as _ \ IO))))

    @Test
    def typeMatchEff12(): Unit \ Assert = assertTrue(not hof((x -> unchecked_cast(x as _ \ IO)) >> (y -> unchecked_cast(y as _ \ IO))))

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
    def polyTypeMatchBool01(): Unit \ Assert = assertTrue(reflectBool(true))

    @Test
    def polyTypeMatchBool02(): Unit \ Assert = assertTrue(reflectBool(false))

    @Test
    def polyTypeMatchBool03(): Unit \ Assert = assertTrue(not reflectBool(123i32))

    @Test
    def polyTypeMatchBool04(): Unit \ Assert = assertTrue(not reflectBool("abc"))

    def reflectChar(x: a): Bool = typematch x {
      case r: Char => r <= 'a' or r > 'a'
      case _: _ => false
    }

    @Test
    def polyTypeMatchChar01(): Unit \ Assert = assertTrue(reflectChar('a'))

    @Test
    def polyTypeMatchChar02(): Unit \ Assert = assertTrue(reflectChar('\u1234'))

    @Test
    def polyTypeMatchChar03(): Unit \ Assert = assertTrue(not reflectChar(123i32))

    @Test
    def polyTypeMatchChar04(): Unit \ Assert = assertTrue(not reflectChar("abc"))

    def reflectFloat32(x: a): Bool = typematch x {
      case r: Float32 => r <= 0.0f32 or r > 0.0f32
      case _: _ => false
    }

    @Test
    def polyTypeMatchFloat32_01(): Unit \ Assert = assertTrue(reflectFloat32(123.0f32))

    @Test
    def polyTypeMatchFloat32_02(): Unit \ Assert = assertTrue(reflectFloat32(-123.0f32))

    @Test
    def polyTypeMatchFloat32_03(): Unit \ Assert = assertTrue(not reflectFloat32(123i32))

    @Test
    def polyTypeMatchFloat32_04(): Unit \ Assert = assertTrue(not reflectFloat32("abc"))

    def reflectFloat64(x: a): Bool = typematch x {
      case r: Float64 => r <= 0.0f64 or r > 0.0f64
      case _: _ => false
    }

    @Test
    def polyTypeMatchFloat64_01(): Unit \ Assert = assertTrue(reflectFloat64(123.0f64))

    @Test
    def polyTypeMatchFloat64_02(): Unit \ Assert = assertTrue(reflectFloat64(-123.0f64))

    @Test
    def polyTypeMatchFloat64_03(): Unit \ Assert = assertTrue(not reflectFloat64(123i64))

    @Test
    def polyTypeMatchFloat64_04(): Unit \ Assert = assertTrue(not reflectFloat64("abc"))

    def reflectInt8(x: a): Bool = typematch x {
      case r: Int8 => r <= 0i8 or r > 0i8
      case _: _ => false
    }

    @Test
    def polyTypeMatchInt8_01(): Unit \ Assert = assertTrue(reflectInt8(123i8))

    @Test
    def polyTypeMatchInt8_02(): Unit \ Assert = assertTrue(reflectInt8(-123i8))

    @Test
    def polyTypeMatchInt8_03(): Unit \ Assert = assertTrue(not reflectInt8(123i64))

    @Test
    def polyTypeMatchInt8_04(): Unit \ Assert = assertTrue(not reflectInt8("abc"))

    def reflectInt16(x: a): Bool = typematch x {
      case r: Int16 => r <= 0i16 or r > 0i16
      case _: _ => false
    }

    @Test
    def polyTypeMatchInt16_01(): Unit \ Assert = assertTrue(reflectInt16(123i16))

    @Test
    def polyTypeMatchInt16_02(): Unit \ Assert = assertTrue(reflectInt16(-123i16))

    @Test
    def polyTypeMatchInt16_03(): Unit \ Assert = assertTrue(not reflectInt16(123i64))

    @Test
    def polyTypeMatchInt16_04(): Unit \ Assert = assertTrue(not reflectInt16("abc"))

    def reflectInt32(x: a): Bool = typematch x {
      case r: Int32 => r <= 0i32 or r > 0i32
      case _: _ => false
    }

    @Test
    def polyTypeMatchInt32_01(): Unit \ Assert = assertTrue(reflectInt32(123i32))

    @Test
    def polyTypeMatchInt32_02(): Unit \ Assert = assertTrue(reflectInt32(-123i32))

    @Test
    def polyTypeMatchInt32_03(): Unit \ Assert = assertTrue(not reflectInt32(123i64))

    @Test
    def polyTypeMatchInt32_04(): Unit \ Assert = assertTrue(not reflectInt32("abc"))

    def reflectInt64(x: a): Bool = typematch x {
      case r: Int64 => r <= 0i64 or r > 0i64
      case _: _ => false
    }

    @Test
    def polyTypeMatchInt64_01(): Unit \ Assert = assertTrue(reflectInt64(123i64))

    @Test
    def polyTypeMatchInt64_02(): Unit \ Assert = assertTrue(reflectInt64(-123i64))

    @Test
    def polyTypeMatchInt64_03(): Unit \ Assert = assertTrue(not reflectInt64(123ii))

    @Test
    def polyTypeMatchInt64_04(): Unit \ Assert = assertTrue(not reflectInt64("abc"))

    def reflectBigInt(x: a): Bool = typematch x {
      case r: BigInt => r <= 0ii or r > 0ii
      case _: _ => false
    }

    @Test
    def polyTypeMatchBigInt_01(): Unit \ Assert = assertTrue(reflectBigInt(123ii))

    @Test
    def polyTypeMatchBigInt_02(): Unit \ Assert = assertTrue(reflectBigInt(-123ii))

    @Test
    def polyTypeMatchBigInt_03(): Unit \ Assert = assertTrue(not reflectBigInt(123i64))

    @Test
    def polyTypeMatchBigInt_04(): Unit \ Assert = assertTrue(not reflectBigInt("abc"))

    def reflectString(x: a): Bool = typematch x {
      case r: String => r <= "hi" or r > "hi"
      case _: _ => false
    }

    @Test
    def polyTypeMatchString_01(): Unit \ Assert = assertTrue(reflectString("hi"))

    @Test
    def polyTypeMatchString_02(): Unit \ Assert = assertTrue(reflectString("\u1234"))

    @Test
    def polyTypeMatchString_03(): Unit \ Assert = assertTrue(not reflectString(123i64))

    @Test
    def polyTypeMatchString_04(): Unit \ Assert = assertTrue(not reflectString('a'))

    //////////////////////////////////////
    // Complex tests                    //
    //////////////////////////////////////

    def sameTypes(x: a, _: b): Bool = typematch x {
        case _: b => true
        case _: _ => false
    }

    @Test
    def sameTypes01(): Unit \ Assert = assertTrue(sameTypes(123, 456))

    @Test
    def sameTypes02(): Unit \ Assert = assertTrue(sameTypes(123 :: Nil, 456 :: Nil))

    @Test
    def sameTypes03(): Unit \ Assert = assertTrue(not sameTypes(123, 456.0))

    @Test
    def sameTypes04(): Unit \ Assert = assertTrue(not sameTypes(123 :: Nil, 456.0 :: Nil))

    @Test
    def sameTypes05(): Unit \ Assert = assertTrue(not sameTypes(123 :: Nil, Nil))

    def isPredicateNotFromInt32(f: a): Bool = typematch f {
        case _: Int32 -> _ => false
        case _: _ -> Bool => true
        case _: _ => false
    }

    @Test
    def isPredicateNotFromInt32_01(): Unit \ Assert = assertTrue(isPredicateNotFromInt32(_ -> true))

    @Test
    def isPredicateNotFromInt32_02(): Unit \ Assert = assertTrue(isPredicateNotFromInt32(x -> not x))

    @Test
    def isPredicateNotFromInt32_03(): Unit \ Assert = assertTrue(not isPredicateNotFromInt32("hi"))

    @Test
    def isPredicateNotFromInt32_04(): Unit \ Assert = assertTrue(not isPredicateNotFromInt32(x -> x == 123))

    def isAppliedType(x: a): Bool = typematch x {
        case _: _[_] => true
        case _: _ => false
    }

    @Test
    def isAppliedType01(): Unit \ Assert = assertTrue(isAppliedType(Nil))

    @Test
    def isAppliedType02(): Unit \ Assert = assertTrue(isAppliedType(Map#{}))

    @Test
    def isAppliedType03(): Unit \ Assert = assertTrue(not isAppliedType(123))

    def isEmptyRecord(x: {| r}): Bool = typematch x {
        case _: {|} => true
        case _: _ => false
    }

    @Test
    def isEmptyRecord01(): Unit \ Assert = assertTrue(isEmptyRecord({}))

    @Test
    def isEmptyRecord02(): Unit \ Assert = assertTrue(not isEmptyRecord({age = 123}))

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
    def applyThePureOne01(): Unit \ Assert = {
        let f1 = _ -> true;
        let f2 = _ -> false;
        assertTrue(applyThePureOne(f1, f2, 123))
    }

    @Test
    def applyThePureOne02(): Unit \ { Assert, IO } = {
        let f1 = _ -> checked_ecast(false);
        let f2 = _ -> true;
        assertTrue(applyThePureOne(f1, f2, 123))
    }

    @Test
    def applyThePureOne03(): Unit \ Assert = {
        let f1 = _ -> true;
        let f2 = _ -> checked_ecast(false);
        assertTrue(applyThePureOne(f1, f2, 123))
    }

    @Test
    def applyThePureOne05(): Unit \ { Assert, IO } = {
        let f1 = _ -> unchecked_cast(true as _ \ IO);
        let f2 = _ -> unchecked_cast(false as _ \ IO);
        assertTrue(applyThePureOne(f1, f2, 123))
    }

    @Test
    def typeMatchFallThrough01(): Unit \ Assert = assertTrue(typematch "hello" {
        case x: Int8 => x == 123i8
        case x: Int64 => x == 123i64
        case _: _ => true
    })

    @Test
    def typeMatchPolymorphic01(): Unit \ Assert = assertTrue(typematch Nil {
        case _: List[Unit] => false
        case _: List[_] => true
        case _: _ => true
    })

    @Test
    def typeMatchPolymorphic02(): Unit \ Assert = assertTrue(typematch (Nil: List[String]) {
        case _: List[Unit] => false
        case _: List[String] => true
        case _: _ => false
    })

    @Test
    def typeMatchPolymorphic03(): Unit \ Assert = assertTrue(typematch (Nil: List[String]) {
        case _: List[_] => true
        case _: List[String] => false
        case _: _ => false
    })

    //////////////////////////////////////
    // Recursive tests                  //
    //////////////////////////////////////

    def pairTreeSize(x: a): Int32 = typematch x {
        case pair: (_, _) => 1 + pairTreeSize(fst(pair)) + pairTreeSize(snd(pair))
        case _: _ => 1
    }

    @Test
    def testPairTreeSize01(): Unit \ Assert = assertTrue(pairTreeSize("hi") == 1)

    @Test
    def testPairTreeSize02(): Unit \ Assert = assertTrue(pairTreeSize(("hi", "there")) == 3)

    @Test
    def testPairTreeSize03(): Unit \ Assert = assertTrue(pairTreeSize(("hi", ("there", "pal"))) == 5)

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
    def testMagicList01(): Unit \ Assert = assertTrue(magicList(3) == 3 :: 2 :: 1 :: Nil)

    @Test
    def testMagicList02(): Unit \ Assert = assertTrue(magicList(3) |> List.isEmpty)

    //////////////////////////////////////
    // Record tests                     //
    //////////////////////////////////////

    def hasCookie(x: {| _}): Bool = typematch x {
        case _: {cookie = _ | _} => true
        case _: _ => false
    }

    @Test
    def testCookie01(): Unit \ Assert = assertTrue(hasCookie({cookie = "woohoo"}))

    @Test
    def testCookie02(): Unit \ Assert = assertTrue(hasCookie({cookie = 123}))

    @Test
    def testCookie03(): Unit \ Assert = assertTrue(not hasCookie({}))

    @Test
    def testCookie04(): Unit \ Assert = assertTrue(not hasCookie({blah = 'h'}))

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

    def reg2(x: a): String \ IO =
        typematch x {
            case y: Array[_, Static] =>
                discard Array.get(0, y);
                "Array"
            case _: _ =>
                "Not Array"
        }

    @Test
     def testReg1(): Unit \ { Assert, IO } = assertTrue(reg1(Array#{1, 2, 3} @ Static) == "Array")

    @Test
     def testReg2(): Unit \ { Assert, IO } = assertTrue(reg2(Array#{1, 2, 3} @ Static) == "Array")
}
