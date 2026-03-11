mod Test.DefaultHandler {
    pub eff EUnit {
        def op(): Unit
    }

    pub eff EBool {
        def op(): Bool
    }

    pub eff EChar {
        def op(): Char
    }

    pub eff EFloat32 {
        def op(): Float32
    }

    pub eff EFloat64 {
        def op(): Float64
    }

    pub eff EInt8 {
        def op(): Int8
    }

    pub eff EInt16 {
        def op(): Int16
    }

    pub eff EInt32 {
        def op(): Int32
    }

    pub eff EInt64 {
        def op(): Int64
    }

    pub eff EBigInt {
        def op(): BigInt
    }

    pub eff EString {
        def op(): String
    }

    pub eff EArrayChar {
        def op(): Array[Char, Static]
    }

    pub eff EArrayInt32 {
        def op(): Array[Int32, Static]
    }

    pub eff EArrayOption {
        def op(): Array[Option[Int32], Static]
    }

    pub eff EArrayList {
        def op(): Array[List[String], Static]
    }

    pub eff EArrayResult {
        def op(): Array[Result[String, Int32], Static]
    }

    pub eff EEnum {
        def op(): Color
    }

    pub eff ETuple {
        def op(): (Bool, Char, String)
    }

    pub eff EAdd {
        def add(x: Int32, y: Int32): Int32
    }

    pub enum Color with Eq, ToString {
        case Red
        case Green
        case Blue
    }

    mod EUnit {
        @DefaultHandler
        pub def runWithIO(f: Unit -> a \ ef): a \ (ef - EUnit) + IO =
            run {
                f()
            } with handler EUnit {
                def op(k) = {
                    k(())
                }
            }
    }

    mod EBool {
        @DefaultHandler
        pub def runWithIO(f: Unit -> a \ ef): a \ (ef - EBool) + IO =
            run {
                f()
            } with handler EBool {
                def op(k) = {
                    k(true)
                }
            }
    }

    mod EChar {
        @DefaultHandler
        pub def runWithIO(f: Unit -> a \ ef): a \ (ef - EChar) + IO =
            run {
                f()
            } with handler EChar {
                def op(k) = {
                    k('X')
                }
            }
    }

    mod EFloat32 {
        @DefaultHandler
        pub def runWithIO(f: Unit -> a \ ef): a \ (ef - EFloat32) + IO =
            run {
                f()
            } with handler EFloat32 {
                def op(k) = {
                    k(3.14f32)
                }
            }
    }

    mod EFloat64 {
        @DefaultHandler
        pub def runWithIO(f: Unit -> a \ ef): a \ (ef - EFloat64) + IO =
            run {
                f()
            } with handler EFloat64 {
                def op(k) = {
                    k(2.718281828459045)
                }
            }
    }

    mod EInt8 {
        @DefaultHandler
        pub def runWithIO(f: Unit -> a \ ef): a \ (ef - EInt8) + IO =
            run {
                f()
            } with handler EInt8 {
                def op(k) = {
                    k(42i8)
                }
            }
    }

    mod EInt16 {
        @DefaultHandler
        pub def runWithIO(f: Unit -> a \ ef): a \ (ef - EInt16) + IO =
            run {
                f()
            } with handler EInt16 {
                def op(k) = {
                    k(1000i16)
                }
            }
    }

    mod EInt32 {
        @DefaultHandler
        pub def runWithIO(f: Unit -> a \ ef): a \ (ef - EInt32) + IO =
            run {
                f()
            } with handler EInt32 {
                def op(k) = {
                    k(5)
                }
            }
    }

    mod EInt64 {
        @DefaultHandler
        pub def runWithIO(f: Unit -> a \ ef): a \ (ef - EInt64) + IO =
            run {
                f()
            } with handler EInt64 {
                def op(k) = {
                    k(9999999999i64)
                }
            }
    }

    mod EBigInt {
        @DefaultHandler
        pub def runWithIO(f: Unit -> a \ ef): a \ (ef - EBigInt) + IO =
            run {
                f()
            } with handler EBigInt {
                def op(k) = {
                    k(12345678901234567890ii)
                }
            }
    }

    mod EString {
        @DefaultHandler
        pub def runWithIO(f: Unit -> a \ ef): a \ (ef - EString) + IO =
            run {
                f()
            } with handler EString {
                def op(k) = {
                    k("Default string")
                }
            }
    }

    mod EArrayChar {
        @DefaultHandler
        pub def runWithIO(f: Unit -> a \ ef): a \ (ef - EArrayChar) + IO =
            run {
                f()
            } with handler EArrayChar {
                def op(k) = {
                    let arr = Array#{'A', 'B', 'C'} @ Static;
                    k(arr)
                }
            }
    }

    mod EArrayInt32 {
        @DefaultHandler
        pub def runWithIO(f: Unit -> a \ ef): a \ (ef - EArrayInt32) + IO =
            run {
                f()
            } with handler EArrayInt32 {
                def op(k) = {
                    let arr = Array#{1, 2, 3} @ Static;
                    k(arr)
                }
            }
    }

    mod EArrayOption {
        @DefaultHandler
        pub def runWithIO(f: Unit -> a \ ef): a \ (ef - EArrayOption) + IO =
            run {
                f()
            } with handler EArrayOption {
                def op(k) = {
                    let arr = Array#{Some(10), None, Some(20)} @ Static;
                    k(arr)
                }
            }
    }

    mod EArrayList {
        @DefaultHandler
        pub def runWithIO(f: Unit -> a \ ef): a \ (ef - EArrayList) + IO =
            run {
                f()
            } with handler EArrayList {
                def op(k) = {
                    let arr = Array#{List#{"a", "b"}, List#{"c"},  Nil} @ Static;
                    k(arr)
                }
            }
    }

    mod EArrayResult {
        @DefaultHandler
        pub def runWithIO(f: Unit -> a \ ef): a \ (ef - EArrayResult) + IO =
            run {
                f()
            } with handler EArrayResult {
                def op(k) = {
                    let arr = Array#{Ok(1), Err("error"),  Ok(2)} @ Static;
                    k(arr)
                }
            }
    }

    mod EEnum {
        @DefaultHandler
        pub def runWithIO(f: Unit -> a \ ef): a \ (ef - EEnum) + IO =
            run {
                f()
            } with handler EEnum {
                def op(k) = {
                    k(Test.DefaultHandler.Color.Green)
                }
            }
    }

    mod ETuple {
        @DefaultHandler
        pub def runWithIO(f: Unit -> a \ ef): a \ (ef - ETuple) + IO =
            run {
                f()
            } with handler ETuple {
                def op(k) = {
                    k((true, 'Z', "tuple"))
                }
            }
    }

    mod EAdd {
        @DefaultHandler
        pub def runWithIO(f: Unit -> a \ ef): a \ (ef - EAdd) + IO =
            run {
                f()
            } with handler EAdd {
                def add(x, y, k) = {
                    k(x + y)
                }
            }
    }

    @Test
    def testUnitDefaultHandler01(): Unit \ Assert + EUnit =
        Assert.assertEq(expected=(), EUnit.op())

    @Test
    def testBoolDefaultHandler01(): Unit \ Assert + EBool =
        Assert.assertEq(expected=true, EBool.op())

    @Test
    def testCharDefaultHandler01(): Unit \ Assert + EChar =
        Assert.assertEq(expected='X', EChar.op())

    @Test
    def testFloat32DefaultHandler01(): Unit \ Assert + EFloat32 =
        Assert.assertEq(expected=3.14f32, EFloat32.op())

    @Test
    def testFloat64DefaultHandler01(): Unit \ Assert + EFloat64 =
        Assert.assertEq(expected=2.718281828459045, EFloat64.op())

    @Test
    def testInt8DefaultHandler01(): Unit \ Assert + EInt8 =
        Assert.assertEq(expected=42i8, EInt8.op())

    @Test
    def testInt16DefaultHandler01(): Unit \ Assert + EInt16 =
        Assert.assertEq(expected=1000i16, EInt16.op())

    @Test
    def testInt32DefaultHandler01(): Unit \ Assert + EInt32 =
        Assert.assertEq(expected=5, EInt32.op())

    @Test
    def testInt64DefaultHandler01(): Unit \ Assert + EInt64 =
        Assert.assertEq(expected=9999999999i64, EInt64.op())

    @Test
    def testBigIntDefaultHandler01(): Unit \ Assert + EBigInt =
        Assert.assertEq(expected=12345678901234567890ii, EBigInt.op())

    @Test
    def testStringDefaultHandler01(): Unit \ Assert + EString =
        Assert.assertEq(expected="Default string", EString.op())

    @Test
    def testArrayCharDefaultHandler01(): Unit \ Assert + EArrayChar + IO = {
        let result = EArrayChar.op();
        let expected = Array#{'A', 'B', 'C'} @ Static;
        let areEqual = Array.sameElements(expected, result);
        Assert.assertTrue(areEqual)
    }

    @Test
    def testArrayInt32DefaultHandler01(): Unit \ Assert + EArrayInt32 + IO = {
        let result = EArrayInt32.op();
        let expected = Array#{ 1,  2,  3} @ Static;
        let areEqual = Array.sameElements(expected, result);
        Assert.assertTrue(areEqual)
    }

    @Test
    def testArrayOptionDefaultHandler01(): Unit \ Assert + EArrayOption + IO = {
        let result = EArrayOption.op();
        let expected = Array#{Some(10), None, Some(20)} @ Static;
        let areEqual = Array.sameElements(expected, result);
        Assert.assertTrue(areEqual)
    }

    @Test
    def testArrayListDefaultHandler01(): Unit \ Assert + EArrayList + IO = {
        let result = EArrayList.op();
        let expected = Array#{List#{"a", "b"}, List#{"c"},  Nil} @ Static;
        let areEqual = Array.sameElements(expected, result);
        Assert.assertTrue(areEqual)
    }

    @Test
    def testArrayResultDefaultHandler01(): Unit \ Assert + EArrayResult + IO = {
        let result = EArrayResult.op();
        let expected = Array#{Ok(1),  Err("error"), Ok(2)} @ Static;
        let areEqual = Array.sameElements(expected, result);
        Assert.assertTrue(areEqual)
    }

    @Test
    def testEnumDefaultHandler01(): Unit \ Assert + EEnum =
        Assert.assertEq(expected=Color.Green, EEnum.op())

    @Test
    def testTupleDefaultHandler01(): Unit \ Assert + ETuple =
        Assert.assertEq(expected=(true, 'Z', "tuple"), ETuple.op())

    @Test
    def testMultipleHandlers01(): Unit \ Assert + EInt32 + EString = {
        let num = EInt32.op();
        let str = EString.op();
        let result = "${str}: ${num}";
        Assert.assertEq(expected="Default string: 5", result)
    }

    @Test
    def testHandlerOrderVariation01(): Unit \ Assert + EBool + EChar = {
        let b = EBool.op();
        let c = EChar.op();
        Assert.assertTrue(b and c == 'X')
    }

    @Test
    def testHandlerOrderVariation02(): Unit \ Assert + EChar + EBool = {
        let c = EChar.op();
        let b = EBool.op();
        Assert.assertTrue(b and c == 'X')
    }

    @Test
    def testSameHandlerMultipleTimes01(): Unit \ Assert + EInt32 = {
        let x = EInt32.op();
        let y = EInt32.op();
        let z = EInt32.op();
        let sum = x + y + z;
        Assert.assertEq(expected=15, sum)
    }

    @Test
    def testSameHandlerMultipleTimes02(): Unit \ Assert + EString = {
        let s1 = EString.op();
        let s2 = EString.op();
        let combined = "${s1} ${s2}";
        Assert.assertEq(expected="Default string Default string", combined)
    }

    @Test
    def testUnrequiredHandler01(): Unit \ Assert + EInt32 + EString = {
        let x = EInt32.op();
        let result = checked_ecast(x * 2);
        Assert.assertEq(expected=10, result)
    }

    @Test
    def testDefaultHandlerWithArguments01(): Unit \ Assert + EAdd = {
        let result = EAdd.add(10, 20);
        Assert.assertEq(expected=30, result)
    }

    @Test
    def testDefaultHandlerWithArgumentsRepeated01(): Unit \ Assert + EAdd = {
        let a = EAdd.add(5, 3);
        let b = EAdd.add(10, 7);
        let c = EAdd.add(a, b);
        Assert.assertEq(expected=25, c)
    }

    @Test
    def testDefaultHandlerMixedEffects01(): Unit \ Assert + EAdd + EInt32 = {
        let base = EInt32.op();
        let added = EAdd.add(base, 10);
        Assert.assertEq(expected=15, added)
    }

    @Test
    def testComplexCombination01(): Unit \ Assert + EInt32 + EString + EAdd = {
        let x = EInt32.op();
        let y = EInt32.op();
        let sum = EAdd.add(x, y);
        let str = EString.op();
        let result = "${str}: ${sum}";
        Assert.assertEq(expected="Default string: 10", result)
    }
}
