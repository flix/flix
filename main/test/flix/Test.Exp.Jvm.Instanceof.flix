mod Test.Exp.Jvm.Instanceof {

    use Assert.{assertTrue, assertFalse}

    import java.lang.CharSequence
    import java.lang.Enum
    import java.lang.Long
    import java.lang.Number
    import java.lang.Object
    import java.lang.{String => JString}
    import java.lang.StringBuilder
    import java.math.BigInteger
    import java.nio.file.OpenOption
    import java.nio.file.StandardOpenOption
    import java.util.ArrayList
    import java.util.HashMap
    import java.util.Optional

    def isString(o: Object): Bool = o instanceof {
        case _: JString => true
        case _ => false
    }

    def isCharSequence(o: Object): Bool = o instanceof {
        case _: CharSequence => true
        case _ => false
    }

    def isObject(o: Object): Bool = o instanceof {
        case _: Object => true
        case _ => false
    }

    def isBigInteger(o: Object): Bool = o instanceof {
        case _: BigInteger => true
        case _ => false
    }

    def isNumber(o: Object): Bool = o instanceof {
        case _: Number => true
        case _ => false
    }

    def isStandardOpenOption(o: Object): Bool = o instanceof {
        case _: StandardOpenOption => true
        case _ => false
    }

    def isOpenOption(o: Object): Bool = o instanceof {
        case _: OpenOption => true
        case _ => false
    }

    def isLong(o: Object): Bool = o instanceof {
        case _: Long => true
        case _ => false
    }

    def isStringBuilder(o: Object): Bool = o instanceof {
        case _: StringBuilder => true
        case _ => false
    }

    @Test
    def testInstanceof01(): Unit \ Assert =
        assertTrue(isString(checked_cast("Hello World")))

    @Test
    def testInstanceof02(): Unit \ Assert =
        assertTrue(isCharSequence(checked_cast("Hello World")))

    @Test
    def testInstanceof03(): Unit \ Assert =
        assertTrue(isObject(checked_cast("Hello World")))

    @Test
    def testInstanceof04(): Unit \ Assert =
        assertTrue(isBigInteger(checked_cast(100ii)))

    @Test
    def testInstanceof05(): Unit \ Assert =
        assertTrue(isNumber(checked_cast(100ii)))

    @Test
    def testInstanceof06(): Unit \ Assert =
        assertTrue(isObject(checked_cast(100ii)))

    @Test
    def testInstanceof07(): Unit \ Assert =
        assertTrue(isStandardOpenOption(checked_cast(StandardOpenOption.DELETE_ON_CLOSE)))

    @Test
    def testInstanceof08(): Unit \ Assert =
        assertTrue(isOpenOption(checked_cast(StandardOpenOption.DELETE_ON_CLOSE)))

    @Test
    def testInstanceof10(): Unit \ Assert =
        assertTrue(isObject(checked_cast(StandardOpenOption.DELETE_ON_CLOSE)))

    @Test
    def testInstanceof11(): Unit \ Assert =
        assertFalse(isLong(checked_cast(100ii)))

    @Test
    def testInstanceof12(): Unit \ Assert =
        assertFalse(isStringBuilder(checked_cast("Hello World")))

    @Test
    def testInstanceof13(): Unit \ Assert + IO = {
        let arr = Array#{"hi"} @ Static;
        assertTrue(isString(checked_cast(Array.get(0, arr))))
    }

    // instanceof binds the scrutinee as the typed class and lets us use it.
    @Test
    def testInstanceof14(): Unit \ Assert = {
        let o: Object = checked_cast("Hello");
        let len = o instanceof {
            case s: JString => String.length(s)
            case _ => -1
        };
        Assert.assertEq(expected = 5, len)
    }

    @Test
    def testInstanceof15(): Unit \ Assert = {
        let o: Object = checked_cast(100ii);
        let tag = o instanceof {
            case _: JString => "string"
            case _: BigInteger => "bigint"
            case _ => "other"
        };
        Assert.assertEq(expected = "bigint", tag)
    }

    // First matching case wins: BigInteger is a Number, but the String case comes first and doesn't match.
    @Test
    def testInstanceof16(): Unit \ Assert = {
        let o: Object = checked_cast(100ii);
        let tag = o instanceof {
            case _: JString => "string"
            case _: BigInteger => "bigint"
            case _: Number => "number"
            case _ => "other"
        };
        Assert.assertEq(expected = "bigint", tag)
    }

    // A more general class earlier shadows a more specific one later.
    @Test
    def testInstanceof17(): Unit \ Assert = {
        let o: Object = checked_cast(100ii);
        let tag = o instanceof {
            case _: Number => "number"
            case _: BigInteger => "bigint"
            case _ => "other"
        };
        Assert.assertEq(expected = "number", tag)
    }

    // Fallthrough wildcard binds the scrutinee.
    @Test
    def testInstanceof18(): Unit \ Assert = {
        let o: Object = checked_cast(StandardOpenOption.DELETE_ON_CLOSE);
        let tag = o instanceof {
            case _: JString => "string"
            case _: Number => "number"
            case x => if (isObject(x)) "other-object" else "impossible"
        };
        Assert.assertEq(expected = "other-object", tag)
    }

    // instanceof on enums dispatches correctly on the runtime class.
    @Test
    def testInstanceof19(): Unit \ Assert = {
        let o: Object = checked_cast(StandardOpenOption.DELETE_ON_CLOSE);
        let tag = o instanceof {
            case _: JString => "string"
            case _: StandardOpenOption => "open-option"
            case _ => "other"
        };
        Assert.assertEq(expected = "open-option", tag)
    }

    // Subclass check via instanceof: OpenOption is an interface of StandardOpenOption.
    @Test
    def testInstanceof20(): Unit \ Assert = {
        let o: Object = checked_cast(StandardOpenOption.DELETE_ON_CLOSE);
        let tag = o instanceof {
            case _: JString => "string"
            case _: OpenOption => "open-option-iface"
            case _ => "other"
        };
        Assert.assertEq(expected = "open-option-iface", tag)
    }

    // Refined-type binding: bound name is typed as BigInteger and a BigInteger method is invoked on it.
    @Test
    def testInstanceofBind01(): Unit \ Assert = {
        let o: Object = checked_cast(100ii);
        let s = o instanceof {
            case b: BigInteger => b.toString()
            case _ => "other"
        };
        Assert.assertEq(expected = "100", s)
    }

    // Refined-type binding: bound name is typed as Number and Number.intValue is invoked on it.
    @Test
    def testInstanceofBind02(): Unit \ Assert = {
        let o: Object = checked_cast(100ii);
        let n = o instanceof {
            case x: Number => x.intValue()
            case _ => -1
        };
        Assert.assertEq(expected = 100, n)
    }

    // Refined-type binding: bound name is typed as Enum and Enum.name is invoked on it.
    @Test
    def testInstanceofBind03(): Unit \ Assert = {
        let o: Object = checked_cast(StandardOpenOption.DELETE_ON_CLOSE);
        let s = o instanceof {
            case e: Enum => e.name()
            case _ => "other"
        };
        Assert.assertEq(expected = "DELETE_ON_CLOSE", s)
    }

    // Refined-type binding: bound name is typed as JString and a String method is invoked on it.
    @Test
    def testInstanceofBind04(): Unit \ Assert = {
        let o: Object = checked_cast("hello");
        let n = o instanceof {
            case s: JString => String.length(s)
            case _ => -1
        };
        Assert.assertEq(expected = 5, n)
    }

    // Parametric Java type: matching against the raw class ArrayList.
    @Test
    def testInstanceofParametric01(): Unit \ {Assert, IO} = {
        let l: ArrayList[String] = new ArrayList();
        let o: Object = checked_cast(l);
        let result = o instanceof {
            case _: ArrayList => true
            case _ => false
        };
        assertTrue(result)
    }

    // Parametric Java type: matching against the raw class HashMap.
    @Test
    def testInstanceofParametric02(): Unit \ {Assert, IO} = {
        let m: HashMap[String, Int32] = new HashMap();
        let o: Object = checked_cast(m);
        let result = o instanceof {
            case _: HashMap => true
            case _ => false
        };
        assertTrue(result)
    }

    // Parametric Java type: matching against the raw class Optional.
    @Test
    def testInstanceofParametric03(): Unit \ {Assert, IO} = {
        let opt: Optional[String] = Optional.of("hi");
        let o: Object = checked_cast(opt);
        let result = o instanceof {
            case _: Optional => true
            case _ => false
        };
        assertTrue(result)
    }

    // Parametric Java type: dispatch between ArrayList and HashMap on a value of static type Object.
    @Test
    def testInstanceofParametric04(): Unit \ {Assert, IO} = {
        let m: HashMap[String, Int32] = new HashMap();
        let o: Object = checked_cast(m);
        let tag = o instanceof {
            case _: ArrayList => "list"
            case _: HashMap => "map"
            case _ => "other"
        };
        Assert.assertEq(expected = "map", tag)
    }
}
