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
}
