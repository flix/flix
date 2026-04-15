mod Test.Exp.Jvm.JMatch {

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

    def isString(o: Object): Bool = jmatch o {
        case _: JString => true
        case _ => false
    }

    def isCharSequence(o: Object): Bool = jmatch o {
        case _: CharSequence => true
        case _ => false
    }

    def isObject(o: Object): Bool = jmatch o {
        case _: Object => true
        case _ => false
    }

    def isBigInteger(o: Object): Bool = jmatch o {
        case _: BigInteger => true
        case _ => false
    }

    def isNumber(o: Object): Bool = jmatch o {
        case _: Number => true
        case _ => false
    }

    def isStandardOpenOption(o: Object): Bool = jmatch o {
        case _: StandardOpenOption => true
        case _ => false
    }

    def isOpenOption(o: Object): Bool = jmatch o {
        case _: OpenOption => true
        case _ => false
    }

    def isLong(o: Object): Bool = jmatch o {
        case _: Long => true
        case _ => false
    }

    def isStringBuilder(o: Object): Bool = jmatch o {
        case _: StringBuilder => true
        case _ => false
    }

    @Test
    def testJMatch01(): Unit \ Assert =
        assertTrue(isString(checked_cast("Hello World")))

    @Test
    def testJMatch02(): Unit \ Assert =
        assertTrue(isCharSequence(checked_cast("Hello World")))

    @Test
    def testJMatch03(): Unit \ Assert =
        assertTrue(isObject(checked_cast("Hello World")))

    @Test
    def testJMatch04(): Unit \ Assert =
        assertTrue(isBigInteger(checked_cast(100ii)))

    @Test
    def testJMatch05(): Unit \ Assert =
        assertTrue(isNumber(checked_cast(100ii)))

    @Test
    def testJMatch06(): Unit \ Assert =
        assertTrue(isObject(checked_cast(100ii)))

    @Test
    def testJMatch07(): Unit \ Assert =
        assertTrue(isStandardOpenOption(checked_cast(StandardOpenOption.DELETE_ON_CLOSE)))

    @Test
    def testJMatch08(): Unit \ Assert =
        assertTrue(isOpenOption(checked_cast(StandardOpenOption.DELETE_ON_CLOSE)))

    @Test
    def testJMatch10(): Unit \ Assert =
        assertTrue(isObject(checked_cast(StandardOpenOption.DELETE_ON_CLOSE)))

    @Test
    def testJMatch11(): Unit \ Assert =
        assertFalse(isLong(checked_cast(100ii)))

    @Test
    def testJMatch12(): Unit \ Assert =
        assertFalse(isStringBuilder(checked_cast("Hello World")))

    @Test
    def testJMatch13(): Unit \ Assert + IO = {
        let arr = Array#{"hi"} @ Static;
        assertTrue(isString(checked_cast(Array.get(0, arr))))
    }

    // jmatch binds the scrutinee as the typed class and lets us use it.
    @Test
    def testJMatch14(): Unit \ Assert = {
        let o: Object = checked_cast("Hello");
        let len = jmatch o {
            case s: JString => String.length(s)
            case _ => -1
        };
        Assert.assertEq(expected = 5, len)
    }

    @Test
    def testJMatch15(): Unit \ Assert = {
        let o: Object = checked_cast(100ii);
        let tag = jmatch o {
            case _: JString => "string"
            case _: BigInteger => "bigint"
            case _ => "other"
        };
        Assert.assertEq(expected = "bigint", tag)
    }

    // First matching case wins: BigInteger is a Number, but the String case comes first and doesn't match.
    @Test
    def testJMatch16(): Unit \ Assert = {
        let o: Object = checked_cast(100ii);
        let tag = jmatch o {
            case _: JString => "string"
            case _: BigInteger => "bigint"
            case _: Number => "number"
            case _ => "other"
        };
        Assert.assertEq(expected = "bigint", tag)
    }

    // A more general class earlier shadows a more specific one later.
    @Test
    def testJMatch17(): Unit \ Assert = {
        let o: Object = checked_cast(100ii);
        let tag = jmatch o {
            case _: Number => "number"
            case _: BigInteger => "bigint"
            case _ => "other"
        };
        Assert.assertEq(expected = "number", tag)
    }

    // Fallthrough wildcard binds the scrutinee.
    @Test
    def testJMatch18(): Unit \ Assert = {
        let o: Object = checked_cast(StandardOpenOption.DELETE_ON_CLOSE);
        let tag = jmatch o {
            case _: JString => "string"
            case _: Number => "number"
            case x => if (isObject(x)) "other-object" else "impossible"
        };
        Assert.assertEq(expected = "other-object", tag)
    }

    // jmatch on enums dispatches correctly on the runtime class.
    @Test
    def testJMatch19(): Unit \ Assert = {
        let o: Object = checked_cast(StandardOpenOption.DELETE_ON_CLOSE);
        let tag = jmatch o {
            case _: JString => "string"
            case _: StandardOpenOption => "open-option"
            case _ => "other"
        };
        Assert.assertEq(expected = "open-option", tag)
    }

    // Subclass check via jmatch: OpenOption is an interface of StandardOpenOption.
    @Test
    def testJMatch20(): Unit \ Assert = {
        let o: Object = checked_cast(StandardOpenOption.DELETE_ON_CLOSE);
        let tag = jmatch o {
            case _: JString => "string"
            case _: OpenOption => "open-option-iface"
            case _ => "other"
        };
        Assert.assertEq(expected = "open-option-iface", tag)
    }
}
