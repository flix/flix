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

    @Test
    def testInstanceof01(): Unit \ Assert = {
        let o: Object = checked_cast("Hello World");
        assertTrue(o instanceof {
            case _: JString => true
            case _: Object => false
        })
    }

    @Test
    def testInstanceof02(): Unit \ Assert = {
        let o: Object = checked_cast("Hello World");
        assertTrue(o instanceof {
            case _: CharSequence => true
            case _: Object => false
        })
    }

    @Test
    def testInstanceof03(): Unit \ Assert = {
        let o: Object = checked_cast("Hello World");
        assertTrue(o instanceof {
            case _: Object => true
        })
    }

    @Test
    def testInstanceof04(): Unit \ Assert = {
        let o: Object = checked_cast(100ii);
        assertTrue(o instanceof {
            case _: BigInteger => true
            case _: Object => false
        })
    }

    @Test
    def testInstanceof05(): Unit \ Assert = {
        let o: Object = checked_cast(100ii);
        assertTrue(o instanceof {
            case _: Number => true
            case _: Object => false
        })
    }

    @Test
    def testInstanceof06(): Unit \ Assert = {
        let o: Object = checked_cast(100ii);
        assertTrue(o instanceof {
            case _: Object => true
        })
    }

    @Test
    def testInstanceof07(): Unit \ Assert = {
        let o: Object = checked_cast(StandardOpenOption.DELETE_ON_CLOSE);
        assertTrue(o instanceof {
            case _: StandardOpenOption => true
            case _: Object => false
        })
    }

    @Test
    def testInstanceof08(): Unit \ Assert = {
        let o: Object = checked_cast(StandardOpenOption.DELETE_ON_CLOSE);
        assertTrue(o instanceof {
            case _: OpenOption => true
            case _: Object => false
        })
    }

    @Test
    def testInstanceof10(): Unit \ Assert = {
        let o: Object = checked_cast(StandardOpenOption.DELETE_ON_CLOSE);
        assertTrue(o instanceof {
            case _: Object => true
        })
    }

    @Test
    def testInstanceof11(): Unit \ Assert = {
        let o: Object = checked_cast(100ii);
        assertFalse(o instanceof {
            case _: Long => true
            case _: Object => false
        })
    }

    @Test
    def testInstanceof12(): Unit \ Assert = {
        let o: Object = checked_cast("Hello World");
        assertFalse(o instanceof {
            case _: StringBuilder => true
            case _: Object => false
        })
    }

    @Test
    def testInstanceof13(): Unit \ Assert + IO = {
        let arr = Array#{"hi"} @ Static;
        let o: Object = checked_cast(Array.get(0, arr));
        assertTrue(o instanceof {
            case _: JString => true
            case _: Object => false
        })
    }

    // instanceof binds the scrutinee as the typed class and lets us use it.
    @Test
    def testInstanceof14(): Unit \ Assert = {
        let o: Object = checked_cast("Hello");
        let len = o instanceof {
            case s: JString => String.length(s)
            case _: Object => -1
        };
        Assert.assertEq(expected = 5, len)
    }

    @Test
    def testInstanceof15(): Unit \ Assert = {
        let o: Object = checked_cast(100ii);
        let tag = o instanceof {
            case _: JString => "string"
            case _: BigInteger => "bigint"
            case _: Object => "other"
        };
        Assert.assertEq(expected = "bigint", tag)
    }

    // First matching case wins: BigInteger is a Number, but the BigInteger case comes first.
    @Test
    def testInstanceof16(): Unit \ Assert = {
        let o: Object = checked_cast(100ii);
        let tag = o instanceof {
            case _: JString => "string"
            case _: BigInteger => "bigint"
            case _: Number => "number"
            case _: Object => "other"
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
            case _: Object => "other"
        };
        Assert.assertEq(expected = "number", tag)
    }

    // Catch-all binds the scrutinee at its static type.
    @Test
    def testInstanceof18(): Unit \ Assert = {
        let o: Object = checked_cast(StandardOpenOption.DELETE_ON_CLOSE);
        let tag = o instanceof {
            case _: JString => "string"
            case _: Number => "number"
            case x: Object =>
                let isObj = x instanceof {
                    case _: Object => true
                };
                if (isObj) "other-object" else "impossible"
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
            case _: Object => "other"
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
            case _: Object => "other"
        };
        Assert.assertEq(expected = "open-option-iface", tag)
    }

    // Refined-type binding: bound name is typed as BigInteger and a BigInteger method is invoked on it.
    @Test
    def testInstanceofBind01(): Unit \ Assert = {
        let o: Object = checked_cast(100ii);
        let s = o instanceof {
            case b: BigInteger => b.toString()
            case _: Object => "other"
        };
        Assert.assertEq(expected = "100", s)
    }

    // Refined-type binding: bound name is typed as Number and Number.intValue is invoked on it.
    @Test
    def testInstanceofBind02(): Unit \ Assert = {
        let o: Object = checked_cast(100ii);
        let n = o instanceof {
            case x: Number => x.intValue()
            case _: Object => -1
        };
        Assert.assertEq(expected = 100, n)
    }

    // Refined-type binding: bound name is typed as Enum and Enum.name is invoked on it.
    @Test
    def testInstanceofBind03(): Unit \ Assert = {
        let o: Object = checked_cast(StandardOpenOption.DELETE_ON_CLOSE);
        let s = o instanceof {
            case e: Enum[_] => e.name()
            case _: Object => "other"
        };
        Assert.assertEq(expected = "DELETE_ON_CLOSE", s)
    }

    // Refined-type binding: bound name is typed as JString and a String method is invoked on it.
    @Test
    def testInstanceofBind04(): Unit \ Assert = {
        let o: Object = checked_cast("hello");
        let n = o instanceof {
            case s: JString => String.length(s)
            case _: Object => -1
        };
        Assert.assertEq(expected = 5, n)
    }

    // Parametric Java type: matching against ArrayList with a wildcard type argument.
    @Test
    def testInstanceofParametric01(): Unit \ {Assert, IO} = {
        let l: ArrayList[String] = new ArrayList();
        let o: Object = checked_cast(l);
        let result = o instanceof {
            case _: ArrayList[_] => true
            case _: Object => false
        };
        assertTrue(result)
    }

    // Parametric Java type: matching against HashMap with wildcard type arguments.
    @Test
    def testInstanceofParametric02(): Unit \ {Assert, IO} = {
        let m: HashMap[String, Int32] = new HashMap();
        let o: Object = checked_cast(m);
        let result = o instanceof {
            case _: HashMap[_, _] => true
            case _: Object => false
        };
        assertTrue(result)
    }

    // Parametric Java type: matching against Optional with a wildcard type argument.
    @Test
    def testInstanceofParametric03(): Unit \ {Assert, IO} = {
        let opt: Optional[String] = Optional.of("hi");
        let o: Object = checked_cast(opt);
        let result = o instanceof {
            case _: Optional[_] => true
            case _: Object => false
        };
        assertTrue(result)
    }

    // Parametric Java type: dispatch between ArrayList and HashMap on a value of static type Object.
    @Test
    def testInstanceofParametric04(): Unit \ {Assert, IO} = {
        let m: HashMap[String, Int32] = new HashMap();
        let o: Object = checked_cast(m);
        let tag = o instanceof {
            case _: ArrayList[_] => "list"
            case _: HashMap[_, _] => "map"
            case _: Object => "other"
        };
        Assert.assertEq(expected = "map", tag)
    }

    // Wildcard type vars are rigid: a parametric ArrayList can be bound and queried,
    // but its element type is opaque (e.g. size() works; add(specificString) does not).
    @Test
    def testInstanceofParametric05(): Unit \ {Assert, IO} = {
        let l: ArrayList[String] = new ArrayList();
        let o: Object = checked_cast(l);
        let n = o instanceof {
            case xs: ArrayList[_] => xs.size()
            case _: Object => -1
        };
        Assert.assertEq(expected = 0, n)
    }
}
