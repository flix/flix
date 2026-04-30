mod Test.Exp.Jvm.Instanceof {

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
        o instanceof {
            case _: JString => ()
            case _: Object  => Assert.fail("expected JString")
        }
    }

    @Test
    def testInstanceof02(): Unit \ Assert = {
        let o: Object = checked_cast("Hello World");
        o instanceof {
            case _: CharSequence => ()
            case _: Object       => Assert.fail("expected CharSequence")
        }
    }

    @Test
    def testInstanceof03(): Unit \ Assert = {
        let o: Object = checked_cast("Hello World");
        o instanceof {
            case _: Object => ()
        }
    }

    @Test
    def testInstanceof04(): Unit \ Assert = {
        let o: Object = checked_cast(100ii);
        o instanceof {
            case _: BigInteger => ()
            case _: Object     => Assert.fail("expected BigInteger")
        }
    }

    @Test
    def testInstanceof05(): Unit \ Assert = {
        let o: Object = checked_cast(100ii);
        o instanceof {
            case _: Number => ()
            case _: Object => Assert.fail("expected Number")
        }
    }

    @Test
    def testInstanceof06(): Unit \ Assert = {
        let o: Object = checked_cast(100ii);
        o instanceof {
            case _: Object => ()
        }
    }

    @Test
    def testInstanceof07(): Unit \ Assert = {
        let o: Object = checked_cast(StandardOpenOption.DELETE_ON_CLOSE);
        o instanceof {
            case _: StandardOpenOption => ()
            case _: Object             => Assert.fail("expected StandardOpenOption")
        }
    }

    @Test
    def testInstanceof08(): Unit \ Assert = {
        let o: Object = checked_cast(StandardOpenOption.DELETE_ON_CLOSE);
        o instanceof {
            case _: OpenOption => ()
            case _: Object     => Assert.fail("expected OpenOption")
        }
    }

    @Test
    def testInstanceof10(): Unit \ Assert = {
        let o: Object = checked_cast(StandardOpenOption.DELETE_ON_CLOSE);
        o instanceof {
            case _: Object => ()
        }
    }

    @Test
    def testInstanceof11(): Unit \ Assert = {
        let o: Object = checked_cast(100ii);
        o instanceof {
            case _: Long   => Assert.fail("did not expect Long")
            case _: Object => ()
        }
    }

    @Test
    def testInstanceof12(): Unit \ Assert = {
        let o: Object = checked_cast("Hello World");
        o instanceof {
            case _: StringBuilder => Assert.fail("did not expect StringBuilder")
            case _: Object        => ()
        }
    }

    @Test
    def testInstanceof13(): Unit \ Assert + IO = {
        let arr = Array#{"hi"} @ Static;
        let o: Object = checked_cast(Array.get(0, arr));
        o instanceof {
            case _: JString => ()
            case _: Object  => Assert.fail("expected JString")
        }
    }

    // instanceof binds the scrutinee as the typed class and lets us use it.
    @Test
    def testInstanceof14(): Unit \ Assert = {
        let o: Object = checked_cast("Hello");
        let len = o instanceof {
            case s: JString => String.length(s)
            case _: Object  => -1
        };
        Assert.assertEq(expected = 5, len)
    }

    @Test
    def testInstanceof15(): Unit \ Assert = {
        let o: Object = checked_cast(100ii);
        let tag = o instanceof {
            case _: JString    => "string"
            case _: BigInteger => "bigint"
            case _: Object     => "other"
        };
        Assert.assertEq(expected = "bigint", tag)
    }

    // First matching case wins: BigInteger is a Number, but the BigInteger case comes first.
    @Test
    def testInstanceof16(): Unit \ Assert = {
        let o: Object = checked_cast(100ii);
        let tag = o instanceof {
            case _: JString    => "string"
            case _: BigInteger => "bigint"
            case _: Number     => "number"
            case _: Object     => "other"
        };
        Assert.assertEq(expected = "bigint", tag)
    }

    // A more general class earlier shadows a more specific one later.
    @Test
    def testInstanceof17(): Unit \ Assert = {
        let o: Object = checked_cast(100ii);
        let tag = o instanceof {
            case _: Number     => "number"
            case _: BigInteger => "bigint"
            case _: Object     => "other"
        };
        Assert.assertEq(expected = "number", tag)
    }

    // Catch-all binds the scrutinee at its static type.
    @Test
    def testInstanceof18(): Unit \ Assert = {
        let o: Object = checked_cast(StandardOpenOption.DELETE_ON_CLOSE);
        let tag = o instanceof {
            case _: JString => "string"
            case _: Number  => "number"
            case _: Object  => "other-object"
        };
        Assert.assertEq(expected = "other-object", tag)
    }

    // instanceof on enums dispatches correctly on the runtime class.
    @Test
    def testInstanceof19(): Unit \ Assert = {
        let o: Object = checked_cast(StandardOpenOption.DELETE_ON_CLOSE);
        let tag = o instanceof {
            case _: JString            => "string"
            case _: StandardOpenOption => "open-option"
            case _: Object             => "other"
        };
        Assert.assertEq(expected = "open-option", tag)
    }

    // Subclass check via instanceof: OpenOption is an interface of StandardOpenOption.
    @Test
    def testInstanceof20(): Unit \ Assert = {
        let o: Object = checked_cast(StandardOpenOption.DELETE_ON_CLOSE);
        let tag = o instanceof {
            case _: JString    => "string"
            case _: OpenOption => "open-option-iface"
            case _: Object     => "other"
        };
        Assert.assertEq(expected = "open-option-iface", tag)
    }

    // Refined-type binding: bound name is typed as BigInteger and a BigInteger method is invoked on it.
    @Test
    def testInstanceofBind01(): Unit \ Assert = {
        let o: Object = checked_cast(100ii);
        let s = o instanceof {
            case b: BigInteger => b.toString()
            case _: Object     => "other"
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
            case _: Object  => "other"
        };
        Assert.assertEq(expected = "DELETE_ON_CLOSE", s)
    }

    // Refined-type binding: bound name is typed as JString and a String method is invoked on it.
    @Test
    def testInstanceofBind04(): Unit \ Assert = {
        let o: Object = checked_cast("hello");
        let n = o instanceof {
            case s: JString => String.length(s)
            case _: Object  => -1
        };
        Assert.assertEq(expected = 5, n)
    }

    // Parametric Java type: matching against ArrayList with a wildcard type argument.
    @Test
    def testInstanceofParametric01(): Unit \ {Assert, IO} = {
        let l: ArrayList[String] = new ArrayList();
        let o: Object = checked_cast(l);
        o instanceof {
            case _: ArrayList[_] => ()
            case _: Object       => Assert.fail("expected ArrayList")
        }
    }

    // Parametric Java type: matching against HashMap with wildcard type arguments.
    @Test
    def testInstanceofParametric02(): Unit \ {Assert, IO} = {
        let m: HashMap[String, Int32] = new HashMap();
        let o: Object = checked_cast(m);
        o instanceof {
            case _: HashMap[_, _] => ()
            case _: Object        => Assert.fail("expected HashMap")
        }
    }

    // Parametric Java type: matching against Optional with a wildcard type argument.
    @Test
    def testInstanceofParametric03(): Unit \ {Assert, IO} = {
        let opt: Optional[String] = Optional.of("hi");
        let o: Object = checked_cast(opt);
        o instanceof {
            case _: Optional[_] => ()
            case _: Object      => Assert.fail("expected Optional")
        }
    }

    // Parametric Java type: dispatch between ArrayList and HashMap on a value of static type Object.
    @Test
    def testInstanceofParametric04(): Unit \ {Assert, IO} = {
        let m: HashMap[String, Int32] = new HashMap();
        let o: Object = checked_cast(m);
        let tag = o instanceof {
            case _: ArrayList[_]  => "list"
            case _: HashMap[_, _] => "map"
            case _: Object        => "other"
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
            case _: Object        => -1
        };
        Assert.assertEq(expected = 0, n)
    }
}
