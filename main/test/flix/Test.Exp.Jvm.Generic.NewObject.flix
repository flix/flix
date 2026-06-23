mod Test.Exp.Jvm.Generic.NewObject {

    use Assert.{assertEq, assertTrue}

    import java.io.Serializable
    import java.lang.{Comparable => JComparable}
    import java.lang.{Integer => JInteger}
    import java.lang.{Iterable => JIterable}
    import java.lang.Object
    import java.lang.Runnable
    import java.util.ArrayList
    import java.util.{Collections => JCollections}
    import java.util.Comparator
    import java.util.{Iterator => JIterator}
    import java.util.TreeMap
    import java.util.concurrent.Callable

    // CompileTests

    @CompileTest
    def testComparatorCompile01(): Comparator[String] \ IO =
        new Comparator[String] {
            def compare(_this: Comparator[String], t: String, u: String): Int32 =
                t.compareTo(u)
        }

    @CompileTest
    def testComparatorCompile02(): Comparator[JInteger] \ IO =
        new Comparator[JInteger] {
            def compare(_this: Comparator[JInteger], t: JInteger, u: JInteger): Int32 =
                t.compareTo(u)
        }

    @CompileTest
    def testCallableString01(): Callable[String] \ IO =
        new Callable[String] {
            def call(_this: Callable[String]): String =
                "hello"
        }

    @CompileTest
    def testCallableInteger01(): Callable[JInteger] \ IO =
        new Callable[JInteger] {
            def call(_this: Callable[JInteger]): JInteger =
                JInteger.valueOf(42i32)
        }

    @CompileTest
    def testIterableCompile01(): JIterable[Object] \ IO =
        new JIterable[Object] {
            def iterator(_this: JIterable[Object]): JIterator[Object] \ IO =
                let l: ArrayList[Object] = new ArrayList();
                l.iterator()
        }

    @CompileTest
    def testComparableCompile01(): JComparable[String] \ IO =
        new JComparable[String] {
            def compareTo(_this: JComparable[String], o: String): Int32 =
                "hello".compareTo(o)
        }

    // Tests

    @Test
    def testComparatorString01(): Unit \ {Assert, IO} =
        let c = new Comparator[String] {
            def compare(_this: Comparator[String], t: String, u: String): Int32 =
                t.compareTo(u)
        };
        assertTrue(c.compare("a", "b") < 0)

    @Test
    def testComparatorString02(): Unit \ {Assert, IO} =
        let c = new Comparator[String] {
            def compare(_this: Comparator[String], t: String, u: String): Int32 =
                u.compareTo(t)
        };
        assertTrue(c.compare("a", "b") > 0)

    @Test
    def testComparatorInteger01(): Unit \ {Assert, IO} =
        let c = new Comparator[JInteger] {
            def compare(_this: Comparator[JInteger], t: JInteger, u: JInteger): Int32 =
                t.compareTo(u)
        };
        assertTrue(c.compare(JInteger.valueOf(1i32), JInteger.valueOf(2i32)) < 0)

    @Test
    def testComparatorWithTreeMap01(): Unit \ {Assert, IO} =
        let c = new Comparator[String] {
            def compare(_this: Comparator[String], t: String, u: String): Int32 =
                u.compareTo(t)
        };
        let m: TreeMap[String, String] = new TreeMap(c);
        m.put("a", "1");
        m.put("b", "2");
        m.put("c", "3");
        let r: String = m.firstKey();
        assertEq(expected = "c", r)

    @Test
    def testComparatorRaw01(): Unit \ {Assert, IO} =
        let c = new Comparator[String] {
            def compare(_this: Comparator[String], _t: String, _u: String): Int32 = 0
        };
        assertEq(expected = 0, c.compare("foo", "bar"))

    @Test
    def testRunnableWithGenericCapture01(): Unit \ {Assert, IO} =
        let l: ArrayList[String] = new ArrayList();
        let r = new Runnable {
            def $run(_this: Runnable): Unit \ IO =
                l.add("captured");
                ()
        };
        r.$run();
        assertEq(expected = 1, l.size())

    @Test
    def testComparatorCompare01(): Unit \ {Assert, IO} =
        let c = new Comparator[String] {
            def compare(_this: Comparator[String], t: String, u: String): Int32 =
                t.compareTo(u)
        };
        assertEq(expected = 0, c.compare("hello", "hello"))

    @Test
    def testComparatorCompareReverse01(): Unit \ {Assert, IO} =
        let c = new Comparator[String] {
            def compare(_this: Comparator[String], t: String, u: String): Int32 =
                u.compareTo(t)
        };
        assertTrue(c.compare("a", "b") > 0)

    @Test
    def testComparatorUsedInSort01(): Unit \ {Assert, IO} =
        let c = new Comparator[String] {
            def compare(_this: Comparator[String], t: String, u: String): Int32 =
                u.compareTo(t)
        };
        let l: ArrayList[String] = new ArrayList();
        l.add("a");
        l.add("c");
        l.add("b");
        JCollections.sort(l, c);
        let r: String = l.get(0);
        assertEq(expected = "c", r)

    @Test
    def testComparatorCapture01(): Unit \ {Assert, IO} =
        let n = 1;
        let c = new Comparator[JInteger] {
            def compare(_this: Comparator[JInteger], t: JInteger, u: JInteger): Int32 =
                t.compareTo(u) + n - n
        };
        assertTrue(c.compare(JInteger.valueOf(1i32), JInteger.valueOf(2i32)) < 0)

    @Test
    def testSerializableStillWorks01(): Unit \ {Assert, IO} =
        let a = new Serializable { };
        assertTrue(a.toString().startsWith("Anon"))

    @Test
    def testComparatorWithTreeMapOrdering01(): Unit \ {Assert, IO} =
        let c = new Comparator[String] {
            def compare(_this: Comparator[String], t: String, u: String): Int32 =
                t.compareTo(u)
        };
        let m: TreeMap[String, String] = new TreeMap(c);
        m.put("c", "3");
        m.put("a", "1");
        m.put("b", "2");
        let r: String = m.firstKey();
        assertEq(expected = "a", r)

    @Test
    def testRunnableCapturingGeneric01(): Unit \ {Assert, IO} =
        let l: ArrayList[String] = new ArrayList();
        l.add("before");
        let r = new Runnable {
            def $run(_this: Runnable): Unit \ IO =
                l.add("after");
                ()
        };
        r.$run();
        assertEq(expected = 2, l.size())

    // Tests for explicit type arguments on new object expressions.
    // With type arguments, method parameters use the resolved generic types
    // (e.g., String instead of Object), eliminating the need for casts.

    // --- Compile tests: verify type arguments produce the correct result type ---

    @CompileTest
    def testNewObjectTargsComparatorString01(): Comparator[String] \ IO =
        new Comparator[String] {
            def compare(_this: Comparator[String], t: String, u: String): Int32 =
                t.compareTo(u)
        }

    @CompileTest
    def testNewObjectTargsComparatorInteger01(): Comparator[JInteger] \ IO =
        new Comparator[JInteger] {
            def compare(_this: Comparator[JInteger], t: JInteger, u: JInteger): Int32 =
                t.compareTo(u)
        }

    @CompileTest
    def testNewObjectTargsCallableString01(): Callable[String] \ IO =
        new Callable[String] {
            def call(_this: Callable[String]): String =
                "hello"
        }

    @CompileTest
    def testNewObjectTargsCallableInteger01(): Callable[JInteger] \ IO =
        new Callable[JInteger] {
            def call(_this: Callable[JInteger]): JInteger =
                JInteger.valueOf(42i32)
        }

    @CompileTest
    def testNewObjectTargsIterableString01(): JIterable[String] \ IO =
        new JIterable[String] {
            def iterator(_this: JIterable[String]): JIterator[String] \ IO =
                let l: ArrayList[String] = new ArrayList();
                l.iterator()
        }

    @CompileTest
    def testNewObjectTargsComparableString01(): JComparable[String] \ IO =
        new JComparable[String] {
            def compareTo(_this: JComparable[String], o: String): Int32 =
                "hello".compareTo(o)
        }

    // --- Runtime tests: verify end-to-end correctness with generic params ---

    @Test
    def testNewObjectTargsComparatorRuntime01(): Unit \ {Assert, IO} =
        let c = new Comparator[String] {
            def compare(_this: Comparator[String], t: String, u: String): Int32 =
                t.compareTo(u)
        };
        assertTrue(c.compare("a", "b") < 0)

    @Test
    def testNewObjectTargsComparatorRuntime02(): Unit \ {Assert, IO} =
        let c = new Comparator[JInteger] {
            def compare(_this: Comparator[JInteger], t: JInteger, u: JInteger): Int32 =
                t.compareTo(u)
        };
        assertTrue(c.compare(JInteger.valueOf(1i32), JInteger.valueOf(2i32)) < 0)

    @Test
    def testNewObjectTargsComparatorTreeMap01(): Unit \ {Assert, IO} =
        let c = new Comparator[String] {
            def compare(_this: Comparator[String], t: String, u: String): Int32 =
                u.compareTo(t)
        };
        let m: TreeMap[String, String] = new TreeMap(c);
        m.put("a", "1");
        m.put("b", "2");
        m.put("c", "3");
        let r: String = m.firstKey();
        assertEq(expected = "c", r)

    @Test
    def testNewObjectTargsComparatorSort01(): Unit \ {Assert, IO} =
        let c = new Comparator[String] {
            def compare(_this: Comparator[String], t: String, u: String): Int32 =
                u.compareTo(t)
        };
        let l: ArrayList[String] = new ArrayList();
        l.add("a");
        l.add("c");
        l.add("b");
        JCollections.sort(l, c);
        let r: String = l.get(0);
        assertEq(expected = "c", r)

    @Test
    def testNewObjectTargsCallableRuntime01(): Unit \ {Assert, IO} =
        let c: Callable[String] = new Callable[String] {
            def call(_this: Callable[String]): String =
                "hello"
        };
        let r: String = c.call();
        assertEq(expected = "hello", r)

    // --- Backward compatibility: explicit Object type arg works like pre-targs behavior ---

    @Test
    def testNewObjectTargsBackwardCompat01(): Unit \ {Assert, IO} =
        let c = new Comparator[String] {
            def compare(_this: Comparator[String], _t: String, _u: String): Int32 = 0
        };
        assertEq(expected = 0, c.compare("foo", "bar"))

    @Test
    def testNewObjectTargsBackwardCompat02(): Unit \ {Assert, IO} =
        let c: Comparator[String] = new Comparator[String] {
            def compare(_this: Comparator[String], t: String, u: String): Int32 =
                t.compareTo(u)
        };
        assertTrue(c.compare("a", "b") < 0)

    // --- Generic type args match resolved method signatures ---

    @Test
    def testNewObjectTargsObjectParams01(): Unit \ {Assert, IO} =
        let c = new Comparator[String] {
            def compare(_this: Comparator[String], t: String, u: String): Int32 =
                t.compareTo(u)
        };
        assertTrue(c.compare("a", "b") < 0)

    // --- Parameterized _this type ---

    @Test
    def testNewObjectTargsThisParam01(): Unit \ {Assert, IO} =
        let c = new Comparator[String] {
            def compare(_this: Comparator[String], t: String, u: String): Int32 =
                t.compareTo(u)
        };
        assertTrue(c.compare("a", "b") < 0)

    // --- Generic interface instantiated with a non-reference (primitive / Unit) type param ---
    // Regression tests for https://github.com/flix/flix/issues/12876. The overridden method's
    // erased JVM return type is `Object`, so a primitive result must be boxed (e.g. via
    // `Integer.valueOf`) and `Unit` returned as its runtime value; otherwise the generated
    // anonymous class fails JVM verification with a `VerifyError`.

    @Test
    def testNewObjectPrimitiveCallableInt8(): Unit \ {Assert, IO} =
        let c = new Callable[Int8] {
            def call(_this: Callable[Int8]): Int8 = 42i8
        };
        assertEq(expected = 42i8, c.call())

    @Test
    def testNewObjectPrimitiveCallableInt16(): Unit \ {Assert, IO} =
        let c = new Callable[Int16] {
            def call(_this: Callable[Int16]): Int16 = 42i16
        };
        assertEq(expected = 42i16, c.call())

    @Test
    def testNewObjectPrimitiveCallableInt32(): Unit \ {Assert, IO} =
        let c = new Callable[Int32] {
            def call(_this: Callable[Int32]): Int32 = 42i32
        };
        assertEq(expected = 42i32, c.call())

    @Test
    def testNewObjectPrimitiveCallableInt64(): Unit \ {Assert, IO} =
        let c = new Callable[Int64] {
            def call(_this: Callable[Int64]): Int64 = 42i64
        };
        assertEq(expected = 42i64, c.call())

    @Test
    def testNewObjectPrimitiveCallableFloat32(): Unit \ {Assert, IO} =
        let c = new Callable[Float32] {
            def call(_this: Callable[Float32]): Float32 = 42.0f32
        };
        assertEq(expected = 42.0f32, c.call())

    @Test
    def testNewObjectPrimitiveCallableFloat64(): Unit \ {Assert, IO} =
        let c = new Callable[Float64] {
            def call(_this: Callable[Float64]): Float64 = 42.0f64
        };
        assertEq(expected = 42.0f64, c.call())

    @Test
    def testNewObjectPrimitiveCallableBool(): Unit \ {Assert, IO} =
        let c = new Callable[Bool] {
            def call(_this: Callable[Bool]): Bool = true
        };
        assertTrue(c.call())

    @Test
    def testNewObjectPrimitiveCallableChar(): Unit \ {Assert, IO} =
        let c = new Callable[Char] {
            def call(_this: Callable[Char]): Char = 'x'
        };
        assertEq(expected = 'x', c.call())

    @Test
    def testNewObjectUnitCallable(): Unit \ {Assert, IO} =
        let c = new Callable[Unit] {
            def call(_this: Callable[Unit]): Unit = ()
        };
        assertEq(expected = (), c.call())

    // --- A single generic anonymous-class site specialized at multiple types ---
    // Regression tests for the anonymous-class specialization collision. The `new Callable[a]`
    // site below lives in one generic def; monomorphization specializes it once per type. Each
    // specialization must get its own generated anonymous class. Otherwise distinct
    // specializations (e.g. `boxedCallable[Int32]` vs `boxedCallable[Bool]`) share one class
    // name and collide, so one specialization runs with another's class -- a `ClassCastException`
    // (or a `VerifyError` when the colliding representations differ) at runtime.

    def boxedCallable(x: a): Callable[a] \ IO =
        new Callable[a] { def call(_this: Callable[a]): a = x }

    @Test
    def testNewObjectSpecializationInt8(): Unit \ {Assert, IO} =
        assertEq(expected = 1i8, boxedCallable(1i8).call())

    @Test
    def testNewObjectSpecializationInt16(): Unit \ {Assert, IO} =
        assertEq(expected = 2i16, boxedCallable(2i16).call())

    @Test
    def testNewObjectSpecializationInt32(): Unit \ {Assert, IO} =
        assertEq(expected = 3i32, boxedCallable(3i32).call())

    @Test
    def testNewObjectSpecializationInt64(): Unit \ {Assert, IO} =
        assertEq(expected = 4i64, boxedCallable(4i64).call())

    @Test
    def testNewObjectSpecializationFloat32(): Unit \ {Assert, IO} =
        assertEq(expected = 5.0f32, boxedCallable(5.0f32).call())

    @Test
    def testNewObjectSpecializationFloat64(): Unit \ {Assert, IO} =
        assertEq(expected = 6.0f64, boxedCallable(6.0f64).call())

    @Test
    def testNewObjectSpecializationBool(): Unit \ {Assert, IO} =
        assertEq(expected = true, boxedCallable(true).call())

    @Test
    def testNewObjectSpecializationChar(): Unit \ {Assert, IO} =
        assertEq(expected = 'q', boxedCallable('q').call())

    @Test
    def testNewObjectSpecializationUnit(): Unit \ {Assert, IO} =
        assertEq(expected = (), boxedCallable(()).call())

    @Test
    def testNewObjectSpecializationString(): Unit \ {Assert, IO} =
        assertEq(expected = "hello", boxedCallable("hello").call())

    // Mixes many specializations of the same site in one expression -- this is the exact shape
    // that failed before the fix: the first call would run with a later specialization's class.
    @Test
    def testNewObjectSpecializationMixed(): Unit \ {Assert, IO} =
        assertEq(expected = 7i8, boxedCallable(7i8).call());
        assertEq(expected = 8i16, boxedCallable(8i16).call());
        assertEq(expected = 9i32, boxedCallable(9i32).call());
        assertEq(expected = 10i64, boxedCallable(10i64).call());
        assertEq(expected = 11.0f32, boxedCallable(11.0f32).call());
        assertEq(expected = 12.0f64, boxedCallable(12.0f64).call());
        assertEq(expected = false, boxedCallable(false).call());
        assertEq(expected = 'z', boxedCallable('z').call());
        assertEq(expected = (), boxedCallable(()).call());
        assertEq(expected = "world", boxedCallable("world").call())

}
