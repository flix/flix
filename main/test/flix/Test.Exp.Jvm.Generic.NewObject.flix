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

}
