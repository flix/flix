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
        new Comparator {
            def compare(_this: Comparator[Object], t: Object, u: Object): Int32 =
                let s1 = unchecked_cast(t as String);
                let s2 = unchecked_cast(u as String);
                s1.compareTo(s2)
        }

    @CompileTest
    def testComparatorCompile02(): Comparator[JInteger] \ IO =
        new Comparator {
            def compare(_this: Comparator[Object], t: Object, u: Object): Int32 =
                let i1 = unchecked_cast(t as JInteger);
                let i2 = unchecked_cast(u as JInteger);
                i1.compareTo(i2)
        }

    @CompileTest
    def testCallableString01(): Callable[String] \ IO =
        new Callable {
            def call(_this: Callable[Object]): Object =
                checked_cast("hello")
        }

    @CompileTest
    def testCallableInteger01(): Callable[JInteger] \ IO =
        new Callable {
            def call(_this: Callable[Object]): Object =
                let v = JInteger.valueOf(42i32);
                checked_cast(v)
        }

    @CompileTest
    def testIterableCompile01(): JIterable[Object] \ IO =
        new JIterable {
            def iterator(_this: JIterable[Object]): JIterator[Object] \ IO =
                let l: ArrayList[Object] = new ArrayList();
                l.iterator()
        }

    @CompileTest
    def testComparableCompile01(): JComparable[String] \ IO =
        new JComparable {
            def compareTo(_this: JComparable[Object], o: Object): Int32 =
                let s = unchecked_cast(o as String);
                "hello".compareTo(s)
        }

    // Tests

    @Test
    def testComparatorString01(): Unit \ {Assert, IO} =
        let c = new Comparator {
            def compare(_this: Comparator[Object], t: Object, u: Object): Int32 =
                let s1 = unchecked_cast(t as String);
                let s2 = unchecked_cast(u as String);
                s1.compareTo(s2)
        };
        assertTrue(c.compare("a", "b") < 0)

    @Test
    def testComparatorString02(): Unit \ {Assert, IO} =
        let c = new Comparator {
            def compare(_this: Comparator[Object], t: Object, u: Object): Int32 =
                let s1 = unchecked_cast(t as String);
                let s2 = unchecked_cast(u as String);
                s2.compareTo(s1)
        };
        assertTrue(c.compare("a", "b") > 0)

    @Test
    def testComparatorInteger01(): Unit \ {Assert, IO} =
        let c = new Comparator {
            def compare(_this: Comparator[Object], t: Object, u: Object): Int32 =
                let i1 = unchecked_cast(t as JInteger);
                let i2 = unchecked_cast(u as JInteger);
                i1.compareTo(i2)
        };
        assertTrue(c.compare(JInteger.valueOf(1i32), JInteger.valueOf(2i32)) < 0)

    @Test
    def testComparatorWithTreeMap01(): Unit \ {Assert, IO} =
        let c = new Comparator {
            def compare(_this: Comparator[Object], t: Object, u: Object): Int32 =
                let s1 = unchecked_cast(t as String);
                let s2 = unchecked_cast(u as String);
                s2.compareTo(s1)
        };
        let m: TreeMap[String, String] = new TreeMap(c);
        m.put("a", "1");
        m.put("b", "2");
        m.put("c", "3");
        let r: String = m.firstKey();
        assertEq(expected = "c", r)

    @Test
    def testComparatorRaw01(): Unit \ {Assert, IO} =
        let c = new Comparator {
            def compare(_this: Comparator[Object], _t: Object, _u: Object): Int32 = 0
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
        let c = new Comparator {
            def compare(_this: Comparator[Object], t: Object, u: Object): Int32 =
                let s1 = unchecked_cast(t as String);
                let s2 = unchecked_cast(u as String);
                s1.compareTo(s2)
        };
        assertEq(expected = 0, c.compare("hello", "hello"))

    @Test
    def testComparatorCompareReverse01(): Unit \ {Assert, IO} =
        let c = new Comparator {
            def compare(_this: Comparator[Object], t: Object, u: Object): Int32 =
                let s1 = unchecked_cast(t as String);
                let s2 = unchecked_cast(u as String);
                s2.compareTo(s1)
        };
        assertTrue(c.compare("a", "b") > 0)

    @Test
    def testComparatorUsedInSort01(): Unit \ {Assert, IO} =
        let c = new Comparator {
            def compare(_this: Comparator[Object], t: Object, u: Object): Int32 =
                let s1 = unchecked_cast(t as String);
                let s2 = unchecked_cast(u as String);
                s2.compareTo(s1)
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
        let c = new Comparator {
            def compare(_this: Comparator[Object], t: Object, u: Object): Int32 =
                let i1 = unchecked_cast(t as JInteger);
                let i2 = unchecked_cast(u as JInteger);
                i1.compareTo(i2) + n - n
        };
        assertTrue(c.compare(JInteger.valueOf(1i32), JInteger.valueOf(2i32)) < 0)

    @Test
    def testSerializableStillWorks01(): Unit \ {Assert, IO} =
        let a = new Serializable { };
        assertTrue(a.toString().startsWith("Anon"))

    @Test
    def testComparatorWithTreeMapOrdering01(): Unit \ {Assert, IO} =
        let c = new Comparator {
            def compare(_this: Comparator[Object], t: Object, u: Object): Int32 =
                let s1 = unchecked_cast(t as String);
                let s2 = unchecked_cast(u as String);
                s1.compareTo(s2)
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

}
