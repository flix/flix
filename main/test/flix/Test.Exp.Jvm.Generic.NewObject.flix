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

    // Comparator tests

    @Test
    def testComparatorString01(): Unit \ Assert + IO =
        let comp = new Comparator {
            def compare(_this: Comparator, t: Object, u: Object): Int32 =
                let s1 = unchecked_cast(t as String);
                let s2 = unchecked_cast(u as String);
                s1.compareTo(s2)
        };
        assertTrue(comp.compare("a", "b") < 0)

    @Test
    def testComparatorString02(): Unit \ Assert + IO =
        let comp = new Comparator {
            def compare(_this: Comparator, t: Object, u: Object): Int32 =
                let s1 = unchecked_cast(t as String);
                let s2 = unchecked_cast(u as String);
                s2.compareTo(s1)
        };
        assertTrue(comp.compare("a", "b") > 0)

    @Test
    def testComparatorInteger01(): Unit \ Assert + IO =
        let comp = new Comparator {
            def compare(_this: Comparator, t: Object, u: Object): Int32 =
                let i1 = unchecked_cast(t as JInteger);
                let i2 = unchecked_cast(u as JInteger);
                i1.compareTo(i2)
        };
        assertTrue(comp.compare(JInteger.valueOf(1i32), JInteger.valueOf(2i32)) < 0)

    @Test
    def testComparatorWithTreeMap01(): Unit \ Assert + IO =
        let comp = new Comparator {
            def compare(_this: Comparator, t: Object, u: Object): Int32 =
                let s1 = unchecked_cast(t as String);
                let s2 = unchecked_cast(u as String);
                s2.compareTo(s1)
        };
        let map: TreeMap[String, String] = new TreeMap(comp);
        map.put("a", "1");
        map.put("b", "2");
        map.put("c", "3");
        let first = unchecked_cast(map.firstKey() as String);
        assertEq(expected = "c", first)

    @Test
    def testComparatorRaw01(): Unit \ Assert + IO =
        let comp = new Comparator {
            def compare(_this: Comparator, _t: Object, _u: Object): Int32 = 0
        };
        assertEq(expected = 0, comp.compare("foo", "bar"))

    @Test
    def testRunnableWithGenericCapture01(): Unit \ Assert + IO =
        let list: ArrayList[String] = new ArrayList();
        let r = new Runnable {
            def $run(_this: Runnable): Unit \ IO =
                list.add("captured");
                ()
        };
        r.$run();
        assertEq(expected = 1, list.size())

    @CompileTest
    def testComparatorCompile01(): Comparator[String] \ IO =
        new Comparator {
            def compare(_this: Comparator, t: Object, u: Object): Int32 =
                let s1 = unchecked_cast(t as String);
                let s2 = unchecked_cast(u as String);
                s1.compareTo(s2)
        }

    @CompileTest
    def testComparatorCompile02(): Comparator[JInteger] \ IO =
        new Comparator {
            def compare(_this: Comparator, t: Object, u: Object): Int32 =
                let i1 = unchecked_cast(t as JInteger);
                let i2 = unchecked_cast(u as JInteger);
                i1.compareTo(i2)
        }

    @Test
    def testComparatorCompare01(): Unit \ Assert + IO =
        let comp = new Comparator {
            def compare(_this: Comparator, t: Object, u: Object): Int32 =
                let s1 = unchecked_cast(t as String);
                let s2 = unchecked_cast(u as String);
                s1.compareTo(s2)
        };
        assertEq(expected = 0, comp.compare("hello", "hello"))

    @Test
    def testComparatorCompareReverse01(): Unit \ Assert + IO =
        let comp = new Comparator {
            def compare(_this: Comparator, t: Object, u: Object): Int32 =
                let s1 = unchecked_cast(t as String);
                let s2 = unchecked_cast(u as String);
                s2.compareTo(s1)
        };
        assertTrue(comp.compare("a", "b") > 0)

    @Test
    def testComparatorUsedInSort01(): Unit \ Assert + IO =
        let comp = new Comparator {
            def compare(_this: Comparator, t: Object, u: Object): Int32 =
                let s1 = unchecked_cast(t as String);
                let s2 = unchecked_cast(u as String);
                s2.compareTo(s1)
        };
        let list: ArrayList[String] = new ArrayList();
        list.add("a");
        list.add("c");
        list.add("b");
        JCollections.sort(list, comp);
        let first = unchecked_cast(list.get(0) as String);
        assertEq(expected = "c", first)

    @CompileTest
    def testCallableString01(): Callable[String] \ IO =
        new Callable {
            def call(_this: Callable): Object =
                checked_cast("hello")
        }

    @CompileTest
    def testCallableInteger01(): Callable[JInteger] \ IO =
        new Callable {
            def call(_this: Callable): Object =
                let v = JInteger.valueOf(42i32);
                checked_cast(v)
        }

    @CompileTest
    def testIterableCompile01(): JIterable[String] \ IO =
        new JIterable {
            def iterator(_this: JIterable): JIterator \ IO =
                let list: ArrayList[String] = new ArrayList();
                list.iterator()
        }

    @CompileTest
    def testComparableCompile01(): JComparable[String] \ IO =
        new JComparable {
            def compareTo(_this: JComparable, o: Object): Int32 =
                let str = unchecked_cast(o as String);
                "hello".compareTo(str)
        }

    @Test
    def testComparatorCapture01(): Unit \ Assert + IO =
        let offset = 1;
        let comp = new Comparator {
            def compare(_this: Comparator, t: Object, u: Object): Int32 =
                let i1 = unchecked_cast(t as JInteger);
                let i2 = unchecked_cast(u as JInteger);
                i1.compareTo(i2) + offset - offset
        };
        assertTrue(comp.compare(JInteger.valueOf(1i32), JInteger.valueOf(2i32)) < 0)

    @Test
    def testSerializableStillWorks01(): Unit \ Assert + IO =
        let anon = new Serializable { };
        assertTrue(anon.toString().startsWith("Anon"))

    @Test
    def testComparatorWithTreeMapOrdering01(): Unit \ Assert + IO =
        let comp = new Comparator {
            def compare(_this: Comparator, t: Object, u: Object): Int32 =
                let s1 = unchecked_cast(t as String);
                let s2 = unchecked_cast(u as String);
                s1.compareTo(s2)
        };
        let map: TreeMap[String, String] = new TreeMap(comp);
        map.put("c", "3");
        map.put("a", "1");
        map.put("b", "2");
        let first = unchecked_cast(map.firstKey() as String);
        assertEq(expected = "a", first)

    @Test
    def testRunnableCapturingGeneric01(): Unit \ Assert + IO =
        let list: ArrayList[String] = new ArrayList();
        list.add("before");
        let r = new Runnable {
            def $run(_this: Runnable): Unit \ IO =
                list.add("after");
                ()
        };
        r.$run();
        assertEq(expected = 2, list.size())

}
