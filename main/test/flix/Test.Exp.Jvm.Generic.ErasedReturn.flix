mod Test.Exp.Jvm.Generic.ErasedReturn {

    use Assert.{assertEq, assertTrue}

    import java.util.ArrayList
    import java.util.HashMap
    import java.util.Objects
    import java.util.Optional

    //
    // A generic Java method returns its erasure, so the value must be cast to the Flix type of the call
    // before it is used as a vector, tuple, enum, record, or function.
    //

    @Test
    def testVectorOfInt32(): Unit \ {Assert, IO} =
        let l: ArrayList[Vector[Int32]] = new ArrayList();
        discard l.add(Vector#{1, 2});
        let v = l.get(0);
        assertEq(expected = 2, Vector.length(v))

    @Test
    def testVectorOfString(): Unit \ {Assert, IO} =
        let l: ArrayList[Vector[String]] = new ArrayList();
        discard l.add(Vector#{"a", "b", "c"});
        let v = l.get(0);
        assertEq(expected = 3, Vector.length(v))

    @Test
    def testTuple(): Unit \ {Assert, IO} =
        let l: ArrayList[(Int32, Int32)] = new ArrayList();
        discard l.add((1, 2));
        let (a, b) = l.get(0);
        assertEq(expected = 3, a + b)

    @Test
    def testEnum(): Unit \ {Assert, IO} =
        let l: ArrayList[Option[Int32]] = new ArrayList();
        discard l.add(Some(42));
        let r = match l.get(0) {
            case Some(x) => x
            case None    => 0
        };
        assertEq(expected = 42, r)

    @Test
    def testRecord(): Unit \ {Assert, IO} =
        let l: ArrayList[{x = Int32, y = Int32}] = new ArrayList();
        discard l.add({x = 1, y = 2});
        let r = l.get(0);
        assertEq(expected = 3, r#x + r#y)

    @Test
    def testFunction(): Unit \ {Assert, IO} =
        let l: ArrayList[Int32 -> Int32] = new ArrayList();
        discard l.add(x -> x + 1);
        let f = l.get(0);
        assertEq(expected = 42, f(41))

    @Test
    def testHashMapValue(): Unit \ {Assert, IO} =
        let m: HashMap[String, (Int32, Int32)] = new HashMap();
        discard m.put("k", (1, 2));
        let (a, b) = m.get("k");
        assertEq(expected = 3, a + b)

    @Test
    def testIterator(): Unit \ {Assert, IO} =
        let l: ArrayList[Vector[Int32]] = new ArrayList();
        discard l.add(Vector#{1, 2, 3});
        let v = l.iterator().next();
        assertEq(expected = 3, Vector.length(v))

    @Test
    def testOptionalGet(): Unit \ {Assert, IO} =
        let o = Optional.of((1, 2));
        let (a, b) = o.get();
        assertEq(expected = 3, a + b)

    @Test
    def testStaticGenericMethod(): Unit \ {Assert, IO} =
        // Objects.requireNonNull(T) returns T
        let v = Objects.requireNonNull(Vector#{1, 2});
        assertEq(expected = 2, Vector.length(v))

    @Test
    def testNativeUnaffected(): Unit \ {Assert, IO} =
        let l: ArrayList[String] = new ArrayList();
        discard l.add("hello");
        assertTrue(l.get(0).startsWith("he"))

}
