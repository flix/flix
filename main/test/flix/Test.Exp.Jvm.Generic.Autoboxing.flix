mod Test.Exp.Jvm.Generic.Autoboxing {

    use Assert.{assertEq, assertTrue}

    import java.util.ArrayList
    import java.util.HashMap
    import java.util.HashSet
    import java.util.LinkedList

    //
    // ArrayList tests — add and get for each primitive type
    //

    @Test
    def testArrayListInt32(): Unit \ {Assert, IO} =
        let l: ArrayList[Int32] = new ArrayList();
        l.add(42);
        let r: Int32 = l.get(0);
        assertEq(expected = 42, r)

    @Test
    def testArrayListInt64(): Unit \ {Assert, IO} =
        let l: ArrayList[Int64] = new ArrayList();
        l.add(123i64);
        let r: Int64 = l.get(0);
        assertEq(expected = 123i64, r)

    @Test
    def testArrayListFloat32(): Unit \ {Assert, IO} =
        let l: ArrayList[Float32] = new ArrayList();
        l.add(3.14f32);
        let r: Float32 = l.get(0);
        assertEq(expected = 3.14f32, r)

    @Test
    def testArrayListFloat64(): Unit \ {Assert, IO} =
        let l: ArrayList[Float64] = new ArrayList();
        l.add(2.718f64);
        let r: Float64 = l.get(0);
        assertEq(expected = 2.718f64, r)

    @Test
    def testArrayListBool(): Unit \ {Assert, IO} =
        let l: ArrayList[Bool] = new ArrayList();
        l.add(true);
        let r: Bool = l.get(0);
        assertTrue(r)

    @Test
    def testArrayListChar(): Unit \ {Assert, IO} =
        let l: ArrayList[Char] = new ArrayList();
        l.add('x');
        let r: Char = l.get(0);
        assertEq(expected = 'x', r)

    @Test
    def testArrayListInt8(): Unit \ {Assert, IO} =
        let l: ArrayList[Int8] = new ArrayList();
        l.add(7i8);
        let r: Int8 = l.get(0);
        assertEq(expected = 7i8, r)

    @Test
    def testArrayListInt16(): Unit \ {Assert, IO} =
        let l: ArrayList[Int16] = new ArrayList();
        l.add(42i16);
        let r: Int16 = l.get(0);
        assertEq(expected = 42i16, r)

    //
    // LinkedList tests — add, getFirst, getLast
    //

    @Test
    def testLinkedListInt32Add(): Unit \ {Assert, IO} =
        let l: LinkedList[Int32] = new LinkedList();
        l.add(10);
        assertEq(expected = 1, l.size())

    @Test
    def testLinkedListInt32GetFirst(): Unit \ {Assert, IO} =
        let l: LinkedList[Int32] = new LinkedList();
        l.addFirst(1);
        l.addFirst(2);
        let r: Int32 = l.getFirst();
        assertEq(expected = 2, r)

    @Test
    def testLinkedListBoolAdd(): Unit \ {Assert, IO} =
        let l: LinkedList[Bool] = new LinkedList();
        l.add(false);
        let r: Bool = l.get(0);
        assertEq(expected = false, r)

    @Test
    def testLinkedListFloat64Add(): Unit \ {Assert, IO} =
        let l: LinkedList[Float64] = new LinkedList();
        l.add(9.99f64);
        let r: Float64 = l.getFirst();
        assertEq(expected = 9.99f64, r)

    @Test
    def testLinkedListCharAdd(): Unit \ {Assert, IO} =
        let l: LinkedList[Char] = new LinkedList();
        l.add('a');
        l.add('b');
        let r: Char = l.getLast();
        assertEq(expected = 'b', r)

    @Test
    def testLinkedListInt64Add(): Unit \ {Assert, IO} =
        let l: LinkedList[Int64] = new LinkedList();
        l.add(100i64);
        let r: Int64 = l.get(0);
        assertEq(expected = 100i64, r)

    //
    // HashMap tests — put and get for each primitive type
    //

    @Test
    def testHashMapStringInt32Put(): Unit \ {Assert, IO} =
        let m: HashMap[String, Int32] = new HashMap();
        m.put("k", 42);
        assertEq(expected = 1, m.size())

    @Test
    def testHashMapStringInt32Get(): Unit \ {Assert, IO} =
        let m: HashMap[String, Int32] = new HashMap();
        m.put("k", 42);
        let r: Int32 = m.get("k");
        assertEq(expected = 42, r)

    @Test
    def testHashMapStringBoolGet(): Unit \ {Assert, IO} =
        let m: HashMap[String, Bool] = new HashMap();
        m.put("k", true);
        let r: Bool = m.get("k");
        assertTrue(r)

    @Test
    def testHashMapStringFloat64Get(): Unit \ {Assert, IO} =
        let m: HashMap[String, Float64] = new HashMap();
        m.put("k", 3.14f64);
        let r: Float64 = m.get("k");
        assertEq(expected = 3.14f64, r)

    @Test
    def testHashMapStringCharGet(): Unit \ {Assert, IO} =
        let m: HashMap[String, Char] = new HashMap();
        m.put("k", 'z');
        let r: Char = m.get("k");
        assertEq(expected = 'z', r)

    @Test
    def testHashMapStringInt64Get(): Unit \ {Assert, IO} =
        let m: HashMap[String, Int64] = new HashMap();
        m.put("k", 99i64);
        let r: Int64 = m.get("k");
        assertEq(expected = 99i64, r)

    @Test
    def testHashMapStringInt8Get(): Unit \ {Assert, IO} =
        let m: HashMap[String, Int8] = new HashMap();
        m.put("k", 5i8);
        let r: Int8 = m.get("k");
        assertEq(expected = 5i8, r)

    @Test
    def testHashMapStringInt16Get(): Unit \ {Assert, IO} =
        let m: HashMap[String, Int16] = new HashMap();
        m.put("k", 32i16);
        let r: Int16 = m.get("k");
        assertEq(expected = 32i16, r)

    @Test
    def testHashMapStringFloat32Get(): Unit \ {Assert, IO} =
        let m: HashMap[String, Float32] = new HashMap();
        m.put("k", 1.5f32);
        let r: Float32 = m.get("k");
        assertEq(expected = 1.5f32, r)

    @Test
    def testHashMapMultiplePut(): Unit \ {Assert, IO} =
        let m: HashMap[String, Int32] = new HashMap();
        m.put("a", 1);
        m.put("b", 2);
        m.put("c", 3);
        assertEq(expected = 3, m.size());
        let r1: Int32 = m.get("a");
        let r2: Int32 = m.get("b");
        let r3: Int32 = m.get("c");
        assertEq(expected = 1, r1);
        assertEq(expected = 2, r2);
        assertEq(expected = 3, r3)

    //
    // HashSet tests — add and size for various primitives
    //

    @Test
    def testHashSetInt32Add(): Unit \ {Assert, IO} =
        let s: HashSet[Int32] = new HashSet();
        s.add(42);
        assertEq(expected = 1, s.size())

    @Test
    def testHashSetBoolAdd(): Unit \ {Assert, IO} =
        let s: HashSet[Bool] = new HashSet();
        s.add(true);
        s.add(false);
        assertEq(expected = 2, s.size())

    @Test
    def testHashSetCharAdd(): Unit \ {Assert, IO} =
        let s: HashSet[Char] = new HashSet();
        s.add('x');
        s.add('y');
        assertEq(expected = 2, s.size())

    @Test
    def testHashSetFloat64Add(): Unit \ {Assert, IO} =
        let s: HashSet[Float64] = new HashSet();
        s.add(1.0f64);
        assertEq(expected = 1, s.size())

    @Test
    def testHashSetInt64Add(): Unit \ {Assert, IO} =
        let s: HashSet[Int64] = new HashSet();
        s.add(1i64);
        s.add(2i64);
        assertEq(expected = 2, s.size())

    @Test
    def testHashSetInt32Duplicate(): Unit \ {Assert, IO} =
        let s: HashSet[Int32] = new HashSet();
        s.add(1);
        s.add(1);
        assertEq(expected = 1, s.size())

    //
    // Mixed / advanced tests
    //

    @Test
    def testArrayListInt32Set(): Unit \ {Assert, IO} =
        let l: ArrayList[Int32] = new ArrayList();
        l.add(1);
        l.set(0, 99);
        let r: Int32 = l.get(0);
        assertEq(expected = 99, r)

    @Test
    def testHashMapInt32Overwrite(): Unit \ {Assert, IO} =
        let m: HashMap[String, Int32] = new HashMap();
        m.put("k", 1);
        m.put("k", 2);
        let r: Int32 = m.get("k");
        assertEq(expected = 2, r)

    @Test
    def testArrayListBoolMultiple(): Unit \ {Assert, IO} =
        let l: ArrayList[Bool] = new ArrayList();
        l.add(true);
        l.add(false);
        l.add(true);
        assertEq(expected = 3, l.size());
        let r: Bool = l.get(1);
        assertEq(expected = false, r)

    @Test
    def testHashMapClear(): Unit \ {Assert, IO} =
        let m: HashMap[String, Int32] = new HashMap();
        m.put("k", 42);
        m.clear();
        assertTrue(m.isEmpty())

    @Test
    def testArrayListInt32Remove(): Unit \ {Assert, IO} =
        let l: ArrayList[Int32] = new ArrayList();
        l.add(10);
        l.add(20);
        let r: Int32 = l.remove(0);
        assertEq(expected = 10, r);
        assertEq(expected = 1, l.size())

}
