mod Test.Exp.Jvm.Generic.InvokeConstructor {

    use Assert.{assertEq, assertTrue}

    import java.io.File
    import java.lang.{Integer => JInteger}
    import java.lang.Object
    import java.util.ArrayList
    import java.util.HashMap
    import java.util.HashSet
    import java.util.LinkedHashMap
    import java.util.LinkedList
    import java.util.TreeMap

    // CompileTests

    @CompileTest
    def testArrayListStringConstructor01(): ArrayList[String] \ IO =
        new ArrayList()

    @CompileTest
    def testArrayListIntegerConstructor01(): ArrayList[JInteger] \ IO =
        new ArrayList()

    @CompileTest
    def testArrayListObjectConstructor01(): ArrayList[Object] \ IO =
        new ArrayList()

    @CompileTest
    def testHashMapConstructor01(): HashMap[String, JInteger] \ IO =
        new HashMap()

    @CompileTest
    def testHashMapConstructor02(): HashMap[String, JInteger] \ IO =
        new HashMap(16)

    @CompileTest
    def testHashMapConstructor03(): HashMap[String, JInteger] \ IO =
        new HashMap(16, 0.75f32)

    @CompileTest
    def testLinkedListConstructor01(): LinkedList[String] \ IO =
        new LinkedList()

    @CompileTest
    def testHashSetConstructor01(): HashSet[String] \ IO =
        new HashSet()

    @CompileTest
    def testTreeMapConstructor01(): TreeMap[String, JInteger] \ IO =
        new TreeMap()

    @CompileTest
    def testLinkedHashMapConstructor01(): LinkedHashMap[String, JInteger] \ IO =
        new LinkedHashMap()

    @CompileTest
    def testNestedGenericConstructor01(): ArrayList[ArrayList[String]] \ IO =
        new ArrayList()

    @CompileTest
    def testArrayListNativeTypeArg01(): ArrayList[File] \ IO =
        new ArrayList()

    @CompileTest
    def testHashMapNativeValueArg01(): HashMap[String, File] \ IO =
        new HashMap()

    @CompileTest
    def testLinkedHashMapConstructor02(): LinkedHashMap[JInteger, String] \ IO =
        new LinkedHashMap(16, 0.75f32)

    // Tests

    @Test
    def testArrayListAddAfterConstruct01(): Unit \ {Assert, IO} =
        let l: ArrayList[String] = new ArrayList();
        l.add("hello");
        assertEq(expected = 1, l.size())

    @Test
    def testArrayListAddAfterConstruct02(): Unit \ {Assert, IO} =
        let l: ArrayList[JInteger] = new ArrayList();
        l.add(JInteger.valueOf(42i32));
        assertEq(expected = 1, l.size())

    @Test
    def testHashMapPutAfterConstruct01(): Unit \ {Assert, IO} =
        let m: HashMap[String, JInteger] = new HashMap();
        m.put("key", JInteger.valueOf(1i32));
        assertEq(expected = 1, m.size())

    @Test
    def testLinkedListAddAfterConstruct01(): Unit \ {Assert, IO} =
        let l: LinkedList[String] = new LinkedList();
        l.add("hello");
        assertEq(expected = 1, l.size())

    @Test
    def testHashSetAddAfterConstruct01(): Unit \ {Assert, IO} =
        let s: HashSet[String] = new HashSet();
        s.add("hello");
        assertEq(expected = 1, s.size())

    @Test
    def testTreeMapPutAfterConstruct01(): Unit \ {Assert, IO} =
        let m: TreeMap[String, String] = new TreeMap();
        m.put("key", "value");
        assertEq(expected = 1, m.size())

    @Test
    def testConstructorTypeInference01(): Unit \ {Assert, IO} =
        let l: ArrayList[String] = new ArrayList();
        l.add("hello");
        assertEq(expected = 1, l.size())

    @Test
    def testConstructorMultipleInstances01(): Unit \ {Assert, IO} =
        let l1: ArrayList[String] = new ArrayList();
        let l2: ArrayList[JInteger] = new ArrayList();
        l1.add("hello");
        l2.add(JInteger.valueOf(42i32));
        assertEq(expected = 1, l1.size());
        assertEq(expected = 1, l2.size())

    @Test
    def testArrayListCapacityConstructor01(): Unit \ {Assert, IO} =
        let l: ArrayList[String] = new ArrayList(10);
        l.add("hello");
        assertEq(expected = 1, l.size())

}
