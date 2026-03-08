mod Test.Exp.Jvm.Generic.InvokeConstructor {

    use Assert.{assertEq, assertTrue}

    import java.lang.{Integer => JInteger}
    import java.lang.Object
    import java.io.File
    import java.util.ArrayList
    import java.util.HashMap
    import java.util.LinkedList
    import java.util.HashSet
    import java.util.TreeMap
    import java.util.LinkedHashMap

    // Compile tests: verify generic constructors type-check

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
    def testArrayListRawConstructor01(): ArrayList \ IO =
        new ArrayList()

    @CompileTest
    def testHashMapRawConstructor01(): HashMap \ IO =
        new HashMap()

    // Runtime tests: construct and use generic collections

    @Test
    def testArrayListAddAfterConstruct01(): Unit \ Assert + IO =
        let list: ArrayList[String] = new ArrayList();
        list.add("hello");
        assertEq(expected = 1, list.size())

    @Test
    def testArrayListAddAfterConstruct02(): Unit \ Assert + IO =
        let list: ArrayList[JInteger] = new ArrayList();
        list.add(JInteger.valueOf(42i32));
        assertEq(expected = 1, list.size())

    @Test
    def testHashMapPutAfterConstruct01(): Unit \ Assert + IO =
        let map: HashMap[String, JInteger] = new HashMap();
        map.put("key", JInteger.valueOf(1i32));
        assertEq(expected = 1, map.size())

    @Test
    def testLinkedListAddAfterConstruct01(): Unit \ Assert + IO =
        let list: LinkedList[String] = new LinkedList();
        list.add("hello");
        assertEq(expected = 1, list.size())

    @Test
    def testHashSetAddAfterConstruct01(): Unit \ Assert + IO =
        let set: HashSet[String] = new HashSet();
        set.add("hello");
        assertEq(expected = 1, set.size())

    @Test
    def testTreeMapPutAfterConstruct01(): Unit \ Assert + IO =
        let map: TreeMap[String, String] = new TreeMap();
        map.put("key", "value");
        assertEq(expected = 1, map.size())

    @CompileTest
    def testNestedGenericConstructor01(): ArrayList[ArrayList[String]] \ IO =
        new ArrayList()

    @Test
    def testConstructorTypeInference01(): Unit \ Assert + IO =
        let x: ArrayList[String] = new ArrayList();
        x.add("hello");
        assertEq(expected = 1, x.size())

    @Test
    def testConstructorMultipleInstances01(): Unit \ Assert + IO =
        let strings: ArrayList[String] = new ArrayList();
        let ints: ArrayList[JInteger] = new ArrayList();
        strings.add("hello");
        ints.add(JInteger.valueOf(42i32));
        assertEq(expected = 1, strings.size());
        assertEq(expected = 1, ints.size())

    @CompileTest
    def testArrayListNativeTypeArg01(): ArrayList[File] \ IO =
        new ArrayList()

    @CompileTest
    def testHashMapNativeValueArg01(): HashMap[String, File] \ IO =
        new HashMap()

    @Test
    def testArrayListCapacityConstructor01(): Unit \ Assert + IO =
        let list: ArrayList[String] = new ArrayList(10);
        list.add("hello");
        assertEq(expected = 1, list.size())

    @CompileTest
    def testLinkedHashMapConstructor02(): LinkedHashMap[JInteger, String] \ IO =
        new LinkedHashMap(16, 0.75f32)

}
