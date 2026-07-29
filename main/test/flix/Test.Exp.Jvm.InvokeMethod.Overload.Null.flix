mod Test.Exp.Jvm.InvokeMethod.Overload.Null {

    use Assert.{assertEq, assertFalse, assertTrue}

    import java.lang.Object
    import java.lang.StringBuilder
    import java.lang.{String => JString}
    import java.util.ArrayList
    import java.util.Collections
    import java.util.List
    import java.util.Objects
    import java.util.Optional

    @Test
    def testInvokeMethod_01(): Unit \ Assert =
        let obj = "hello world";
        assertFalse(obj.equals(null))

    @Test
    def testInvokeMethod_02(): Unit \ Assert =
        let obj = "null";
        assertFalse(obj.equals(null))

    @Test
    def testInvokeMethod_03(): Unit \ Assert =
        let obj = "txt";
        let val: String = (checked_cast(null): String);
        assertFalse(obj.equals(val))

    @Test
    def testInvokeMethod_04(): Unit \ Assert + IO =
        let sb = new StringBuilder("hello ");
        sb.append((checked_cast(null): String));
        assertEq(expected = "hello null", sb.toString())

    @Test
    def testInvokeMethod_05(): Unit \ Assert + IO =
        assertEq(expected = "null", JString.valueOf((checked_cast(null): Object)))

    @Test
    def testInvokeMethod_06(): Unit \ Assert =
        assertTrue(Objects.isNull((checked_cast(null): Object)))

    @Test
    def testInvokeMethod_07(): Unit \ Assert + IO =
        assertFalse(Objects.nonNull((checked_cast(null): Object)))

    @Test
    def testInvokeMethod_08(): Unit \ Assert + IO =
        assertEq(expected = "default", Objects.toString((checked_cast(null): Object), "default"))

    @Test
    def testInvokeMethod_09(): Unit \ Assert + IO =
        let opt = Optional.ofNullable((checked_cast(null): Object));
        assertFalse(opt.isPresent())

    // Bare `null` (no cast) passed to a method with a specific reference parameter.
    // `equalsIgnoreCase(String)` is the unique overload and is null-safe at runtime.
    @Test
    def testInvokeMethod_10(): Unit \ Assert + IO =
        let obj = "hello";
        assertFalse(obj.equalsIgnoreCase(null))

    // Bare `null` passed to a generic instance method: `List<E>.add(E)` with `E = String`.
    @Test
    def testInvokeMethod_11(): Unit \ Assert + IO =
        let list: ArrayList[String] = new ArrayList();
        discard list.add(null);
        discard list.add("x");
        assertEq(expected = 2, list.size())

    // Bare `null` passed to a generic static method: `Collections.singletonList(T)`,
    // with the return-type annotation fixing `T = String`.
    @Test
    def testInvokeMethod_12(): Unit \ Assert + IO =
        let single: List[String] = Collections.singletonList(null);
        assertEq(expected = 1, single.size())

}
