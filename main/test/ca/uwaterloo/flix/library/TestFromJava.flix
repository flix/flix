mod TestFromJava {

    use Assert.assertTrue;
    import java.util.{Map => JMap};
    import java.util.{Set => JSet};
    import java.util.{List => JList};
    import java.lang.{String => JString};
    import java.math.BigInteger;
    import java.math.BigDecimal;

    /////////////////////////////////////////////////////////////////////////////
    // Int8                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def int8FromJava01(): Unit \ Assert =
        let i = Int8.valueOf(0i8);
        assertTrue(ToFlix.toFlix(i) == 0i8)

    @Test
    def int8FromJava02(): Unit \ Assert =
        let i = Int8.valueOf(100i8);
        assertTrue(ToFlix.toFlix(i) == 100i8)

    /////////////////////////////////////////////////////////////////////////////
    // Int16                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def int16FromJava01(): Unit \ Assert =
        let i = Int16.valueOf(0i16);
        assertTrue(ToFlix.toFlix(i) == 0i16)

    @Test
    def int16FromJava02(): Unit \ Assert =
        let i = Int16.valueOf(100i16);
        assertTrue(ToFlix.toFlix(i) == 100i16)

    /////////////////////////////////////////////////////////////////////////////
    // Int32                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def int32FromJava01(): Unit \ Assert =
        let i = Int32.valueOf(0);
        assertTrue(ToFlix.toFlix(i) == 0)

    @Test
    def int32FromJava02(): Unit \ Assert =
        let i = Int32.valueOf(100);
        assertTrue(ToFlix.toFlix(i) == 100)

    /////////////////////////////////////////////////////////////////////////////
    // Int64                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def int64FromJava01(): Unit \ Assert =
        let i = Int64.valueOf(0i64);
        assertTrue(ToFlix.toFlix(i) == 0i64)

    @Test
    def int64FromJava02(): Unit \ Assert =
        let i = Int64.valueOf(100i64);
        assertTrue(ToFlix.toFlix(i) == 100i64)

    /////////////////////////////////////////////////////////////////////////////
    // Float32                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def float32FromJava01(): Unit \ Assert =
        let d = Float32.valueOf(0.0f32);
        assertTrue(ToFlix.toFlix(d) == 0.0f32)

    @Test
    def float32FromJava02(): Unit \ Assert =
        let d = Float32.valueOf(100.0f32);
        assertTrue(ToFlix.toFlix(d) == 100.0f32)

    /////////////////////////////////////////////////////////////////////////////
    // Float64                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def float64FromJava01(): Unit \ Assert =
        let d = Float64.valueOf(0.0f64);
        assertTrue(ToFlix.toFlix(d) == 0.0f64)

    @Test
    def float64FromJava02(): Unit \ Assert =
        let d = Float64.valueOf(100.0f64);
        assertTrue(ToFlix.toFlix(d) == 100.0f64)

    /////////////////////////////////////////////////////////////////////////////
    // BigInt                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def bigIntFromJava01(): Unit \ Assert =
        let i = unsafe BigInteger.valueOf(0i64);
        assertTrue(ToFlix.toFlix(i) == 0ii)

    @Test
    def bigIntFromJava02(): Unit \ Assert =
        let i = unsafe BigInteger.valueOf(100i64);
        assertTrue(ToFlix.toFlix(i) == 100ii)

    /////////////////////////////////////////////////////////////////////////////
    // BigDecimal                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def bigDecimalFromJava01(): Unit \ Assert =
        let d = unsafe BigDecimal.valueOf(0i64);
        assertTrue(ToFlix.toFlix(d) == 0.0ff)

    @Test
    def bigDecimalFromJava02(): Unit \ Assert =
        let d = unsafe BigDecimal.valueOf(100i64);
        assertTrue(ToFlix.toFlix(d) == 100.0ff)

    /////////////////////////////////////////////////////////////////////////////
    // Char                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def charFromJava01(): Unit \ Assert =
        let c = Char.valueOf('0');
        assertTrue(ToFlix.toFlix(c) == '0')

    @Test
    def charFromJava02(): Unit \ Assert =
        let c = Char.valueOf('a');
        assertTrue(ToFlix.toFlix(c) == 'a')

    /////////////////////////////////////////////////////////////////////////////
    // String                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def stringFromJava01(): Unit \ Assert =
        let s = unsafe JString.valueOf(true);
        assertTrue(ToFlix.toFlix(s) == "true")

    @Test
    def stringFromJava02(): Unit \ Assert =
        let s = unsafe JString.valueOf(false);
        assertTrue(ToFlix.toFlix(s) == "false")

    /////////////////////////////////////////////////////////////////////////////
    // Chain                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def chainFromJava01(): Unit \ Assert =
        let l = unsafe JList.of();
        assertTrue((ToFlix.toFlix(l): Chain[String]) == (Chain.empty(): Chain[String]))

    @Test
    def chainFromJava02(): Unit \ Assert =
        let l = unsafe JList.of("one");
        assertTrue((ToFlix.toFlix(l): Chain[String]) == Chain.singleton("one"))

    @Test
    def chainFromJava03(): Unit \ Assert =
        let l = unsafe JList.of("one", "two");
        assertTrue((ToFlix.toFlix(l): Chain[String]) == List.toChain(List#{"one", "two"}))

    /////////////////////////////////////////////////////////////////////////////
    // List                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def listFromJava01(): Unit \ Assert =
        let l = unsafe JList.of();
        assertTrue((ToFlix.toFlix(l): List[String]) == (Nil: List[String]))

    @Test
    def listFromJava02(): Unit \ Assert =
        let l = unsafe JList.of("one");
        assertTrue((ToFlix.toFlix(l): List[String]) == List#{"one"})

    @Test
    def listFromJava03(): Unit \ Assert =
        let l = unsafe JList.of("one", "two");
        assertTrue((ToFlix.toFlix(l): List[String]) == List#{"one", "two"})

    /////////////////////////////////////////////////////////////////////////////
    // Map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapFromJava01(): Unit \ Assert =
        let m = unsafe JMap.of();
        assertTrue((ToFlix.toFlix(m): Map[String, String]) == Map#{})

    @Test
    def mapFromJava02(): Unit \ Assert =
        let m = unsafe JMap.of("a", "one");
        assertTrue((ToFlix.toFlix(m): Map[String, String]) == Map#{"a" => "one"})

    @Test
    def mapFromJava03(): Unit \ Assert =
        let m = unsafe JMap.of("a", "one", "b", "two");
        assertTrue((ToFlix.toFlix(m): Map[String, String]) == Map#{"a" => "one", "b" => "two"})

    /////////////////////////////////////////////////////////////////////////////
    // Set                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def setFromJava01(): Unit \ Assert =
        let s = unsafe JSet.of();
        assertTrue((ToFlix.toFlix(s): Set[String]) == Set#{})

    @Test
    def setFromJava02(): Unit \ Assert =
        let s = unsafe JSet.of("one");
        assertTrue((ToFlix.toFlix(s): Set[String]) == Set#{"one"})

    @Test
    def setFromJava03(): Unit \ Assert =
        let s = unsafe JSet.of("one", "two");
        assertTrue((ToFlix.toFlix(s): Set[String]) == Set#{"one", "two"})

    /////////////////////////////////////////////////////////////////////////////
    // Vector                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def vectorFromJava01(): Unit \ Assert =
        let v = unsafe JList.of();
        assertTrue((ToFlix.toFlix(v): Vector[String]) == Vector#{})

    @Test
    def vectorFromJava02(): Unit \ Assert =
        let v = unsafe JList.of("one");
        assertTrue((ToFlix.toFlix(v): Vector[String]) == Vector#{"one"})

    @Test
    def vectorFromJava03(): Unit \ Assert =
        let v = unsafe JList.of("one", "two");
        assertTrue((ToFlix.toFlix(v): Vector[String]) == Vector#{"one", "two"})

}
