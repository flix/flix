mod TestFromJava {

    use Assert.{assertTrue, assertEq};
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
        assertEq(expected = 0i8, ToFlix.toFlix(i))

    @Test
    def int8FromJava02(): Unit \ Assert =
        let i = Int8.valueOf(100i8);
        assertEq(expected = 100i8, ToFlix.toFlix(i))

    /////////////////////////////////////////////////////////////////////////////
    // Int16                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def int16FromJava01(): Unit \ Assert =
        let i = Int16.valueOf(0i16);
        assertEq(expected = 0i16, ToFlix.toFlix(i))

    @Test
    def int16FromJava02(): Unit \ Assert =
        let i = Int16.valueOf(100i16);
        assertEq(expected = 100i16, ToFlix.toFlix(i))

    /////////////////////////////////////////////////////////////////////////////
    // Int32                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def int32FromJava01(): Unit \ Assert =
        let i = Int32.valueOf(0);
        assertEq(expected = 0, ToFlix.toFlix(i))

    @Test
    def int32FromJava02(): Unit \ Assert =
        let i = Int32.valueOf(100);
        assertEq(expected = 100, ToFlix.toFlix(i))

    /////////////////////////////////////////////////////////////////////////////
    // Int64                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def int64FromJava01(): Unit \ Assert =
        let i = Int64.valueOf(0i64);
        assertEq(expected = 0i64, ToFlix.toFlix(i))

    @Test
    def int64FromJava02(): Unit \ Assert =
        let i = Int64.valueOf(100i64);
        assertEq(expected = 100i64, ToFlix.toFlix(i))

    /////////////////////////////////////////////////////////////////////////////
    // Float32                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def float32FromJava01(): Unit \ Assert =
        let d = Float32.valueOf(0.0f32);
        assertEq(expected = 0.0f32, ToFlix.toFlix(d))

    @Test
    def float32FromJava02(): Unit \ Assert =
        let d = Float32.valueOf(100.0f32);
        assertEq(expected = 100.0f32, ToFlix.toFlix(d))

    /////////////////////////////////////////////////////////////////////////////
    // Float64                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def float64FromJava01(): Unit \ Assert =
        let d = Float64.valueOf(0.0f64);
        assertEq(expected = 0.0f64, ToFlix.toFlix(d))

    @Test
    def float64FromJava02(): Unit \ Assert =
        let d = Float64.valueOf(100.0f64);
        assertEq(expected = 100.0f64, ToFlix.toFlix(d))

    /////////////////////////////////////////////////////////////////////////////
    // BigInt                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def bigIntFromJava01(): Unit \ Assert =
        let i = BigInteger.valueOf(0i64);
        assertEq(expected = 0ii, ToFlix.toFlix(i))

    @Test
    def bigIntFromJava02(): Unit \ Assert =
        let i = BigInteger.valueOf(100i64);
        assertEq(expected = 100ii, ToFlix.toFlix(i))

    /////////////////////////////////////////////////////////////////////////////
    // BigDecimal                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def bigDecimalFromJava01(): Unit \ Assert =
        let d = BigDecimal.valueOf(0i64);
        assertEq(expected = 0.0ff, ToFlix.toFlix(d))

    @Test
    def bigDecimalFromJava02(): Unit \ Assert =
        let d = BigDecimal.valueOf(100i64);
        assertEq(expected = 100.0ff, ToFlix.toFlix(d))

    /////////////////////////////////////////////////////////////////////////////
    // Char                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def charFromJava01(): Unit \ Assert =
        let c = Char.valueOf('0');
        assertEq(expected = '0', ToFlix.toFlix(c))

    @Test
    def charFromJava02(): Unit \ Assert =
        let c = Char.valueOf('a');
        assertEq(expected = 'a', ToFlix.toFlix(c))

    /////////////////////////////////////////////////////////////////////////////
    // String                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def stringFromJava01(): Unit \ Assert =
        let s = unsafe JString.valueOf(true);
        assertEq(expected = "true", ToFlix.toFlix(s))

    @Test
    def stringFromJava02(): Unit \ Assert =
        let s = unsafe JString.valueOf(false);
        assertEq(expected = "false", ToFlix.toFlix(s))

    /////////////////////////////////////////////////////////////////////////////
    // Chain                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def chainFromJava01(): Unit \ Assert =
        let l = unsafe JList.of();
        assertEq(expected = (Chain.empty(): Chain[String]), (ToFlix.toFlix(l): Chain[String]))

    @Test
    def chainFromJava02(): Unit \ Assert =
        let l = unsafe JList.of("one");
        assertEq(expected = Chain.singleton("one"), (ToFlix.toFlix(l): Chain[String]))

    @Test
    def chainFromJava03(): Unit \ Assert =
        let l = unsafe JList.of("one", "two");
        assertEq(expected = List.toChain(List#{"one", "two"}), (ToFlix.toFlix(l): Chain[String]))

    /////////////////////////////////////////////////////////////////////////////
    // List                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def listFromJava01(): Unit \ Assert =
        let l = unsafe JList.of();
        assertEq(expected = (Nil: List[String]), (ToFlix.toFlix(l): List[String]))

    @Test
    def listFromJava02(): Unit \ Assert =
        let l = unsafe JList.of("one");
        assertEq(expected = List#{"one"}, (ToFlix.toFlix(l): List[String]))

    @Test
    def listFromJava03(): Unit \ Assert =
        let l = unsafe JList.of("one", "two");
        assertEq(expected = List#{"one", "two"}, (ToFlix.toFlix(l): List[String]))

    /////////////////////////////////////////////////////////////////////////////
    // Map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapFromJava01(): Unit \ Assert =
        let m = unsafe JMap.of();
        assertEq(expected = Map#{}, (ToFlix.toFlix(m): Map[String, String]))

    @Test
    def mapFromJava02(): Unit \ Assert =
        let m = unsafe JMap.of("a", "one");
        assertEq(expected = Map#{"a" => "one"}, (ToFlix.toFlix(m): Map[String, String]))

    @Test
    def mapFromJava03(): Unit \ Assert =
        let m = unsafe JMap.of("a", "one", "b", "two");
        assertEq(expected = Map#{"a" => "one", "b" => "two"}, (ToFlix.toFlix(m): Map[String, String]))

    /////////////////////////////////////////////////////////////////////////////
    // Set                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def setFromJava01(): Unit \ Assert =
        let s = unsafe JSet.of();
        assertEq(expected = Set#{}, (ToFlix.toFlix(s): Set[String]))

    @Test
    def setFromJava02(): Unit \ Assert =
        let s = unsafe JSet.of("one");
        assertEq(expected = Set#{"one"}, (ToFlix.toFlix(s): Set[String]))

    @Test
    def setFromJava03(): Unit \ Assert =
        let s = unsafe JSet.of("one", "two");
        assertEq(expected = Set#{"one", "two"}, (ToFlix.toFlix(s): Set[String]))

    /////////////////////////////////////////////////////////////////////////////
    // Vector                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def vectorFromJava01(): Unit \ Assert =
        let v = unsafe JList.of();
        assertEq(expected = Vector#{}, (ToFlix.toFlix(v): Vector[String]))

    @Test
    def vectorFromJava02(): Unit \ Assert =
        let v = unsafe JList.of("one");
        assertEq(expected = Vector#{"one"}, (ToFlix.toFlix(v): Vector[String]))

    @Test
    def vectorFromJava03(): Unit \ Assert =
        let v = unsafe JList.of("one", "two");
        assertEq(expected = Vector#{"one", "two"}, (ToFlix.toFlix(v): Vector[String]))

}
