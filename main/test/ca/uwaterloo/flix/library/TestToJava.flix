mod TestToJava {

    use Assert.{assertEq, assertTrue};

    import java.util.{List => JList}
    import java.util.{Map => JMap}
    import java.util.{Set => JSet}
    import java.lang.Object

    /////////////////////////////////////////////////////////////////////////////
    // toJavaObject                                                            //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def toJavaObject01(): Unit \ Assert =
        let x = ToJava.toJavaObject(0);
        assertTrue(match Reflect.reflectValue(x) { case Reflect.JvmValue.JvmObject(_) => true case _ => false })

    @Test
    def toJavaObject02(): Unit \ Assert =
        let x = ToJava.toJavaObject("hello world!");
        assertTrue(match Reflect.reflectValue(x) { case Reflect.JvmValue.JvmObject(_) => true case _ => false })


    /////////////////////////////////////////////////////////////////////////////
    // Int8                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def int8ToJava01(): Unit \ Assert =
        let i = ToJava.toJava(0i8);
        assertEq(expected = 0i8, Int8.byteValue(i))

    @Test
    def int8ToJava02(): Unit \ Assert =
        let i = ToJava.toJava(100i8);
        assertEq(expected = 100i8, Int8.byteValue(i))


    /////////////////////////////////////////////////////////////////////////////
    // Int16                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def int16ToJava01(): Unit \ Assert =
        let i = ToJava.toJava(0i16);
        assertEq(expected = 0i16, Int16.shortValue(i))

    @Test
    def int16ToJava02(): Unit \ Assert =
        let i = ToJava.toJava(100i16);
        assertEq(expected = 100i16, Int16.shortValue(i))


    /////////////////////////////////////////////////////////////////////////////
    // Int32                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def int32ToJava01(): Unit \ Assert =
        let i = ToJava.toJava(0);
        assertEq(expected = 0, Int32.intValue(i))

    @Test
    def int32ToJava02(): Unit \ Assert =
        let i = ToJava.toJava(100);
        assertEq(expected = 100, Int32.intValue(i))


    /////////////////////////////////////////////////////////////////////////////
    // Int64                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def int64ToJava01(): Unit \ Assert =
        let i = ToJava.toJava(0i64);
        assertEq(expected = 0i64, Int64.longValue(i))

    @Test
    def int64ToJava02(): Unit \ Assert =
        let i = ToJava.toJava(100i64);
        assertEq(expected = 100i64, Int64.longValue(i))


    /////////////////////////////////////////////////////////////////////////////
    // Float32                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def float32ToJava01(): Unit \ Assert =
        let d = ToJava.toJava(0.0f32);
        assertEq(expected = 0.0f32, Float32.floatValue(d))

    @Test
    def float32ToJava02(): Unit \ Assert =
        let d = ToJava.toJava(100.0f32);
        assertEq(expected = 100.0f32, Float32.floatValue(d))


    /////////////////////////////////////////////////////////////////////////////
    // Float64                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def float64ToJava01(): Unit \ Assert =
        let d = ToJava.toJava(0.0f64);
        assertEq(expected = 0.0f64, Float64.doubleValue(d))

    @Test
    def float64ToJava02(): Unit \ Assert =
        let d = ToJava.toJava(100.0f64);
        assertEq(expected = 100.0f64, Float64.doubleValue(d))


    /////////////////////////////////////////////////////////////////////////////
    // BigInt                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def bigIntToJava01(): Unit \ Assert =
        let i = ToJava.toJava(0ii);
        assertEq(expected = 0ii, i)

    @Test
    def bigIntToJava02(): Unit \ Assert =
        let i = ToJava.toJava(100ii);
        assertEq(expected = 100ii, i)


    /////////////////////////////////////////////////////////////////////////////
    // BigDecimal                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def bigDecimalToJava01(): Unit \ Assert =
        let d = ToJava.toJava(0.0ff);
        assertEq(expected = 0.0ff, d)

    @Test
    def bigDecimalToJava02(): Unit \ Assert =
        let d = ToJava.toJava(100.0ff);
        assertEq(expected = 100.0ff, d)


    /////////////////////////////////////////////////////////////////////////////
    // Char                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def charToJava01(): Unit \ Assert =
        let c = ToJava.toJava('0');
        assertEq(expected = '0', Char.charValue(c))

    @Test
    def charToJava02(): Unit \ Assert =
        let c = ToJava.toJava('a');
        assertEq(expected = 'a', Char.charValue(c))


    /////////////////////////////////////////////////////////////////////////////
    // String                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def stringToJava01(): Unit \ Assert =
        let s = ToJava.toJava("");
        assertEq(expected = "", s)

    @Test
    def stringToJava02(): Unit \ Assert =
        let s = ToJava.toJava("hello world!");
        assertEq(expected = "hello world!", s)


    /////////////////////////////////////////////////////////////////////////////
    // Chain                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def chainToJava01(): Unit \ {Assert, IO} =
        let l: JList = ToJava.toJava((Chain.empty() : Chain[String]));
        assertTrue(JList.of().equals(l))

    @Test
    def chainToJava02(): Unit \ {Assert, IO} =
        let l: JList = ToJava.toJava(Chain.singleton("one"));
        assertTrue(JList.of("one").equals(l))

    @Test
    def chainToJava03(): Unit \ {Assert, IO} =
        let l: JList = ToJava.toJava(Chain.cons("one", Chain.singleton("two")));
        assertTrue(JList.of("one", "two").equals(l))


    /////////////////////////////////////////////////////////////////////////////
    // List                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def listToJava01(): Unit \ {Assert, IO} =
        let l: JList = ToJava.toJava((Nil : List[String]));
        assertTrue(JList.of().equals(l))

    @Test
    def listToJava02(): Unit \ {Assert, IO} =
        let l: JList = ToJava.toJava(List#{"one"});
        assertTrue(JList.of("one").equals(l))

    @Test
    def listToJava03(): Unit \ {Assert, IO} =
        let l: JList = ToJava.toJava(List#{"one", "two"});
        assertTrue(JList.of("one", "two").equals(l))


    /////////////////////////////////////////////////////////////////////////////
    // Map                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def mapToJava01(): Unit \ {Assert, IO} =
        let m: JMap = ToJava.toJava((Map#{} : Map[String, String]));
        assertTrue(JMap.of().equals(m))

    @Test
    def mapToJava02(): Unit \ {Assert, IO} =
        let m: JMap = ToJava.toJava(Map#{"a" => "one"});
        assertTrue(JMap.of("a", "one").equals(m))

    @Test
    def mapToJava03(): Unit \ {Assert, IO} =
        let m: JMap = ToJava.toJava(Map#{"a" => "one", "b" => "two"});
        assertTrue(JMap.of("a", "one", "b", "two").equals(m))


    /////////////////////////////////////////////////////////////////////////////
    // Nec                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def necToJava01(): Unit \ {Assert, IO} =
        let l: JList = ToJava.toJava(Nec.singleton("one"));
        assertTrue(JList.of("one").equals(l))

    @Test
    def necToJava02(): Unit \ {Assert, IO} =
        let l: JList = ToJava.toJava(Nec.cons("one", Nec.singleton("two")));
        assertTrue(JList.of("one", "two").equals(l))


    /////////////////////////////////////////////////////////////////////////////
    // Nel                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def nelToJava01(): Unit \ {Assert, IO} =
        let l: JList = ToJava.toJava(Nel.singleton("one"));
        assertTrue(JList.of("one").equals(l))

    @Test
    def nelToJava02(): Unit \ {Assert, IO} =
        let l: JList = ToJava.toJava(Nel.cons("one", Nel.singleton("two")));
        assertTrue(JList.of("one", "two").equals(l))


    /////////////////////////////////////////////////////////////////////////////
    // Set                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def setToJava01(): Unit \ {Assert, IO} =
        let s: JSet = ToJava.toJava((Set#{} : Set[String]));
        assertTrue(JSet.of().equals(s))

    @Test
    def setToJava02(): Unit \ {Assert, IO} =
        let s: JSet = ToJava.toJava(Set#{"one"});
        assertTrue(JSet.of("one").equals(s))

    @Test
    def setToJava03(): Unit \ {Assert, IO} =
        let s: JSet = ToJava.toJava(Set#{"one", "two"});
        assertTrue(JSet.of("one", "two").equals(s))


    /////////////////////////////////////////////////////////////////////////////
    // Vector                                                                  //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def vectorToJava01(): Unit \ {Assert, IO} =
        let v: JList = ToJava.toJava((Vector#{} : Vector[String]));
        assertTrue(JList.of().equals(v))

    @Test
    def vectorToJava02(): Unit \ {Assert, IO} =
        let v: JList = ToJava.toJava(Vector#{"one"});
        assertTrue(JList.of("one").equals(v))

    @Test
    def vectorToJava03(): Unit \ {Assert, IO} =
        let v: JList = ToJava.toJava(Vector#{"one", "two"});
        assertTrue(JList.of("one", "two").equals(v))

}
