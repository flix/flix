mod TestStringBuilder {

    use Assert.assertEq;

    /////////////////////////////////////////////////////////////////////////////
    // appendString                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def appendString01(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.appendString("hello", sb);
        assertEq(expected = "hello", StringBuilder.toString(sb))
    }

    @Test
    def appendString02(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.appendString("hello", sb);
        StringBuilder.appendString("!", sb);
        assertEq(expected = "hello!", StringBuilder.toString(sb))
    }

    @Test
    def appendString03(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.appendString("hello", sb);
        StringBuilder.appendString(" ", sb);
        StringBuilder.appendString("world!", sb);
        assertEq(expected = "hello world!", StringBuilder.toString(sb))
    }

    /////////////////////////////////////////////////////////////////////////////
    // appendCodePoint                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def appendCodePoint01(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.appendCodePoint(0x1f3dd, sb);
        assertEq(expected = CodePoint.toString(0x1f3dd), StringBuilder.toString(sb))
    }

    @Test
    def appendCodePoint02(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.appendCodePoint(0x1f3dd, sb);
        StringBuilder.appendCodePoint(0x0020, sb);
        assertEq(expected = "\uD83C\uDFDD ", StringBuilder.toString(sb))
    }

    @Test
    def appendCodePoint03(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.appendCodePoint(0x0020, sb);
        StringBuilder.appendCodePoint(0x1f3dd, sb);
        StringBuilder.appendCodePoint(0x0020, sb);
        assertEq(expected = " \uD83C\uDFDD ", StringBuilder.toString(sb))
    }

    /////////////////////////////////////////////////////////////////////////////
    // append (Char)                                                           //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def appendChar01(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.append('A', sb);
        assertEq(expected = "A", StringBuilder.toString(sb))
    }

    @Test
    def appendChar02(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.append('A', sb);
        StringBuilder.append('B', sb);
        assertEq(expected = "AB", StringBuilder.toString(sb))
    }

    @Test
    def appendChar03(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.append('A', sb);
        StringBuilder.append('B', sb);
        StringBuilder.append('C', sb);
        assertEq(expected = "ABC", StringBuilder.toString(sb))
    }

    /////////////////////////////////////////////////////////////////////////////
    // append (Int8)                                                           //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def appendInt801(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.append(0i8, sb);
        assertEq(expected = "0", StringBuilder.toString(sb))
    }

    @Test
    def appendInt802(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.append(1i8, sb);
        assertEq(expected = "1", StringBuilder.toString(sb))
    }

    @Test
    def appendInt803(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.append(-1i8, sb);
        assertEq(expected = "-1", StringBuilder.toString(sb))
    }

    @Test
    def appendInt804(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.append(127i8, sb);
        assertEq(expected = "127", StringBuilder.toString(sb))
    }

    @Test
    def appendInt805(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.append(-127i8, sb);
        assertEq(expected = "-127", StringBuilder.toString(sb))
    }

    /////////////////////////////////////////////////////////////////////////////
    // append (Int16)                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def appendInt1601(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.append(0i16, sb);
        assertEq(expected = "0", StringBuilder.toString(sb))
    }

    @Test
    def appendInt1602(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.append(1i16, sb);
        assertEq(expected = "1", StringBuilder.toString(sb))
    }

    @Test
    def appendInt1603(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.append(-1i16, sb);
        assertEq(expected = "-1", StringBuilder.toString(sb))
    }

    /////////////////////////////////////////////////////////////////////////////
    // append (Int32)                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def appendInt3201(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.append(0, sb);
        assertEq(expected = "0", StringBuilder.toString(sb))
    }

    @Test
    def appendInt3202(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.append(1, sb);
        assertEq(expected = "1", StringBuilder.toString(sb))
    }

    @Test
    def appendInt3203(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.append(-1, sb);
        assertEq(expected = "-1", StringBuilder.toString(sb))
    }

    /////////////////////////////////////////////////////////////////////////////
    // append (Int64)                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def appendInt6401(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.append(0i64, sb);
        assertEq(expected = "0", StringBuilder.toString(sb))
    }

    @Test
    def appendInt6402(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.append(1i64, sb);
        assertEq(expected = "1", StringBuilder.toString(sb))
    }

    @Test
    def appendInt6403(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.append(-1i64, sb);
        assertEq(expected = "-1", StringBuilder.toString(sb))
    }

    /////////////////////////////////////////////////////////////////////////////
    // append (BigInt)                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def appendBigInt01(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.append(0ii, sb);
        assertEq(expected = "0", StringBuilder.toString(sb))
    }

    @Test
    def appendBigInt02(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.append(1ii, sb);
        assertEq(expected = "1", StringBuilder.toString(sb))
    }

    @Test
    def appendBigInt03(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.append(-1ii, sb);
        assertEq(expected = "-1", StringBuilder.toString(sb))
    }

    /////////////////////////////////////////////////////////////////////////////
    // appendFloat32                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def appendFloat3201(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.append(0.0f32, sb);
        assertEq(expected = "0.0", StringBuilder.toString(sb))
    }

    @Test
    def appendFloat3202(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.append(Float32.nan(), sb);
        assertEq(expected = "NaN", StringBuilder.toString(sb))
    }

    @Test
    def appendFloat3203(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.append(Float32.positiveInfinity(), sb);
        assertEq(expected = "Infinity", StringBuilder.toString(sb))
    }

    @Test
    def appendFloat3204(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.append(Float32.negativeInfinity(), sb);
        assertEq(expected = "-Infinity", StringBuilder.toString(sb))
    }

    /////////////////////////////////////////////////////////////////////////////
    // appendFloat64                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def appendFloat6401(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.append(0.0f64, sb);
        assertEq(expected = "0.0", StringBuilder.toString(sb))
    }

    @Test
    def appendFloat6402(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.append(Float64.nan(), sb);
        assertEq(expected = "NaN", StringBuilder.toString(sb))
    }

    @Test
    def appendFloat6403(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.append(Float64.positiveInfinity(), sb);
        assertEq(expected = "Infinity", StringBuilder.toString(sb))
    }

    @Test
    def appendFloat6404(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.append(Float64.negativeInfinity(), sb);
        assertEq(expected = "-Infinity", StringBuilder.toString(sb))
    }

    /////////////////////////////////////////////////////////////////////////////
    // appendLineSeparator                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def appendLineSeparator01(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.appendLineSeparator(sb);
        assertEq(expected = String.lineSeparator(), StringBuilder.toString(sb))
    }

    @Test
    def appendLineSeparator02(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.append('A', sb);
        StringBuilder.appendLineSeparator(sb);
        StringBuilder.append('B', sb);
        let s1 = "A" + String.lineSeparator() + "B";
        assertEq(expected = s1, StringBuilder.toString(sb))
    }

    /////////////////////////////////////////////////////////////////////////////
    // appendLine                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def appendLine01(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.appendLine("", sb);
        assertEq(expected = String.lineSeparator(), StringBuilder.toString(sb))
    }

    @Test
    def appendLine02(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.appendLine("A", sb);
        let s1 = "A" + String.lineSeparator();
        assertEq(expected = s1, StringBuilder.toString(sb))
    }

    @Test
    def appendLine03(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.appendLine("A", sb);
        StringBuilder.appendLine("B", sb);
        let s1 = "A" + String.lineSeparator() + "B" + String.lineSeparator();
        assertEq(expected = s1, StringBuilder.toString(sb))
    }

    @Test
    def appendLine04(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.appendLine("A", sb);
        StringBuilder.appendLine("B", sb);
        StringBuilder.appendLine("C", sb);
        let s1 = "A" + String.lineSeparator() + "B" + String.lineSeparator() + "C" + String.lineSeparator();
        assertEq(expected = s1, StringBuilder.toString(sb))
    }

    /////////////////////////////////////////////////////////////////////////////
    // appendLineWith                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def appendLineWith01(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.appendLineWith(Int32.toString, 1, sb);
        let s1 = "1" + String.lineSeparator();
        assertEq(expected = s1, StringBuilder.toString(sb))
    }

    @Test
    def appendLineWith02(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.appendLineWith(Int32.toString, 1, sb);
        StringBuilder.appendLineWith(Int32.toString, 2, sb);
        let s1 = "1" + String.lineSeparator() + "2" + String.lineSeparator();
        assertEq(expected = s1, StringBuilder.toString(sb))
    }

    @Test
    def appendLineWith03(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.appendLineWith(Int32.toString, 1, sb);
        StringBuilder.appendLineWith(Int32.toString, 2, sb);
        StringBuilder.appendLineWith(Int32.toString, 3, sb);
        let s1 = "1" + String.lineSeparator() + "2" + String.lineSeparator() + "3" + String.lineSeparator();
        assertEq(expected = s1, StringBuilder.toString(sb))
    }

    @Test
    def appendLineWith04(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.appendLineWith(Int32.toString, 1, sb);
        StringBuilder.appendLineWith(Int32.toString, 2, sb);
        StringBuilder.appendLineWith(Int32.toString, 3, sb);
        StringBuilder.appendLineWith(Int32.toString, 4, sb);
        let s1 = "1" + String.lineSeparator() + "2" + String.lineSeparator() + "3" + String.lineSeparator() + "4" + String.lineSeparator();
        assertEq(expected = s1, StringBuilder.toString(sb))
    }

    /////////////////////////////////////////////////////////////////////////////
    // appendLines                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def appendLines01(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.appendLines(Array#{} @ rc, sb);
        assertEq(expected = "", StringBuilder.toString(sb))
    }

    @Test
    def appendLines02(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.appendLines(Array#{"A"} @ rc, sb);
        let s1 = "A" + String.lineSeparator();
        assertEq(expected = s1, StringBuilder.toString(sb))
    }

    @Test
    def appendLines03(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.appendLines(Array#{"A", "B"} @ rc, sb);
        let s1 = "A" + String.lineSeparator() + "B" + String.lineSeparator();
        assertEq(expected = s1, StringBuilder.toString(sb))
    }

    @Test
    def appendLines04(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.appendLines(Array#{"A", "B", "C"} @ rc, sb);
        let s1 = "A" + String.lineSeparator() + "B" + String.lineSeparator() + "C" + String.lineSeparator();
        assertEq(expected = s1, StringBuilder.toString(sb))
    }

    /////////////////////////////////////////////////////////////////////////////
    // appendLinesWith                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def appendLinesWith01(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.appendLinesWith(Int32.toString, Nil, sb);
        assertEq(expected = "", StringBuilder.toString(sb))
    }

    @Test
    def appendLinesWith02(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.appendLinesWith(Int32.toString, Set#{1}, sb);
        let s1 = "1" + String.lineSeparator();
        assertEq(expected = s1, StringBuilder.toString(sb))
    }

    @Test
    def appendLinesWith03(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.appendLinesWith(Int32.toString, List.toChain(1 :: 2 :: Nil), sb);
        let s1 = "1" + String.lineSeparator() + "2" + String.lineSeparator();
        assertEq(expected = s1, StringBuilder.toString(sb))
    }

    @Test
    def appendLinesWith04(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.appendLinesWith(Int32.toString, 1 :: 2 :: 3 :: Nil, sb);
        let s1 = "1" + String.lineSeparator() + "2" + String.lineSeparator() + "3" + String.lineSeparator();
        assertEq(expected = s1, StringBuilder.toString(sb))
    }

    /////////////////////////////////////////////////////////////////////////////
    // intercalate                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def intercalate01(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.intercalate("", Array#{} @ rc, sb);
        assertEq(expected = "", StringBuilder.toString(sb))
    }

    @Test
    def intercalate02(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.intercalate("::", Array#{} @ rc, sb);
        assertEq(expected = "", StringBuilder.toString(sb))
    }

    @Test
    def intercalate03(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.intercalate("", Array#{"abc", "def"} @ rc, sb);
        assertEq(expected = "abcdef", StringBuilder.toString(sb))
    }

    @Test
    def intercalate04(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.intercalate("::", Array#{"abc"} @ rc, sb);
        assertEq(expected = "abc", StringBuilder.toString(sb))
    }

    @Test
    def intercalate05(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.intercalate("::", Array#{"abc", "def"} @ rc, sb);
        assertEq(expected = "abc::def", StringBuilder.toString(sb))
    }

    @Test
    def intercalate06(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.intercalate("::", Array#{"abc", "def", "ghi"} @ rc, sb);
        assertEq(expected = "abc::def::ghi", StringBuilder.toString(sb))
    }


    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iterator01(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        assertEq(expected = Nil, StringBuilder.iterator(rc, sb) |> Iterator.toList)
    }

    @Test
    def iterator02(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.appendString("abc", sb);
        assertEq(expected = 'a' :: 'b' :: 'c' :: Nil, StringBuilder.iterator(rc, sb) |> Iterator.toList)
    }
    /////////////////////////////////////////////////////////////////////////////
    // length                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def length01(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        assertEq(expected = 0, StringBuilder.length(sb))
    }

    @Test
    def length02(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.append('A', sb);
        assertEq(expected = 1, StringBuilder.length(sb))
    }

    @Test
    def length03(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.append('A', sb);
        StringBuilder.append('B', sb);
        assertEq(expected = 2, StringBuilder.length(sb))
    }

    @Test
    def length04(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.appendString("A", sb);
        StringBuilder.appendString("B", sb);
        assertEq(expected = 2, StringBuilder.length(sb))
    }

    @Test
    def length05(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.appendString("Hello", sb);
        StringBuilder.append(' ', sb);
        StringBuilder.appendString("World", sb);
        StringBuilder.append('!', sb);
        assertEq(expected = 12, StringBuilder.length(sb))
    }

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def size01(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        assertEq(expected = 0, StringBuilder.size(sb))
    }

    @Test
    def size02(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.append('A', sb);
        assertEq(expected = 1, StringBuilder.size(sb))
    }

    @Test
    def size03(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.append('A', sb);
        StringBuilder.append('B', sb);
        assertEq(expected = 2, StringBuilder.size(sb))
    }

    @Test
    def size04(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.appendString("A", sb);
        StringBuilder.appendString("B", sb);
        assertEq(expected = 2, StringBuilder.size(sb))
    }

    @Test
    def size05(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.appendString("Hello", sb);
        StringBuilder.append(' ', sb);
        StringBuilder.appendString("World", sb);
        StringBuilder.append('!', sb);
        assertEq(expected = 12, StringBuilder.size(sb))
    }


    /////////////////////////////////////////////////////////////////////////////
    // setLength                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def setLength01(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.append("1234567890", sb);
        StringBuilder.setLength(4, sb);
        assertEq(expected = "1234", StringBuilder.toString(sb))
    }

    @Test
    def setLength02(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.append("ABC", sb);
        StringBuilder.setLength(0, sb);
        assertEq(expected = "", StringBuilder.toString(sb))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toString01(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        assertEq(expected = "", StringBuilder.toString(sb))
    }

    @Test
    def toString02(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.appendString("", sb);
        assertEq(expected = "", StringBuilder.toString(sb))
    }

    @Test
    def toString03(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.appendString("abcdefghijklmnopqrstuvwxyz", sb);
        assertEq(expected = "abcdefghijklmnopqrstuvwxyz", StringBuilder.toString(sb))
    }

    /////////////////////////////////////////////////////////////////////////////
    // Writable instance                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def writable01(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        let result = Writable.writeString("", sb);
        assertEq(expected = Ok(()), result);
        assertEq(expected = "", StringBuilder.toString(sb))
    }

    @Test
    def writable02(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        let result = Writable.writeString("hello", sb);
        assertEq(expected = Ok(()), result);
        assertEq(expected = "hello", StringBuilder.toString(sb))
    }

    @Test
    def writable03(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        let result = forM(
            _ <- Writable.writeString("hello", sb);
            _ <- Writable.writeString(" ", sb);
            _ <- Writable.writeString("world", sb)
        ) yield ();
        assertEq(expected = Ok(()), result);
        assertEq(expected = "hello world", StringBuilder.toString(sb))
    }

    @Test
    def writable04(): Unit \ {Assert, IO} = region rc {
        let sb = StringBuilder.empty(rc);
        let result = Writable.writeNewLine(sb);
        assertEq(expected = Ok(()), result);
        assertEq(expected = String.lineSeparator(), StringBuilder.toString(sb))
    }

    @Test
    def writable05(): Unit \ {Assert, IO} = region rc {
        let sb = StringBuilder.empty(rc);
        let result = forM(
            _ <- Writable.writeString("hello", sb);
            _ <- Writable.writeNewLine(sb)
        ) yield ();
        assertEq(expected = Ok(()), result);
        assertEq(expected = "hello" ++ String.lineSeparator(), StringBuilder.toString(sb))
    }

    @Test
    def writable06(): Unit \ {Assert, IO} = region rc {
        let sb = StringBuilder.empty(rc);
        let result = Writable.writeLine("hello", sb);
        assertEq(expected = Ok(()), result);
        assertEq(expected = "hello" ++ String.lineSeparator(), StringBuilder.toString(sb))
    }

    @Test
    def writable07(): Unit \ {Assert, IO} = region rc {
        let sb = StringBuilder.empty(rc);
        let result = forM(
            _ <- Writable.writeLine("line1", sb);
            _ <- Writable.writeLine("line2", sb);
            _ <- Writable.writeLine("line3", sb)
        ) yield ();
        assertEq(expected = Ok(()), result);
        let expected = "line1" ++ String.lineSeparator() ++ "line2" ++ String.lineSeparator() ++ "line3" ++ String.lineSeparator();
        assertEq(expected = expected, StringBuilder.toString(sb))
    }

    @Test
    def writable08(): Unit \ Assert = region rc {
        let sb = StringBuilder.empty(rc);
        StringBuilder.appendString("prefix: ", sb);
        let result = Writable.writeString("suffix", sb);
        assertEq(expected = Ok(()), result);
        assertEq(expected = "prefix: suffix", StringBuilder.toString(sb))
    }

}
