mod Test.Exp.Binary.Arithmetic {

    use Assert.assertEq

    @Test
    def testBinaryPlus01(): Unit \ Assert =
        assertEq(expected = -2147483648, 2147483647 + 1)

    @Test
    def testBinaryPlus02(): Unit \ Assert =
        assertEq(expected = 500000, 100000 + 400000)

    @Test
    def testBinaryPlus03(): Unit \ Assert =
        assertEq(expected = -300000, -400000 + 100000)

    @Test
    def testBinaryPlus04(): Unit \ Assert =
        assertEq(expected = 300000, -100000 + 400000)

    @Test
    def testBinaryPlus05(): Unit \ Assert =
        assertEq(expected = 2147483647, -2147483648 + -1)

    @Test
    def testBinaryPlus06(): Unit \ Assert =
        assertEq(expected = -128i8, 127i8 + 1i8)

    @Test
    def testBinaryPlus07(): Unit \ Assert =
        assertEq(expected = 50i8, 10i8 + 40i8)

    @Test
    def testBinaryPlus08(): Unit \ Assert =
        assertEq(expected = -30i8, -40i8 + 10i8)

    @Test
    def testBinaryPlus09(): Unit \ Assert =
        assertEq(expected = 30i8, -10i8 + 40i8)

    @Test
    def testBinaryPlus10(): Unit \ Assert =
        assertEq(expected = 127i8, -128i8 + -1i8)

    @Test
    def testBinaryPlus11(): Unit \ Assert =
        assertEq(expected = -32768i16, 32767i16 + 1i16)

    @Test
    def testBinaryPlus12(): Unit \ Assert =
        assertEq(expected = 5000i16, 1000i16 + 4000i16)

    @Test
    def testBinaryPlus13(): Unit \ Assert =
        assertEq(expected = -3000i16, -4000i16 + 1000i16)

    @Test
    def testBinaryPlus14(): Unit \ Assert =
        assertEq(expected = 3000i16, -1000i16 + 4000i16)

    @Test
    def testBinaryPlus15(): Unit \ Assert =
        assertEq(expected = 32767i16, -32768i16 + -1i16)

    @Test
    def testBinaryPlus16(): Unit \ Assert =
        assertEq(expected = -2147483648i32, 2147483647i32 + 1i32)

    @Test
    def testBinaryPlus17(): Unit \ Assert =
        assertEq(expected = 500000i32, 100000i32 + 400000i32)

    @Test
    def testBinaryPlus18(): Unit \ Assert =
        assertEq(expected = -300000i32, -400000i32 + 100000i32)

    @Test
    def testBinaryPlus19(): Unit \ Assert =
        assertEq(expected = 300000i32, -100000i32 + 400000i32)

    @Test
    def testBinaryPlus20(): Unit \ Assert =
        assertEq(expected = 2147483647i32, -2147483648i32 + -1i32)

    @Test
    def testBinaryPlus21(): Unit \ Assert =
        assertEq(expected = -9223372036854775808i64, 9223372036854775807i64 + 1i64)

    @Test
    def testBinaryPlus22(): Unit \ Assert =
        assertEq(expected = 50000000000i64, 10000000000i64 + 40000000000i64)

    @Test
    def testBinaryPlus23(): Unit \ Assert =
        assertEq(expected = -30000000000i64, -40000000000i64 + 10000000000i64)

    @Test
    def testBinaryPlus24(): Unit \ Assert =
        assertEq(expected = 30000000000i64, -10000000000i64 + 40000000000i64)

    @Test
    def testBinaryPlus25(): Unit \ Assert =
        assertEq(expected = 9223372036854775807i64, -9223372036854775808i64 + -1i64)

    @Test
    def testBinaryPlus26(): Unit \ Assert =
        assertEq(expected = 69.12, 12.34 + 56.78)

    @Test
    def testBinaryPlus27(): Unit \ Assert =
        assertEq(expected = 1234567890000000000000000000000000000000000000223.209654321,
                 1234567890000000000000000000000000000000000000000.987654321 + 222.222)

    @Test
    def testBinaryPlus28(): Unit \ Assert =
        assertEq(expected = -1234567890000000000000000000000000000000000000000.987654321,
                 -1234567890000000000000000000000000000000000000000.987654321 + 0.0)

    @Test
    def testBinaryPlus29(): Unit \ Assert =
        assertEq(expected = 0.222, 0.0000000000000000000000000000000000000000987654321 + 0.222)

    @Test
    def testBinaryPlus30(): Unit \ Assert =
        assertEq(expected = 0.222, -0.0000000000000000000000000000000000000000987654321 + 0.222)

    @Test
    def testBinaryPlus31(): Unit \ Assert =
        assertEq(expected = 69.119995f32, 12.34f32 + 56.78f32)

    @Test
    def testBinaryPlus32(): Unit \ Assert =
        assertEq(expected = 1.23456706e30f32, 1.234567e30f32 + 1.0e23f32)

    @Test
    def testBinaryPlus33(): Unit \ Assert =
        assertEq(expected = -1.234567e30f32, -1.234567e30f32 + 0.0f32)

    @Test
    def testBinaryPlus34(): Unit \ Assert =
        assertEq(expected = 0.222f32, 0.0000000000000000000000000000000000000000987654321f32 + 0.222f32)

    @Test
    def testBinaryPlus35(): Unit \ Assert =
        assertEq(expected = 0.222f32, -0.0000000000000000000000000000000000000000987654321f32 + 0.222f32)

    @Test
    def testBinaryPlus36(): Unit \ Assert =
        assertEq(expected = 69.119995f32, 12.34f32 + 56.78f32)

    @Test
    def testBinaryPlus37(): Unit \ Assert =
        assertEq(expected = 1234567890000000000000000000000000000000000000223.209654321f64,
                 1234567890000000000000000000000000000000000000000.987654321f64 + 222.222f64)

    @Test
    def testBinaryPlus38(): Unit \ Assert =
        assertEq(expected = -1234567890000000000000000000000000000000000000000.987654321f64,
                 -1234567890000000000000000000000000000000000000000.987654321f64 + 0.0f64)

    @Test
    def testBinaryPlus39(): Unit \ Assert =
        assertEq(expected = 0.222f64, 0.0000000000000000000000000000000000000000987654321f64 + 0.222f64)

    @Test
    def testBinaryPlus40(): Unit \ Assert =
        assertEq(expected = 0.222f64, -0.0000000000000000000000000000000000000000987654321f64 + 0.222f64)

    @Test
    def testBinaryPlus41(): Unit \ Assert =
        assertEq(expected = 9223372036854775809ii, 9223372036854775808ii + 1ii)

    @Test
    def testBinaryPlus42(): Unit \ Assert =
        assertEq(expected = 50000000000000000000ii, 10000000000000000000ii + 40000000000000000000ii)

    @Test
    def testBinaryPlus43(): Unit \ Assert =
        assertEq(expected = -30000000000000000000ii, -40000000000000000000ii + 10000000000000000000ii)

    @Test
    def testBinaryPlus44(): Unit \ Assert =
        assertEq(expected = 30000000000000000000ii, -10000000000000000000ii + 40000000000000000000ii)

    @Test
    def testBinaryPlus45(): Unit \ Assert =
        assertEq(expected = -9223372036854775810ii, -9223372036854775809ii + -1ii)

    @Test
    def testBinaryMinus01(): Unit \ Assert =
        assertEq(expected = 2147483647, -2147483648 - 1)

    @Test
    def testBinaryMinus02(): Unit \ Assert =
        assertEq(expected = 300000, 400000 - 100000)

    @Test
    def testBinaryMinus03(): Unit \ Assert =
        assertEq(expected = -500000, -400000 - 100000)

    @Test
    def testBinaryMinus04(): Unit \ Assert =
        assertEq(expected = -500000, -100000 - 400000)

    @Test
    def testBinaryMinus05(): Unit \ Assert =
        assertEq(expected = -2147483648, 2147483647 - -1)

    @Test
    def testBinaryMinus06(): Unit \ Assert =
        assertEq(expected = 127i8, -128i8 - 1i8)

    @Test
    def testBinaryMinus07(): Unit \ Assert =
        assertEq(expected = 30i8, 40i8 - 10i8)

    @Test
    def testBinaryMinus08(): Unit \ Assert =
        assertEq(expected = -50i8, -40i8 - 10i8)

    @Test
    def testBinaryMinus09(): Unit \ Assert =
        assertEq(expected = -50i8, -10i8 - 40i8)

    @Test
    def testBinaryMinus10(): Unit \ Assert =
        assertEq(expected = 127i8, -128i8 - 1i8)

    @Test
    def testBinaryMinus11(): Unit \ Assert =
        assertEq(expected = 32767i16, -32768i16 - 1i16)

    @Test
    def testBinaryMinus12(): Unit \ Assert =
        assertEq(expected = 3000i16, 4000i16 - 1000i16)

    @Test
    def testBinaryMinus13(): Unit \ Assert =
        assertEq(expected = -5000i16, -4000i16 - 1000i16)

    @Test
    def testBinaryMinus14(): Unit \ Assert =
        assertEq(expected = -5000i16, -1000i16 - 4000i16)

    @Test
    def testBinaryMinus15(): Unit \ Assert =
        assertEq(expected = -32768i16, 32767i16 - -1i16)

    @Test
    def testBinaryMinus16(): Unit \ Assert =
        assertEq(expected = 2147483647i32, -2147483648i32 - 1i32)

    @Test
    def testBinaryMinus17(): Unit \ Assert =
        assertEq(expected = 300000i32, 400000i32 - 100000i32)

    @Test
    def testBinaryMinus18(): Unit \ Assert =
        assertEq(expected = -500000i32, -400000i32 - 100000i32)

    @Test
    def testBinaryMinus19(): Unit \ Assert =
        assertEq(expected = -500000i32, -100000i32 - 400000i32)

    @Test
    def testBinaryMinus20(): Unit \ Assert =
        assertEq(expected = -2147483648i32, 2147483647i32 - -1i32)

    @Test
    def testBinaryMinus21(): Unit \ Assert =
        assertEq(expected = 9223372036854775807i64, -9223372036854775808i64 - 1i64)

    @Test
    def testBinaryMinus22(): Unit \ Assert =
        assertEq(expected = 30000000000i64, 40000000000i64 - 10000000000i64)

    @Test
    def testBinaryMinus23(): Unit \ Assert =
        assertEq(expected = -50000000000i64, -40000000000i64 - 10000000000i64)

    @Test
    def testBinaryMinus24(): Unit \ Assert =
        assertEq(expected = -50000000000i64, -10000000000i64 - 40000000000i64)

    @Test
    def testBinaryMinus25(): Unit \ Assert =
        assertEq(expected = -9223372036854775808i64, 9223372036854775807i64 - -1i64)

    @Test
    def testBinaryMinus26(): Unit \ Assert =
        assertEq(expected = -44.44, 12.34 - 56.78)

    @Test
    def testBinaryMinus27(): Unit \ Assert =
        assertEq(expected = 1234567890000000000000000000000000000000000000000.0,
                 1234567890000000000000000000000000000000000000000.987654321 - 222.222)

    @Test
    def testBinaryMinus28(): Unit \ Assert =
        assertEq(expected = -1234567890000000000000000000000000000000000000000.0,
                 -1234567890000000000000000000000000000000000000000.987654321 - 0.0)

    @Test
    def testBinaryMinus29(): Unit \ Assert =
        assertEq(expected = -0.222, 0.0000000000000000000000000000000000000000987654321 - 0.222)

    @Test
    def testBinaryMinus30(): Unit \ Assert =
        assertEq(expected = -0.222, -0.0000000000000000000000000000000000000000987654321 - 0.222)

    @Test
    def testBinaryMinus31(): Unit \ Assert =
        assertEq(expected = -44.44f32, 12.34f32 - 56.78f32)

    @Test
    def testBinaryMinus32(): Unit \ Assert =
        assertEq(expected = 123456789000000000000000000000000000000.0f32,
                 123456789000000000000000000000000000000.987654321f32 - 222.222f32)

    @Test
    def testBinaryMinus33(): Unit \ Assert =
        assertEq(expected = -123456789000000000000000000000000000000.0f32,
                 -123456789000000000000000000000000000000.987654321f32 - 0.0f32)

    @Test
    def testBinaryMinus34(): Unit \ Assert =
        assertEq(expected = -0.222f32, 0.000000000000000000000000000000987654321f32 - 0.222f32)

    @Test
    def testBinaryMinus35(): Unit \ Assert =
        assertEq(expected = -0.222f32, -0.000000000000000000000000000000987654321f32 - 0.222f32)

    @Test
    def testBinaryMinus36(): Unit \ Assert =
        assertEq(expected = -44.44f64, 12.34f64 - 56.78f64)

    @Test
    def testBinaryMinus37(): Unit \ Assert =
        assertEq(expected = 123456789000000000000000000000000000000.0f64,
                 123456789000000000000000000000000000000.987654321f64 - 222.222f64)

    @Test
    def testBinaryMinus38(): Unit \ Assert =
        assertEq(expected = -123456789000000000000000000000000000000.0f64,
                 -123456789000000000000000000000000000000.987654321f64 - 0.0f64)

    @Test
    def testBinaryMinus39(): Unit \ Assert =
        assertEq(expected = -0.222f64, 0.000000000000000000000000000000987654321f64 - 0.222f64)

    @Test
    def testBinaryMinus40(): Unit \ Assert =
        assertEq(expected = -0.222f64, -0.000000000000000000000000000000987654321f64 - 0.222f64)

    @Test
    def testBinaryMinus41(): Unit \ Assert =
        assertEq(expected = -9223372036854775810ii, -9223372036854775809ii - 1ii)

    @Test
    def testBinaryMinus42(): Unit \ Assert =
        assertEq(expected = 30000000000000000000ii, 40000000000000000000ii - 10000000000000000000ii)

    @Test
    def testBinaryMinus43(): Unit \ Assert =
        assertEq(expected = -50000000000000000000ii, -40000000000000000000ii - 10000000000000000000ii)

    @Test
    def testBinaryMinus44(): Unit \ Assert =
        assertEq(expected = -50000000000000000000ii, -10000000000000000000ii - 40000000000000000000ii)

    @Test
    def testBinaryMinus45(): Unit \ Assert =
        assertEq(expected = 9223372036854775809ii, 9223372036854775808ii - -1ii)

    @Test
    def testBinaryTimes01(): Unit \ Assert =
        assertEq(expected = -2, 2147483647 * 2)

    @Test
    def testBinaryTimes02(): Unit \ Assert =
        assertEq(expected = 60000, 300 * 200)

    @Test
    def testBinaryTimes03(): Unit \ Assert =
        assertEq(expected = -60000, -200 * 300)

    @Test
    def testBinaryTimes04(): Unit \ Assert =
        assertEq(expected = 60000, -200 * -300)

    @Test
    def testBinaryTimes05(): Unit \ Assert =
        assertEq(expected = -2147483648, -2147483648 * -1)

    @Test
    def testBinaryTimes06(): Unit \ Assert =
        assertEq(expected = -2i8, 127i8 * 2i8)

    @Test
    def testBinaryTimes07(): Unit \ Assert =
        assertEq(expected = 6i8, 3i8 * 2i8)

    @Test
    def testBinaryTimes08(): Unit \ Assert =
        assertEq(expected = -6i8, -2i8 * 3i8)

    @Test
    def testBinaryTimes09(): Unit \ Assert =
        assertEq(expected = 6i8, -2i8 * -3i8)

    @Test
    def testBinaryTimes10(): Unit \ Assert =
        assertEq(expected = -128i8, -128i8 * -1i8)

    @Test
    def testBinaryTimes11(): Unit \ Assert =
        assertEq(expected = -2i16, 32767i16 * 2i16)

    @Test
    def testBinaryTimes12(): Unit \ Assert =
        assertEq(expected = 600i16, 30i16 * 20i16)

    @Test
    def testBinaryTimes13(): Unit \ Assert =
        assertEq(expected = -600i16, -20i16 * 30i16)

    @Test
    def testBinaryTimes14(): Unit \ Assert =
        assertEq(expected = 600i16, -20i16 * -30i16)

    @Test
    def testBinaryTimes15(): Unit \ Assert =
        assertEq(expected = -32768i16, -32768i16 * -1i16)

    @Test
    def testBinaryTimes16(): Unit \ Assert =
        assertEq(expected = -2i32, 2147483647i32 * 2i32)

    @Test
    def testBinaryTimes17(): Unit \ Assert =
        assertEq(expected = 60000i32, 300i32 * 200i32)

    @Test
    def testBinaryTimes18(): Unit \ Assert =
        assertEq(expected = -60000i32, -200i32 * 300i32)

    @Test
    def testBinaryTimes19(): Unit \ Assert =
        assertEq(expected = 60000i32, -200i32 * -300i32)

    @Test
    def testBinaryTimes20(): Unit \ Assert =
        assertEq(expected = -2147483648i32, -2147483648i32 * -1i32)

    @Test
    def testBinaryTimes21(): Unit \ Assert =
        assertEq(expected = -2i64, 9223372036854775807i64 * 2i64)

    @Test
    def testBinaryTimes22(): Unit \ Assert =
        assertEq(expected = 60000000000i64, 300000i64 * 200000i64)

    @Test
    def testBinaryTimes23(): Unit \ Assert =
        assertEq(expected = -60000000000i64, -200000i64 * 300000i64)

    @Test
    def testBinaryTimes24(): Unit \ Assert =
        assertEq(expected = 60000000000i64, -200000i64 * -300000i64)

    @Test
    def testBinaryTimes25(): Unit \ Assert =
        assertEq(expected = -9223372036854775808i64, -9223372036854775808i64 * -1i64)

    @Test
    def testBinaryTimes26(): Unit \ Assert =
        assertEq(expected = 700.6652, 12.34 * 56.78)

    @Test
    def testBinaryTimes27(): Unit \ Assert =
        assertEq(expected = 274348145651580030000000000000000000000000000000000.0,
                 1234567890000000000000000000000000000000000000000.987654321 * 222.222)

    @Test
    def testBinaryTimes28(): Unit \ Assert =
        assertEq(expected = -274348145651580030000000000000000000000000000000000.0,
                 -1234567890000000000000000000000000000000000000000.987654321 * 222.222)

    @Test
    def testBinaryTimes29(): Unit \ Assert =
        assertEq(expected = 0.0000000000000000000000000000000000000000219259259262,
                 0.0000000000000000000000000000000000000000987654321 * 0.222)

    @Test
    def testBinaryTimes30(): Unit \ Assert =
        assertEq(expected = -0.0000000000000000000000000000000000000000219259259262,
                 -0.0000000000000000000000000000000000000000987654321 * 0.222)

    @Test
    def testBinaryTimes31(): Unit \ Assert =
        assertEq(expected = 700.6652f32, 12.34f32 * 56.78f32)

    @Test
    def testBinaryTimes32(): Unit \ Assert =
        assertEq(expected = 2.7434794e32f32, 1.234567e30f32 * 222.222f32)

    @Test
    def testBinaryTimes33(): Unit \ Assert =
        assertEq(expected = -2.7434794e32f32, -1.234567e30f32 * 222.222f32)

    @Test
    def testBinaryTimes34(): Unit \ Assert =
        assertEq(expected = 0.0000000000000000000000000000000000000000219259259262f32,
                 0.0000000000000000000000000000000000000000987654321f32 * 0.222f32)

    @Test
    def testBinaryTimes35(): Unit \ Assert =
        assertEq(expected = -0.0000000000000000000000000000000000000000219259259262f32,
                 -0.0000000000000000000000000000000000000000987654321f32 * 0.222f32)

    @Test
    def testBinaryTimes36(): Unit \ Assert =
        assertEq(expected = 700.6652f64, 12.34f64 * 56.78f64)

    @Test
    def testBinaryTimes37(): Unit \ Assert =
        assertEq(expected = 274348145651580030000000000000000000000000000000000.0f64,
                 1234567890000000000000000000000000000000000000000.987654321f64 * 222.222f64)

    @Test
    def testBinaryTimes38(): Unit \ Assert =
        assertEq(expected = -274348145651580030000000000000000000000000000000000.0f64,
                 -1234567890000000000000000000000000000000000000000.987654321f64 * 222.222f64)

    @Test
    def testBinaryTimes39(): Unit \ Assert =
        assertEq(expected = 0.0000000000000000000000000000000000000000219259259262f64,
                 0.0000000000000000000000000000000000000000987654321f64 * 0.222f64)

    @Test
    def testBinaryTimes40(): Unit \ Assert =
        assertEq(expected = -0.0000000000000000000000000000000000000000219259259262f64,
                 -0.0000000000000000000000000000000000000000987654321f64 * 0.222f64)

    @Test
    def testBinaryTimes41(): Unit \ Assert =
        assertEq(expected = 18446744073709551616ii, 9223372036854775808ii * 2ii)

    @Test
    def testBinaryTimes42(): Unit \ Assert =
        assertEq(expected = 600000000000000000000ii, 30000000000ii * 20000000000ii)

    @Test
    def testBinaryTimes43(): Unit \ Assert =
        assertEq(expected = -600000000000000000000ii, -20000000000ii * 30000000000ii)

    @Test
    def testBinaryTimes44(): Unit \ Assert =
        assertEq(expected = 600000000000000000000ii, -20000000000ii * -30000000000ii)

    @Test
    def testBinaryTimes45(): Unit \ Assert =
        assertEq(expected = 9223372036854775809ii, -9223372036854775809ii * -1ii)

    @Test
    def testBinaryDivide01(): Unit \ Assert =
        assertEq(expected = 2147483647, 2147483647 / 1)

    @Test
    def testBinaryDivide02(): Unit \ Assert =
        assertEq(expected = 400000, 1200000 / 3)

    @Test
    def testBinaryDivide03(): Unit \ Assert =
        assertEq(expected = -400000, -1200000 / 3)

    @Test
    def testBinaryDivide04(): Unit \ Assert =
        assertEq(expected = 0, -3 / 1200000)

    @Test
    def testBinaryDivide05(): Unit \ Assert =
        assertEq(expected = -2147483648, -2147483648 / -1)

    @Test
    def testBinaryDivide06(): Unit \ Assert =
        assertEq(expected = 127i8, 127i8 / 1i8)

    @Test
    def testBinaryDivide07(): Unit \ Assert =
        assertEq(expected = 4i8, 12i8 / 3i8)

    @Test
    def testBinaryDivide08(): Unit \ Assert =
        assertEq(expected = -4i8, -12i8 / 3i8)

    @Test
    def testBinaryDivide09(): Unit \ Assert =
        assertEq(expected = 0i8, -3i8 / 12i8)

    @Test
    def testBinaryDivide10(): Unit \ Assert =
        assertEq(expected = -128i8, -128i8 / -1i8)

    @Test
    def testBinaryDivide11(): Unit \ Assert =
        assertEq(expected = 32767i16, 32767i16 / 1i16)

    @Test
    def testBinaryDivide12(): Unit \ Assert =
        assertEq(expected = 4000i16, 12000i16 / 3i16)

    @Test
    def testBinaryDivide13(): Unit \ Assert =
        assertEq(expected = -4000i16, -12000i16 / 3i16)

    @Test
    def testBinaryDivide14(): Unit \ Assert =
        assertEq(expected = 0i16, -3i16 / 12000i16)

    @Test
    def testBinaryDivide15(): Unit \ Assert =
        assertEq(expected = -32768i16, -32768i16 / -1i16)

    @Test
    def testBinaryDivide16(): Unit \ Assert =
        assertEq(expected = 2147483647i32, 2147483647i32 / 1i32)

    @Test
    def testBinaryDivide17(): Unit \ Assert =
        assertEq(expected = 400000i32, 1200000i32 / 3i32)

    @Test
    def testBinaryDivide18(): Unit \ Assert =
        assertEq(expected = -400000i32, -1200000i32 / 3i32)

    @Test
    def testBinaryDivide19(): Unit \ Assert =
        assertEq(expected = 0i32, -3i32 / 1200000i32)

    @Test
    def testBinaryDivide20(): Unit \ Assert =
        assertEq(expected = -2147483648i32, -2147483648i32 / -1i32)

    @Test
    def testBinaryDivide21(): Unit \ Assert =
        assertEq(expected = 9223372036854775807i64, 9223372036854775807i64 / 1i64)

    @Test
    def testBinaryDivide22(): Unit \ Assert =
        assertEq(expected = 40000000000i64, 120000000000i64 / 3i64)

    @Test
    def testBinaryDivide23(): Unit \ Assert =
        assertEq(expected = -40000000000i64, -120000000000i64 / 3i64)

    @Test
    def testBinaryDivide24(): Unit \ Assert =
        assertEq(expected = 0i64, -3i64 / 120000000000i64)

    @Test
    def testBinaryDivide25(): Unit \ Assert =
        assertEq(expected = -9223372036854775808i64, -9223372036854775808i64 / -1i64)

    @Test
    def testBinaryDivide26(): Unit \ Assert =
        assertEq(expected = 0.2173300457907714, 12.34 / 56.78)

    @Test
    def testBinaryDivide27(): Unit \ Assert =
        assertEq(expected = 5555561060561060400000000000000000000000000000.0,
                 1234567890000000000000000000000000000000000000000.987654321 / 222.222)

    @Test
    def testBinaryDivide28(): Unit \ Assert =
        assertEq(expected = -5555561060561060400000000000000000000000000000.0,
                 -1234567890000000000000000000000000000000000000000.987654321 / 222.222)

    @Test
    def testBinaryDivide29(): Unit \ Assert =
        assertEq(expected = 0.00000000000000000000000000000000000000044488933378378374,
                 0.0000000000000000000000000000000000000000987654321 / 0.222)

    @Test
    def testBinaryDivide30(): Unit \ Assert =
        assertEq(expected = -0.00000000000000000000000000000000000000044488933378378374,
                 -0.0000000000000000000000000000000000000000987654321 / 0.222)

    @Test
    def testBinaryDivide31(): Unit \ Assert =
        assertEq(expected = 0.21733005f32, 12.34f32 / 56.78f32)

    @Test
    def testBinaryDivide32(): Unit \ Assert =
        assertEq(expected = 555556080000000000000000000000000000.0f32,
                 123456789000000000000000000000000000000.987654321f32 / 222.222f32)

    @Test
    def testBinaryDivide33(): Unit \ Assert =
        assertEq(expected = -555556080000000000000000000000000000.0f32,
                 -123456789000000000000000000000000000000.987654321f32 / 222.222f32)

    @Test
    def testBinaryDivide34(): Unit \ Assert =
        assertEq(expected = 0.0000000000000000000000000000044488933f32,
                 0.000000000000000000000000000000987654321f32 / 0.222f32)

    @Test
    def testBinaryDivide35(): Unit \ Assert =
        assertEq(expected = -0.0000000000000000000000000000044488933f32,
                 -0.000000000000000000000000000000987654321f32 / 0.222f32)

    @Test
    def testBinaryDivide36(): Unit \ Assert =
        assertEq(expected = 0.2173300457907714f64, 12.34f64 / 56.78f64)

    @Test
    def testBinaryDivide37(): Unit \ Assert =
        assertEq(expected = 5555561060561060400000000000000000000000000000.0f64,
                 1234567890000000000000000000000000000000000000000.987654321f64 / 222.222f64)

    @Test
    def testBinaryDivide38(): Unit \ Assert =
        assertEq(expected = -5555561060561060400000000000000000000000000000.0f64,
                 -1234567890000000000000000000000000000000000000000.987654321f64 / 222.222f64)

    @Test
    def testBinaryDivide39(): Unit \ Assert =
        assertEq(expected = 0.00000000000000000000000000000000000000044488933378378374f64,
                 0.0000000000000000000000000000000000000000987654321f64 / 0.222f64)

    @Test
    def testBinaryDivide40(): Unit \ Assert =
        assertEq(expected = -0.00000000000000000000000000000000000000044488933378378374f64,
                 -0.0000000000000000000000000000000000000000987654321f64 / 0.222f64)

    @Test
    def testBinaryDivide41(): Unit \ Assert =
        assertEq(expected = 9223372036854775808ii, 9223372036854775808ii / 1ii)

    @Test
    def testBinaryDivide42(): Unit \ Assert =
        assertEq(expected = 400000000000000000000ii, 1200000000000000000000ii / 3ii)

    @Test
    def testBinaryDivide43(): Unit \ Assert =
        assertEq(expected = -400000000000000000000ii, -1200000000000000000000ii / 3ii)

    @Test
    def testBinaryDivide44(): Unit \ Assert =
        assertEq(expected = 0ii, -3ii / 1200000000000000000000ii)

    @Test
    def testBinaryDivide45(): Unit \ Assert =
        assertEq(expected = 9223372036854775809ii, -9223372036854775809ii / -1ii)

    @Test
    def testBinaryDivide46(): Unit \ Assert =
        assertEq(expected = 0, 1 / 0)

    @Test
    def testBinaryDivide47(): Unit \ Assert =
        assertEq(expected = 0i8, 1i8 / 0i8)

    @Test
    def testBinaryDivide48(): Unit \ Assert =
        assertEq(expected = 0i16, 1i16 / 0i16)

    @Test
    def testBinaryDivide49(): Unit \ Assert =
        assertEq(expected = 0i32, 1i32 / 0i32)

    @Test
    def testBinaryDivide50(): Unit \ Assert =
        assertEq(expected = 0i64, 1i64 / 0i64)

    @Test
    def testBinaryDivide51(): Unit \ Assert =
        assertEq(expected = 0ii, 1ii / 0ii)

    @Test
    def testBinaryExponentiate01(): Unit \ Assert =
        assertEq(expected = 1, Int32.pow(base = 2, 0))

    @Test
    def testBinaryExponentiate02(): Unit \ Assert =
        assertEq(expected = -2, Int32.pow(base = -2, 1))

    @Test
    def testBinaryExponentiate03(): Unit \ Assert =
        assertEq(expected = 4, Int32.pow(base = 2, 2))

    @Test
    def testBinaryExponentiate04(): Unit \ Assert =
        assertEq(expected = -2147483648, Int32.pow(base = -2, 31))

    @Test
    def testBinaryExponentiate05(): Unit \ Assert =
        assertEq(expected = 1i8, Int8.pow(base = 2i8, 0i8))

    @Test
    def testBinaryExponentiate06(): Unit \ Assert =
        assertEq(expected = -2i8, Int8.pow(base = -2i8, 1i8))

    @Test
    def testBinaryExponentiate07(): Unit \ Assert =
        assertEq(expected = 4i8, Int8.pow(base = 2i8, 2i8))

    @Test
    def testBinaryExponentiate08(): Unit \ Assert =
        assertEq(expected = -128i8, Int8.pow(base = -2i8, 7i8))

    @Test
    def testBinaryExponentiate09(): Unit \ Assert =
        assertEq(expected = 1i16, Int16.pow(base = 2i16, 0i16))

    @Test
    def testBinaryExponentiate10(): Unit \ Assert =
        assertEq(expected = -2i16, Int16.pow(base = -2i16, 1i16))

    @Test
    def testBinaryExponentiate11(): Unit \ Assert =
        assertEq(expected = 4i16, Int16.pow(base = 2i16, 2i16))

    @Test
    def testBinaryExponentiate12(): Unit \ Assert =
        assertEq(expected = -32768i16, Int16.pow(base = -2i16, 15i16))

    @Test
    def testBinaryExponentiate13(): Unit \ Assert =
        assertEq(expected = 1i32, Int32.pow(base = 2i32, 0i32))

    @Test
    def testBinaryExponentiate14(): Unit \ Assert =
        assertEq(expected = -2i32, Int32.pow(base = -2i32, 1i32))

    @Test
    def testBinaryExponentiate15(): Unit \ Assert =
        assertEq(expected = 4i32, Int32.pow(base = 2i32, 2i32))

    @Test
    def testBinaryExponentiate16(): Unit \ Assert =
        assertEq(expected = -2147483648, Int32.pow(base = -2i32, 31i32))

    @Test
    def testBinaryExponentiate17(): Unit \ Assert =
        assertEq(expected = 1i64, Int64.pow(base = 2i64, 0i64))

    @Test
    def testBinaryExponentiate18(): Unit \ Assert =
        assertEq(expected = -2i64, Int64.pow(base = -2i64, 1i64))

    @Test
    def testBinaryExponentiate19(): Unit \ Assert =
        assertEq(expected = 4i64, Int64.pow(base = 2i64, 2i64))

    @Test
    def testBinaryExponentiate20(): Unit \ Assert =
        assertEq(expected = -9223372036854775808i64, Int64.pow(base = -2i64, 63i64))

    @Test
    def testBinaryExponentiate21(): Unit \ Assert =
        assertEq(expected = 1.0, Float64.pow(base = 2.0, 0.0))

    @Test
    def testBinaryExponentiate22(): Unit \ Assert =
        assertEq(expected = -0.5, Float64.pow(base = -2.0, -1.0))

    @Test
    def testBinaryExponentiate23(): Unit \ Assert =
        assertEq(expected = 0.1, Float64.pow(base = 0.01, 0.5))

    @Test
    def testBinaryExponentiate24(): Unit \ Assert =
        assertEq(expected = 1267650600228229400000000000000.0, Float64.pow(base = -2.0, 100.0))

    @Test
    def testBinaryExponentiate25(): Unit \ Assert =
        assertEq(expected = 1.0f32, Float32.pow(base = 2.0f32, 0.0f32))

    @Test
    def testBinaryExponentiate26(): Unit \ Assert =
        assertEq(expected = -0.5f32, Float32.pow(base = -2.0f32, -1.0f32))

    @Test
    def testBinaryExponentiate27(): Unit \ Assert =
        assertEq(expected = 0.1f32, Float32.pow(base = 0.01f32, 0.5f32))

    @Test
    def testBinaryExponentiate28(): Unit \ Assert =
        assertEq(expected = 1267650600000000000000000000000.0f32, Float32.pow(base = -2.0f32, 100.0f32))

    @Test
    def testBinaryExponentiate29(): Unit \ Assert =
        assertEq(expected = 1.0f64, Float64.pow(base = 2.0f64, 0.0f64))

    @Test
    def testBinaryExponentiate30(): Unit \ Assert =
        assertEq(expected = -0.5f64, Float64.pow(base = -2.0f64, -1.0f64))

    @Test
    def testBinaryExponentiate31(): Unit \ Assert =
        assertEq(expected = 0.1f64, Float64.pow(base = 0.01f64, 0.5f64))

    @Test
    def testBinaryExponentiate32(): Unit \ Assert =
        assertEq(expected = 1267650600228229400000000000000.0f64, Float64.pow(base = -2.0f64, 100.0f64))

}
