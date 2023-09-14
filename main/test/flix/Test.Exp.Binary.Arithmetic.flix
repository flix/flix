mod Test.Exp.Binary.Arithmetic {

    @Test
    def testBinaryPlus01(): Bool = (2147483647 + 1) == -2147483648

    @Test
    def testBinaryPlus02(): Bool = (100000 + 400000) == 500000

    @Test
    def testBinaryPlus03(): Bool = (-400000 + 100000) == -300000

    @Test
    def testBinaryPlus04(): Bool = (-100000 + 400000) == 300000

    @Test
    def testBinaryPlus05(): Bool = (-2147483648 + -1) == 2147483647

    @Test
    def testBinaryPlus06(): Bool = (127i8 + 1i8) == -128i8

    @Test
    def testBinaryPlus07(): Bool = (10i8 + 40i8) == 50i8

    @Test
    def testBinaryPlus08(): Bool = (-40i8 + 10i8) == -30i8

    @Test
    def testBinaryPlus09(): Bool = (-10i8 + 40i8) == 30i8

    @Test
    def testBinaryPlus10(): Bool = (-128i8 + -1i8) == 127i8

    @Test
    def testBinaryPlus11(): Bool = (32767i16 + 1i16) == -32768i16

    @Test
    def testBinaryPlus12(): Bool = (1000i16 + 4000i16) == 5000i16

    @Test
    def testBinaryPlus13(): Bool = (-4000i16 + 1000i16) == -3000i16

    @Test
    def testBinaryPlus14(): Bool = (-1000i16 + 4000i16) == 3000i16

    @Test
    def testBinaryPlus15(): Bool = (-32768i16 + -1i16) == 32767i16

    @Test
    def testBinaryPlus16(): Bool = (2147483647i32 + 1i32) == -2147483648i32

    @Test
    def testBinaryPlus17(): Bool = (100000i32 + 400000i32) == 500000i32

    @Test
    def testBinaryPlus18(): Bool = (-400000i32 + 100000i32) == -300000i32

    @Test
    def testBinaryPlus19(): Bool = (-100000i32 + 400000i32) == 300000i32

    @Test
    def testBinaryPlus20(): Bool = (-2147483648i32 + -1i32) == 2147483647i32

    @Test
    def testBinaryPlus21(): Bool = (9223372036854775807i64 + 1i64) == -9223372036854775808i64

    @Test
    def testBinaryPlus22(): Bool = (10000000000i64 + 40000000000i64) == 50000000000i64

    @Test
    def testBinaryPlus23(): Bool = (-40000000000i64 + 10000000000i64) == -30000000000i64

    @Test
    def testBinaryPlus24(): Bool = (-10000000000i64 + 40000000000i64) == 30000000000i64

    @Test
    def testBinaryPlus25(): Bool = (-9223372036854775808i64 + -1i64) == 9223372036854775807i64


    @Test
    def testBinaryPlus26(): Bool = (12.34 + 56.78) == 69.12

    @Test
    def testBinaryPlus27(): Bool = (1234567890000000000000000000000000000000000000000.987654321 + 222.222) ==
                                             1234567890000000000000000000000000000000000000223.209654321

    @Test
    def testBinaryPlus28(): Bool = (-1234567890000000000000000000000000000000000000000.987654321 + 0.0) ==
                                             -1234567890000000000000000000000000000000000000000.987654321

    @Test
    def testBinaryPlus29(): Bool = (0.0000000000000000000000000000000000000000987654321 + 0.222) == 0.222

    @Test
    def testBinaryPlus30(): Bool = (-0.0000000000000000000000000000000000000000987654321 + 0.222) == 0.222

    @Test
    def testBinaryPlus31(): Bool = (12.34f32 + 56.78f32) == 69.119995f32

    @Test
    def testBinaryPlus32(): Bool = (1234567890000000000000000000000000000000000000000.987654321f32 + 222.222f32) ==
                                             1234567890000000000000000000000000000000000000223.209654321f32

    @Test
    def testBinaryPlus33(): Bool = (-1234567890000000000000000000000000000000000000000.987654321f32 + 0.0f32) ==
                                             -1234567890000000000000000000000000000000000000000.987654321f32

    @Test
    def testBinaryPlus34(): Bool = (0.0000000000000000000000000000000000000000987654321f32 + 0.222f32) == 0.222f32

    @Test
    def testBinaryPlus35(): Bool = (-0.0000000000000000000000000000000000000000987654321f32 + 0.222f32) == 0.222f32

    @Test
    def testBinaryPlus36(): Bool = (12.34f32 + 56.78f32) == 69.119995f32

    @Test
    def testBinaryPlus37(): Bool = (1234567890000000000000000000000000000000000000000.987654321f64 + 222.222f64) ==
                                             1234567890000000000000000000000000000000000000223.209654321f64

    @Test
    def testBinaryPlus38(): Bool = (-1234567890000000000000000000000000000000000000000.987654321f64 + 0.0f64) ==
                                             -1234567890000000000000000000000000000000000000000.987654321f64

    @Test
    def testBinaryPlus39(): Bool = (0.0000000000000000000000000000000000000000987654321f64 + 0.222f64) == 0.222f64

    @Test
    def testBinaryPlus40(): Bool = (-0.0000000000000000000000000000000000000000987654321f64 + 0.222f64) == 0.222f64

    @Test
    def testBinaryPlus41(): Bool = (9223372036854775808ii + 1ii) == 9223372036854775809ii

    @Test
    def testBinaryPlus42(): Bool = (10000000000000000000ii + 40000000000000000000ii) == 50000000000000000000ii

    @Test
    def testBinaryPlus43(): Bool = (-40000000000000000000ii + 10000000000000000000ii) == -30000000000000000000ii

    @Test
    def testBinaryPlus44(): Bool = (-10000000000000000000ii + 40000000000000000000ii) == 30000000000000000000ii

    @Test
    def testBinaryPlus45(): Bool = (-9223372036854775809ii + -1ii) == -9223372036854775810ii

    @Test
    def testBinaryMinus01(): Bool = (-2147483648 - 1) == 2147483647

    @Test
    def testBinaryMinus02(): Bool = (400000 - 100000) == 300000

    @Test
    def testBinaryMinus03(): Bool = (-400000 - 100000) == -500000

    @Test
    def testBinaryMinus04(): Bool = (-100000 - 400000) == -500000

    @Test
    def testBinaryMinus05(): Bool = (2147483647 - -1) == -2147483648

    @Test
    def testBinaryMinus06(): Bool = (-128i8 - 1i8) == 127i8

    @Test
    def testBinaryMinus07(): Bool = (40i8 - 10i8) == 30i8

    @Test
    def testBinaryMinus08(): Bool = (-40i8 - 10i8) == -50i8

    @Test
    def testBinaryMinus09(): Bool = (-10i8 - 40i8) == -50i8

    @Test
    def testBinaryMinus10(): Bool = (-128i8 - 1i8) == 127i8

    @Test
    def testBinaryMinus11(): Bool = (-32768i16 - 1i16) == 32767i16

    @Test
    def testBinaryMinus12(): Bool = (4000i16 - 1000i16) == 3000i16

    @Test
    def testBinaryMinus13(): Bool = (-4000i16 - 1000i16) == -5000i16

    @Test
    def testBinaryMinus14(): Bool = (-1000i16 - 4000i16) == -5000i16

    @Test
    def testBinaryMinus15(): Bool = (32767i16 - -1i16) == -32768i16

    @Test
    def testBinaryMinus16(): Bool = (-2147483648i32 - 1i32) == 2147483647i32

    @Test
    def testBinaryMinus17(): Bool = (400000i32 - 100000i32) == 300000i32

    @Test
    def testBinaryMinus18(): Bool = (-400000i32 - 100000i32) == -500000i32

    @Test
    def testBinaryMinus19(): Bool = (-100000i32 - 400000i32) == -500000i32

    @Test
    def testBinaryMinus20(): Bool = (2147483647i32 - -1i32) == -2147483648i32

    @Test
    def testBinaryMinus21(): Bool = (-9223372036854775808i64 - 1i64) == 9223372036854775807i64

    @Test
    def testBinaryMinus22(): Bool = (40000000000i64 - 10000000000i64) == 30000000000i64

    @Test
    def testBinaryMinus23(): Bool = (-40000000000i64 - 10000000000i64) == -50000000000i64

    @Test
    def testBinaryMinus24(): Bool = (-10000000000i64 - 40000000000i64) == -50000000000i64

    @Test
    def testBinaryMinus25(): Bool = (9223372036854775807i64 - -1i64) == -9223372036854775808i64

    @Test
    def testBinaryMinus26(): Bool = (12.34 - 56.78) == -44.44

    @Test
    def testBinaryMinus27(): Bool = (1234567890000000000000000000000000000000000000000.987654321 - 222.222) ==
                                             1234567890000000000000000000000000000000000000000.0

    @Test
    def testBinaryMinus28(): Bool = (-1234567890000000000000000000000000000000000000000.987654321 - 0.0) ==
                                             -1234567890000000000000000000000000000000000000000.0

    @Test
    def testBinaryMinus29(): Bool = (0.0000000000000000000000000000000000000000987654321 - 0.222) == -0.222

    @Test
    def testBinaryMinus30(): Bool = (-0.0000000000000000000000000000000000000000987654321 - 0.222) == -0.222

    @Test
    def testBinaryMinus31(): Bool = (12.34f32 - 56.78f32) == -44.44f32

    @Test
    def testBinaryMinus32(): Bool = (123456789000000000000000000000000000000.987654321f32 - 222.222f32) ==
                                             123456789000000000000000000000000000000.0f32

    @Test
    def testBinaryMinus33(): Bool = (-123456789000000000000000000000000000000.987654321f32 - 0.0f32) ==
                                             -123456789000000000000000000000000000000.0f32

    @Test
    def testBinaryMinus34(): Bool = (0.000000000000000000000000000000987654321f32 - 0.222f32) == -0.222f32

    @Test
    def testBinaryMinus35(): Bool = (-0.000000000000000000000000000000987654321f32 - 0.222f32) == -0.222f32

    @Test
    def testBinaryMinus36(): Bool = (12.34f64 - 56.78f64) == -44.44f64

    @Test
    def testBinaryMinus37(): Bool = (123456789000000000000000000000000000000.987654321f64 - 222.222f64) ==
                                             123456789000000000000000000000000000000.0f64

    @Test
    def testBinaryMinus38(): Bool = (-123456789000000000000000000000000000000.987654321f64 - 0.0f64) ==
                                             -123456789000000000000000000000000000000.0f64

    @Test
    def testBinaryMinus39(): Bool = (0.000000000000000000000000000000987654321f64 - 0.222f64) == -0.222f64

    @Test
    def testBinaryMinus40(): Bool = (-0.000000000000000000000000000000987654321f64 - 0.222f64) == -0.222f64

    @Test
    def testBinaryMinus41(): Bool = (-9223372036854775809ii - 1ii) == -9223372036854775810ii

    @Test
    def testBinaryMinus42(): Bool = (40000000000000000000ii - 10000000000000000000ii) == 30000000000000000000ii

    @Test
    def testBinaryMinus43(): Bool = (-40000000000000000000ii - 10000000000000000000ii) == -50000000000000000000ii

    @Test
    def testBinaryMinus44(): Bool = (-10000000000000000000ii - 40000000000000000000ii) == -50000000000000000000ii

    @Test
    def testBinaryMinus45(): Bool = (9223372036854775808ii - -1ii) == 9223372036854775809ii

    @Test
    def testBinaryTimes01(): Bool = (2147483647 * 2) == -2

    @Test
    def testBinaryTimes02(): Bool = (300 * 200) == 60000

    @Test
    def testBinaryTimes03(): Bool = (-200 * 300) == -60000

    @Test
    def testBinaryTimes04(): Bool = (-200 * -300) == 60000

    @Test
    def testBinaryTimes05(): Bool = (-2147483648 * -1) == -2147483648

    @Test
    def testBinaryTimes06(): Bool = (127i8 * 2i8) == -2i8

    @Test
    def testBinaryTimes07(): Bool = (3i8 * 2i8) == 6i8

    @Test
    def testBinaryTimes08(): Bool = (-2i8 * 3i8) == -6i8

    @Test
    def testBinaryTimes09(): Bool = (-2i8 * -3i8) == 6i8

    @Test
    def testBinaryTimes10(): Bool = (-128i8 * -1i8) == -128i8

    @Test
    def testBinaryTimes11(): Bool = (32767i16 * 2i16) == -2i16

    @Test
    def testBinaryTimes12(): Bool = (30i16 * 20i16) == 600i16

    @Test
    def testBinaryTimes13(): Bool = (-20i16 * 30i16) == -600i16

    @Test
    def testBinaryTimes14(): Bool = (-20i16 * -30i16) == 600i16

    @Test
    def testBinaryTimes15(): Bool = (-32768i16 * -1i16) == -32768i16

    @Test
    def testBinaryTimes16(): Bool = (2147483647i32 * 2i32) == -2i32

    @Test
    def testBinaryTimes17(): Bool = (300i32 * 200i32) == 60000i32

    @Test
    def testBinaryTimes18(): Bool = (-200i32 * 300i32) == -60000i32

    @Test
    def testBinaryTimes19(): Bool = (-200i32 * -300i32) == 60000i32

    @Test
    def testBinaryTimes20(): Bool = (-2147483648i32 * -1i32) == -2147483648i32

    @Test
    def testBinaryTimes21(): Bool = (9223372036854775807i64 * 2i64) == -2i64

    @Test
    def testBinaryTimes22(): Bool = (300000i64 * 200000i64) == 60000000000i64

    @Test
    def testBinaryTimes23(): Bool = (-200000i64 * 300000i64) == -60000000000i64

    @Test
    def testBinaryTimes24(): Bool = (-200000i64 * -300000i64) == 60000000000i64

    @Test
    def testBinaryTimes25(): Bool = (-9223372036854775808i64 * -1i64) == -9223372036854775808i64

    @Test
    def testBinaryTimes26(): Bool = (12.34 * 56.78) == 700.6652

    @Test
    def testBinaryTimes27(): Bool = (1234567890000000000000000000000000000000000000000.987654321 * 222.222) ==
                                              274348145651580030000000000000000000000000000000000.0

    @Test
    def testBinaryTimes28(): Bool = (-1234567890000000000000000000000000000000000000000.987654321 * 222.222) ==
                                              -274348145651580030000000000000000000000000000000000.0

    @Test
    def testBinaryTimes29(): Bool = (0.0000000000000000000000000000000000000000987654321 * 0.222) ==
                                              0.0000000000000000000000000000000000000000219259259262

    @Test
    def testBinaryTimes30(): Bool = (-0.0000000000000000000000000000000000000000987654321 * 0.222) ==
                                              -0.0000000000000000000000000000000000000000219259259262

    @Test
    def testBinaryTimes31(): Bool = (12.34f32 * 56.78f32) == 700.6652f32

    @Test
    def testBinaryTimes32(): Bool = (1234567890000000000000000000000000000000000000000.987654321f32 * 222.222f32) ==
                                              274348145651580030000000000000000000000000000000000.0f32

    @Test
    def testBinaryTimes33(): Bool = (-1234567890000000000000000000000000000000000000000.987654321f32 * 222.222f32) ==
                                              -274348145651580030000000000000000000000000000000000.0f32

    @Test
    def testBinaryTimes34(): Bool = (0.0000000000000000000000000000000000000000987654321f32 * 0.222f32) ==
                                              0.0000000000000000000000000000000000000000219259259262f32

    @Test
    def testBinaryTimes35(): Bool = (-0.0000000000000000000000000000000000000000987654321f32 * 0.222f32) ==
                                              -0.0000000000000000000000000000000000000000219259259262f32

    @Test
    def testBinaryTimes36(): Bool = (12.34f64 * 56.78f64) == 700.6652f64

    @Test
    def testBinaryTimes37(): Bool = (1234567890000000000000000000000000000000000000000.987654321f64 * 222.222f64) ==
                                              274348145651580030000000000000000000000000000000000.0f64

    @Test
    def testBinaryTimes38(): Bool = (-1234567890000000000000000000000000000000000000000.987654321f64 * 222.222f64) ==
                                              -274348145651580030000000000000000000000000000000000.0f64

    @Test
    def testBinaryTimes39(): Bool = (0.0000000000000000000000000000000000000000987654321f64 * 0.222f64) ==
                                              0.0000000000000000000000000000000000000000219259259262f64

    @Test
    def testBinaryTimes40(): Bool = (-0.0000000000000000000000000000000000000000987654321f64 * 0.222f64) ==
                                              -0.0000000000000000000000000000000000000000219259259262f64

    @Test
    def testBinaryTimes41(): Bool = (9223372036854775808ii * 2ii) == 18446744073709551616ii

    @Test
    def testBinaryTimes42(): Bool = (30000000000ii * 20000000000ii) == 600000000000000000000ii

    @Test
    def testBinaryTimes43(): Bool = (-20000000000ii * 30000000000ii) == -600000000000000000000ii

    @Test
    def testBinaryTimes44(): Bool = (-20000000000ii * -30000000000ii) == 600000000000000000000ii

    @Test
    def testBinaryTimes45(): Bool = (-9223372036854775809ii * -1ii) == 9223372036854775809ii

    @Test
    def testBinaryDivide01(): Bool = (2147483647 / 1) == 2147483647

    @Test
    def testBinaryDivide02(): Bool = (1200000 / 3) == 400000

    @Test
    def testBinaryDivide03(): Bool = (-1200000 / 3) == -400000

    @Test
    def testBinaryDivide04(): Bool = (-3 / 1200000) == 0

    @Test
    def testBinaryDivide05(): Bool = (-2147483648 / -1) == -2147483648

    @Test
    def testBinaryDivide06(): Bool = (127i8 / 1i8) == 127i8

    @Test
    def testBinaryDivide07(): Bool = (12i8 / 3i8) == 4i8

    @Test
    def testBinaryDivide08(): Bool = (-12i8 / 3i8) == -4i8

    @Test
    def testBinaryDivide09(): Bool = (-3i8 / 12i8) == 0i8

    @Test
    def testBinaryDivide10(): Bool = (-128i8 / -1i8) == -128i8

    @Test
    def testBinaryDivide11(): Bool = (32767i16 / 1i16) == 32767i16

    @Test
    def testBinaryDivide12(): Bool = (12000i16 / 3i16) == 4000i16

    @Test
    def testBinaryDivide13(): Bool = (-12000i16 / 3i16) == -4000i16

    @Test
    def testBinaryDivide14(): Bool = (-3i16 / 12000i16) == 0i16

    @Test
    def testBinaryDivide15(): Bool = (-32768i16 / -1i16) == -32768i16

    @Test
    def testBinaryDivide16(): Bool = (2147483647i32 / 1i32) == 2147483647i32

    @Test
    def testBinaryDivide17(): Bool = (1200000i32 / 3i32) == 400000i32

    @Test
    def testBinaryDivide18(): Bool = (-1200000i32 / 3i32) == -400000i32

    @Test
    def testBinaryDivide19(): Bool = (-3i32 / 1200000i32) == 0i32

    @Test
    def testBinaryDivide20(): Bool = (-2147483648i32 / -1i32) == -2147483648i32

    @Test
    def testBinaryDivide21(): Bool = (9223372036854775807i64 / 1i64) == 9223372036854775807i64

    @Test
    def testBinaryDivide22(): Bool = (120000000000i64 / 3i64) == 40000000000i64

    @Test
    def testBinaryDivide23(): Bool = (-120000000000i64 / 3i64) == -40000000000i64

    @Test
    def testBinaryDivide24(): Bool = (-3i64 / 120000000000i64) == 0i64

    @Test
    def testBinaryDivide25(): Bool = (-9223372036854775808i64 / -1i64) == -9223372036854775808i64

    @Test
    def testBinaryDivide26(): Bool = (12.34 / 56.78) == 0.2173300457907714

    @Test
    def testBinaryDivide27(): Bool = (1234567890000000000000000000000000000000000000000.987654321 / 222.222) ==
                                               5555561060561060400000000000000000000000000000.0

    @Test
    def testBinaryDivide28(): Bool = (-1234567890000000000000000000000000000000000000000.987654321 / 222.222) ==
                                               -5555561060561060400000000000000000000000000000.0

    @Test
    def testBinaryDivide29(): Bool = (0.0000000000000000000000000000000000000000987654321 / 0.222) ==
                                               0.00000000000000000000000000000000000000044488933378378374

    @Test
    def testBinaryDivide30(): Bool = (-0.0000000000000000000000000000000000000000987654321 / 0.222) ==
                                               -0.00000000000000000000000000000000000000044488933378378374

    @Test
    def testBinaryDivide31(): Bool = (12.34f32 / 56.78f32) == 0.21733005f32

    @Test
    def testBinaryDivide32(): Bool = (123456789000000000000000000000000000000.987654321f32 / 222.222f32) ==
                                               555556080000000000000000000000000000.0f32
    @Test
    def testBinaryDivide33(): Bool = (-123456789000000000000000000000000000000.987654321f32 / 222.222f32) ==
                                               -555556080000000000000000000000000000.0f32

    @Test
    def testBinaryDivide34(): Bool = (0.000000000000000000000000000000987654321f32 / 0.222f32) ==
                                               0.0000000000000000000000000000044488933f32
    @Test
    def testBinaryDivide35(): Bool = (-0.000000000000000000000000000000987654321f32 / 0.222f32) ==
                                               -0.0000000000000000000000000000044488933f32

    @Test
    def testBinaryDivide36(): Bool = (12.34f64 / 56.78f64) == 0.2173300457907714f64

    @Test
    def testBinaryDivide37(): Bool = (1234567890000000000000000000000000000000000000000.987654321f64 / 222.222f64) ==
                                               5555561060561060400000000000000000000000000000.0f64

    @Test
    def testBinaryDivide38(): Bool = (-1234567890000000000000000000000000000000000000000.987654321f64 / 222.222f64) ==
                                               -5555561060561060400000000000000000000000000000.0f64

    @Test
    def testBinaryDivide39(): Bool = (0.0000000000000000000000000000000000000000987654321f64 / 0.222f64) ==
                                               0.00000000000000000000000000000000000000044488933378378374f64

    @Test
    def testBinaryDivide40(): Bool = (-0.0000000000000000000000000000000000000000987654321f64 / 0.222f64) ==
                                               -0.00000000000000000000000000000000000000044488933378378374f64

    @Test
    def testBinaryDivide41(): Bool = (9223372036854775808ii / 1ii) == 9223372036854775808ii

    @Test
    def testBinaryDivide42(): Bool = (1200000000000000000000ii / 3ii) == 400000000000000000000ii

    @Test
    def testBinaryDivide43(): Bool = (-1200000000000000000000ii / 3ii) == -400000000000000000000ii

    @Test
    def testBinaryDivide44(): Bool = (-3ii / 1200000000000000000000ii) == 0ii

    @Test
    def testBinaryDivide45(): Bool = (-9223372036854775809ii / -1ii) == 9223372036854775809ii

    @Test
    def testBinaryDivide46(): Bool = (1 / 0) == 0

    @Test
    def testBinaryDivide47(): Bool = (1i8 / 0i8) == 0i8

    @Test
    def testBinaryDivide48(): Bool = (1i16 / 0i16) == 0i16

    @Test
    def testBinaryDivide49(): Bool = (1i32 / 0i32) == 0i32

    @Test
    def testBinaryDivide50(): Bool = (1i64 / 0i64) == 0i64

    @Test
    def testBinaryDivide51(): Bool = (1ii / 0ii) == 0ii

    @Test
    def testBinaryExponentiate01(): Bool = Int32.pow(2, 0) == 1

    @Test
    def testBinaryExponentiate02(): Bool = Int32.pow(-2, 1) == -2

    @Test
    def testBinaryExponentiate03(): Bool = Int32.pow(2, 2) == 4

    @Test
    def testBinaryExponentiate04(): Bool = Int32.pow(-2, 31) == -2147483648

    @Test
    def testBinaryExponentiate05(): Bool = Int8.pow(2i8, 0i8) == 1i8

    @Test
    def testBinaryExponentiate06(): Bool = Int8.pow(-2i8, 1i8) == -2i8

    @Test
    def testBinaryExponentiate07(): Bool = Int8.pow(2i8, 2i8) == 4i8

    @Test
    def testBinaryExponentiate08(): Bool = Int8.pow(-2i8, 7i8) == -128i8

    @Test
    def testBinaryExponentiate09(): Bool = Int16.pow(2i16, 0i16) == 1i16

    @Test
    def testBinaryExponentiate10(): Bool = Int16.pow(-2i16, 1i16) == -2i16

    @Test
    def testBinaryExponentiate11(): Bool = Int16.pow(2i16, 2i16) == 4i16

    @Test
    def testBinaryExponentiate12(): Bool = Int16.pow(-2i16, 15i16) == -32768i16

    @Test
    def testBinaryExponentiate13(): Bool = Int32.pow(2i32, 0i32) == 1i32

    @Test
    def testBinaryExponentiate14(): Bool = Int32.pow(-2i32, 1i32) == -2i32

    @Test
    def testBinaryExponentiate15(): Bool = Int32.pow(2i32, 2i32) == 4i32

    @Test
    def testBinaryExponentiate16(): Bool = Int32.pow(-2i32, 31i32) == -2147483648

    @Test
    def testBinaryExponentiate17(): Bool = Int64.pow(2i64, 0i64) == 1i64

    @Test
    def testBinaryExponentiate18(): Bool = Int64.pow(-2i64, 1i64) == -2i64

    @Test
    def testBinaryExponentiate19(): Bool = Int64.pow(2i64, 2i64) == 4i64

    @Test
    def testBinaryExponentiate20(): Bool = Int64.pow(-2i64, 63i64) == -9223372036854775808i64

    @Test
    def testBinaryExponentiate21(): Bool = Float64.pow(2.0, 0.0) == 1.0

    @Test
    def testBinaryExponentiate22(): Bool = Float64.pow(-2.0, -1.0) == -0.5

    @Test
    def testBinaryExponentiate23(): Bool = Float64.pow(0.01, 0.5) == 0.1

    @Test
    def testBinaryExponentiate24(): Bool = Float64.pow(-2.0, 100.0) == 1267650600228229400000000000000.0

    @Test
    def testBinaryExponentiate25(): Bool = Float32.pow(2.0f32, 0.0f32) == 1.0f32

    @Test
    def testBinaryExponentiate26(): Bool = Float32.pow(-2.0f32, -1.0f32) == -0.5f32

    @Test
    def testBinaryExponentiate27(): Bool = Float32.pow(0.01f32, 0.5f32) == 0.1f32

    @Test
    def testBinaryExponentiate28(): Bool = Float32.pow(-2.0f32, 100.0f32) == 1267650600000000000000000000000.0f32

    @Test
    def testBinaryExponentiate29(): Bool = Float64.pow(2.0f64, 0.0f64) == 1.0f64

    @Test
    def testBinaryExponentiate30(): Bool = Float64.pow(-2.0f64, -1.0f64) == -0.5f64

    @Test
    def testBinaryExponentiate31(): Bool = Float64.pow(0.01f64, 0.5f64) == 0.1f64

    @Test
    def testBinaryExponentiate32(): Bool = Float64.pow(-2.0f64, 100.0f64) == 1267650600228229400000000000000.0f64

}
