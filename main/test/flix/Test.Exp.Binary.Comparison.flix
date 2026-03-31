mod Test.Exp.Binary.Comparison {

    use Assert.assertEq

    @Test
    def testBinaryLess01(): Unit \ Assert =
        assertEq(expected = false, 120000 < 30000)

    @Test
    def testBinaryLess02(): Unit \ Assert =
        assertEq(expected = true, 30000 < 120000)

    @Test
    def testBinaryLess03(): Unit \ Assert =
        assertEq(expected = false, 30000 < 30000)

    @Test
    def testBinaryLess04(): Unit \ Assert =
        assertEq(expected = true, -120000 < -30000)

    @Test
    def testBinaryLess05(): Unit \ Assert =
        assertEq(expected = false, -30000 < -120000)

    @Test
    def testBinaryLess06(): Unit \ Assert =
        assertEq(expected = false, -30000 < -30000)

    @Test
    def testBinaryLess07(): Unit \ Assert =
        assertEq(expected = false, 12i8 < 3i8)

    @Test
    def testBinaryLess08(): Unit \ Assert =
        assertEq(expected = true, 3i8 < 12i8)

    @Test
    def testBinaryLess09(): Unit \ Assert =
        assertEq(expected = false, 3i8 < 3i8)

    @Test
    def testBinaryLess10(): Unit \ Assert =
        assertEq(expected = true, -12i8 < -3i8)

    @Test
    def testBinaryLess11(): Unit \ Assert =
        assertEq(expected = false, -3i8 < -12i8)

    @Test
    def testBinaryLess12(): Unit \ Assert =
        assertEq(expected = false, -3i8 < -3i8)

    @Test
    def testBinaryLess13(): Unit \ Assert =
        assertEq(expected = false, 1200i16 < 300i16)

    @Test
    def testBinaryLess14(): Unit \ Assert =
        assertEq(expected = true, 300i16 < 1200i16)

    @Test
    def testBinaryLess15(): Unit \ Assert =
        assertEq(expected = false, 300i16 < 300i16)

    @Test
    def testBinaryLess16(): Unit \ Assert =
        assertEq(expected = true, -1200i16 < -300i16)

    @Test
    def testBinaryLess17(): Unit \ Assert =
        assertEq(expected = false, -300i16 < -1200i16)

    @Test
    def testBinaryLess18(): Unit \ Assert =
        assertEq(expected = false, -300i16 < -300i16)

    @Test
    def testBinaryLess19(): Unit \ Assert =
        assertEq(expected = false, 120000i32 < 30000i32)

    @Test
    def testBinaryLess20(): Unit \ Assert =
        assertEq(expected = true, 30000i32 < 120000i32)

    @Test
    def testBinaryLess21(): Unit \ Assert =
        assertEq(expected = false, 30000i32 < 30000i32)

    @Test
    def testBinaryLess22(): Unit \ Assert =
        assertEq(expected = true, -120000i32 < -30000i32)

    @Test
    def testBinaryLess23(): Unit \ Assert =
        assertEq(expected = false, -30000i32 < -120000i32)

    @Test
    def testBinaryLess24(): Unit \ Assert =
        assertEq(expected = false, -30000i32 < -30000i32)

    @Test
    def testBinaryLess25(): Unit \ Assert =
        assertEq(expected = false, 12000000000i64 < 3000000000i64)

    @Test
    def testBinaryLess26(): Unit \ Assert =
        assertEq(expected = true, 3000000000i64 < 12000000000i64)

    @Test
    def testBinaryLess27(): Unit \ Assert =
        assertEq(expected = false, 3000000000i64 < 3000000000i64)

    @Test
    def testBinaryLess28(): Unit \ Assert =
        assertEq(expected = true, -12000000000i64 < -3000000000i64)

    @Test
    def testBinaryLess29(): Unit \ Assert =
        assertEq(expected = false, -3000000000i64 < -12000000000i64)

    @Test
    def testBinaryLess30(): Unit \ Assert =
        assertEq(expected = false, -3000000000i64 < -3000000000i64)

    @Test
    def testBinaryLess31(): Unit \ Assert =
        assertEq(expected = false, 120000000000000000000000000000000000000000.0 < 30000000000000000000000000000000000000000.0)

    @Test
    def testBinaryLess32(): Unit \ Assert =
        assertEq(expected = true, 30000000000000000000000000000000000000000.0 < 120000000000000000000000000000000000000000.0)

    @Test
    def testBinaryLess33(): Unit \ Assert =
        assertEq(expected = false, 30000000000000000000000000000000000000000.0 < 30000000000000000000000000000000000000000.0)

    @Test
    def testBinaryLess34(): Unit \ Assert =
        assertEq(expected = true, -120000000000000000000000000000000000000000.0 < -30000000000000000000000000000000000000000.0)

    @Test
    def testBinaryLess35(): Unit \ Assert =
        assertEq(expected = false, -30000000000000000000000000000000000000000.0 < -120000000000000000000000000000000000000000.0)

    @Test
    def testBinaryLess36(): Unit \ Assert =
        assertEq(expected = false, -30000000000000000000000000000000000000000.0 < -30000000000000000000000000000000000000000.0)

    @Test
    def testBinaryLess37(): Unit \ Assert =
        assertEq(expected = false, 1200000000000000000000.0f32 < 300000000000000000000.0f32)

    @Test
    def testBinaryLess38(): Unit \ Assert =
        assertEq(expected = true, 300000000000000000000.0f32 < 1200000000000000000000.0f32)

    @Test
    def testBinaryLess39(): Unit \ Assert =
        assertEq(expected = false, 300000000000000000000.0f32 < 300000000000000000000.0f32)

    @Test
    def testBinaryLess40(): Unit \ Assert =
        assertEq(expected = true, -1200000000000000000000.0f32 < -300000000000000000000.0f32)

    @Test
    def testBinaryLess41(): Unit \ Assert =
        assertEq(expected = false, -300000000000000000000.0f32 < -1200000000000000000000.0f32)

    @Test
    def testBinaryLess42(): Unit \ Assert =
        assertEq(expected = false, -300000000000000000000.0f32 < -300000000000000000000.0f32)

    @Test
    def testBinaryLess43(): Unit \ Assert =
        assertEq(expected = false, 120000000000000000000000000000000000000000.0f64 < 30000000000000000000000000000000000000000.0f64)

    @Test
    def testBinaryLess44(): Unit \ Assert =
        assertEq(expected = true, 30000000000000000000000000000000000000000.0f64 < 120000000000000000000000000000000000000000.0f64)

    @Test
    def testBinaryLess45(): Unit \ Assert =
        assertEq(expected = false, 30000000000000000000000000000000000000000.0f64 < 30000000000000000000000000000000000000000.0f64)

    @Test
    def testBinaryLess46(): Unit \ Assert =
        assertEq(expected = true, -120000000000000000000000000000000000000000.0f64 < -30000000000000000000000000000000000000000.0f64)

    @Test
    def testBinaryLess47(): Unit \ Assert =
        assertEq(expected = false, -30000000000000000000000000000000000000000.0f64 < -120000000000000000000000000000000000000000.0f64)

    @Test
    def testBinaryLess48(): Unit \ Assert =
        assertEq(expected = false, -30000000000000000000000000000000000000000.0f64 < -30000000000000000000000000000000000000000.0f64)

    @Test
    def testBinaryLess49(): Unit \ Assert =
        assertEq(expected = false, 12000000000000000000ii < 3000000000000000000ii)

    @Test
    def testBinaryLess50(): Unit \ Assert =
        assertEq(expected = true, 3000000000000000000ii < 12000000000000000000ii)

    @Test
    def testBinaryLess51(): Unit \ Assert =
        assertEq(expected = false, 3000000000000000000ii < 3000000000000000000ii)

    @Test
    def testBinaryLess52(): Unit \ Assert =
        assertEq(expected = true, -12000000000000000000ii < -3000000000000000000ii)

    @Test
    def testBinaryLess53(): Unit \ Assert =
        assertEq(expected = false, -3000000000000000000ii < -12000000000000000000ii)

    @Test
    def testBinaryLess54(): Unit \ Assert =
        assertEq(expected = false, -3000000000000000000ii < -3000000000000000000ii)

    @Test
    def testBinaryLess55(): Unit \ Assert =
        assertEq(expected = false, '十' < '\u0000')

    @Test
    def testBinaryLess56(): Unit \ Assert =
        assertEq(expected = true, '\u0000' < '十')

    @Test
    def testBinaryLess57(): Unit \ Assert =
        assertEq(expected = false, '\u0000' < '\u0000')

    @Test
    def testBinaryLessEqual01(): Unit \ Assert =
        assertEq(expected = false, 120000 <= 30000)

    @Test
    def testBinaryLessEqual02(): Unit \ Assert =
        assertEq(expected = true, 30000 <= 120000)

    @Test
    def testBinaryLessEqual03(): Unit \ Assert =
        assertEq(expected = true, 30000 <= 30000)

    @Test
    def testBinaryLessEqual04(): Unit \ Assert =
        assertEq(expected = true, -120000 <= -30000)

    @Test
    def testBinaryLessEqual05(): Unit \ Assert =
        assertEq(expected = false, -30000 <= -120000)

    @Test
    def testBinaryLessEqual06(): Unit \ Assert =
        assertEq(expected = true, -30000 <= -30000)

    @Test
    def testBinaryLessEqual07(): Unit \ Assert =
        assertEq(expected = false, 12i8 <= 3i8)

    @Test
    def testBinaryLessEqual08(): Unit \ Assert =
        assertEq(expected = true, 3i8 <= 12i8)

    @Test
    def testBinaryLessEqual09(): Unit \ Assert =
        assertEq(expected = true, 3i8 <= 3i8)

    @Test
    def testBinaryLessEqual10(): Unit \ Assert =
        assertEq(expected = true, -12i8 <= -3i8)

    @Test
    def testBinaryLessEqual11(): Unit \ Assert =
        assertEq(expected = false, -3i8 <= -12i8)

    @Test
    def testBinaryLessEqual12(): Unit \ Assert =
        assertEq(expected = true, -3i8 <= -3i8)

    @Test
    def testBinaryLessEqual13(): Unit \ Assert =
        assertEq(expected = false, 1200i16 <= 300i16)

    @Test
    def testBinaryLessEqual14(): Unit \ Assert =
        assertEq(expected = true, 300i16 <= 1200i16)

    @Test
    def testBinaryLessEqual15(): Unit \ Assert =
        assertEq(expected = true, 300i16 <= 300i16)

    @Test
    def testBinaryLessEqual16(): Unit \ Assert =
        assertEq(expected = true, -1200i16 <= -300i16)

    @Test
    def testBinaryLessEqual17(): Unit \ Assert =
        assertEq(expected = false, -300i16 <= -1200i16)

    @Test
    def testBinaryLessEqual18(): Unit \ Assert =
        assertEq(expected = true, -300i16 <= -300i16)

    @Test
    def testBinaryLessEqual19(): Unit \ Assert =
        assertEq(expected = false, 120000i32 <= 30000i32)

    @Test
    def testBinaryLessEqual20(): Unit \ Assert =
        assertEq(expected = true, 30000i32 <= 120000i32)

    @Test
    def testBinaryLessEqual21(): Unit \ Assert =
        assertEq(expected = true, 30000i32 <= 30000i32)

    @Test
    def testBinaryLessEqual22(): Unit \ Assert =
        assertEq(expected = true, -120000i32 <= -30000i32)

    @Test
    def testBinaryLessEqual23(): Unit \ Assert =
        assertEq(expected = false, -30000i32 <= -120000i32)

    @Test
    def testBinaryLessEqual24(): Unit \ Assert =
        assertEq(expected = true, -30000i32 <= -30000i32)

    @Test
    def testBinaryLessEqual25(): Unit \ Assert =
        assertEq(expected = false, 12000000000i64 <= 3000000000i64)

    @Test
    def testBinaryLessEqual26(): Unit \ Assert =
        assertEq(expected = true, 3000000000i64 <= 12000000000i64)

    @Test
    def testBinaryLessEqual27(): Unit \ Assert =
        assertEq(expected = true, 3000000000i64 <= 3000000000i64)

    @Test
    def testBinaryLessEqual28(): Unit \ Assert =
        assertEq(expected = true, -12000000000i64 <= -3000000000i64)

    @Test
    def testBinaryLessEqual29(): Unit \ Assert =
        assertEq(expected = false, -3000000000i64 <= -12000000000i64)

    @Test
    def testBinaryLessEqual30(): Unit \ Assert =
        assertEq(expected = true, -3000000000i64 <= -3000000000i64)

    @Test
    def testBinaryLessEqual31(): Unit \ Assert =
        assertEq(expected = false, 120000000000000000000000000000000000000000.0 <= 30000000000000000000000000000000000000000.0)

    @Test
    def testBinaryLessEqual32(): Unit \ Assert =
        assertEq(expected = true, 30000000000000000000000000000000000000000.0 <= 120000000000000000000000000000000000000000.0)

    @Test
    def testBinaryLessEqual33(): Unit \ Assert =
        assertEq(expected = true, 30000000000000000000000000000000000000000.0 <= 30000000000000000000000000000000000000000.0)

    @Test
    def testBinaryLessEqual34(): Unit \ Assert =
        assertEq(expected = true, -120000000000000000000000000000000000000000.0 <= -30000000000000000000000000000000000000000.0)

    @Test
    def testBinaryLessEqual35(): Unit \ Assert =
        assertEq(expected = false, -30000000000000000000000000000000000000000.0 <= -120000000000000000000000000000000000000000.0)

    @Test
    def testBinaryLessEqual36(): Unit \ Assert =
        assertEq(expected = true, -30000000000000000000000000000000000000000.0 <= -30000000000000000000000000000000000000000.0)

    @Test
    def testBinaryLessEqual37(): Unit \ Assert =
        assertEq(expected = false, 1200000000000000000000.0f32 <= 300000000000000000000.0f32)

    @Test
    def testBinaryLessEqual38(): Unit \ Assert =
        assertEq(expected = true, 300000000000000000000.0f32 <= 1200000000000000000000.0f32)

    @Test
    def testBinaryLessEqual39(): Unit \ Assert =
        assertEq(expected = true, 300000000000000000000.0f32 <= 300000000000000000000.0f32)

    @Test
    def testBinaryLessEqual40(): Unit \ Assert =
        assertEq(expected = true, -1200000000000000000000.0f32 <= -300000000000000000000.0f32)

    @Test
    def testBinaryLessEqual41(): Unit \ Assert =
        assertEq(expected = false, -300000000000000000000.0f32 <= -1200000000000000000000.0f32)

    @Test
    def testBinaryLessEqual42(): Unit \ Assert =
        assertEq(expected = true, -300000000000000000000.0f32 <= -300000000000000000000.0f32)

    @Test
    def testBinaryLessEqual43(): Unit \ Assert =
        assertEq(expected = false, 120000000000000000000000000000000000000000.0f64 <= 30000000000000000000000000000000000000000.0f64)

    @Test
    def testBinaryLessEqual44(): Unit \ Assert =
        assertEq(expected = true, 30000000000000000000000000000000000000000.0f64 <= 120000000000000000000000000000000000000000.0f64)

    @Test
    def testBinaryLessEqual45(): Unit \ Assert =
        assertEq(expected = true, 30000000000000000000000000000000000000000.0f64 <= 30000000000000000000000000000000000000000.0f64)

    @Test
    def testBinaryLessEqual46(): Unit \ Assert =
        assertEq(expected = true, -120000000000000000000000000000000000000000.0f64 <= -30000000000000000000000000000000000000000.0f64)

    @Test
    def testBinaryLessEqual47(): Unit \ Assert =
        assertEq(expected = false, -30000000000000000000000000000000000000000.0f64 <= -120000000000000000000000000000000000000000.0f64)

    @Test
    def testBinaryLessEqual48(): Unit \ Assert =
        assertEq(expected = true, -30000000000000000000000000000000000000000.0f64 <= -30000000000000000000000000000000000000000.0f64)

    @Test
    def testBinaryLessEqual49(): Unit \ Assert =
        assertEq(expected = false, 12000000000000000000ii <= 3000000000000000000ii)

    @Test
    def testBinaryLessEqual50(): Unit \ Assert =
        assertEq(expected = true, 3000000000000000000ii <= 12000000000000000000ii)

    @Test
    def testBinaryLessEqual51(): Unit \ Assert =
        assertEq(expected = true, 3000000000000000000ii <= 3000000000000000000ii)

    @Test
    def testBinaryLessEqual52(): Unit \ Assert =
        assertEq(expected = true, -12000000000000000000ii <= -3000000000000000000ii)

    @Test
    def testBinaryLessEqual53(): Unit \ Assert =
        assertEq(expected = false, -3000000000000000000ii <= -12000000000000000000ii)

    @Test
    def testBinaryLessEqual54(): Unit \ Assert =
        assertEq(expected = true, -3000000000000000000ii <= -3000000000000000000ii)

    @Test
    def testBinaryLessEqual55(): Unit \ Assert =
        assertEq(expected = false, '十' <= '\u0000')

    @Test
    def testBinaryLessEqual56(): Unit \ Assert =
        assertEq(expected = true, '\u0000' <= '十')

    @Test
    def testBinaryLessEqual57(): Unit \ Assert =
        assertEq(expected = true, '\u0000' <= '\u0000')

    @Test
    def testBinaryGreater01(): Unit \ Assert =
        assertEq(expected = true, 120000 > 30000)

    @Test
    def testBinaryGreater02(): Unit \ Assert =
        assertEq(expected = false, 30000 > 120000)

    @Test
    def testBinaryGreater03(): Unit \ Assert =
        assertEq(expected = false, 30000 > 30000)

    @Test
    def testBinaryGreater04(): Unit \ Assert =
        assertEq(expected = false, -120000 > -30000)

    @Test
    def testBinaryGreater05(): Unit \ Assert =
        assertEq(expected = true, -30000 > -120000)

    @Test
    def testBinaryGreater06(): Unit \ Assert =
        assertEq(expected = false, -30000 > -30000)

    @Test
    def testBinaryGreater07(): Unit \ Assert =
        assertEq(expected = true, 12i8 > 3i8)

    @Test
    def testBinaryGreater08(): Unit \ Assert =
        assertEq(expected = false, 3i8 > 12i8)

    @Test
    def testBinaryGreater09(): Unit \ Assert =
        assertEq(expected = false, 3i8 > 3i8)

    @Test
    def testBinaryGreater10(): Unit \ Assert =
        assertEq(expected = false, -12i8 > -3i8)

    @Test
    def testBinaryGreater11(): Unit \ Assert =
        assertEq(expected = true, -3i8 > -12i8)

    @Test
    def testBinaryGreater12(): Unit \ Assert =
        assertEq(expected = false, -3i8 > -3i8)

    @Test
    def testBinaryGreater13(): Unit \ Assert =
        assertEq(expected = true, 1200i16 > 300i16)

    @Test
    def testBinaryGreater14(): Unit \ Assert =
        assertEq(expected = false, 300i16 > 1200i16)

    @Test
    def testBinaryGreater15(): Unit \ Assert =
        assertEq(expected = false, 300i16 > 300i16)

    @Test
    def testBinaryGreater16(): Unit \ Assert =
        assertEq(expected = false, -1200i16 > -300i16)

    @Test
    def testBinaryGreater17(): Unit \ Assert =
        assertEq(expected = true, -300i16 > -1200i16)

    @Test
    def testBinaryGreater18(): Unit \ Assert =
        assertEq(expected = false, -300i16 > -300i16)

    @Test
    def testBinaryGreater19(): Unit \ Assert =
        assertEq(expected = true, 120000i32 > 30000i32)

    @Test
    def testBinaryGreater20(): Unit \ Assert =
        assertEq(expected = false, 30000i32 > 120000i32)

    @Test
    def testBinaryGreater21(): Unit \ Assert =
        assertEq(expected = false, 30000i32 > 30000i32)

    @Test
    def testBinaryGreater22(): Unit \ Assert =
        assertEq(expected = false, -120000i32 > -30000i32)

    @Test
    def testBinaryGreater23(): Unit \ Assert =
        assertEq(expected = true, -30000i32 > -120000i32)

    @Test
    def testBinaryGreater24(): Unit \ Assert =
        assertEq(expected = false, -30000i32 > -30000i32)

    @Test
    def testBinaryGreater25(): Unit \ Assert =
        assertEq(expected = true, 12000000000i64 > 3000000000i64)

    @Test
    def testBinaryGreater26(): Unit \ Assert =
        assertEq(expected = false, 3000000000i64 > 12000000000i64)

    @Test
    def testBinaryGreater27(): Unit \ Assert =
        assertEq(expected = false, 3000000000i64 > 3000000000i64)

    @Test
    def testBinaryGreater28(): Unit \ Assert =
        assertEq(expected = false, -12000000000i64 > -3000000000i64)

    @Test
    def testBinaryGreater29(): Unit \ Assert =
        assertEq(expected = true, -3000000000i64 > -12000000000i64)

    @Test
    def testBinaryGreater30(): Unit \ Assert =
        assertEq(expected = false, -3000000000i64 > -3000000000i64)

    @Test
    def testBinaryGreater31(): Unit \ Assert =
        assertEq(expected = true, 120000000000000000000000000000000000000000.0 > 30000000000000000000000000000000000000000.0)

    @Test
    def testBinaryGreater32(): Unit \ Assert =
        assertEq(expected = false, 30000000000000000000000000000000000000000.0 > 120000000000000000000000000000000000000000.0)

    @Test
    def testBinaryGreater33(): Unit \ Assert =
        assertEq(expected = false, 30000000000000000000000000000000000000000.0 > 30000000000000000000000000000000000000000.0)

    @Test
    def testBinaryGreater34(): Unit \ Assert =
        assertEq(expected = false, -120000000000000000000000000000000000000000.0 > -30000000000000000000000000000000000000000.0)

    @Test
    def testBinaryGreater35(): Unit \ Assert =
        assertEq(expected = true, -30000000000000000000000000000000000000000.0 > -120000000000000000000000000000000000000000.0)

    @Test
    def testBinaryGreater36(): Unit \ Assert =
        assertEq(expected = false, -30000000000000000000000000000000000000000.0 > -30000000000000000000000000000000000000000.0)

    @Test
    def testBinaryGreater37(): Unit \ Assert =
        assertEq(expected = true, 1200000000000000000000.0f32 > 300000000000000000000.0f32)

    @Test
    def testBinaryGreater38(): Unit \ Assert =
        assertEq(expected = false, 300000000000000000000.0f32 > 1200000000000000000000.0f32)

    @Test
    def testBinaryGreater39(): Unit \ Assert =
        assertEq(expected = false, 300000000000000000000.0f32 > 300000000000000000000.0f32)

    @Test
    def testBinaryGreater40(): Unit \ Assert =
        assertEq(expected = false, -1200000000000000000000.0f32 > -300000000000000000000.0f32)

    @Test
    def testBinaryGreater41(): Unit \ Assert =
        assertEq(expected = true, -300000000000000000000.0f32 > -1200000000000000000000.0f32)

    @Test
    def testBinaryGreater42(): Unit \ Assert =
        assertEq(expected = false, -300000000000000000000.0f32 > -300000000000000000000.0f32)

    @Test
    def testBinaryGreater43(): Unit \ Assert =
        assertEq(expected = true, 120000000000000000000000000000000000000000.0f64 > 30000000000000000000000000000000000000000.0f64)

    @Test
    def testBinaryGreater44(): Unit \ Assert =
        assertEq(expected = false, 30000000000000000000000000000000000000000.0f64 > 120000000000000000000000000000000000000000.0f64)

    @Test
    def testBinaryGreater45(): Unit \ Assert =
        assertEq(expected = false, 30000000000000000000000000000000000000000.0f64 > 30000000000000000000000000000000000000000.0f64)

    @Test
    def testBinaryGreater46(): Unit \ Assert =
        assertEq(expected = false, -120000000000000000000000000000000000000000.0f64 > -30000000000000000000000000000000000000000.0f64)

    @Test
    def testBinaryGreater47(): Unit \ Assert =
        assertEq(expected = true, -30000000000000000000000000000000000000000.0f64 > -120000000000000000000000000000000000000000.0f64)

    @Test
    def testBinaryGreater48(): Unit \ Assert =
        assertEq(expected = false, -30000000000000000000000000000000000000000.0f64 > -30000000000000000000000000000000000000000.0f64)

    @Test
    def testBinaryGreater49(): Unit \ Assert =
        assertEq(expected = true, 12000000000000000000ii > 3000000000000000000ii)

    @Test
    def testBinaryGreater50(): Unit \ Assert =
        assertEq(expected = false, 3000000000000000000ii > 12000000000000000000ii)

    @Test
    def testBinaryGreater51(): Unit \ Assert =
        assertEq(expected = false, 3000000000000000000ii > 3000000000000000000ii)

    @Test
    def testBinaryGreater52(): Unit \ Assert =
        assertEq(expected = false, -12000000000000000000ii > -3000000000000000000ii)

    @Test
    def testBinaryGreater53(): Unit \ Assert =
        assertEq(expected = true, -3000000000000000000ii > -12000000000000000000ii)

    @Test
    def testBinaryGreater54(): Unit \ Assert =
        assertEq(expected = false, -3000000000000000000ii > -3000000000000000000ii)

    @Test
    def testBinaryGreater55(): Unit \ Assert =
        assertEq(expected = true, '十' > '\u0000')

    @Test
    def testBinaryGreater56(): Unit \ Assert =
        assertEq(expected = false, '\u0000' > '十')

    @Test
    def testBinaryGreater57(): Unit \ Assert =
        assertEq(expected = false, '\u0000' > '\u0000')

    @Test
    def testBinaryGreaterEqual01(): Unit \ Assert =
        assertEq(expected = true, 120000 >= 30000)

    @Test
    def testBinaryGreaterEqual02(): Unit \ Assert =
        assertEq(expected = false, 30000 >= 120000)

    @Test
    def testBinaryGreaterEqual03(): Unit \ Assert =
        assertEq(expected = true, 30000 >= 30000)

    @Test
    def testBinaryGreaterEqual04(): Unit \ Assert =
        assertEq(expected = false, -120000 >= -30000)

    @Test
    def testBinaryGreaterEqual05(): Unit \ Assert =
        assertEq(expected = true, -30000 >= -120000)

    @Test
    def testBinaryGreaterEqual06(): Unit \ Assert =
        assertEq(expected = true, -30000 >= -30000)

    @Test
    def testBinaryGreaterEqual07(): Unit \ Assert =
        assertEq(expected = true, 12i8 >= 3i8)

    @Test
    def testBinaryGreaterEqual08(): Unit \ Assert =
        assertEq(expected = false, 3i8 >= 12i8)

    @Test
    def testBinaryGreaterEqual09(): Unit \ Assert =
        assertEq(expected = true, 3i8 >= 3i8)

    @Test
    def testBinaryGreaterEqual10(): Unit \ Assert =
        assertEq(expected = false, -12i8 >= -3i8)

    @Test
    def testBinaryGreaterEqual11(): Unit \ Assert =
        assertEq(expected = true, -3i8 >= -12i8)

    @Test
    def testBinaryGreaterEqual12(): Unit \ Assert =
        assertEq(expected = true, -3i8 >= -3i8)

    @Test
    def testBinaryGreaterEqual13(): Unit \ Assert =
        assertEq(expected = true, 1200i16 >= 300i16)

    @Test
    def testBinaryGreaterEqual14(): Unit \ Assert =
        assertEq(expected = false, 300i16 >= 1200i16)

    @Test
    def testBinaryGreaterEqual15(): Unit \ Assert =
        assertEq(expected = true, 300i16 >= 300i16)

    @Test
    def testBinaryGreaterEqual16(): Unit \ Assert =
        assertEq(expected = false, -1200i16 >= -300i16)

    @Test
    def testBinaryGreaterEqual17(): Unit \ Assert =
        assertEq(expected = true, -300i16 >= -1200i16)

    @Test
    def testBinaryGreaterEqual18(): Unit \ Assert =
        assertEq(expected = true, -300i16 >= -300i16)

    @Test
    def testBinaryGreaterEqual19(): Unit \ Assert =
        assertEq(expected = true, 120000i32 >= 30000i32)

    @Test
    def testBinaryGreaterEqual20(): Unit \ Assert =
        assertEq(expected = false, 30000i32 >= 120000i32)

    @Test
    def testBinaryGreaterEqual21(): Unit \ Assert =
        assertEq(expected = true, 30000i32 >= 30000i32)

    @Test
    def testBinaryGreaterEqual22(): Unit \ Assert =
        assertEq(expected = false, -120000i32 >= -30000i32)

    @Test
    def testBinaryGreaterEqual23(): Unit \ Assert =
        assertEq(expected = true, -30000i32 >= -120000i32)

    @Test
    def testBinaryGreaterEqual24(): Unit \ Assert =
        assertEq(expected = true, -30000i32 >= -30000i32)

    @Test
    def testBinaryGreaterEqual25(): Unit \ Assert =
        assertEq(expected = true, 12000000000i64 >= 3000000000i64)

    @Test
    def testBinaryGreaterEqual26(): Unit \ Assert =
        assertEq(expected = false, 3000000000i64 >= 12000000000i64)

    @Test
    def testBinaryGreaterEqual27(): Unit \ Assert =
        assertEq(expected = true, 3000000000i64 >= 3000000000i64)

    @Test
    def testBinaryGreaterEqual28(): Unit \ Assert =
        assertEq(expected = false, -12000000000i64 >= -3000000000i64)

    @Test
    def testBinaryGreaterEqual29(): Unit \ Assert =
        assertEq(expected = true, -3000000000i64 >= -12000000000i64)

    @Test
    def testBinaryGreaterEqual30(): Unit \ Assert =
        assertEq(expected = true, -3000000000i64 >= -3000000000i64)

    @Test
    def testBinaryGreaterEqual31(): Unit \ Assert =
        assertEq(expected = true, 120000000000000000000000000000000000000000.0 >= 30000000000000000000000000000000000000000.0)

    @Test
    def testBinaryGreaterEqual32(): Unit \ Assert =
        assertEq(expected = false, 30000000000000000000000000000000000000000.0 >= 120000000000000000000000000000000000000000.0)

    @Test
    def testBinaryGreaterEqual33(): Unit \ Assert =
        assertEq(expected = true, 30000000000000000000000000000000000000000.0 >= 30000000000000000000000000000000000000000.0)

    @Test
    def testBinaryGreaterEqual34(): Unit \ Assert =
        assertEq(expected = false, -120000000000000000000000000000000000000000.0 >= -30000000000000000000000000000000000000000.0)

    @Test
    def testBinaryGreaterEqual35(): Unit \ Assert =
        assertEq(expected = true, -30000000000000000000000000000000000000000.0 >= -120000000000000000000000000000000000000000.0)

    @Test
    def testBinaryGreaterEqual36(): Unit \ Assert =
        assertEq(expected = true, -30000000000000000000000000000000000000000.0 >= -30000000000000000000000000000000000000000.0)

    @Test
    def testBinaryGreaterEqual37(): Unit \ Assert =
        assertEq(expected = true, 1200000000000000000000.0f32 >= 300000000000000000000.0f32)

    @Test
    def testBinaryGreaterEqual38(): Unit \ Assert =
        assertEq(expected = false, 300000000000000000000.0f32 >= 1200000000000000000000.0f32)

    @Test
    def testBinaryGreaterEqual39(): Unit \ Assert =
        assertEq(expected = true, 300000000000000000000.0f32 >= 300000000000000000000.0f32)

    @Test
    def testBinaryGreaterEqual40(): Unit \ Assert =
        assertEq(expected = false, -1200000000000000000000.0f32 >= -300000000000000000000.0f32)

    @Test
    def testBinaryGreaterEqual41(): Unit \ Assert =
        assertEq(expected = true, -300000000000000000000.0f32 >= -1200000000000000000000.0f32)

    @Test
    def testBinaryGreaterEqual42(): Unit \ Assert =
        assertEq(expected = true, -300000000000000000000.0f32 >= -300000000000000000000.0f32)

    @Test
    def testBinaryGreaterEqual43(): Unit \ Assert =
        assertEq(expected = true, 120000000000000000000000000000000000000000.0f64 >= 30000000000000000000000000000000000000000.0f64)

    @Test
    def testBinaryGreaterEqual44(): Unit \ Assert =
        assertEq(expected = false, 30000000000000000000000000000000000000000.0f64 >= 120000000000000000000000000000000000000000.0f64)

    @Test
    def testBinaryGreaterEqual45(): Unit \ Assert =
        assertEq(expected = true, 30000000000000000000000000000000000000000.0f64 >= 30000000000000000000000000000000000000000.0f64)

    @Test
    def testBinaryGreaterEqual46(): Unit \ Assert =
        assertEq(expected = false, -120000000000000000000000000000000000000000.0f64 >= -30000000000000000000000000000000000000000.0f64)

    @Test
    def testBinaryGreaterEqual47(): Unit \ Assert =
        assertEq(expected = true, -30000000000000000000000000000000000000000.0f64 >= -120000000000000000000000000000000000000000.0f64)

    @Test
    def testBinaryGreaterEqual48(): Unit \ Assert =
        assertEq(expected = true, -30000000000000000000000000000000000000000.0f64 >= -30000000000000000000000000000000000000000.0f64)

    @Test
    def testBinaryGreaterEqual49(): Unit \ Assert =
        assertEq(expected = true, 12000000000000000000ii >= 3000000000000000000ii)

    @Test
    def testBinaryGreaterEqual50(): Unit \ Assert =
        assertEq(expected = false, 3000000000000000000ii >= 12000000000000000000ii)

    @Test
    def testBinaryGreaterEqual51(): Unit \ Assert =
        assertEq(expected = true, 3000000000000000000ii >= 3000000000000000000ii)

    @Test
    def testBinaryGreaterEqual52(): Unit \ Assert =
        assertEq(expected = false, -12000000000000000000ii >= -3000000000000000000ii)

    @Test
    def testBinaryGreaterEqual53(): Unit \ Assert =
        assertEq(expected = true, -3000000000000000000ii >= -12000000000000000000ii)

    @Test
    def testBinaryGreaterEqual54(): Unit \ Assert =
        assertEq(expected = true, -3000000000000000000ii >= -3000000000000000000ii)

    @Test
    def testBinaryGreaterEqual55(): Unit \ Assert =
        assertEq(expected = true, '十' >= '\u0000')

    @Test
    def testBinaryGreaterEqual56(): Unit \ Assert =
        assertEq(expected = false, '\u0000' >= '十')

    @Test
    def testBinaryGreaterEqual57(): Unit \ Assert =
        assertEq(expected = true, '\u0000' >= '\u0000')

    @Test
    def testBinaryEqual01(): Unit \ Assert =
        assertEq(expected = false, 120000 == 30000)

    @Test
    def testBinaryEqual02(): Unit \ Assert =
        assertEq(expected = false, 30000 == 120000)

    @Test
    def testBinaryEqual03(): Unit \ Assert =
        assertEq(expected = true, 30000 == 30000)

    @Test
    def testBinaryEqual04(): Unit \ Assert =
        assertEq(expected = false, -120000 == -30000)

    @Test
    def testBinaryEqual05(): Unit \ Assert =
        assertEq(expected = false, -30000 == -120000)

    @Test
    def testBinaryEqual06(): Unit \ Assert =
        assertEq(expected = true, -30000 == -30000)

    @Test
    def testBinaryEqual07(): Unit \ Assert =
        assertEq(expected = false, 12i8 == 3i8)

    @Test
    def testBinaryEqual08(): Unit \ Assert =
        assertEq(expected = false, 3i8 == 12i8)

    @Test
    def testBinaryEqual09(): Unit \ Assert =
        assertEq(expected = true, 3i8 == 3i8)

    @Test
    def testBinaryEqual10(): Unit \ Assert =
        assertEq(expected = false, -12i8 == -3i8)

    @Test
    def testBinaryEqual11(): Unit \ Assert =
        assertEq(expected = false, -3i8 == -12i8)

    @Test
    def testBinaryEqual12(): Unit \ Assert =
        assertEq(expected = true, -3i8 == -3i8)

    @Test
    def testBinaryEqual13(): Unit \ Assert =
        assertEq(expected = false, 1200i16 == 300i16)

    @Test
    def testBinaryEqual14(): Unit \ Assert =
        assertEq(expected = false, 300i16 == 1200i16)

    @Test
    def testBinaryEqual15(): Unit \ Assert =
        assertEq(expected = true, 300i16 == 300i16)

    @Test
    def testBinaryEqual16(): Unit \ Assert =
        assertEq(expected = false, -1200i16 == -300i16)

    @Test
    def testBinaryEqual17(): Unit \ Assert =
        assertEq(expected = false, -300i16 == -1200i16)

    @Test
    def testBinaryEqual18(): Unit \ Assert =
        assertEq(expected = true, -300i16 == -300i16)

    @Test
    def testBinaryEqual19(): Unit \ Assert =
        assertEq(expected = false, 120000i32 == 30000i32)

    @Test
    def testBinaryEqual20(): Unit \ Assert =
        assertEq(expected = false, 30000i32 == 120000i32)

    @Test
    def testBinaryEqual21(): Unit \ Assert =
        assertEq(expected = true, 30000i32 == 30000i32)

    @Test
    def testBinaryEqual22(): Unit \ Assert =
        assertEq(expected = false, -120000i32 == -30000i32)

    @Test
    def testBinaryEqual23(): Unit \ Assert =
        assertEq(expected = false, -30000i32 == -120000i32)

    @Test
    def testBinaryEqual24(): Unit \ Assert =
        assertEq(expected = true, -30000i32 == -30000i32)

    @Test
    def testBinaryEqual25(): Unit \ Assert =
        assertEq(expected = false, 12000000000i64 == 3000000000i64)

    @Test
    def testBinaryEqual26(): Unit \ Assert =
        assertEq(expected = false, 3000000000i64 == 12000000000i64)

    @Test
    def testBinaryEqual27(): Unit \ Assert =
        assertEq(expected = true, 3000000000i64 == 3000000000i64)

    @Test
    def testBinaryEqual28(): Unit \ Assert =
        assertEq(expected = false, -12000000000i64 == -3000000000i64)

    @Test
    def testBinaryEqual29(): Unit \ Assert =
        assertEq(expected = false, -3000000000i64 == -12000000000i64)

    @Test
    def testBinaryEqual30(): Unit \ Assert =
        assertEq(expected = true, -3000000000i64 == -3000000000i64)

    @Test
    def testBinaryEqual31(): Unit \ Assert =
        assertEq(expected = false, 120000000000000000000000000000000000000000.0 == 30000000000000000000000000000000000000000.0)

    @Test
    def testBinaryEqual32(): Unit \ Assert =
        assertEq(expected = false, 30000000000000000000000000000000000000000.0 == 120000000000000000000000000000000000000000.0)

    @Test
    def testBinaryEqual33(): Unit \ Assert =
        assertEq(expected = true, 30000000000000000000000000000000000000000.0 == 30000000000000000000000000000000000000000.0)

    @Test
    def testBinaryEqual34(): Unit \ Assert =
        assertEq(expected = false, -120000000000000000000000000000000000000000.0 == -30000000000000000000000000000000000000000.0)

    @Test
    def testBinaryEqual35(): Unit \ Assert =
        assertEq(expected = false, -30000000000000000000000000000000000000000.0 == -120000000000000000000000000000000000000000.0)

    @Test
    def testBinaryEqual36(): Unit \ Assert =
        assertEq(expected = true, -30000000000000000000000000000000000000000.0 == -30000000000000000000000000000000000000000.0)

    @Test
    def testBinaryEqual37(): Unit \ Assert =
        assertEq(expected = false, 1200000000000000000000.0f32 == 300000000000000000000.0f32)

    @Test
    def testBinaryEqual38(): Unit \ Assert =
        assertEq(expected = false, 300000000000000000000.0f32 == 1200000000000000000000.0f32)

    @Test
    def testBinaryEqual39(): Unit \ Assert =
        assertEq(expected = true, 300000000000000000000.0f32 == 300000000000000000000.0f32)

    @Test
    def testBinaryEqual40(): Unit \ Assert =
        assertEq(expected = false, -1200000000000000000000.0f32 == -300000000000000000000.0f32)

    @Test
    def testBinaryEqual41(): Unit \ Assert =
        assertEq(expected = false, -300000000000000000000.0f32 == -1200000000000000000000.0f32)

    @Test
    def testBinaryEqual42(): Unit \ Assert =
        assertEq(expected = true, -300000000000000000000.0f32 == -300000000000000000000.0f32)

    @Test
    def testBinaryEqual43(): Unit \ Assert =
        assertEq(expected = false, 120000000000000000000000000000000000000000.0f64 == 30000000000000000000000000000000000000000.0f64)

    @Test
    def testBinaryEqual44(): Unit \ Assert =
        assertEq(expected = false, 30000000000000000000000000000000000000000.0f64 == 120000000000000000000000000000000000000000.0f64)

    @Test
    def testBinaryEqual45(): Unit \ Assert =
        assertEq(expected = true, 30000000000000000000000000000000000000000.0f64 == 30000000000000000000000000000000000000000.0f64)

    @Test
    def testBinaryEqual46(): Unit \ Assert =
        assertEq(expected = false, -120000000000000000000000000000000000000000.0f64 == -30000000000000000000000000000000000000000.0f64)

    @Test
    def testBinaryEqual47(): Unit \ Assert =
        assertEq(expected = false, -30000000000000000000000000000000000000000.0f64 == -120000000000000000000000000000000000000000.0f64)

    @Test
    def testBinaryEqual48(): Unit \ Assert =
        assertEq(expected = true, -30000000000000000000000000000000000000000.0f64 == -30000000000000000000000000000000000000000.0f64)

    @Test
    def testBinaryEqual49(): Unit \ Assert =
        assertEq(expected = false, 12000000000000000000ii == 3000000000000000000ii)

    @Test
    def testBinaryEqual50(): Unit \ Assert =
        assertEq(expected = false, 3000000000000000000ii == 12000000000000000000ii)

    @Test
    def testBinaryEqual51(): Unit \ Assert =
        assertEq(expected = true, 3000000000000000000ii == 3000000000000000000ii)

    @Test
    def testBinaryEqual52(): Unit \ Assert =
        assertEq(expected = false, -12000000000000000000ii == -3000000000000000000ii)

    @Test
    def testBinaryEqual53(): Unit \ Assert =
        assertEq(expected = false, -3000000000000000000ii == -12000000000000000000ii)

    @Test
    def testBinaryEqual54(): Unit \ Assert =
        assertEq(expected = true, -3000000000000000000ii == -3000000000000000000ii)

    @Test
    def testBinaryEqual55(): Unit \ Assert =
        assertEq(expected = false, '十' == '\u0000')

    @Test
    def testBinaryEqual56(): Unit \ Assert =
        assertEq(expected = false, '\u0000' == '十')

    @Test
    def testBinaryEqual57(): Unit \ Assert =
        assertEq(expected = true, '\u0000' == '\u0000')

    @Test
    def testBinaryNotEqual01(): Unit \ Assert =
        assertEq(expected = true, 120000 != 30000)

    @Test
    def testBinaryNotEqual02(): Unit \ Assert =
        assertEq(expected = true, 30000 != 120000)

    @Test
    def testBinaryNotEqual03(): Unit \ Assert =
        assertEq(expected = false, 30000 != 30000)

    @Test
    def testBinaryNotEqual04(): Unit \ Assert =
        assertEq(expected = true, -120000 != -30000)

    @Test
    def testBinaryNotEqual05(): Unit \ Assert =
        assertEq(expected = true, -30000 != -120000)

    @Test
    def testBinaryNotEqual06(): Unit \ Assert =
        assertEq(expected = false, -30000 != -30000)

    @Test
    def testBinaryNotEqual07(): Unit \ Assert =
        assertEq(expected = true, 12i8 != 3i8)

    @Test
    def testBinaryNotEqual08(): Unit \ Assert =
        assertEq(expected = true, 3i8 != 12i8)

    @Test
    def testBinaryNotEqual09(): Unit \ Assert =
        assertEq(expected = false, 3i8 != 3i8)

    @Test
    def testBinaryNotEqual10(): Unit \ Assert =
        assertEq(expected = true, -12i8 != -3i8)

    @Test
    def testBinaryNotEqual11(): Unit \ Assert =
        assertEq(expected = true, -3i8 != -12i8)

    @Test
    def testBinaryNotEqual12(): Unit \ Assert =
        assertEq(expected = false, -3i8 != -3i8)

    @Test
    def testBinaryNotEqual13(): Unit \ Assert =
        assertEq(expected = true, 1200i16 != 300i16)

    @Test
    def testBinaryNotEqual14(): Unit \ Assert =
        assertEq(expected = true, 300i16 != 1200i16)

    @Test
    def testBinaryNotEqual15(): Unit \ Assert =
        assertEq(expected = false, 300i16 != 300i16)

    @Test
    def testBinaryNotEqual16(): Unit \ Assert =
        assertEq(expected = true, -1200i16 != -300i16)

    @Test
    def testBinaryNotEqual17(): Unit \ Assert =
        assertEq(expected = true, -300i16 != -1200i16)

    @Test
    def testBinaryNotEqual18(): Unit \ Assert =
        assertEq(expected = false, -300i16 != -300i16)

    @Test
    def testBinaryNotEqual19(): Unit \ Assert =
        assertEq(expected = true, 120000i32 != 30000i32)

    @Test
    def testBinaryNotEqual20(): Unit \ Assert =
        assertEq(expected = true, 30000i32 != 120000i32)

    @Test
    def testBinaryNotEqual21(): Unit \ Assert =
        assertEq(expected = false, 30000i32 != 30000i32)

    @Test
    def testBinaryNotEqual22(): Unit \ Assert =
        assertEq(expected = true, -120000i32 != -30000i32)

    @Test
    def testBinaryNotEqual23(): Unit \ Assert =
        assertEq(expected = true, -30000i32 != -120000i32)

    @Test
    def testBinaryNotEqual24(): Unit \ Assert =
        assertEq(expected = false, -30000i32 != -30000i32)

    @Test
    def testBinaryNotEqual25(): Unit \ Assert =
        assertEq(expected = true, 12000000000i64 != 3000000000i64)

    @Test
    def testBinaryNotEqual26(): Unit \ Assert =
        assertEq(expected = true, 3000000000i64 != 12000000000i64)

    @Test
    def testBinaryNotEqual27(): Unit \ Assert =
        assertEq(expected = false, 3000000000i64 != 3000000000i64)

    @Test
    def testBinaryNotEqual28(): Unit \ Assert =
        assertEq(expected = true, -12000000000i64 != -3000000000i64)

    @Test
    def testBinaryNotEqual29(): Unit \ Assert =
        assertEq(expected = true, -3000000000i64 != -12000000000i64)

    @Test
    def testBinaryNotEqual30(): Unit \ Assert =
        assertEq(expected = false, -3000000000i64 != -3000000000i64)

    @Test
    def testBinaryNotEqual31(): Unit \ Assert =
        assertEq(expected = true, 120000000000000000000000000000000000000000.0 != 30000000000000000000000000000000000000000.0)

    @Test
    def testBinaryNotEqual32(): Unit \ Assert =
        assertEq(expected = true, 30000000000000000000000000000000000000000.0 != 120000000000000000000000000000000000000000.0)

    @Test
    def testBinaryNotEqual33(): Unit \ Assert =
        assertEq(expected = false, 30000000000000000000000000000000000000000.0 != 30000000000000000000000000000000000000000.0)

    @Test
    def testBinaryNotEqual34(): Unit \ Assert =
        assertEq(expected = true, -120000000000000000000000000000000000000000.0 != -30000000000000000000000000000000000000000.0)

    @Test
    def testBinaryNotEqual35(): Unit \ Assert =
        assertEq(expected = true, -30000000000000000000000000000000000000000.0 != -120000000000000000000000000000000000000000.0)

    @Test
    def testBinaryNotEqual36(): Unit \ Assert =
        assertEq(expected = false, -30000000000000000000000000000000000000000.0 != -30000000000000000000000000000000000000000.0)

    @Test
    def testBinaryNotEqual37(): Unit \ Assert =
        assertEq(expected = true, 1200000000000000000000.0f32 != 300000000000000000000.0f32)

    @Test
    def testBinaryNotEqual38(): Unit \ Assert =
        assertEq(expected = true, 300000000000000000000.0f32 != 1200000000000000000000.0f32)

    @Test
    def testBinaryNotEqual39(): Unit \ Assert =
        assertEq(expected = false, 300000000000000000000.0f32 != 300000000000000000000.0f32)

    @Test
    def testBinaryNotEqual40(): Unit \ Assert =
        assertEq(expected = true, -1200000000000000000000.0f32 != -300000000000000000000.0f32)

    @Test
    def testBinaryNotEqual41(): Unit \ Assert =
        assertEq(expected = true, -300000000000000000000.0f32 != -1200000000000000000000.0f32)

    @Test
    def testBinaryNotEqual42(): Unit \ Assert =
        assertEq(expected = false, -300000000000000000000.0f32 != -300000000000000000000.0f32)

    @Test
    def testBinaryNotEqual43(): Unit \ Assert =
        assertEq(expected = true, 120000000000000000000000000000000000000000.0f64 != 30000000000000000000000000000000000000000.0f64)

    @Test
    def testBinaryNotEqual44(): Unit \ Assert =
        assertEq(expected = true, 30000000000000000000000000000000000000000.0f64 != 120000000000000000000000000000000000000000.0f64)

    @Test
    def testBinaryNotEqual45(): Unit \ Assert =
        assertEq(expected = false, 30000000000000000000000000000000000000000.0f64 != 30000000000000000000000000000000000000000.0f64)

    @Test
    def testBinaryNotEqual46(): Unit \ Assert =
        assertEq(expected = true, -120000000000000000000000000000000000000000.0f64 != -30000000000000000000000000000000000000000.0f64)

    @Test
    def testBinaryNotEqual47(): Unit \ Assert =
        assertEq(expected = true, -30000000000000000000000000000000000000000.0f64 != -120000000000000000000000000000000000000000.0f64)

    @Test
    def testBinaryNotEqual48(): Unit \ Assert =
        assertEq(expected = false, -30000000000000000000000000000000000000000.0f64 != -30000000000000000000000000000000000000000.0f64)

    @Test
    def testBinaryNotEqual49(): Unit \ Assert =
        assertEq(expected = true, 12000000000000000000ii != 3000000000000000000ii)

    @Test
    def testBinaryNotEqual50(): Unit \ Assert =
        assertEq(expected = true, 3000000000000000000ii != 12000000000000000000ii)

    @Test
    def testBinaryNotEqual51(): Unit \ Assert =
        assertEq(expected = false, 3000000000000000000ii != 3000000000000000000ii)

    @Test
    def testBinaryNotEqual52(): Unit \ Assert =
        assertEq(expected = true, -12000000000000000000ii != -3000000000000000000ii)

    @Test
    def testBinaryNotEqual53(): Unit \ Assert =
        assertEq(expected = true, -3000000000000000000ii != -12000000000000000000ii)

    @Test
    def testBinaryNotEqual54(): Unit \ Assert =
        assertEq(expected = false, -3000000000000000000ii != -3000000000000000000ii)

    @Test
    def testBinaryNotEqual55(): Unit \ Assert =
        assertEq(expected = true, '十' != '\u0000')

    @Test
    def testBinaryNotEqual56(): Unit \ Assert =
        assertEq(expected = true, '\u0000' != '十')

    @Test
    def testBinaryNotEqual57(): Unit \ Assert =
        assertEq(expected = false, '\u0000' != '\u0000')

}
