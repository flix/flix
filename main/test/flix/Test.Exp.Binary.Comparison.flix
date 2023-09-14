mod Test.Exp.Binary.Comparison {

    @Test
    def testBinaryLess01(): Bool = (120000 < 30000) == false

    @Test
    def testBinaryLess02(): Bool = 30000 < 120000

    @Test
    def testBinaryLess03(): Bool = (30000 < 30000) == false

    @Test
    def testBinaryLess04(): Bool = -120000 < -30000

    @Test
    def testBinaryLess05(): Bool = (-30000 < -120000) == false

    @Test
    def testBinaryLess06(): Bool = (-30000 < -30000) == false

    @Test
    def testBinaryLess07(): Bool = (12i8 < 3i8) == false

    @Test
    def testBinaryLess08(): Bool = 3i8 < 12i8

    @Test
    def testBinaryLess09(): Bool = (3i8 < 3i8) == false

    @Test
    def testBinaryLess10(): Bool = -12i8 < -3i8

    @Test
    def testBinaryLess11(): Bool = (-3i8 < -12i8) == false

    @Test
    def testBinaryLess12(): Bool = (-3i8 < -3i8) == false

    @Test
    def testBinaryLess13(): Bool = (1200i16 < 300i16) == false

    @Test
    def testBinaryLess14(): Bool = 300i16 < 1200i16

    @Test
    def testBinaryLess15(): Bool = (300i16 < 300i16) == false

    @Test
    def testBinaryLess16(): Bool = -1200i16 < -300i16

    @Test
    def testBinaryLess17(): Bool = (-300i16 < -1200i16) == false

    @Test
    def testBinaryLess18(): Bool = (-300i16 < -300i16) == false

    @Test
    def testBinaryLess19(): Bool = (120000i32 < 30000i32) == false

    @Test
    def testBinaryLess20(): Bool = 30000i32 < 120000i32

    @Test
    def testBinaryLess21(): Bool = (30000i32 < 30000i32) == false

    @Test
    def testBinaryLess22(): Bool = -120000i32 < -30000i32

    @Test
    def testBinaryLess23(): Bool = (-30000i32 < -120000i32) == false

    @Test
    def testBinaryLess24(): Bool = (-30000i32 < -30000i32) == false

    @Test
    def testBinaryLess25(): Bool = (12000000000i64 < 3000000000i64) == false

    @Test
    def testBinaryLess26(): Bool = 3000000000i64 < 12000000000i64

    @Test
    def testBinaryLess27(): Bool = (3000000000i64 < 3000000000i64) == false

    @Test
    def testBinaryLess28(): Bool = -12000000000i64 < -3000000000i64

    @Test
    def testBinaryLess29(): Bool = (-3000000000i64 < -12000000000i64) == false

    @Test
    def testBinaryLess30(): Bool = (-3000000000i64 < -3000000000i64) == false

    @Test
    def testBinaryLess31(): Bool = (120000000000000000000000000000000000000000.0 < 30000000000000000000000000000000000000000.0) == false

    @Test
    def testBinaryLess32(): Bool = 30000000000000000000000000000000000000000.0 < 120000000000000000000000000000000000000000.0

    @Test
    def testBinaryLess33(): Bool = (30000000000000000000000000000000000000000.0 < 30000000000000000000000000000000000000000.0) == false

    @Test
    def testBinaryLess34(): Bool = -120000000000000000000000000000000000000000.0 < -30000000000000000000000000000000000000000.0

    @Test
    def testBinaryLess35(): Bool = (-30000000000000000000000000000000000000000.0 < -120000000000000000000000000000000000000000.0) == false

    @Test
    def testBinaryLess36(): Bool = (-30000000000000000000000000000000000000000.0 < -30000000000000000000000000000000000000000.0) == false

    @Test
    def testBinaryLess37(): Bool = (1200000000000000000000.0f32 < 300000000000000000000.0f32) == false

    @Test
    def testBinaryLess38(): Bool = 300000000000000000000.0f32 < 1200000000000000000000.0f32

    @Test
    def testBinaryLess39(): Bool = (300000000000000000000.0f32 < 300000000000000000000.0f32) == false

    @Test
    def testBinaryLess40(): Bool = -1200000000000000000000.0f32 < -300000000000000000000.0f32

    @Test
    def testBinaryLess41(): Bool = (-300000000000000000000.0f32 < -1200000000000000000000.0f32) == false

    @Test
    def testBinaryLess42(): Bool = (-300000000000000000000.0f32 < -300000000000000000000.0f32) == false

    @Test
    def testBinaryLess43(): Bool = (120000000000000000000000000000000000000000.0f64 < 30000000000000000000000000000000000000000.0f64) == false

    @Test
    def testBinaryLess44(): Bool = 30000000000000000000000000000000000000000.0f64 < 120000000000000000000000000000000000000000.0f64

    @Test
    def testBinaryLess45(): Bool = (30000000000000000000000000000000000000000.0f64 < 30000000000000000000000000000000000000000.0f64) == false

    @Test
    def testBinaryLess46(): Bool = -120000000000000000000000000000000000000000.0f64 < -30000000000000000000000000000000000000000.0f64

    @Test
    def testBinaryLess47(): Bool = (-30000000000000000000000000000000000000000.0f64 < -120000000000000000000000000000000000000000.0f64) == false

    @Test
    def testBinaryLess48(): Bool = (-30000000000000000000000000000000000000000.0f64 < -30000000000000000000000000000000000000000.0f64) == false

    @Test
    def testBinaryLess49(): Bool = (12000000000000000000ii < 3000000000000000000ii) == false

    @Test
    def testBinaryLess50(): Bool = 3000000000000000000ii < 12000000000000000000ii

    @Test
    def testBinaryLess51(): Bool = (3000000000000000000ii < 3000000000000000000ii) == false

    @Test
    def testBinaryLess52(): Bool = -12000000000000000000ii < -3000000000000000000ii

    @Test
    def testBinaryLess53(): Bool = (-3000000000000000000ii < -12000000000000000000ii) == false

    @Test
    def testBinaryLess54(): Bool = (-3000000000000000000ii < -3000000000000000000ii) == false

    @Test
    def testBinaryLess55(): Bool = ('十' < '\u0000') == false

    @Test
    def testBinaryLess56(): Bool = '\u0000' < '十'

    @Test
    def testBinaryLess57(): Bool = ('\u0000' < '\u0000') == false

    @Test
    def testBinaryLessEqual01(): Bool = (120000 <= 30000) == false

    @Test
    def testBinaryLessEqual02(): Bool = 30000 <= 120000

    @Test
    def testBinaryLessEqual03(): Bool = 30000 <= 30000

    @Test
    def testBinaryLessEqual04(): Bool = -120000 <= -30000

    @Test
    def testBinaryLessEqual05(): Bool = (-30000 <= -120000) == false

    @Test
    def testBinaryLessEqual06(): Bool = -30000 <= -30000

    @Test
    def testBinaryLessEqual07(): Bool = (12i8 <= 3i8) == false

    @Test
    def testBinaryLessEqual08(): Bool = 3i8 <= 12i8

    @Test
    def testBinaryLessEqual09(): Bool = 3i8 <= 3i8

    @Test
    def testBinaryLessEqual10(): Bool = -12i8 <= -3i8

    @Test
    def testBinaryLessEqual11(): Bool = (-3i8 <= -12i8) == false

    @Test
    def testBinaryLessEqual12(): Bool = -3i8 <= -3i8

    @Test
    def testBinaryLessEqual13(): Bool = (1200i16 <= 300i16) == false

    @Test
    def testBinaryLessEqual14(): Bool = 300i16 <= 1200i16

    @Test
    def testBinaryLessEqual15(): Bool = 300i16 <= 300i16

    @Test
    def testBinaryLessEqual16(): Bool = -1200i16 <= -300i16

    @Test
    def testBinaryLessEqual17(): Bool = (-300i16 <= -1200i16) == false

    @Test
    def testBinaryLessEqual18(): Bool = -300i16 <= -300i16

    @Test
    def testBinaryLessEqual19(): Bool = (120000i32 <= 30000i32) == false

    @Test
    def testBinaryLessEqual20(): Bool = 30000i32 <= 120000i32

    @Test
    def testBinaryLessEqual21(): Bool = 30000i32 <= 30000i32

    @Test
    def testBinaryLessEqual22(): Bool = -120000i32 <= -30000i32

    @Test
    def testBinaryLessEqual23(): Bool = (-30000i32 <= -120000i32) == false

    @Test
    def testBinaryLessEqual24(): Bool = -30000i32 <= -30000i32

    @Test
    def testBinaryLessEqual25(): Bool = (12000000000i64 <= 3000000000i64) == false

    @Test
    def testBinaryLessEqual26(): Bool = 3000000000i64 <= 12000000000i64

    @Test
    def testBinaryLessEqual27(): Bool = 3000000000i64 <= 3000000000i64

    @Test
    def testBinaryLessEqual28(): Bool = -12000000000i64 <= -3000000000i64

    @Test
    def testBinaryLessEqual29(): Bool = (-3000000000i64 <= -12000000000i64) == false

    @Test
    def testBinaryLessEqual30(): Bool = -3000000000i64 <= -3000000000i64

    @Test
    def testBinaryLessEqual31(): Bool = (120000000000000000000000000000000000000000.0 <= 30000000000000000000000000000000000000000.0) == false

    @Test
    def testBinaryLessEqual32(): Bool = 30000000000000000000000000000000000000000.0 <= 120000000000000000000000000000000000000000.0

    @Test
    def testBinaryLessEqual33(): Bool = 30000000000000000000000000000000000000000.0 <= 30000000000000000000000000000000000000000.0

    @Test
    def testBinaryLessEqual34(): Bool = -120000000000000000000000000000000000000000.0 <= -30000000000000000000000000000000000000000.0

    @Test
    def testBinaryLessEqual35(): Bool = (-30000000000000000000000000000000000000000.0 <= -120000000000000000000000000000000000000000.0) == false

    @Test
    def testBinaryLessEqual36(): Bool = -30000000000000000000000000000000000000000.0 <= -30000000000000000000000000000000000000000.0

    @Test
    def testBinaryLessEqual37(): Bool = (1200000000000000000000.0f32 <= 300000000000000000000.0f32) == false

    @Test
    def testBinaryLessEqual38(): Bool = 300000000000000000000.0f32 <= 1200000000000000000000.0f32

    @Test
    def testBinaryLessEqual39(): Bool = 300000000000000000000.0f32 <= 300000000000000000000.0f32

    @Test
    def testBinaryLessEqual40(): Bool = -1200000000000000000000.0f32 <= -300000000000000000000.0f32

    @Test
    def testBinaryLessEqual41(): Bool = (-300000000000000000000.0f32 <= -1200000000000000000000.0f32) == false

    @Test
    def testBinaryLessEqual42(): Bool = -300000000000000000000.0f32 <= -300000000000000000000.0f32

    @Test
    def testBinaryLessEqual43(): Bool = (120000000000000000000000000000000000000000.0f64 <= 30000000000000000000000000000000000000000.0f64) == false

    @Test
    def testBinaryLessEqual44(): Bool = 30000000000000000000000000000000000000000.0f64 <= 120000000000000000000000000000000000000000.0f64

    @Test
    def testBinaryLessEqual45(): Bool = 30000000000000000000000000000000000000000.0f64 <= 30000000000000000000000000000000000000000.0f64

    @Test
    def testBinaryLessEqual46(): Bool = -120000000000000000000000000000000000000000.0f64 <= -30000000000000000000000000000000000000000.0f64

    @Test
    def testBinaryLessEqual47(): Bool = (-30000000000000000000000000000000000000000.0f64 <= -120000000000000000000000000000000000000000.0f64) == false

    @Test
    def testBinaryLessEqual48(): Bool = -30000000000000000000000000000000000000000.0f64 <= -30000000000000000000000000000000000000000.0f64

    @Test
    def testBinaryLessEqual49(): Bool = (12000000000000000000ii <= 3000000000000000000ii) == false

    @Test
    def testBinaryLessEqual50(): Bool = 3000000000000000000ii <= 12000000000000000000ii

    @Test
    def testBinaryLessEqual51(): Bool = 3000000000000000000ii <= 3000000000000000000ii

    @Test
    def testBinaryLessEqual52(): Bool = -12000000000000000000ii <= -3000000000000000000ii

    @Test
    def testBinaryLessEqual53(): Bool = (-3000000000000000000ii <= -12000000000000000000ii) == false

    @Test
    def testBinaryLessEqual54(): Bool = -3000000000000000000ii <= -3000000000000000000ii

    @Test
    def testBinaryLessEqual55(): Bool = ('十' <= '\u0000') == false

    @Test
    def testBinaryLessEqual56(): Bool = '\u0000' <= '十'

    @Test
    def testBinaryLessEqual57(): Bool = '\u0000' <= '\u0000'

    @Test
    def testBinaryGreater01(): Bool = 120000 > 30000

    @Test
    def testBinaryGreater02(): Bool = (30000 > 120000) == false

    @Test
    def testBinaryGreater03(): Bool = (30000 > 30000) == false

    @Test
    def testBinaryGreater04(): Bool = (-120000 > -30000) == false

    @Test
    def testBinaryGreater05(): Bool = -30000 > -120000

    @Test
    def testBinaryGreater06(): Bool = (-30000 > -30000) == false

    @Test
    def testBinaryGreater07(): Bool = 12i8 > 3i8

    @Test
    def testBinaryGreater08(): Bool = (3i8 > 12i8) == false

    @Test
    def testBinaryGreater09(): Bool = (3i8 > 3i8) == false

    @Test
    def testBinaryGreater10(): Bool = (-12i8 > -3i8) == false

    @Test
    def testBinaryGreater11(): Bool = -3i8 > -12i8

    @Test
    def testBinaryGreater12(): Bool = (-3i8 > -3i8) == false

    @Test
    def testBinaryGreater13(): Bool = 1200i16 > 300i16

    @Test
    def testBinaryGreater14(): Bool = (300i16 > 1200i16) == false

    @Test
    def testBinaryGreater15(): Bool = (300i16 > 300i16) == false

    @Test
    def testBinaryGreater16(): Bool = (-1200i16 > -300i16) == false

    @Test
    def testBinaryGreater17(): Bool = -300i16 > -1200i16

    @Test
    def testBinaryGreater18(): Bool = (-300i16 > -300i16) == false

    @Test
    def testBinaryGreater19(): Bool = 120000i32 > 30000i32

    @Test
    def testBinaryGreater20(): Bool = (30000i32 > 120000i32) == false

    @Test
    def testBinaryGreater21(): Bool = (30000i32 > 30000i32) == false

    @Test
    def testBinaryGreater22(): Bool = (-120000i32 > -30000i32) == false

    @Test
    def testBinaryGreater23(): Bool = -30000i32 > -120000i32

    @Test
    def testBinaryGreater24(): Bool = (-30000i32 > -30000i32) == false

    @Test
    def testBinaryGreater25(): Bool = 12000000000i64 > 3000000000i64

    @Test
    def testBinaryGreater26(): Bool = (3000000000i64 > 12000000000i64) == false

    @Test
    def testBinaryGreater27(): Bool = (3000000000i64 > 3000000000i64) == false

    @Test
    def testBinaryGreater28(): Bool = (-12000000000i64 > -3000000000i64) == false

    @Test
    def testBinaryGreater29(): Bool = -3000000000i64 > -12000000000i64

    @Test
    def testBinaryGreater30(): Bool = (-3000000000i64 > -3000000000i64) == false

    @Test
    def testBinaryGreater31(): Bool = 120000000000000000000000000000000000000000.0 > 30000000000000000000000000000000000000000.0

    @Test
    def testBinaryGreater32(): Bool = (30000000000000000000000000000000000000000.0 > 120000000000000000000000000000000000000000.0) == false

    @Test
    def testBinaryGreater33(): Bool = (30000000000000000000000000000000000000000.0 > 30000000000000000000000000000000000000000.0) == false

    @Test
    def testBinaryGreater34(): Bool = (-120000000000000000000000000000000000000000.0 > -30000000000000000000000000000000000000000.0) == false

    @Test
    def testBinaryGreater35(): Bool = -30000000000000000000000000000000000000000.0 > -120000000000000000000000000000000000000000.0

    @Test
    def testBinaryGreater36(): Bool = (-30000000000000000000000000000000000000000.0 > -30000000000000000000000000000000000000000.0) == false

    @Test
    def testBinaryGreater37(): Bool = 1200000000000000000000.0f32 > 300000000000000000000.0f32

    @Test
    def testBinaryGreater38(): Bool = (300000000000000000000.0f32 > 1200000000000000000000.0f32) == false

    @Test
    def testBinaryGreater39(): Bool = (300000000000000000000.0f32 > 300000000000000000000.0f32) == false

    @Test
    def testBinaryGreater40(): Bool = (-1200000000000000000000.0f32 > -300000000000000000000.0f32) == false

    @Test
    def testBinaryGreater41(): Bool = -300000000000000000000.0f32 > -1200000000000000000000.0f32

    @Test
    def testBinaryGreater42(): Bool = (-300000000000000000000.0f32 > -300000000000000000000.0f32) == false

    @Test
    def testBinaryGreater43(): Bool = 120000000000000000000000000000000000000000.0f64 > 30000000000000000000000000000000000000000.0f64

    @Test
    def testBinaryGreater44(): Bool = (30000000000000000000000000000000000000000.0f64 > 120000000000000000000000000000000000000000.0f64) == false

    @Test
    def testBinaryGreater45(): Bool = (30000000000000000000000000000000000000000.0f64 > 30000000000000000000000000000000000000000.0f64) == false

    @Test
    def testBinaryGreater46(): Bool = (-120000000000000000000000000000000000000000.0f64 > -30000000000000000000000000000000000000000.0f64) == false

    @Test
    def testBinaryGreater47(): Bool = -30000000000000000000000000000000000000000.0f64 > -120000000000000000000000000000000000000000.0f64

    @Test
    def testBinaryGreater48(): Bool = (-30000000000000000000000000000000000000000.0f64 > -30000000000000000000000000000000000000000.0f64) == false

    @Test
    def testBinaryGreater49(): Bool = 12000000000000000000ii > 3000000000000000000ii

    @Test
    def testBinaryGreater50(): Bool = (3000000000000000000ii > 12000000000000000000ii) == false

    @Test
    def testBinaryGreater51(): Bool = (3000000000000000000ii > 3000000000000000000ii) == false

    @Test
    def testBinaryGreater52(): Bool = (-12000000000000000000ii > -3000000000000000000ii) == false

    @Test
    def testBinaryGreater53(): Bool = -3000000000000000000ii > -12000000000000000000ii

    @Test
    def testBinaryGreater54(): Bool = (-3000000000000000000ii > -3000000000000000000ii) == false

    @Test
    def testBinaryGreater55(): Bool = '十' > '\u0000'

    @Test
    def testBinaryGreater56(): Bool = ('\u0000' > '十') == false

    @Test
    def testBinaryGreater57(): Bool = ('\u0000' > '\u0000') == false

    @Test
    def testBinaryGreaterEqual01(): Bool = 120000 >= 30000

    @Test
    def testBinaryGreaterEqual02(): Bool = (30000 >= 120000) == false

    @Test
    def testBinaryGreaterEqual03(): Bool = 30000 >= 30000

    @Test
    def testBinaryGreaterEqual04(): Bool = (-120000 >= -30000) == false

    @Test
    def testBinaryGreaterEqual05(): Bool = -30000 >= -120000

    @Test
    def testBinaryGreaterEqual06(): Bool = -30000 >= -30000

    @Test
    def testBinaryGreaterEqual07(): Bool = 12i8 >= 3i8

    @Test
    def testBinaryGreaterEqual08(): Bool = (3i8 >= 12i8) == false

    @Test
    def testBinaryGreaterEqual09(): Bool = 3i8 >= 3i8

    @Test
    def testBinaryGreaterEqual10(): Bool = (-12i8 >= -3i8) == false

    @Test
    def testBinaryGreaterEqual11(): Bool = -3i8 >= -12i8

    @Test
    def testBinaryGreaterEqual12(): Bool = -3i8 >= -3i8

    @Test
    def testBinaryGreaterEqual13(): Bool = 1200i16 >= 300i16

    @Test
    def testBinaryGreaterEqual14(): Bool = (300i16 >= 1200i16) == false

    @Test
    def testBinaryGreaterEqual15(): Bool = 300i16 >= 300i16

    @Test
    def testBinaryGreaterEqual16(): Bool = (-1200i16 >= -300i16) == false

    @Test
    def testBinaryGreaterEqual17(): Bool = -300i16 >= -1200i16

    @Test
    def testBinaryGreaterEqual18(): Bool = -300i16 >= -300i16

    @Test
    def testBinaryGreaterEqual19(): Bool = 120000i32 >= 30000i32

    @Test
    def testBinaryGreaterEqual20(): Bool = (30000i32 >= 120000i32) == false

    @Test
    def testBinaryGreaterEqual21(): Bool = 30000i32 >= 30000i32

    @Test
    def testBinaryGreaterEqual22(): Bool = (-120000i32 >= -30000i32) == false

    @Test
    def testBinaryGreaterEqual23(): Bool = -30000i32 >= -120000i32

    @Test
    def testBinaryGreaterEqual24(): Bool = -30000i32 >= -30000i32

    @Test
    def testBinaryGreaterEqual25(): Bool = 12000000000i64 >= 3000000000i64

    @Test
    def testBinaryGreaterEqual26(): Bool = (3000000000i64 >= 12000000000i64) == false

    @Test
    def testBinaryGreaterEqual27(): Bool = 3000000000i64 >= 3000000000i64

    @Test
    def testBinaryGreaterEqual28(): Bool = (-12000000000i64 >= -3000000000i64) == false

    @Test
    def testBinaryGreaterEqual29(): Bool = -3000000000i64 >= -12000000000i64

    @Test
    def testBinaryGreaterEqual30(): Bool = -3000000000i64 >= -3000000000i64

    @Test
    def testBinaryGreaterEqual31(): Bool = 120000000000000000000000000000000000000000.0 >= 30000000000000000000000000000000000000000.0

    @Test
    def testBinaryGreaterEqual32(): Bool = (30000000000000000000000000000000000000000.0 >= 120000000000000000000000000000000000000000.0) == false

    @Test
    def testBinaryGreaterEqual33(): Bool = 30000000000000000000000000000000000000000.0 >= 30000000000000000000000000000000000000000.0

    @Test
    def testBinaryGreaterEqual34(): Bool = (-120000000000000000000000000000000000000000.0 >= -30000000000000000000000000000000000000000.0) == false

    @Test
    def testBinaryGreaterEqual35(): Bool = -30000000000000000000000000000000000000000.0 >= -120000000000000000000000000000000000000000.0

    @Test
    def testBinaryGreaterEqual36(): Bool = -30000000000000000000000000000000000000000.0 >= -30000000000000000000000000000000000000000.0

    @Test
    def testBinaryGreaterEqual37(): Bool = 1200000000000000000000.0f32 >= 300000000000000000000.0f32

    @Test
    def testBinaryGreaterEqual38(): Bool = (300000000000000000000.0f32 >= 1200000000000000000000.0f32) == false

    @Test
    def testBinaryGreaterEqual39(): Bool = 300000000000000000000.0f32 >= 300000000000000000000.0f32

    @Test
    def testBinaryGreaterEqual40(): Bool = (-1200000000000000000000.0f32 >= -300000000000000000000.0f32) == false

    @Test
    def testBinaryGreaterEqual41(): Bool = -300000000000000000000.0f32 >= -1200000000000000000000.0f32

    @Test
    def testBinaryGreaterEqual42(): Bool = -300000000000000000000.0f32 >= -300000000000000000000.0f32

    @Test
    def testBinaryGreaterEqual43(): Bool = 120000000000000000000000000000000000000000.0f64 >= 30000000000000000000000000000000000000000.0f64

    @Test
    def testBinaryGreaterEqual44(): Bool = (30000000000000000000000000000000000000000.0f64 >= 120000000000000000000000000000000000000000.0f64) == false

    @Test
    def testBinaryGreaterEqual45(): Bool = 30000000000000000000000000000000000000000.0f64 >= 30000000000000000000000000000000000000000.0f64

    @Test
    def testBinaryGreaterEqual46(): Bool = (-120000000000000000000000000000000000000000.0f64 >= -30000000000000000000000000000000000000000.0f64) == false

    @Test
    def testBinaryGreaterEqual47(): Bool =-30000000000000000000000000000000000000000.0f64 >= -120000000000000000000000000000000000000000.0f64

    @Test
    def testBinaryGreaterEqual48(): Bool =  -30000000000000000000000000000000000000000.0f64 >= -30000000000000000000000000000000000000000.0f64

    @Test
    def testBinaryGreaterEqual49(): Bool = 12000000000000000000ii >= 3000000000000000000ii

    @Test
    def testBinaryGreaterEqual50(): Bool = (3000000000000000000ii >= 12000000000000000000ii) == false

    @Test
    def testBinaryGreaterEqual51(): Bool = 3000000000000000000ii >= 3000000000000000000ii

    @Test
    def testBinaryGreaterEqual52(): Bool = (-12000000000000000000ii >= -3000000000000000000ii) == false

    @Test
    def testBinaryGreaterEqual53(): Bool = -3000000000000000000ii >= -12000000000000000000ii

    @Test
    def testBinaryGreaterEqual54(): Bool = -3000000000000000000ii >= -3000000000000000000ii

    @Test
    def testBinaryGreaterEqual55(): Bool = '十' >= '\u0000'

    @Test
    def testBinaryGreaterEqual56(): Bool = ('\u0000' >= '十') == false

    @Test
    def testBinaryGreaterEqual57(): Bool = '\u0000' >= '\u0000'

    @Test
    def testBinaryEqual01(): Bool = (120000 == 30000) == false

    @Test
    def testBinaryEqual02(): Bool = (30000 == 120000) == false

    @Test
    def testBinaryEqual03(): Bool = 30000 == 30000

    @Test
    def testBinaryEqual04(): Bool = (-120000 == -30000) == false

    @Test
    def testBinaryEqual05(): Bool = (-30000 == -120000) == false

    @Test
    def testBinaryEqual06(): Bool = -30000 == -30000

    @Test
    def testBinaryEqual07(): Bool = (12i8 == 3i8) == false

    @Test
    def testBinaryEqual08(): Bool = (3i8 == 12i8) == false

    @Test
    def testBinaryEqual09(): Bool = 3i8 == 3i8

    @Test
    def testBinaryEqual10(): Bool = (-12i8 == -3i8) == false

    @Test
    def testBinaryEqual11(): Bool = (-3i8 == -12i8) == false

    @Test
    def testBinaryEqual12(): Bool = -3i8 == -3i8

    @Test
    def testBinaryEqual13(): Bool = (1200i16 == 300i16) == false

    @Test
    def testBinaryEqual14(): Bool = (300i16 == 1200i16) == false

    @Test
    def testBinaryEqual15(): Bool = 300i16 == 300i16

    @Test
    def testBinaryEqual16(): Bool = (-1200i16 == -300i16) == false

    @Test
    def testBinaryEqual17(): Bool = (-300i16 == -1200i16) == false

    @Test
    def testBinaryEqual18(): Bool = -300i16 == -300i16

    @Test
    def testBinaryEqual19(): Bool = (120000i32 == 30000i32) == false

    @Test
    def testBinaryEqual20(): Bool = (30000i32 == 120000i32) == false

    @Test
    def testBinaryEqual21(): Bool = 30000i32 == 30000i32

    @Test
    def testBinaryEqual22(): Bool = (-120000i32 == -30000i32) == false

    @Test
    def testBinaryEqual23(): Bool = (-30000i32 == -120000i32) == false

    @Test
    def testBinaryEqual24(): Bool = -30000i32 == -30000i32

    @Test
    def testBinaryEqual25(): Bool = (12000000000i64 == 3000000000i64) == false

    @Test
    def testBinaryEqual26(): Bool = (3000000000i64 == 12000000000i64) == false

    @Test
    def testBinaryEqual27(): Bool = 3000000000i64 == 3000000000i64

    @Test
    def testBinaryEqual28(): Bool = (-12000000000i64 == -3000000000i64) == false

    @Test
    def testBinaryEqual29(): Bool = (-3000000000i64 == -12000000000i64) == false

    @Test
    def testBinaryEqual30(): Bool = -3000000000i64 == -3000000000i64

    @Test
    def testBinaryEqual31(): Bool = (120000000000000000000000000000000000000000.0 == 30000000000000000000000000000000000000000.0) == false

    @Test
    def testBinaryEqual32(): Bool = (30000000000000000000000000000000000000000.0 == 120000000000000000000000000000000000000000.0) == false

    @Test
    def testBinaryEqual33(): Bool = 30000000000000000000000000000000000000000.0 == 30000000000000000000000000000000000000000.0

    @Test
    def testBinaryEqual34(): Bool = (-120000000000000000000000000000000000000000.0 == -30000000000000000000000000000000000000000.0) == false

    @Test
    def testBinaryEqual35(): Bool = (-30000000000000000000000000000000000000000.0 == -120000000000000000000000000000000000000000.0) == false

    @Test
    def testBinaryEqual36(): Bool = -30000000000000000000000000000000000000000.0 == -30000000000000000000000000000000000000000.0

    @Test
    def testBinaryEqual37(): Bool = (1200000000000000000000.0f32 == 300000000000000000000.0f32) == false

    @Test
    def testBinaryEqual38(): Bool = (300000000000000000000.0f32 == 1200000000000000000000.0f32) == false

    @Test
    def testBinaryEqual39(): Bool = 300000000000000000000.0f32 == 300000000000000000000.0f32

    @Test
    def testBinaryEqual40(): Bool = (-1200000000000000000000.0f32 == -300000000000000000000.0f32) == false

    @Test
    def testBinaryEqual41(): Bool = (-300000000000000000000.0f32 == -1200000000000000000000.0f32) == false

    @Test
    def testBinaryEqual42(): Bool = -300000000000000000000.0f32 == -300000000000000000000.0f32

    @Test
    def testBinaryEqual43(): Bool = (120000000000000000000000000000000000000000.0f64 == 30000000000000000000000000000000000000000.0f64) == false

    @Test
    def testBinaryEqual44(): Bool = (30000000000000000000000000000000000000000.0f64 == 120000000000000000000000000000000000000000.0f64) == false

    @Test
    def testBinaryEqual45(): Bool = 30000000000000000000000000000000000000000.0f64 == 30000000000000000000000000000000000000000.0f64

    @Test
    def testBinaryEqual46(): Bool = (-120000000000000000000000000000000000000000.0f64 == -30000000000000000000000000000000000000000.0f64) == false

    @Test
    def testBinaryEqual47(): Bool = (-30000000000000000000000000000000000000000.0f64 == -120000000000000000000000000000000000000000.0f64) == false

    @Test
    def testBinaryEqual48(): Bool =  -30000000000000000000000000000000000000000.0f64 == -30000000000000000000000000000000000000000.0f64

    @Test
    def testBinaryEqual49(): Bool = (12000000000000000000ii == 3000000000000000000ii) == false

    @Test
    def testBinaryEqual50(): Bool = (3000000000000000000ii == 12000000000000000000ii) == false

    @Test
    def testBinaryEqual51(): Bool = 3000000000000000000ii == 3000000000000000000ii

    @Test
    def testBinaryEqual52(): Bool = (-12000000000000000000ii == -3000000000000000000ii) == false

    @Test
    def testBinaryEqual53(): Bool = (-3000000000000000000ii == -12000000000000000000ii) == false

    @Test
    def testBinaryEqual54(): Bool = -3000000000000000000ii == -3000000000000000000ii

    @Test
    def testBinaryEqual55(): Bool = ('十' == '\u0000') == false

    @Test
    def testBinaryEqual56(): Bool = ('\u0000' == '十') == false

    @Test
    def testBinaryEqual57(): Bool = '\u0000' == '\u0000'

    @Test
    def testBinaryNotEqual01(): Bool = 120000 != 30000

    @Test
    def testBinaryNotEqual02(): Bool = 30000 != 120000

    @Test
    def testBinaryNotEqual03(): Bool = (30000 != 30000) == false

    @Test
    def testBinaryNotEqual04(): Bool = -120000 != -30000

    @Test
    def testBinaryNotEqual05(): Bool = -30000 != -120000

    @Test
    def testBinaryNotEqual06(): Bool = (-30000 != -30000) == false

    @Test
    def testBinaryNotEqual07(): Bool = 12i8 != 3i8

    @Test
    def testBinaryNotEqual08(): Bool = 3i8 != 12i8

    @Test
    def testBinaryNotEqual09(): Bool = (3i8 != 3i8) == false

    @Test
    def testBinaryNotEqual10(): Bool = -12i8 != -3i8

    @Test
    def testBinaryNotEqual11(): Bool = -3i8 != -12i8

    @Test
    def testBinaryNotEqual12(): Bool = (-3i8 != -3i8) == false

    @Test
    def testBinaryNotEqual13(): Bool = 1200i16 != 300i16

    @Test
    def testBinaryNotEqual14(): Bool = 300i16 != 1200i16

    @Test
    def testBinaryNotEqual15(): Bool = (300i16 != 300i16) == false

    @Test
    def testBinaryNotEqual16(): Bool = -1200i16 != -300i16

    @Test
    def testBinaryNotEqual17(): Bool = -300i16 != -1200i16

    @Test
    def testBinaryNotEqual18(): Bool = (-300i16 != -300i16) == false

    @Test
    def testBinaryNotEqual19(): Bool = 120000i32 != 30000i32

    @Test
    def testBinaryNotEqual20(): Bool = 30000i32 != 120000i32

    @Test
    def testBinaryNotEqual21(): Bool = (30000i32 != 30000i32) == false

    @Test
    def testBinaryNotEqual22(): Bool = -120000i32 != -30000i32

    @Test
    def testBinaryNotEqual23(): Bool = -30000i32 != -120000i32

    @Test
    def testBinaryNotEqual24(): Bool = (-30000i32 != -30000i32) == false

    @Test
    def testBinaryNotEqual25(): Bool = 12000000000i64 != 3000000000i64

    @Test
    def testBinaryNotEqual26(): Bool = 3000000000i64 != 12000000000i64

    @Test
    def testBinaryNotEqual27(): Bool = (3000000000i64 != 3000000000i64) == false

    @Test
    def testBinaryNotEqual28(): Bool = -12000000000i64 != -3000000000i64

    @Test
    def testBinaryNotEqual29(): Bool = -3000000000i64 != -12000000000i64

    @Test
    def testBinaryNotEqual30(): Bool = (-3000000000i64 != -3000000000i64) == false

    @Test
    def testBinaryNotEqual31(): Bool = 120000000000000000000000000000000000000000.0 != 30000000000000000000000000000000000000000.0

    @Test
    def testBinaryNotEqual32(): Bool = 30000000000000000000000000000000000000000.0 != 120000000000000000000000000000000000000000.0

    @Test
    def testBinaryNotEqual33(): Bool = (30000000000000000000000000000000000000000.0 != 30000000000000000000000000000000000000000.0) == false

    @Test
    def testBinaryNotEqual34(): Bool = -120000000000000000000000000000000000000000.0 != -30000000000000000000000000000000000000000.0

    @Test
    def testBinaryNotEqual35(): Bool = -30000000000000000000000000000000000000000.0 != -120000000000000000000000000000000000000000.0

    @Test
    def testBinaryNotEqual36(): Bool = (-30000000000000000000000000000000000000000.0 != -30000000000000000000000000000000000000000.0) == false

    @Test
    def testBinaryNotEqual37(): Bool = 1200000000000000000000.0f32 != 300000000000000000000.0f32

    @Test
    def testBinaryNotEqual38(): Bool = 300000000000000000000.0f32 != 1200000000000000000000.0f32

    @Test
    def testBinaryNotEqual39(): Bool = (300000000000000000000.0f32 != 300000000000000000000.0f32) == false

    @Test
    def testBinaryNotEqual40(): Bool = -1200000000000000000000.0f32 != -300000000000000000000.0f32

    @Test
    def testBinaryNotEqual41(): Bool = -300000000000000000000.0f32 != -1200000000000000000000.0f32

    @Test
    def testBinaryNotEqual42(): Bool = (-300000000000000000000.0f32 != -300000000000000000000.0f32) == false

    @Test
    def testBinaryNotEqual43(): Bool = 120000000000000000000000000000000000000000.0f64 != 30000000000000000000000000000000000000000.0f64

    @Test
    def testBinaryNotEqual44(): Bool = 30000000000000000000000000000000000000000.0f64 != 120000000000000000000000000000000000000000.0f64

    @Test
    def testBinaryNotEqual45(): Bool = (30000000000000000000000000000000000000000.0f64 != 30000000000000000000000000000000000000000.0f64) == false

    @Test
    def testBinaryNotEqual46(): Bool = -120000000000000000000000000000000000000000.0f64 != -30000000000000000000000000000000000000000.0f64

    @Test
    def testBinaryNotEqual47(): Bool = -30000000000000000000000000000000000000000.0f64 != -120000000000000000000000000000000000000000.0f64

    @Test
    def testBinaryNotEqual48(): Bool = (-30000000000000000000000000000000000000000.0f64 != -30000000000000000000000000000000000000000.0f64) == false

    @Test
    def testBinaryNotEqual49(): Bool = 12000000000000000000ii != 3000000000000000000ii

    @Test
    def testBinaryNotEqual50(): Bool = 3000000000000000000ii != 12000000000000000000ii

    @Test
    def testBinaryNotEqual51(): Bool = (3000000000000000000ii != 3000000000000000000ii) == false

    @Test
    def testBinaryNotEqual52(): Bool = -12000000000000000000ii != -3000000000000000000ii

    @Test
    def testBinaryNotEqual53(): Bool = -3000000000000000000ii != -12000000000000000000ii

    @Test
    def testBinaryNotEqual54(): Bool = (-3000000000000000000ii != -3000000000000000000ii) == false

    @Test
    def testBinaryNotEqual55(): Bool = '十' != '\u0000'

    @Test
    def testBinaryNotEqual56(): Bool = '\u0000' != '十'

    @Test
    def testBinaryNotEqual57(): Bool = ('\u0000' != '\u0000') == false

}
