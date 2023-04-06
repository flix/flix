mod Test.Exp.Binary.Comparison {

    @test
    def testBinaryLess01(): Bool = (120000 < 30000) == false

    @test
    def testBinaryLess02(): Bool = 30000 < 120000

    @test
    def testBinaryLess03(): Bool = (30000 < 30000) == false

    @test
    def testBinaryLess04(): Bool = -120000 < -30000

    @test
    def testBinaryLess05(): Bool = (-30000 < -120000) == false

    @test
    def testBinaryLess06(): Bool = (-30000 < -30000) == false

    @test
    def testBinaryLess07(): Bool = (12i8 < 3i8) == false

    @test
    def testBinaryLess08(): Bool = 3i8 < 12i8

    @test
    def testBinaryLess09(): Bool = (3i8 < 3i8) == false

    @test
    def testBinaryLess10(): Bool = -12i8 < -3i8

    @test
    def testBinaryLess11(): Bool = (-3i8 < -12i8) == false

    @test
    def testBinaryLess12(): Bool = (-3i8 < -3i8) == false

    @test
    def testBinaryLess13(): Bool = (1200i16 < 300i16) == false

    @test
    def testBinaryLess14(): Bool = 300i16 < 1200i16

    @test
    def testBinaryLess15(): Bool = (300i16 < 300i16) == false

    @test
    def testBinaryLess16(): Bool = -1200i16 < -300i16

    @test
    def testBinaryLess17(): Bool = (-300i16 < -1200i16) == false

    @test
    def testBinaryLess18(): Bool = (-300i16 < -300i16) == false

    @test
    def testBinaryLess19(): Bool = (120000i32 < 30000i32) == false

    @test
    def testBinaryLess20(): Bool = 30000i32 < 120000i32

    @test
    def testBinaryLess21(): Bool = (30000i32 < 30000i32) == false

    @test
    def testBinaryLess22(): Bool = -120000i32 < -30000i32

    @test
    def testBinaryLess23(): Bool = (-30000i32 < -120000i32) == false

    @test
    def testBinaryLess24(): Bool = (-30000i32 < -30000i32) == false

    @test
    def testBinaryLess25(): Bool = (12000000000i64 < 3000000000i64) == false

    @test
    def testBinaryLess26(): Bool = 3000000000i64 < 12000000000i64

    @test
    def testBinaryLess27(): Bool = (3000000000i64 < 3000000000i64) == false

    @test
    def testBinaryLess28(): Bool = -12000000000i64 < -3000000000i64

    @test
    def testBinaryLess29(): Bool = (-3000000000i64 < -12000000000i64) == false

    @test
    def testBinaryLess30(): Bool = (-3000000000i64 < -3000000000i64) == false

    @test
    def testBinaryLess31(): Bool = (120000000000000000000000000000000000000000.0 < 30000000000000000000000000000000000000000.0) == false

    @test
    def testBinaryLess32(): Bool = 30000000000000000000000000000000000000000.0 < 120000000000000000000000000000000000000000.0

    @test
    def testBinaryLess33(): Bool = (30000000000000000000000000000000000000000.0 < 30000000000000000000000000000000000000000.0) == false

    @test
    def testBinaryLess34(): Bool = -120000000000000000000000000000000000000000.0 < -30000000000000000000000000000000000000000.0

    @test
    def testBinaryLess35(): Bool = (-30000000000000000000000000000000000000000.0 < -120000000000000000000000000000000000000000.0) == false

    @test
    def testBinaryLess36(): Bool = (-30000000000000000000000000000000000000000.0 < -30000000000000000000000000000000000000000.0) == false

    @test
    def testBinaryLess37(): Bool = (1200000000000000000000.0f32 < 300000000000000000000.0f32) == false

    @test
    def testBinaryLess38(): Bool = 300000000000000000000.0f32 < 1200000000000000000000.0f32

    @test
    def testBinaryLess39(): Bool = (300000000000000000000.0f32 < 300000000000000000000.0f32) == false

    @test
    def testBinaryLess40(): Bool = -1200000000000000000000.0f32 < -300000000000000000000.0f32

    @test
    def testBinaryLess41(): Bool = (-300000000000000000000.0f32 < -1200000000000000000000.0f32) == false

    @test
    def testBinaryLess42(): Bool = (-300000000000000000000.0f32 < -300000000000000000000.0f32) == false

    @test
    def testBinaryLess43(): Bool = (120000000000000000000000000000000000000000.0f64 < 30000000000000000000000000000000000000000.0f64) == false

    @test
    def testBinaryLess44(): Bool = 30000000000000000000000000000000000000000.0f64 < 120000000000000000000000000000000000000000.0f64

    @test
    def testBinaryLess45(): Bool = (30000000000000000000000000000000000000000.0f64 < 30000000000000000000000000000000000000000.0f64) == false

    @test
    def testBinaryLess46(): Bool = -120000000000000000000000000000000000000000.0f64 < -30000000000000000000000000000000000000000.0f64

    @test
    def testBinaryLess47(): Bool = (-30000000000000000000000000000000000000000.0f64 < -120000000000000000000000000000000000000000.0f64) == false

    @test
    def testBinaryLess48(): Bool = (-30000000000000000000000000000000000000000.0f64 < -30000000000000000000000000000000000000000.0f64) == false

    @test
    def testBinaryLess49(): Bool = (12000000000000000000ii < 3000000000000000000ii) == false

    @test
    def testBinaryLess50(): Bool = 3000000000000000000ii < 12000000000000000000ii

    @test
    def testBinaryLess51(): Bool = (3000000000000000000ii < 3000000000000000000ii) == false

    @test
    def testBinaryLess52(): Bool = -12000000000000000000ii < -3000000000000000000ii

    @test
    def testBinaryLess53(): Bool = (-3000000000000000000ii < -12000000000000000000ii) == false

    @test
    def testBinaryLess54(): Bool = (-3000000000000000000ii < -3000000000000000000ii) == false

    @test
    def testBinaryLess55(): Bool = ('十' < '\u0000') == false

    @test
    def testBinaryLess56(): Bool = '\u0000' < '十'

    @test
    def testBinaryLess57(): Bool = ('\u0000' < '\u0000') == false

    @test
    def testBinaryLessEqual01(): Bool = (120000 <= 30000) == false

    @test
    def testBinaryLessEqual02(): Bool = 30000 <= 120000

    @test
    def testBinaryLessEqual03(): Bool = 30000 <= 30000

    @test
    def testBinaryLessEqual04(): Bool = -120000 <= -30000

    @test
    def testBinaryLessEqual05(): Bool = (-30000 <= -120000) == false

    @test
    def testBinaryLessEqual06(): Bool = -30000 <= -30000

    @test
    def testBinaryLessEqual07(): Bool = (12i8 <= 3i8) == false

    @test
    def testBinaryLessEqual08(): Bool = 3i8 <= 12i8

    @test
    def testBinaryLessEqual09(): Bool = 3i8 <= 3i8

    @test
    def testBinaryLessEqual10(): Bool = -12i8 <= -3i8

    @test
    def testBinaryLessEqual11(): Bool = (-3i8 <= -12i8) == false

    @test
    def testBinaryLessEqual12(): Bool = -3i8 <= -3i8

    @test
    def testBinaryLessEqual13(): Bool = (1200i16 <= 300i16) == false

    @test
    def testBinaryLessEqual14(): Bool = 300i16 <= 1200i16

    @test
    def testBinaryLessEqual15(): Bool = 300i16 <= 300i16

    @test
    def testBinaryLessEqual16(): Bool = -1200i16 <= -300i16

    @test
    def testBinaryLessEqual17(): Bool = (-300i16 <= -1200i16) == false

    @test
    def testBinaryLessEqual18(): Bool = -300i16 <= -300i16

    @test
    def testBinaryLessEqual19(): Bool = (120000i32 <= 30000i32) == false

    @test
    def testBinaryLessEqual20(): Bool = 30000i32 <= 120000i32

    @test
    def testBinaryLessEqual21(): Bool = 30000i32 <= 30000i32

    @test
    def testBinaryLessEqual22(): Bool = -120000i32 <= -30000i32

    @test
    def testBinaryLessEqual23(): Bool = (-30000i32 <= -120000i32) == false

    @test
    def testBinaryLessEqual24(): Bool = -30000i32 <= -30000i32

    @test
    def testBinaryLessEqual25(): Bool = (12000000000i64 <= 3000000000i64) == false

    @test
    def testBinaryLessEqual26(): Bool = 3000000000i64 <= 12000000000i64

    @test
    def testBinaryLessEqual27(): Bool = 3000000000i64 <= 3000000000i64

    @test
    def testBinaryLessEqual28(): Bool = -12000000000i64 <= -3000000000i64

    @test
    def testBinaryLessEqual29(): Bool = (-3000000000i64 <= -12000000000i64) == false

    @test
    def testBinaryLessEqual30(): Bool = -3000000000i64 <= -3000000000i64

    @test
    def testBinaryLessEqual31(): Bool = (120000000000000000000000000000000000000000.0 <= 30000000000000000000000000000000000000000.0) == false

    @test
    def testBinaryLessEqual32(): Bool = 30000000000000000000000000000000000000000.0 <= 120000000000000000000000000000000000000000.0

    @test
    def testBinaryLessEqual33(): Bool = 30000000000000000000000000000000000000000.0 <= 30000000000000000000000000000000000000000.0

    @test
    def testBinaryLessEqual34(): Bool = -120000000000000000000000000000000000000000.0 <= -30000000000000000000000000000000000000000.0

    @test
    def testBinaryLessEqual35(): Bool = (-30000000000000000000000000000000000000000.0 <= -120000000000000000000000000000000000000000.0) == false

    @test
    def testBinaryLessEqual36(): Bool = -30000000000000000000000000000000000000000.0 <= -30000000000000000000000000000000000000000.0

    @test
    def testBinaryLessEqual37(): Bool = (1200000000000000000000.0f32 <= 300000000000000000000.0f32) == false

    @test
    def testBinaryLessEqual38(): Bool = 300000000000000000000.0f32 <= 1200000000000000000000.0f32

    @test
    def testBinaryLessEqual39(): Bool = 300000000000000000000.0f32 <= 300000000000000000000.0f32

    @test
    def testBinaryLessEqual40(): Bool = -1200000000000000000000.0f32 <= -300000000000000000000.0f32

    @test
    def testBinaryLessEqual41(): Bool = (-300000000000000000000.0f32 <= -1200000000000000000000.0f32) == false

    @test
    def testBinaryLessEqual42(): Bool = -300000000000000000000.0f32 <= -300000000000000000000.0f32

    @test
    def testBinaryLessEqual43(): Bool = (120000000000000000000000000000000000000000.0f64 <= 30000000000000000000000000000000000000000.0f64) == false

    @test
    def testBinaryLessEqual44(): Bool = 30000000000000000000000000000000000000000.0f64 <= 120000000000000000000000000000000000000000.0f64

    @test
    def testBinaryLessEqual45(): Bool = 30000000000000000000000000000000000000000.0f64 <= 30000000000000000000000000000000000000000.0f64

    @test
    def testBinaryLessEqual46(): Bool = -120000000000000000000000000000000000000000.0f64 <= -30000000000000000000000000000000000000000.0f64

    @test
    def testBinaryLessEqual47(): Bool = (-30000000000000000000000000000000000000000.0f64 <= -120000000000000000000000000000000000000000.0f64) == false

    @test
    def testBinaryLessEqual48(): Bool = -30000000000000000000000000000000000000000.0f64 <= -30000000000000000000000000000000000000000.0f64

    @test
    def testBinaryLessEqual49(): Bool = (12000000000000000000ii <= 3000000000000000000ii) == false

    @test
    def testBinaryLessEqual50(): Bool = 3000000000000000000ii <= 12000000000000000000ii

    @test
    def testBinaryLessEqual51(): Bool = 3000000000000000000ii <= 3000000000000000000ii

    @test
    def testBinaryLessEqual52(): Bool = -12000000000000000000ii <= -3000000000000000000ii

    @test
    def testBinaryLessEqual53(): Bool = (-3000000000000000000ii <= -12000000000000000000ii) == false

    @test
    def testBinaryLessEqual54(): Bool = -3000000000000000000ii <= -3000000000000000000ii

    @test
    def testBinaryLessEqual55(): Bool = ('十' <= '\u0000') == false

    @test
    def testBinaryLessEqual56(): Bool = '\u0000' <= '十'

    @test
    def testBinaryLessEqual57(): Bool = '\u0000' <= '\u0000'

    @test
    def testBinaryGreater01(): Bool = 120000 > 30000

    @test
    def testBinaryGreater02(): Bool = (30000 > 120000) == false

    @test
    def testBinaryGreater03(): Bool = (30000 > 30000) == false

    @test
    def testBinaryGreater04(): Bool = (-120000 > -30000) == false

    @test
    def testBinaryGreater05(): Bool = -30000 > -120000

    @test
    def testBinaryGreater06(): Bool = (-30000 > -30000) == false

    @test
    def testBinaryGreater07(): Bool = 12i8 > 3i8

    @test
    def testBinaryGreater08(): Bool = (3i8 > 12i8) == false

    @test
    def testBinaryGreater09(): Bool = (3i8 > 3i8) == false

    @test
    def testBinaryGreater10(): Bool = (-12i8 > -3i8) == false

    @test
    def testBinaryGreater11(): Bool = -3i8 > -12i8

    @test
    def testBinaryGreater12(): Bool = (-3i8 > -3i8) == false

    @test
    def testBinaryGreater13(): Bool = 1200i16 > 300i16

    @test
    def testBinaryGreater14(): Bool = (300i16 > 1200i16) == false

    @test
    def testBinaryGreater15(): Bool = (300i16 > 300i16) == false

    @test
    def testBinaryGreater16(): Bool = (-1200i16 > -300i16) == false

    @test
    def testBinaryGreater17(): Bool = -300i16 > -1200i16

    @test
    def testBinaryGreater18(): Bool = (-300i16 > -300i16) == false

    @test
    def testBinaryGreater19(): Bool = 120000i32 > 30000i32

    @test
    def testBinaryGreater20(): Bool = (30000i32 > 120000i32) == false

    @test
    def testBinaryGreater21(): Bool = (30000i32 > 30000i32) == false

    @test
    def testBinaryGreater22(): Bool = (-120000i32 > -30000i32) == false

    @test
    def testBinaryGreater23(): Bool = -30000i32 > -120000i32

    @test
    def testBinaryGreater24(): Bool = (-30000i32 > -30000i32) == false

    @test
    def testBinaryGreater25(): Bool = 12000000000i64 > 3000000000i64

    @test
    def testBinaryGreater26(): Bool = (3000000000i64 > 12000000000i64) == false

    @test
    def testBinaryGreater27(): Bool = (3000000000i64 > 3000000000i64) == false

    @test
    def testBinaryGreater28(): Bool = (-12000000000i64 > -3000000000i64) == false

    @test
    def testBinaryGreater29(): Bool = -3000000000i64 > -12000000000i64

    @test
    def testBinaryGreater30(): Bool = (-3000000000i64 > -3000000000i64) == false

    @test
    def testBinaryGreater31(): Bool = 120000000000000000000000000000000000000000.0 > 30000000000000000000000000000000000000000.0

    @test
    def testBinaryGreater32(): Bool = (30000000000000000000000000000000000000000.0 > 120000000000000000000000000000000000000000.0) == false

    @test
    def testBinaryGreater33(): Bool = (30000000000000000000000000000000000000000.0 > 30000000000000000000000000000000000000000.0) == false

    @test
    def testBinaryGreater34(): Bool = (-120000000000000000000000000000000000000000.0 > -30000000000000000000000000000000000000000.0) == false

    @test
    def testBinaryGreater35(): Bool = -30000000000000000000000000000000000000000.0 > -120000000000000000000000000000000000000000.0

    @test
    def testBinaryGreater36(): Bool = (-30000000000000000000000000000000000000000.0 > -30000000000000000000000000000000000000000.0) == false

    @test
    def testBinaryGreater37(): Bool = 1200000000000000000000.0f32 > 300000000000000000000.0f32

    @test
    def testBinaryGreater38(): Bool = (300000000000000000000.0f32 > 1200000000000000000000.0f32) == false

    @test
    def testBinaryGreater39(): Bool = (300000000000000000000.0f32 > 300000000000000000000.0f32) == false

    @test
    def testBinaryGreater40(): Bool = (-1200000000000000000000.0f32 > -300000000000000000000.0f32) == false

    @test
    def testBinaryGreater41(): Bool = -300000000000000000000.0f32 > -1200000000000000000000.0f32

    @test
    def testBinaryGreater42(): Bool = (-300000000000000000000.0f32 > -300000000000000000000.0f32) == false

    @test
    def testBinaryGreater43(): Bool = 120000000000000000000000000000000000000000.0f64 > 30000000000000000000000000000000000000000.0f64

    @test
    def testBinaryGreater44(): Bool = (30000000000000000000000000000000000000000.0f64 > 120000000000000000000000000000000000000000.0f64) == false

    @test
    def testBinaryGreater45(): Bool = (30000000000000000000000000000000000000000.0f64 > 30000000000000000000000000000000000000000.0f64) == false

    @test
    def testBinaryGreater46(): Bool = (-120000000000000000000000000000000000000000.0f64 > -30000000000000000000000000000000000000000.0f64) == false

    @test
    def testBinaryGreater47(): Bool = -30000000000000000000000000000000000000000.0f64 > -120000000000000000000000000000000000000000.0f64

    @test
    def testBinaryGreater48(): Bool = (-30000000000000000000000000000000000000000.0f64 > -30000000000000000000000000000000000000000.0f64) == false

    @test
    def testBinaryGreater49(): Bool = 12000000000000000000ii > 3000000000000000000ii

    @test
    def testBinaryGreater50(): Bool = (3000000000000000000ii > 12000000000000000000ii) == false

    @test
    def testBinaryGreater51(): Bool = (3000000000000000000ii > 3000000000000000000ii) == false

    @test
    def testBinaryGreater52(): Bool = (-12000000000000000000ii > -3000000000000000000ii) == false

    @test
    def testBinaryGreater53(): Bool = -3000000000000000000ii > -12000000000000000000ii

    @test
    def testBinaryGreater54(): Bool = (-3000000000000000000ii > -3000000000000000000ii) == false

    @test
    def testBinaryGreater55(): Bool = '十' > '\u0000'

    @test
    def testBinaryGreater56(): Bool = ('\u0000' > '十') == false

    @test
    def testBinaryGreater57(): Bool = ('\u0000' > '\u0000') == false

    @test
    def testBinaryGreaterEqual01(): Bool = 120000 >= 30000

    @test
    def testBinaryGreaterEqual02(): Bool = (30000 >= 120000) == false

    @test
    def testBinaryGreaterEqual03(): Bool = 30000 >= 30000

    @test
    def testBinaryGreaterEqual04(): Bool = (-120000 >= -30000) == false

    @test
    def testBinaryGreaterEqual05(): Bool = -30000 >= -120000

    @test
    def testBinaryGreaterEqual06(): Bool = -30000 >= -30000

    @test
    def testBinaryGreaterEqual07(): Bool = 12i8 >= 3i8

    @test
    def testBinaryGreaterEqual08(): Bool = (3i8 >= 12i8) == false

    @test
    def testBinaryGreaterEqual09(): Bool = 3i8 >= 3i8

    @test
    def testBinaryGreaterEqual10(): Bool = (-12i8 >= -3i8) == false

    @test
    def testBinaryGreaterEqual11(): Bool = -3i8 >= -12i8

    @test
    def testBinaryGreaterEqual12(): Bool = -3i8 >= -3i8

    @test
    def testBinaryGreaterEqual13(): Bool = 1200i16 >= 300i16

    @test
    def testBinaryGreaterEqual14(): Bool = (300i16 >= 1200i16) == false

    @test
    def testBinaryGreaterEqual15(): Bool = 300i16 >= 300i16

    @test
    def testBinaryGreaterEqual16(): Bool = (-1200i16 >= -300i16) == false

    @test
    def testBinaryGreaterEqual17(): Bool = -300i16 >= -1200i16

    @test
    def testBinaryGreaterEqual18(): Bool = -300i16 >= -300i16

    @test
    def testBinaryGreaterEqual19(): Bool = 120000i32 >= 30000i32

    @test
    def testBinaryGreaterEqual20(): Bool = (30000i32 >= 120000i32) == false

    @test
    def testBinaryGreaterEqual21(): Bool = 30000i32 >= 30000i32

    @test
    def testBinaryGreaterEqual22(): Bool = (-120000i32 >= -30000i32) == false

    @test
    def testBinaryGreaterEqual23(): Bool = -30000i32 >= -120000i32

    @test
    def testBinaryGreaterEqual24(): Bool = -30000i32 >= -30000i32

    @test
    def testBinaryGreaterEqual25(): Bool = 12000000000i64 >= 3000000000i64

    @test
    def testBinaryGreaterEqual26(): Bool = (3000000000i64 >= 12000000000i64) == false

    @test
    def testBinaryGreaterEqual27(): Bool = 3000000000i64 >= 3000000000i64

    @test
    def testBinaryGreaterEqual28(): Bool = (-12000000000i64 >= -3000000000i64) == false

    @test
    def testBinaryGreaterEqual29(): Bool = -3000000000i64 >= -12000000000i64

    @test
    def testBinaryGreaterEqual30(): Bool = -3000000000i64 >= -3000000000i64

    @test
    def testBinaryGreaterEqual31(): Bool = 120000000000000000000000000000000000000000.0 >= 30000000000000000000000000000000000000000.0

    @test
    def testBinaryGreaterEqual32(): Bool = (30000000000000000000000000000000000000000.0 >= 120000000000000000000000000000000000000000.0) == false

    @test
    def testBinaryGreaterEqual33(): Bool = 30000000000000000000000000000000000000000.0 >= 30000000000000000000000000000000000000000.0

    @test
    def testBinaryGreaterEqual34(): Bool = (-120000000000000000000000000000000000000000.0 >= -30000000000000000000000000000000000000000.0) == false

    @test
    def testBinaryGreaterEqual35(): Bool = -30000000000000000000000000000000000000000.0 >= -120000000000000000000000000000000000000000.0

    @test
    def testBinaryGreaterEqual36(): Bool = -30000000000000000000000000000000000000000.0 >= -30000000000000000000000000000000000000000.0

    @test
    def testBinaryGreaterEqual37(): Bool = 1200000000000000000000.0f32 >= 300000000000000000000.0f32

    @test
    def testBinaryGreaterEqual38(): Bool = (300000000000000000000.0f32 >= 1200000000000000000000.0f32) == false

    @test
    def testBinaryGreaterEqual39(): Bool = 300000000000000000000.0f32 >= 300000000000000000000.0f32

    @test
    def testBinaryGreaterEqual40(): Bool = (-1200000000000000000000.0f32 >= -300000000000000000000.0f32) == false

    @test
    def testBinaryGreaterEqual41(): Bool = -300000000000000000000.0f32 >= -1200000000000000000000.0f32

    @test
    def testBinaryGreaterEqual42(): Bool = -300000000000000000000.0f32 >= -300000000000000000000.0f32

    @test
    def testBinaryGreaterEqual43(): Bool = 120000000000000000000000000000000000000000.0f64 >= 30000000000000000000000000000000000000000.0f64

    @test
    def testBinaryGreaterEqual44(): Bool = (30000000000000000000000000000000000000000.0f64 >= 120000000000000000000000000000000000000000.0f64) == false

    @test
    def testBinaryGreaterEqual45(): Bool = 30000000000000000000000000000000000000000.0f64 >= 30000000000000000000000000000000000000000.0f64

    @test
    def testBinaryGreaterEqual46(): Bool = (-120000000000000000000000000000000000000000.0f64 >= -30000000000000000000000000000000000000000.0f64) == false

    @test
    def testBinaryGreaterEqual47(): Bool =-30000000000000000000000000000000000000000.0f64 >= -120000000000000000000000000000000000000000.0f64

    @test
    def testBinaryGreaterEqual48(): Bool =  -30000000000000000000000000000000000000000.0f64 >= -30000000000000000000000000000000000000000.0f64

    @test
    def testBinaryGreaterEqual49(): Bool = 12000000000000000000ii >= 3000000000000000000ii

    @test
    def testBinaryGreaterEqual50(): Bool = (3000000000000000000ii >= 12000000000000000000ii) == false

    @test
    def testBinaryGreaterEqual51(): Bool = 3000000000000000000ii >= 3000000000000000000ii

    @test
    def testBinaryGreaterEqual52(): Bool = (-12000000000000000000ii >= -3000000000000000000ii) == false

    @test
    def testBinaryGreaterEqual53(): Bool = -3000000000000000000ii >= -12000000000000000000ii

    @test
    def testBinaryGreaterEqual54(): Bool = -3000000000000000000ii >= -3000000000000000000ii

    @test
    def testBinaryGreaterEqual55(): Bool = '十' >= '\u0000'

    @test
    def testBinaryGreaterEqual56(): Bool = ('\u0000' >= '十') == false

    @test
    def testBinaryGreaterEqual57(): Bool = '\u0000' >= '\u0000'

    @test
    def testBinaryEqual01(): Bool = (120000 == 30000) == false

    @test
    def testBinaryEqual02(): Bool = (30000 == 120000) == false

    @test
    def testBinaryEqual03(): Bool = 30000 == 30000

    @test
    def testBinaryEqual04(): Bool = (-120000 == -30000) == false

    @test
    def testBinaryEqual05(): Bool = (-30000 == -120000) == false

    @test
    def testBinaryEqual06(): Bool = -30000 == -30000

    @test
    def testBinaryEqual07(): Bool = (12i8 == 3i8) == false

    @test
    def testBinaryEqual08(): Bool = (3i8 == 12i8) == false

    @test
    def testBinaryEqual09(): Bool = 3i8 == 3i8

    @test
    def testBinaryEqual10(): Bool = (-12i8 == -3i8) == false

    @test
    def testBinaryEqual11(): Bool = (-3i8 == -12i8) == false

    @test
    def testBinaryEqual12(): Bool = -3i8 == -3i8

    @test
    def testBinaryEqual13(): Bool = (1200i16 == 300i16) == false

    @test
    def testBinaryEqual14(): Bool = (300i16 == 1200i16) == false

    @test
    def testBinaryEqual15(): Bool = 300i16 == 300i16

    @test
    def testBinaryEqual16(): Bool = (-1200i16 == -300i16) == false

    @test
    def testBinaryEqual17(): Bool = (-300i16 == -1200i16) == false

    @test
    def testBinaryEqual18(): Bool = -300i16 == -300i16

    @test
    def testBinaryEqual19(): Bool = (120000i32 == 30000i32) == false

    @test
    def testBinaryEqual20(): Bool = (30000i32 == 120000i32) == false

    @test
    def testBinaryEqual21(): Bool = 30000i32 == 30000i32

    @test
    def testBinaryEqual22(): Bool = (-120000i32 == -30000i32) == false

    @test
    def testBinaryEqual23(): Bool = (-30000i32 == -120000i32) == false

    @test
    def testBinaryEqual24(): Bool = -30000i32 == -30000i32

    @test
    def testBinaryEqual25(): Bool = (12000000000i64 == 3000000000i64) == false

    @test
    def testBinaryEqual26(): Bool = (3000000000i64 == 12000000000i64) == false

    @test
    def testBinaryEqual27(): Bool = 3000000000i64 == 3000000000i64

    @test
    def testBinaryEqual28(): Bool = (-12000000000i64 == -3000000000i64) == false

    @test
    def testBinaryEqual29(): Bool = (-3000000000i64 == -12000000000i64) == false

    @test
    def testBinaryEqual30(): Bool = -3000000000i64 == -3000000000i64

    @test
    def testBinaryEqual31(): Bool = (120000000000000000000000000000000000000000.0 == 30000000000000000000000000000000000000000.0) == false

    @test
    def testBinaryEqual32(): Bool = (30000000000000000000000000000000000000000.0 == 120000000000000000000000000000000000000000.0) == false

    @test
    def testBinaryEqual33(): Bool = 30000000000000000000000000000000000000000.0 == 30000000000000000000000000000000000000000.0

    @test
    def testBinaryEqual34(): Bool = (-120000000000000000000000000000000000000000.0 == -30000000000000000000000000000000000000000.0) == false

    @test
    def testBinaryEqual35(): Bool = (-30000000000000000000000000000000000000000.0 == -120000000000000000000000000000000000000000.0) == false

    @test
    def testBinaryEqual36(): Bool = -30000000000000000000000000000000000000000.0 == -30000000000000000000000000000000000000000.0

    @test
    def testBinaryEqual37(): Bool = (1200000000000000000000.0f32 == 300000000000000000000.0f32) == false

    @test
    def testBinaryEqual38(): Bool = (300000000000000000000.0f32 == 1200000000000000000000.0f32) == false

    @test
    def testBinaryEqual39(): Bool = 300000000000000000000.0f32 == 300000000000000000000.0f32

    @test
    def testBinaryEqual40(): Bool = (-1200000000000000000000.0f32 == -300000000000000000000.0f32) == false

    @test
    def testBinaryEqual41(): Bool = (-300000000000000000000.0f32 == -1200000000000000000000.0f32) == false

    @test
    def testBinaryEqual42(): Bool = -300000000000000000000.0f32 == -300000000000000000000.0f32

    @test
    def testBinaryEqual43(): Bool = (120000000000000000000000000000000000000000.0f64 == 30000000000000000000000000000000000000000.0f64) == false

    @test
    def testBinaryEqual44(): Bool = (30000000000000000000000000000000000000000.0f64 == 120000000000000000000000000000000000000000.0f64) == false

    @test
    def testBinaryEqual45(): Bool = 30000000000000000000000000000000000000000.0f64 == 30000000000000000000000000000000000000000.0f64

    @test
    def testBinaryEqual46(): Bool = (-120000000000000000000000000000000000000000.0f64 == -30000000000000000000000000000000000000000.0f64) == false

    @test
    def testBinaryEqual47(): Bool = (-30000000000000000000000000000000000000000.0f64 == -120000000000000000000000000000000000000000.0f64) == false

    @test
    def testBinaryEqual48(): Bool =  -30000000000000000000000000000000000000000.0f64 == -30000000000000000000000000000000000000000.0f64

    @test
    def testBinaryEqual49(): Bool = (12000000000000000000ii == 3000000000000000000ii) == false

    @test
    def testBinaryEqual50(): Bool = (3000000000000000000ii == 12000000000000000000ii) == false

    @test
    def testBinaryEqual51(): Bool = 3000000000000000000ii == 3000000000000000000ii

    @test
    def testBinaryEqual52(): Bool = (-12000000000000000000ii == -3000000000000000000ii) == false

    @test
    def testBinaryEqual53(): Bool = (-3000000000000000000ii == -12000000000000000000ii) == false

    @test
    def testBinaryEqual54(): Bool = -3000000000000000000ii == -3000000000000000000ii

    @test
    def testBinaryEqual55(): Bool = ('十' == '\u0000') == false

    @test
    def testBinaryEqual56(): Bool = ('\u0000' == '十') == false

    @test
    def testBinaryEqual57(): Bool = '\u0000' == '\u0000'

    @test
    def testBinaryNotEqual01(): Bool = 120000 != 30000

    @test
    def testBinaryNotEqual02(): Bool = 30000 != 120000

    @test
    def testBinaryNotEqual03(): Bool = (30000 != 30000) == false

    @test
    def testBinaryNotEqual04(): Bool = -120000 != -30000

    @test
    def testBinaryNotEqual05(): Bool = -30000 != -120000

    @test
    def testBinaryNotEqual06(): Bool = (-30000 != -30000) == false

    @test
    def testBinaryNotEqual07(): Bool = 12i8 != 3i8

    @test
    def testBinaryNotEqual08(): Bool = 3i8 != 12i8

    @test
    def testBinaryNotEqual09(): Bool = (3i8 != 3i8) == false

    @test
    def testBinaryNotEqual10(): Bool = -12i8 != -3i8

    @test
    def testBinaryNotEqual11(): Bool = -3i8 != -12i8

    @test
    def testBinaryNotEqual12(): Bool = (-3i8 != -3i8) == false

    @test
    def testBinaryNotEqual13(): Bool = 1200i16 != 300i16

    @test
    def testBinaryNotEqual14(): Bool = 300i16 != 1200i16

    @test
    def testBinaryNotEqual15(): Bool = (300i16 != 300i16) == false

    @test
    def testBinaryNotEqual16(): Bool = -1200i16 != -300i16

    @test
    def testBinaryNotEqual17(): Bool = -300i16 != -1200i16

    @test
    def testBinaryNotEqual18(): Bool = (-300i16 != -300i16) == false

    @test
    def testBinaryNotEqual19(): Bool = 120000i32 != 30000i32

    @test
    def testBinaryNotEqual20(): Bool = 30000i32 != 120000i32

    @test
    def testBinaryNotEqual21(): Bool = (30000i32 != 30000i32) == false

    @test
    def testBinaryNotEqual22(): Bool = -120000i32 != -30000i32

    @test
    def testBinaryNotEqual23(): Bool = -30000i32 != -120000i32

    @test
    def testBinaryNotEqual24(): Bool = (-30000i32 != -30000i32) == false

    @test
    def testBinaryNotEqual25(): Bool = 12000000000i64 != 3000000000i64

    @test
    def testBinaryNotEqual26(): Bool = 3000000000i64 != 12000000000i64

    @test
    def testBinaryNotEqual27(): Bool = (3000000000i64 != 3000000000i64) == false

    @test
    def testBinaryNotEqual28(): Bool = -12000000000i64 != -3000000000i64

    @test
    def testBinaryNotEqual29(): Bool = -3000000000i64 != -12000000000i64

    @test
    def testBinaryNotEqual30(): Bool = (-3000000000i64 != -3000000000i64) == false

    @test
    def testBinaryNotEqual31(): Bool = 120000000000000000000000000000000000000000.0 != 30000000000000000000000000000000000000000.0

    @test
    def testBinaryNotEqual32(): Bool = 30000000000000000000000000000000000000000.0 != 120000000000000000000000000000000000000000.0

    @test
    def testBinaryNotEqual33(): Bool = (30000000000000000000000000000000000000000.0 != 30000000000000000000000000000000000000000.0) == false

    @test
    def testBinaryNotEqual34(): Bool = -120000000000000000000000000000000000000000.0 != -30000000000000000000000000000000000000000.0

    @test
    def testBinaryNotEqual35(): Bool = -30000000000000000000000000000000000000000.0 != -120000000000000000000000000000000000000000.0

    @test
    def testBinaryNotEqual36(): Bool = (-30000000000000000000000000000000000000000.0 != -30000000000000000000000000000000000000000.0) == false

    @test
    def testBinaryNotEqual37(): Bool = 1200000000000000000000.0f32 != 300000000000000000000.0f32

    @test
    def testBinaryNotEqual38(): Bool = 300000000000000000000.0f32 != 1200000000000000000000.0f32

    @test
    def testBinaryNotEqual39(): Bool = (300000000000000000000.0f32 != 300000000000000000000.0f32) == false

    @test
    def testBinaryNotEqual40(): Bool = -1200000000000000000000.0f32 != -300000000000000000000.0f32

    @test
    def testBinaryNotEqual41(): Bool = -300000000000000000000.0f32 != -1200000000000000000000.0f32

    @test
    def testBinaryNotEqual42(): Bool = (-300000000000000000000.0f32 != -300000000000000000000.0f32) == false

    @test
    def testBinaryNotEqual43(): Bool = 120000000000000000000000000000000000000000.0f64 != 30000000000000000000000000000000000000000.0f64

    @test
    def testBinaryNotEqual44(): Bool = 30000000000000000000000000000000000000000.0f64 != 120000000000000000000000000000000000000000.0f64

    @test
    def testBinaryNotEqual45(): Bool = (30000000000000000000000000000000000000000.0f64 != 30000000000000000000000000000000000000000.0f64) == false

    @test
    def testBinaryNotEqual46(): Bool = -120000000000000000000000000000000000000000.0f64 != -30000000000000000000000000000000000000000.0f64

    @test
    def testBinaryNotEqual47(): Bool = -30000000000000000000000000000000000000000.0f64 != -120000000000000000000000000000000000000000.0f64

    @test
    def testBinaryNotEqual48(): Bool = (-30000000000000000000000000000000000000000.0f64 != -30000000000000000000000000000000000000000.0f64) == false

    @test
    def testBinaryNotEqual49(): Bool = 12000000000000000000ii != 3000000000000000000ii

    @test
    def testBinaryNotEqual50(): Bool = 3000000000000000000ii != 12000000000000000000ii

    @test
    def testBinaryNotEqual51(): Bool = (3000000000000000000ii != 3000000000000000000ii) == false

    @test
    def testBinaryNotEqual52(): Bool = -12000000000000000000ii != -3000000000000000000ii

    @test
    def testBinaryNotEqual53(): Bool = -3000000000000000000ii != -12000000000000000000ii

    @test
    def testBinaryNotEqual54(): Bool = (-3000000000000000000ii != -3000000000000000000ii) == false

    @test
    def testBinaryNotEqual55(): Bool = '十' != '\u0000'

    @test
    def testBinaryNotEqual56(): Bool = '\u0000' != '十'

    @test
    def testBinaryNotEqual57(): Bool = ('\u0000' != '\u0000') == false

}
