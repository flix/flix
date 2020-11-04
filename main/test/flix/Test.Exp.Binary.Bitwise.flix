namespace Test/Exp/Binary/Bitwise {

    @test
    def testBinaryBitwiseAnd01(): Bool = (40000 &&& -1) == 40000

    @test
    def testBinaryBitwiseAnd02(): Bool = (40000 &&& 40000) == 40000

    @test
    def testBinaryBitwiseAnd03(): Bool = (40000 &&& 0) == 0

    @test
    def testBinaryBitwiseAnd04(): Bool = (-1 &&& -1) == -1

    @test
    def testBinaryBitwiseAnd05(): Bool = (40i8 &&& -1i8) == 40i8

    @test
    def testBinaryBitwiseAnd06(): Bool = (40i8 &&& 40i8) == 40i8

    @test
    def testBinaryBitwiseAnd07(): Bool = (40i8 &&& 0i8) == 0i8

    @test
    def testBinaryBitwiseAnd08(): Bool = (-1i8 &&& -1i8) == -1i8

    @test
    def testBinaryBitwiseAnd09(): Bool = (400i16 &&& -1i16) == 400i16

    @test
    def testBinaryBitwiseAnd10(): Bool = (400i16 &&& 400i16) == 400i16

    @test
    def testBinaryBitwiseAnd11(): Bool = (400i16 &&& 0i16) == 0i16

    @test
    def testBinaryBitwiseAnd12(): Bool = (-1i16 &&& -1i16) == -1i16

    @test
    def testBinaryBitwiseAnd13(): Bool = (40000i32 &&& -1i32) == 40000i32

    @test
    def testBinaryBitwiseAnd14(): Bool = (40000i32 &&& 40000i32) == 40000i32

    @test
    def testBinaryBitwiseAnd15(): Bool = (40000i32 &&& 0i32) == 0i32

    @test
    def testBinaryBitwiseAnd16(): Bool = (-1i32 &&& -1i32) == -1i32

    @test
    def testBinaryBitwiseAnd17(): Bool = (40000000000i64 &&& -1i64) == 40000000000i64

    @test
    def testBinaryBitwiseAnd18(): Bool = (40000000000i64 &&& 40000000000i64) == 40000000000i64

    @test
    def testBinaryBitwiseAnd19(): Bool = (40000000000i64 &&& 0i64) == 0i64

    @test
    def testBinaryBitwiseAnd20(): Bool = (-1i64 &&& -1i64) == -1i64

    @test
    def testBinaryBitwiseAnd21(): Bool = (40000000000000000000ii &&& -1ii) == 40000000000000000000ii

    @test
    def testBinaryBitwiseAnd22(): Bool = (40000000000000000000ii &&& 40000000000000000000ii) == 40000000000000000000ii

    @test
    def testBinaryBitwiseAnd23(): Bool = (40000000000000000000ii &&& 0ii) == 0ii

    @test
    def testBinaryBitwiseAnd24(): Bool = (-1ii &&& -1ii) == -1ii

    @test
    def testBinaryBitwiseOr01(): Bool = (40000 ||| -1) == -1

    @test
    def testBinaryBitwiseOr02(): Bool = (40000 ||| 40000) == 40000

    @test
    def testBinaryBitwiseOr03(): Bool = (40000 ||| 0) == 40000

    @test
    def testBinaryBitwiseOr04(): Bool = (-1 ||| -1) == -1

    @test
    def testBinaryBitwiseOr05(): Bool = (40i8 ||| -1i8) == -1i8

    @test
    def testBinaryBitwiseOr06(): Bool = (40i8 ||| 40i8) == 40i8

    @test
    def testBinaryBitwiseOr07(): Bool = (40i8 ||| 0i8) == 40i8

    @test
    def testBinaryBitwiseOr08(): Bool = (-1i8 ||| -1i8) == -1i8

    @test
    def testBinaryBitwiseOr09(): Bool = (400i16 ||| -1i16) == -1i16

    @test
    def testBinaryBitwiseOr10(): Bool = (400i16 ||| 400i16) == 400i16

    @test
    def testBinaryBitwiseOr11(): Bool = (400i16 ||| 0i16) == 400i16

    @test
    def testBinaryBitwiseOr12(): Bool = (-1i16 ||| -1i16) == -1i16

    @test
    def testBinaryBitwiseOr13(): Bool = (40000i32 ||| -1i32) == -1i32

    @test
    def testBinaryBitwiseOr14(): Bool = (40000i32 ||| 40000i32) == 40000i32

    @test
    def testBinaryBitwiseOr15(): Bool = (40000i32 ||| 0i32) == 40000i32

    @test
    def testBinaryBitwiseOr16(): Bool = (-1i32 ||| -1i32) == -1i32

    @test
    def testBinaryBitwiseOr17(): Bool = (40000000000i64 ||| -1i64) == -1i64

    @test
    def testBinaryBitwiseOr18(): Bool = (40000000000i64 ||| 40000000000i64) == 40000000000i64

    @test
    def testBinaryBitwiseOr19(): Bool = (40000000000i64 ||| 0i64) == 40000000000i64

    @test
    def testBinaryBitwiseOr20(): Bool = (-1i64 ||| -1i64) == -1i64

    @test
    def testBinaryBitwiseOr21(): Bool = (40000000000000000000ii ||| -1ii) == -1ii

    @test
    def testBinaryBitwiseOr22(): Bool = (40000000000000000000ii ||| 40000000000000000000ii) == 40000000000000000000ii

    @test
    def testBinaryBitwiseOr23(): Bool = (40000000000000000000ii ||| 0ii) == 40000000000000000000ii

    @test
    def testBinaryBitwiseOr24(): Bool = (-1ii ||| -1ii) == -1ii

    @test
    def testBinaryBitwiseXor01(): Bool = (40000 ^^^ -1) == -40001

    @test
    def testBinaryBitwiseXor02(): Bool = (40000 ^^^ 40000) == 0

    @test
    def testBinaryBitwiseXor03(): Bool = (40000 ^^^ 0) == 40000

    @test
    def testBinaryBitwiseXor04(): Bool = (-1 ^^^ -1) == 0

    @test
    def testBinaryBitwiseXor05(): Bool = (40i8 ^^^ -1i8) == -41i8

    @test
    def testBinaryBitwiseXor06(): Bool = (40i8 ^^^ 40i8) == 0i8

    @test
    def testBinaryBitwiseXor07(): Bool = (40i8 ^^^ 0i8) == 40i8

    @test
    def testBinaryBitwiseXor08(): Bool = (-1i8 ^^^ -1i8) == 0i8

    @test
    def testBinaryBitwiseXor09(): Bool = (400i16 ^^^ -1i16) == -401i16

    @test
    def testBinaryBitwiseXor10(): Bool = (400i16 ^^^ 400i16) == 0i16

    @test
    def testBinaryBitwiseXor11(): Bool = (400i16 ^^^ 0i16) == 400i16

    @test
    def testBinaryBitwiseXor12(): Bool = (-1i16 ^^^ -1i16) == 0i16

    @test
    def testBinaryBitwiseXor13(): Bool = (40000i32 ^^^ -1i32) == -40001i32

    @test
    def testBinaryBitwiseXor14(): Bool = (40000i32 ^^^ 40000i32) == 0i32

    @test
    def testBinaryBitwiseXor15(): Bool = (40000i32 ^^^ 0i32) == 40000i32

    @test
    def testBinaryBitwiseXor16(): Bool = (-1i32 ^^^ -1i32) == 0i32

    @test
    def testBinaryBitwiseXor17(): Bool = (40000000000i64 ^^^ -1i64) == -40000000001i64

    @test
    def testBinaryBitwiseXor18(): Bool = (40000000000i64 ^^^ 40000000000i64) == 0i64

    @test
    def testBinaryBitwiseXor19(): Bool = (40000000000i64 ^^^ 0i64) == 40000000000i64

    @test
    def testBinaryBitwiseXor20(): Bool = (-1i64 ^^^ -1i64) == 0i64

    @test
    def testBinaryBitwiseXor21(): Bool = (40000000000000000000ii ^^^ -1ii) == -40000000000000000001ii

    @test
    def testBinaryBitwiseXor22(): Bool = (40000000000000000000ii ^^^ 40000000000000000000ii) == 0ii

    @test
    def testBinaryBitwiseXor23(): Bool = (40000000000000000000ii ^^^ 0ii) == 40000000000000000000ii

    @test
    def testBinaryBitwiseXor24(): Bool = (-1ii ^^^ -1ii) == 0ii

    @test
    def testBinaryBitwiseLeftShift01(): Bool = (8 <<< 0) == 8

    @test
    def testBinaryBitwiseLeftShift02(): Bool = (8 <<< 16) == 524288

    @test
    def testBinaryBitwiseLeftShift03(): Bool = (8 <<< 28) == -2147483648

    @test
    def testBinaryBitwiseLeftShift04(): Bool = (8 <<< 29) == 0

    @test
    def testBinaryBitwiseLeftShift05(): Bool = (8i8 <<< 0) == 8i8

    @test
    def testBinaryBitwiseLeftShift06(): Bool = (8i8 <<< 2) == 32i8

    @test
    def testBinaryBitwiseLeftShift07(): Bool = (8i8 <<< 4) == -128i8

    @test
    def testBinaryBitwiseLeftShift08(): Bool = (8i8 <<< 5) == 0i8

    @test
    def testBinaryBitwiseLeftShift09(): Bool = (8i16 <<< 0) == 8i16

    @test
    def testBinaryBitwiseLeftShift10(): Bool = (8i16 <<< 8) == 2048i16

    @test
    def testBinaryBitwiseLeftShift11(): Bool = (8i16 <<< 12) == -32768i16

    @test
    def testBinaryBitwiseLeftShift12(): Bool = (8i16 <<< 13) == 0i16

    @test
    def testBinaryBitwiseLeftShift13(): Bool = (8i32 <<< 0) == 8i32

    @test
    def testBinaryBitwiseLeftShift14(): Bool = (8i32 <<< 16) == 524288i32

    @test
    def testBinaryBitwiseLeftShift15(): Bool = (8i32 <<< 28) == -2147483648i32

    @test
    def testBinaryBitwiseLeftShift16(): Bool = (8i32 <<< 29) == 0i32

    @test
    def testBinaryBitwiseLeftShift17(): Bool = (8i64 <<< 0) == 8i64

    @test
    def testBinaryBitwiseLeftShift18(): Bool = (8i64 <<< 32) == 34359738368i64

    @test
    def testBinaryBitwiseLeftShift19(): Bool = (8i64 <<< 60) == -9223372036854775808i64

    @test
    def testBinaryBitwiseLeftShift20(): Bool = (8i64 <<< 61) == 0i64

    @test
    def testBinaryBitwiseLeftShift21(): Bool = (8ii <<< 0) == 8ii

    @test
    def testBinaryBitwiseLeftShift22(): Bool = (8ii <<< 32) == 34359738368ii

    @test
    def testBinaryBitwiseLeftShift23(): Bool = (8ii <<< 64) == 147573952589676412928ii

    @test
    def testBinaryBitwiseRightShift01(): Bool = (120000 >>> 0) == 120000

    @test
    def testBinaryBitwiseRightShift02(): Bool = (120000 >>> 2) == 30000

    @test
    def testBinaryBitwiseRightShift03(): Bool = (120000 >>> 31) == 0

    @test
    def testBinaryBitwiseRightShift04(): Bool = (-120000 >>> 2) == -30000

    @test
    def testBinaryBitwiseRightShift05(): Bool = (120i8 >>> 0) == 120i8

    @test
    def testBinaryBitwiseRightShift06(): Bool = (120i8 >>> 2) == 30i8

    @test
    def testBinaryBitwiseRightShift07(): Bool = (120i8 >>> 7) == 0i8

    @test
    def testBinaryBitwiseRightShift08(): Bool = (-120i8 >>> 2) == -30i8

    @test
    def testBinaryBitwiseRightShift09(): Bool = (12000i16 >>> 0) == 12000i16

    @test
    def testBinaryBitwiseRightShift10(): Bool = (12000i16 >>> 2) == 3000i16

    @test
    def testBinaryBitwiseRightShift11(): Bool = (12000i16 >>> 15) == 0i16

    @test
    def testBinaryBitwiseRightShift12(): Bool = (-12000i16 >>> 2) == -3000i16

    @test
    def testBinaryBitwiseRightShift13(): Bool = (120000i32 >>> 0) == 120000i32

    @test
    def testBinaryBitwiseRightShift14(): Bool = (120000i32 >>> 2) == 30000i32

    @test
    def testBinaryBitwiseRightShift15(): Bool = (120000i32 >>> 31) == 0i32

    @test
    def testBinaryBitwiseRightShift16(): Bool = (-120000i32 >>> 2) == -30000i32

    @test
    def testBinaryBitwiseRightShift17(): Bool = (12000000000i64 >>> 0) == 12000000000i64

    @test
    def testBinaryBitwiseRightShift18(): Bool = (12000000000i64 >>> 2) == 3000000000i64

    @test
    def testBinaryBitwiseRightShift19(): Bool = (12000000000i64 >>> 63) == 0i64

    @test
    def testBinaryBitwiseRightShift20(): Bool = (-12000000000i64 >>> 2) == -3000000000i64

    @test
    def testBinaryBitwiseRightShift21(): Bool = (1200000000000000000000ii >>> 0) == 1200000000000000000000ii

    @test
    def testBinaryBitwiseRightShift22(): Bool = (1200000000000000000000ii >>> 2) == 300000000000000000000ii

    @test
    def testBinaryBitwiseRightShift23(): Bool = (1200000000000000000000ii >>> 72) == 0ii

    @test
    def testBinaryBitwiseRightShift24(): Bool = (-1200000000000000000000ii >>> 2) == -300000000000000000000ii

}
