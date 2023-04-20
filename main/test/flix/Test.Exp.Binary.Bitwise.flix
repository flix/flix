mod Test.Exp.Binary.Bitwise {

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

}
