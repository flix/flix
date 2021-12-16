namespace Test/Exp/Stm {

    @test
    def testStm01(): Int32 & Impure =
        f();
        123

    @test
    def testStm02(): Int32 & Impure =
        f();
        g();
        123

    @test
    def testStm03(): Int32 & Impure =
        f();
        g();
        h();
        123

    @test
    def testStm04(): Int32 & Impure =
        let x = 123;
        f();
        x

    @test
    def testStm05(): Int32 & Impure =
        f();
        let x = 123;
        x

    @test
    def testStm06(): Int32 & Impure =
        let x = 123;
        let y = 456;
        f();
        g();
        x + y

    @test
    def testStm07(): Int32 & Impure =
        let x = 123;
        f();
        let y = 456;
        g();
        x + y

    @test
    def testStm08(): Int32 & Impure =
        let x = 123;
        f();
        g();
        let y = 456;
        x + y

    @test
    def testStm09(): Int32 & Impure =
        f();
        let x = 123;
        g();
        let y = 456;
        x + y

    @test
    def testStm10(): Int32 & Impure =
        f();
        g();
        let x = 123;
        let y = 456;
        x + y

    @test
    def testStm11(): Int32 & Impure =
        let x = {
            f();
            123
        };
        x

    @test
    def testStm12(): Int32 & Impure =
        let x = {
            f();
            g();
            123
        };
        h();
        x

    def f(): Unit & Impure = () as & Impure

    def g(): Unit & Impure = () as & Impure

    def h(): Unit & Impure = () as & Impure

}
