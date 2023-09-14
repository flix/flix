mod Test.Exp.Stm {

    @Test
    def testStm01(): Int32 \ IO =
        f();
        123

    @Test
    def testStm02(): Int32 \ IO =
        f();
        g();
        123

    @Test
    def testStm03(): Int32 \ IO =
        f();
        g();
        h();
        123

    @Test
    def testStm04(): Int32 \ IO =
        let x = 123;
        f();
        x

    @Test
    def testStm05(): Int32 \ IO =
        f();
        let x = 123;
        x

    @Test
    def testStm06(): Int32 \ IO =
        let x = 123;
        let y = 456;
        f();
        g();
        x + y

    @Test
    def testStm07(): Int32 \ IO =
        let x = 123;
        f();
        let y = 456;
        g();
        x + y

    @Test
    def testStm08(): Int32 \ IO =
        let x = 123;
        f();
        g();
        let y = 456;
        x + y

    @Test
    def testStm09(): Int32 \ IO =
        f();
        let x = 123;
        g();
        let y = 456;
        x + y

    @Test
    def testStm10(): Int32 \ IO =
        f();
        g();
        let x = 123;
        let y = 456;
        x + y

    @Test
    def testStm11(): Int32 \ IO =
        let x = {
            f();
            123
        };
        x

    @Test
    def testStm12(): Int32 \ IO =
        let x = {
            f();
            g();
            123
        };
        h();
        x

    def f(): Unit \ IO = checked_ecast(())

    def g(): Unit \ IO = checked_ecast(())

    def h(): Unit \ IO = checked_ecast(())

}
