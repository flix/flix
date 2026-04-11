mod Test.Portable.Timer {

    @Test
    def sleepMillis01(): Unit \ Timer =
        Timer.sleepMillis(1i64)

    @Test
    def sleepUnit01(): Unit \ Timer =
        Timer.sleepFor(1i64, TimeUnit.Milliseconds)

    pub def runAll(): Unit \ Timer = {
        let _ = sleepMillis01();
        let _ = sleepUnit01();
        ()
    }

}
