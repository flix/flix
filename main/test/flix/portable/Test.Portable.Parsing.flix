mod Test.Portable.Parsing {

    @Test
    def int32FromStringOk01(): Unit =
        match Int32.fromString(" 123 ") {
            case Some(v) if v == 123i32 => ()
            case _ => bug!("expected Int32.fromString to succeed.")
        }

    @Test
    def int32FromStringErr01(): Unit =
        match Int32.fromString("abc") {
            case None => ()
            case _ => bug!("expected Int32.fromString to fail.")
        }

    @Test
    def int32ParseOk01(): Unit =
        match Int32.parse(16, "ff") {
            case Ok(v) if v == 255i32 => ()
            case _ => bug!("expected Int32.parse to succeed.")
        }

    @Test
    def float64FromStringOk01(): Unit =
        match Float64.fromString(" 1.5 ") {
            case Some(v) if v == 1.5f64 => ()
            case _ => bug!("expected Float64.fromString to succeed.")
        }

    pub def runAll(): Unit = {
        let _ = int32FromStringOk01();
        let _ = int32FromStringErr01();
        let _ = int32ParseOk01();
        let _ = float64FromStringOk01();
        ()
    }

}
