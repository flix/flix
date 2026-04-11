mod Test.Portable.Exceptions {

    def assertTrue(cond: Bool, msg: String): Unit =
        if (cond) () else bug!(msg)

    @Test
    def catchTyped01(): Unit \ IO = {
        let v = try {
            throw Exn.mk(21);
            0
        } catch {
            case exn: Int32 => Exn.payloadAs(exn)
            case _: Exn => -1
        };
        assertTrue(v == 21, "catchTyped01 failed.")
    }

    @Test
    def catchAll01(): Unit \ IO = {
        let v = try {
            throw Exn.mk(123);
            0
        } catch {
            case _: Exn => 42
        };
        assertTrue(v == 42, "catchAll01 failed.")
    }

    @Test
    def catchOrder01(): Unit \ IO = {
        let v = try {
            throw Exn.mk(9);
            0
        } catch {
            case _: Exn => 1
            case _: Int32 => 2
        };
        assertTrue(v == 1, "catchOrder01 failed.")
    }

    @Test
    def noMatchPropagates01(): Unit \ IO = {
        let v = try {
            let _ = try {
                throw Exn.mk(42);
                ()
            } catch {
                case _: String => ()
            };
            0
        } catch {
            case exn: Int32 => Exn.payloadAs(exn)
            case _: Exn => -1
        };
        assertTrue(v == 42, "noMatchPropagates01 failed.")
    }

    pub def runAll(): Unit \ IO = {
        let _ = catchTyped01();
        let _ = catchAll01();
        let _ = catchOrder01();
        let _ = noMatchPropagates01();
        ()
    }

}
