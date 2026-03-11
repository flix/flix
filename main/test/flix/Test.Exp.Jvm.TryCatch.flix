mod Test.Exp.Jvm.TryCatch {

    use Assert.{success, fail}

    import java.lang.Math
    import java.lang.ArithmeticException
    import java.lang.IllegalArgumentException
    import java.lang.Exception

    @Test
    def testNoException01(): Unit \ Assert =
        try {
            success("no exception thrown")
        } catch {
            case _: Exception => fail("unexpected exception")
        }

    @Test
    def testSimpleException01(): Unit \ Assert + IO =
        try {
            exception();
            fail("expected exception")
        } catch {
            case _: Exception => success("caught exception")
        }

    @Test
    def testMultipleExceptions01(): Unit \ Assert + IO =
        try {
            exception();
            fail("expected exception")
        } catch {
            case _: ArithmeticException => success("caught ArithmeticException")
            case _: IllegalArgumentException => fail("wrong exception type")
        }

    @Test
    def testMultipleExceptions02(): Unit \ Assert + IO =
        try {
            exception();
            fail("expected exception")
        } catch {
            case _: IllegalArgumentException => fail("wrong exception type")
            case _: ArithmeticException => success("caught ArithmeticException")
        }

    @Test
    def testMultipleExceptions03(): Unit \ Assert + IO =
        try {
            exception();
            fail("expected exception")
        } catch {
            case _: IllegalArgumentException => fail("wrong exception type")
        } catch {
            case _: ArithmeticException => success("caught ArithmeticException")
        }

    @Test
    def testBinding01(): Unit \ Assert + IO =
        try {
            exception();
            fail("expected exception")
        } catch {
            case e: ArithmeticException => if (e.toString().isEmpty()) fail("empty toString") else success("got exception message")
            case _: Exception => fail("wrong exception type")
        }

    @Test
    def testBinding02(): Unit \ Assert + IO =
        try {
            exception();
            fail("expected exception")
        } catch {
            case e: ArithmeticException => if (e.getMessage().isEmpty()) fail("empty getMessage") else success("got exception message")
            case _: Exception => fail("wrong exception type")
        }

    @Test
    def testPriority01(): Unit \ Assert + IO =
        try {
            exception();
            fail("expected exception")
        } catch {
            case _: Exception => success("caught by Exception handler first")
            case _: ArithmeticException => fail("should not reach ArithmeticException handler")
        }

    @Test
    def testPriority02(): Unit \ Assert + IO =
        try {
            exception();
            fail("expected exception")
        } catch {
            case _: ArithmeticException => success("caught ArithmeticException")
            case _: Exception => fail("should not reach Exception handler")
        }

    @Test
    def testNesting01(): Unit \ Assert + IO = {
        try {
            try {
                exception();
                fail("expected exception")
            } catch {
                case _: ArithmeticException => success("caught in inner handler")
            }
        } catch {
            case _: Exception => fail("should not reach outer handler")
        }
    }

    @Test
    def testNesting02(): Unit \ Assert + IO = {
        try {
            try {
                exception();
                fail("expected exception")
            } catch {
                case _: IllegalArgumentException => fail("wrong exception type")
            }
        } catch {
            case _: ArithmeticException => success("caught in outer handler")
        }
    }

    @Test
    def testNesting03(): Unit \ Assert + IO = {
        try {
            try {
                exception();
                fail("expected exception")
            } catch {
                case _: ArithmeticException =>
                    exception();
                    fail("expected exception in handler")
            }
        } catch {
            case _: Exception => success("caught in outer Exception handler")
            case _: ArithmeticException => fail("should not reach outer ArithmeticException handler")
        }
    }

    @Test
    def testNesting04(): Unit \ Assert + IO = {
        try {
            try {
                exception();
                fail("expected exception")
            } catch {
                case _: ArithmeticException =>
                    exception();
                    fail("expected exception in handler")
            }
        } catch {
            case _: ArithmeticException => success("caught in outer ArithmeticException handler")
            case _: Exception => fail("should not reach outer Exception handler")
        }
    }

    @Test
    def testNesting05(): Unit \ Assert + IO = {
        try {
            try {
                exception();
                fail("expected exception")
            } catch {
                case _: ArithmeticException =>
                    exception();
                    fail("expected exception in handler")
                case _: Exception => fail("should not reach inner Exception handler")
            }
        } catch {
            case _: ArithmeticException => success("caught in outer ArithmeticException handler")
            case _: Exception => fail("should not reach outer Exception handler")
        }
    }

    def exception(): Unit \ IO =
        discard Math.floorDiv(1, 0);
        ()

}
