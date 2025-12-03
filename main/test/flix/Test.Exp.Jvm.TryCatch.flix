mod Test.Exp.Jvm.TryCatch {

    import java.lang.Math
    import java.lang.ArithmeticException
    import java.lang.IllegalArgumentException
    import java.lang.Exception

    def exception(): Unit \ IO =
        discard Math.floorDiv(1, 0);
        ()

    @test
    def testNoException01(): Bool =
        try {
            true
        } catch {
            case _: Exception => false
        }

    @test
    def testSimpleException01(): Bool \ IO =
        try {
            exception();
            false
        } catch {
            case _: Exception => true
        }

    @test
    def testMultipleExceptions01(): Bool \ IO =
        try {
            exception();
            false
        } catch {
            case _: ArithmeticException => true
            case _: IllegalArgumentException => false
        }

    @test
    def testMultipleExceptions02(): Bool \ IO =
        try {
            exception();
            false
        } catch {
            case _: IllegalArgumentException => false
            case _: ArithmeticException => true
        }

    @test
    def testMultipleExceptions03(): Bool \ IO =
        try {
            exception();
            false
        } catch {
            case _: IllegalArgumentException => false
        } catch {
            case _: ArithmeticException => true
        }

    @test
    def testBinding01(): Bool \ IO =
        try {
            exception();
            false
        } catch {
            case e: ArithmeticException => if (e.toString().isEmpty()) false else true
            case _: Exception => false
        }

    @test
    def testBinding02(): Bool \ IO =
        try {
            exception();
            false
        } catch {
            case e: ArithmeticException => if (e.getMessage().isEmpty()) false else true
            case _: Exception => false
        }

    @test
    def testPriority01(): Bool \ IO =
        try {
            exception();
            false
        } catch {
            case _: Exception => true
            case _: ArithmeticException => false
        }

    @test
    def testPriority02(): Bool \ IO =
        try {
            exception();
            false
        } catch {
            case _: ArithmeticException => true
            case _: Exception => false
        }

    @test
    def testNesting01(): Bool \ IO = {
        try {
            try {
                exception();
                false
            } catch {
                case _: ArithmeticException => true
            }
        } catch {
            case _: Exception => false
        }
    }

    @test
    def testNesting02(): Bool \ IO = {
        try {
            try {
                exception();
                false
            } catch {
                case _: IllegalArgumentException => false
            }
        } catch {
            case _: ArithmeticException => true
        }
    }

    @test
    def testNesting03(): Bool \ IO = {
        try {
            try {
                exception();
                false
            } catch {
                case _: ArithmeticException =>
                    exception();
                    false
            }
        } catch {
            case _: Exception => true
            case _: ArithmeticException => false
        }
    }

    @test
    def testNesting04(): Bool \ IO = {
        try {
            try {
                exception();
                false
            } catch {
                case _: ArithmeticException =>
                    exception();
                    false
            }
        } catch {
            case _: ArithmeticException => true
            case _: Exception => false
        }
    }

    @test
    def testNesting05(): Bool \ IO = {
        try {
            try {
                exception();
                false
            } catch {
                case _: ArithmeticException =>
                    exception();
                    false
                case _: Exception => false
            }
        } catch {
            case _: ArithmeticException => true
            case _: Exception => false
        }
    }


}
