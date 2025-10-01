mod Test.Exp.Jvm.Throw {

    import java.lang.Exception
    import java.lang.IllegalArgumentException
    import java.lang.ArithmeticException

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
            throw new Exception();
            false
        } catch {
            case _: Exception => true
        }

    @test
    def testMultipleExceptions01(): Bool \ IO =
        try {
            throw new ArithmeticException("TEST");
            false
        } catch {
            case _: ArithmeticException => true
            case _: IllegalArgumentException => false
        }

    @test
    def testMultipleExceptions02(): Bool \ IO =
        try {
            throw new ArithmeticException("TEST");
            false
        } catch {
            case _: IllegalArgumentException => false
            case _: ArithmeticException => true
        }

    @test
    def testMultipleExceptions03(): Bool \ IO =
        try {
            throw new ArithmeticException("TEST");
            false
        } catch {
            case _: IllegalArgumentException => false
        } catch {
            case _: ArithmeticException => true
        }

    @test
    def testMultipleExceptions04(): Bool \ IO =
        try {
            throw new IllegalArgumentException("TEST");
            false
        } catch {
            case _: IllegalArgumentException => true
        } catch {
            case _: ArithmeticException => false
        }

    @test
    def testBinding01(): Bool \ IO =
        try {
            throw new ArithmeticException("TEST");
            false
        } catch {
            case e: ArithmeticException => if (e.toString().isEmpty()) false else true
            case _: Exception => false
        }

    @test
    def testBinding02(): Bool \ IO =
        try {
            throw new ArithmeticException("TEST");
            false
        } catch {
            case e: ArithmeticException => e.getMessage() == "TEST"
            case _: Exception => false
        }

    @test
    def testPriority01(): Bool \ IO =
        try {
            throw new ArithmeticException("TEST");
            false
        } catch {
            case _: Exception => true
            case _: ArithmeticException => false
        }

    @test
    def testPriority02(): Bool \ IO =
        try {
            throw new ArithmeticException("TEST");
            false
        } catch {
            case _: ArithmeticException => true
            case _: Exception => false
        }

}
