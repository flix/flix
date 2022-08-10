package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.tools.Tester.TestEvent.{BeforeTest, TestSuccess}
import ca.uwaterloo.flix.util.{Formatter, InternalCompilerException}
import org.jline.terminal.{Terminal, TerminalBuilder}

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.logging.{Level, Logger}

/**
  * Evaluates all tests in a model.
  */
object Tester {

  /**
    * Evaluates all tests.
    *
    * Returns a pair of (successful, failed)-tests.
    */
  def run(compilationResult: CompilationResult)(implicit flix: Flix): Unit = {

    // Silence JLine warnings about terminal type.
    Logger.getLogger("org.jline").setLevel(Level.OFF)

    // Initialize the terminal.
    implicit val terminal: Terminal = TerminalBuilder
      .builder()
      .system(true)
      .build()

    val queue = new ConcurrentLinkedQueue[TestEvent]()
    val runner = new TestRunner(queue, compilationResult)
    runner.start()
    while (true) {
      queue.poll() match {
        case TestEvent.TestSuccess(sym, elapsed) =>
          terminal.writer().println(s"  - ${green(sym.toString)} ${magenta(elapsed.toString + "ms")}")
          terminal.flush()
        case _ => // nop
      }
    }
    throw InternalCompilerException("Unreachable")
  }

  private def green(s: String): String = Console.GREEN + s + Console.RESET

  private def magenta(s: String): String = Console.MAGENTA + s + Console.RESET

  private def red(s: String): String = Console.RED + s + Console.RESET

  /**
    * Represents the outcome of a single test.
    */
  sealed trait TestResult {
    /**
      * The symbol associated with the test.
      */
    def sym: Symbol.DefnSym
  }

  object TestResult {

    /**
      * Represents a successful test case.
      */
    case class Success(sym: Symbol.DefnSym, msg: String) extends TestResult

    /**
      * Represents a failed test case.
      */
    case class Failure(sym: Symbol.DefnSym, msg: String) extends TestResult

  }

  /**
    * Represents the outcome of a run of a suite of tests.
    */
  sealed trait OverallTestResult

  object OverallTestResult {

    /**
      * Represents the outcome where all tests succeeded.
      */
    case object Success extends OverallTestResult

    /**
      * Represents the outcome where at least one test failed.
      */
    case object Failure extends OverallTestResult

    /**
      * Represents the outcome where no tests were run.
      */
    case object NoTests extends OverallTestResult
  }


  class TestRunner(queue: ConcurrentLinkedQueue[TestEvent], result: CompilationResult)(implicit flix: Flix) extends Thread {

    override def run(): Unit = {
      val results = result.getTests.toList.map {
        case (sym, defn) => runTest(sym, defn)

      }
    }

    /**
      * Runs the given test.
      */
    private def runTest(sym: Symbol.DefnSym, defn: () => AnyRef): TestResult = {
      try {
        Thread.sleep(1500)

        queue.add(BeforeTest(sym))
        val elapsed = 123

        val result = defn()
        result match {
          case java.lang.Boolean.TRUE =>
            queue.add(TestSuccess(sym, elapsed))
            TestResult.Success(sym, "Returned true.")
          case java.lang.Boolean.FALSE =>
            queue.add(TestSuccess(sym, elapsed))
            TestResult.Failure(sym, "Returned false.")
          case _ =>
            queue.add(TestSuccess(sym, elapsed))
            TestResult.Success(sym, "Returned non-boolean value.")
        }
      } catch {
        case ex: Exception =>
          TestResult.Failure(sym, ex.getMessage)
      }
    }
  }

  /**
    * A common super-type for test events.
    */
  sealed trait TestEvent

  object TestEvent {

    case class BeforeTest(sym: Symbol.DefnSym) extends TestEvent

    /**
      * The test was successful.
      */
    case class TestSuccess(sym: Symbol.DefnSym, time: Long) extends TestEvent

    /**
      * The test failed.
      */
    case class TestFailure(sym: Symbol.DefnSym, time: Long) extends TestEvent

  }

}
