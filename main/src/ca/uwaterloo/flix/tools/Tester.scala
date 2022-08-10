/*
 * Copyright 2022 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.util.Duration
import org.jline.terminal.{Terminal, TerminalBuilder}

import java.io.{ByteArrayOutputStream, PrintStream}
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.logging.{Level, Logger}

/**
  * Evaluates all tests in a Flix program.
  */
object Tester {

  /**
    * Runs all tests.
    */
  def run(compilationResult: CompilationResult)(implicit flix: Flix): Unit = {
    //
    // Find all test cases (both active and ignored).
    //
    val tests = getTestCases(compilationResult)

    // Start the TestRunner and TestReporter.
    val queue = new ConcurrentLinkedQueue[TestEvent]()
    val reporter = new TestReporter(queue, tests)
    val runner = new TestRunner(queue, tests)
    reporter.start()
    runner.start()

    // Wait for everything to complete.
    reporter.join()
    runner.join()
  }

  /**
    * A class that reports the results of test events as they come in.
    */
  private class TestReporter(queue: ConcurrentLinkedQueue[TestEvent], tests: Vector[TestCase])(implicit flix: Flix) extends Thread {
    override def run(): Unit = {
      // Silence JLine warnings about terminal type.
      Logger.getLogger("org.jline").setLevel(Level.OFF)

      // Initialize the terminal.
      implicit val terminal: Terminal = TerminalBuilder
        .builder()
        .system(true)
        .build()
      val writer = terminal.writer()

      // Print headline.
      writer.println(s"Running ${tests.length} tests...")
      writer.println()
      writer.flush()

      // Main event loop.
      var passed = 0
      var failed: List[(Symbol.DefnSym, List[String])] = Nil

      var finished = false
      while (!finished) {
        queue.poll() match {
          case TestEvent.Before(sym) =>
            // Note: Print \r to reset the caret.
            writer.print(s"  ${yellowBg(" TEST ")} $sym\r")
            terminal.flush()

          case TestEvent.Success(sym, elapsed) =>
            passed = passed + 1
            writer.println(s"  ${greenBg(" PASS ")} $sym ${magenta(elapsed.fmt)}")
            terminal.flush()

          case TestEvent.Failure(sym, output, elapsed) =>
            failed = (sym, output) :: failed
            writer.println(s"  ${redBg(" FAIL ")} $sym ${magenta(elapsed.fmt)}")
            terminal.flush()

          case TestEvent.Finished(elapsed) =>
            // Print the std out / std err of every failed test.
            if (failed.nonEmpty) {
              writer.println()
              writer.println("-" * 80)
              writer.println()
              for ((sym, output) <- failed; if output.nonEmpty) {
                writer.println(s"  ${redBg(" FAIL ")} $sym")
                for (line <- output) {
                  writer.println(s"    $line")
                }
                writer.println()
              }
              writer.println("-" * 80)
            }

            // Print the summary.
            writer.println()
            writer.println(s"Finished. Passed: ${green(passed.toString)}, Failed: ${red(failed.length.toString)}. Elapsed: ${magenta(elapsed.fmt)}.")
            terminal.flush()
            finished = true

          case _ => // nop
        }
      }
    }

    // TODO: Use flix.formatter
    private def green(s: String): String = Console.GREEN + s + Console.RESET

    // TODO: Use flix.formatter
    private def greenBg(s: String): String = Console.GREEN_B + s + Console.RESET

    // TODO: Use flix.formatter
    private def magenta(s: String): String = Console.MAGENTA + s + Console.RESET

    // TODO: Use flix.formatter
    private def red(s: String): String = Console.RED + s + Console.RESET

    // TODO: Use flix.formatter
    private def redBg(s: String): String = Console.RED_B + s + Console.RESET

    // TODO: Use flix.formatter
    private def yellowBg(s: String): String = Console.YELLOW_B + s + Console.RESET

  }

  /**
    * A class that runs all the given tests emitting test events.
    */
  private class TestRunner(queue: ConcurrentLinkedQueue[TestEvent], tests: Vector[TestCase])(implicit flix: Flix) extends Thread {
    /**
      * Runs all the given tests.
      */
    override def run(): Unit = {
      val start = System.nanoTime()
      for (testCase <- tests) {
        runTest(testCase)
      }
      val elapsed = System.nanoTime() - start
      queue.add(TestEvent.Finished(Duration(elapsed)))
    }

    /**
      * Runs the given `test` emitting test events.
      */
    private def runTest(test: TestCase): Unit = test match {
      case TestCase(sym, run) =>
        queue.add(TestEvent.Before(sym))

        // Redirect std out and std err.
        val redirect = new ConsoleRedirection
        redirect.redirect()

        // Start the clock.
        val start = System.nanoTime()

        try {
          // Run the test case.
          val result = run()

          // Compute elapsed time.
          val elapsed = System.nanoTime() - start

          // Restore std out and std err.
          redirect.restore()

          result match {
            case java.lang.Boolean.TRUE =>
              queue.add(TestEvent.Success(sym, Duration(elapsed)))

            case java.lang.Boolean.FALSE =>
              queue.add(TestEvent.Failure(sym, Nil, Duration(elapsed)))

            case _ =>
              queue.add(TestEvent.Success(sym, Duration(elapsed)))

          }
        } catch {
          case ex: Throwable =>
            // Restore std out and std err.
            redirect.restore()

            // Compute elapsed time.
            val elapsed = System.nanoTime() - start
            queue.add(TestEvent.Failure(sym, redirect.stdOut ++ redirect.stdErr, Duration(elapsed)))
        }
    }
  }

  /**
    * A class used to redirect the standard out and standard error streams.
    */
  class ConsoleRedirection {
    private val bytesOut = new ByteArrayOutputStream()
    private val bytesErr = new ByteArrayOutputStream()
    private val streamOut = new PrintStream(bytesOut)
    private val streamErr = new PrintStream(bytesErr)

    private var oldStreamOut: PrintStream = _
    private var oldStreamErr: PrintStream = _

    /**
      * Returns the string emitted to the std out during redirection.
      */
    def stdOut: List[String] = bytesOut.toString().linesIterator.toList

    /**
      * Returns the string emitted to the std err during redirection.
      */
    def stdErr: List[String] = bytesErr.toString().linesIterator.toList

    /**
      * Redirect std out and std err.
      */
    def redirect(): Unit = {
      // Store the old streams.
      oldStreamOut = System.out
      oldStreamErr = System.err

      // Set the new streams.
      System.setOut(streamOut)
      System.setErr(streamErr)
    }

    /**
      * Restore the std in and std err to their original streams.
      */
    def restore(): Unit = {
      // Flush the new streams.
      System.out.flush()
      System.err.flush()

      // Restore standard out and standard error.
      System.setOut(oldStreamOut)
      System.setErr(oldStreamErr)
    }
  }

  /**
    * Returns all test cases from the given compilation `result`.
    */
  private def getTestCases(compilationResult: CompilationResult): Vector[TestCase] =
    compilationResult.getTests.toVector.sortBy(_._1.toString).map {
      case (sym, defn) => TestCase(sym, defn)
    }

  /**
    * Represents a single test case.
    *
    * @param sym the Flix symbol.
    * @param run the code to run.
    */
  case class TestCase(sym: Symbol.DefnSym, run: () => AnyRef)

  /**
    * A common super-type for test events.
    */
  sealed trait TestEvent

  object TestEvent {

    /**
      * A test event emitted immediately before a test case is executed.
      */
    case class Before(sym: Symbol.DefnSym) extends TestEvent

    /**
      * A test event emitted to indicate that a test succeeded.
      */
    case class Success(sym: Symbol.DefnSym, d: Duration) extends TestEvent

    /**
      * A test event emitted to indicate that a test failed.
      */
    case class Failure(sym: Symbol.DefnSym, output: List[String], d: Duration) extends TestEvent

    /**
      * A test event emitted to indicates that testing has completed.
      */
    case class Finished(d: Duration) extends TestEvent
  }

}
