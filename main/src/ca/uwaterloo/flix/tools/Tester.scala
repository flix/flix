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
import ca.uwaterloo.flix.runtime.{CompilationResult, TestFn}
import ca.uwaterloo.flix.util.Duration
import org.jline.terminal.{Terminal, TerminalBuilder}

import java.io.{ByteArrayOutputStream, PrintStream, PrintWriter, StringWriter}
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.logging.{Level, Logger}
import scala.util.matching.Regex

/**
  * Evaluates all tests in a Flix program.
  */
object Tester {

  /**
    * Runs all tests.
    */
  def run(filters: List[Regex], compilationResult: CompilationResult)(implicit flix: Flix): Unit = {
    //
    // Find all test cases (both active and ignored).
    //
    val tests = getTestCases(filters, compilationResult)

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

      // Import formatter.
      val formatter = flix.getFormatter
      import formatter._

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
      var skipped = 0
      var failed: List[(Symbol.DefnSym, List[String])] = Nil
      var invalid: List[(Symbol.DefnSym, String)] = Nil

      var finished = false
      while (!finished) {
        queue.poll() match {
          case TestEvent.Before(sym) =>
            // Note: Print \r to reset the caret.
            writer.print(s"  ${bgYellow(" TEST ")} $sym\r")
            terminal.flush()

          case TestEvent.Success(sym, elapsed) =>
            passed = passed + 1
            writer.println(s"  ${bgGreen(" PASS ")} $sym ${brightBlack(elapsed.fmt)}")
            terminal.flush()

          case TestEvent.Failure(sym, output, elapsed) =>
            failed = (sym, output) :: failed
            val line = output.headOption.map(s => s"(${red(s)})").getOrElse("")
            writer.println(s"  ${bgRed(" FAIL ")} $sym $line")
            terminal.flush()

          case TestEvent.Invalid(sym, reason) =>
            invalid = (sym, reason) :: invalid
            writer.println(s"  ${bgRed(" INVALID ")} $sym $reason")
            terminal.flush()

          case TestEvent.Skip(sym) =>
            skipped = skipped + 1
            writer.println(s"  ${bgYellow(" SKIP ")} $sym (${yellow("SKIPPED")})")
            terminal.flush()

          case TestEvent.Finished(elapsed) =>
            // Print the std out / std err of every failed test.
            if (failed.nonEmpty) {
              writer.println()
              writer.println("-" * 80)
              writer.println()
              for ((sym, output) <- failed; if output.nonEmpty) {
                writer.println(s"  ${bgRed(" FAIL ")} $sym")
                for (line <- output) {
                  writer.println(s"    $line")
                }
                writer.println()
              }
              for ((sym, reason) <- invalid) {
                writer.println(s"  ${bgRed(" INVALID ")} $sym")
                writer.println(s"    $reason")
                writer.println()
              }
              writer.println("-" * 80)
            }

            // Print the summary.
            writer.println()
            writer.println(
              s"Passed: ${green(passed.toString)}, " +
                s"Failed: ${red(failed.length.toString)}. " +
                s"Invalid: ${red(invalid.length.toString)}. " +
                s"Skipped: ${yellow(skipped.toString)}. " +
                s"Elapsed: ${brightBlack(elapsed.fmt)}."
            )
            terminal.flush()
            finished = true

          case _ => // nop
        }
      }
    }

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
      case TestCase(sym, TestFn.RunProperty.Skipped) => queue.add(TestEvent.Skip(sym))
      case TestCase(sym, TestFn.RunProperty.Invalid(reason)) => queue.add(TestEvent.Invalid(sym, reason))
      case TestCase(sym, TestFn.RunProperty.Runnable(run)) =>

        // We are about to run the test case.
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
              queue.add(TestEvent.Failure(sym, "Assertion Error" :: Nil, Duration(elapsed)))

            case _ =>
              queue.add(TestEvent.Success(sym, Duration(elapsed)))

          }
        } catch {
          case ex: Throwable =>
            // Restore std out and std err.
            redirect.restore()

            // Compute elapsed time.
            val elapsed = System.nanoTime() - start
            queue.add(TestEvent.Failure(sym, redirect.stdOut ++ redirect.stdErr ++ fmtStackTrace(ex), Duration(elapsed)))
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
    * Returns all test cases from the given compilation `result` which satisfy at least one filter.
    */
  private def getTestCases(filters: List[Regex], compilationResult: CompilationResult): Vector[TestCase] = {
    /**
      * Returns `true` if at least one filter matches the given symbol _OR_ if there are no filters.
      */
    def isMatch(test: TestCase): Boolean = {
      val name = test.sym.toString
      filters.isEmpty || filters.exists(regex => regex.matches(name))
    }

    val allTests = compilationResult.getTests.map {
      case (sym, TestFn(_, runProp)) => TestCase(sym, runProp)
    }

    allTests.filter(isMatch).toVector.sorted
  }

  /**
    * Returns the stack trace of the given exception `ex` as a list of strings.
    */
  private def fmtStackTrace(ex: Throwable): Vector[String] = {
    val sw = new StringWriter()
    val pw = new PrintWriter(sw)
    ex.printStackTrace(pw)
    sw.toString.linesIterator.toVector
  }

  /**
    * Represents a single test case.
    *
    * @param sym     the Flix symbol.
    * @param runProp the property relevant to running the test
    */
  case class TestCase(sym: Symbol.DefnSym, runProp: TestFn.RunProperty) extends Ordered[TestCase] {
    override def compare(that: TestCase): Int = this.sym.toString.compareTo(that.sym.toString)
  }

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
      * A test event emitted to indicate that a test was ignored.
      */
    case class Skip(sym: Symbol.DefnSym) extends TestEvent

    /**
      * A test event emitted to indicate that a test was invalid.
      */
    case class Invalid(sym: Symbol.DefnSym, reason: String) extends TestEvent

    /**
      * A test event emitted to indicates that testing has completed.
      */
    case class Finished(d: Duration) extends TestEvent
  }

}
