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
import ca.uwaterloo.flix.util.{Duration, Result}
import org.jline.terminal.{Terminal, TerminalBuilder}

import java.io.{ByteArrayOutputStream, OutputStream, PrintStream, PrintWriter, StringWriter}
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
  def run(filters: List[Regex], compilationResult: CompilationResult)(implicit flix: Flix): Result[Unit, Int] = {
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

    if (reporter.isSuccess()) {
      Result.Ok(())
    } else {
      // Set exit code of program to 1.
      Result.Err(1)
    }
  }

  /**
    * A class that reports the results of test events as they come in.
    */
  private class TestReporter(queue: ConcurrentLinkedQueue[TestEvent], tests: Vector[TestCase])(implicit flix: Flix) extends Thread {

    private val success = new java.util.concurrent.atomic.AtomicBoolean(true)

    def isSuccess(): Boolean = {
      success.get()
    }

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
            success.set(false)

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
                writer.println(s"         ${sym.loc.source.name}:${sym.loc.beginLine}")
                for (line <- output) {
                  writer.println(s"    $line")
                }
                writer.println()
              }
              writer.println("-" * 80)
            }

            // Print the summary.
            writer.println()
            writer.println(
              s"Passed: ${green(passed.toString)}, " +
                s"Failed: ${red(failed.length.toString)}. " +
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
      case TestCase(sym, skip, run) =>
        // Check if the test case should be ignored.
        if (skip) {
          queue.add(TestEvent.Skip(sym))
          return
        }

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
            case java.lang.Boolean.FALSE =>
              // Case 1: Assertion Error.
              queue.add(TestEvent.Failure(sym, "Assertion Error" :: redirect.stdOut ++ redirect.stdErr, Duration(elapsed)))

            case _ =>
              if (redirect.stdErr.isEmpty) {
                // Case 2: Non-False result and no stderr output.
                queue.add(TestEvent.Success(sym, Duration(elapsed)))
              } else {
                // Case 3: Non-False result, but with stderr output.
                queue.add(TestEvent.Failure(sym, "Std Err Output" :: redirect.stdOut ++ redirect.stdErr, Duration(elapsed)))
              }

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
    * A class which outputs to two different output streams
    *
    * Largely taken from org.apache.commons.io.output.TeeOutputStream
    */
  class TeeOutputStream(out: OutputStream, branch: OutputStream) extends PrintStream(out) {

    override def write(b: Array[Byte]) = synchronized {
      super.write(b)
      branch.write(b)
    }

    override def write(b: Array[Byte], off: Int, len: Int) = synchronized {
      super.write(b, off, len)
      branch.write(b, off, len)
    }

    override def write(b: Int) = synchronized {
      super.write(b)
      branch.write(b)
    }

    override def flush() = synchronized {
      super.flush()
      branch.flush()
    }

    override def close() = synchronized {
      try {
        super.close()
      } finally {
        branch.close()
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
      System.setOut(new TeeOutputStream(streamOut, oldStreamOut))
      System.setErr(new TeeOutputStream(streamErr, oldStreamErr))
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
      case (sym, TestFn(_, skip, run)) => TestCase(sym, skip, run)
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
    * @param sym  the Flix symbol.
    * @param skip true if the test case should be skipped.
    * @param run  the code to run.
    */
  case class TestCase(sym: Symbol.DefnSym, skip: Boolean, run: () => AnyRef) extends Ordered[TestCase] {
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
      * A test event emitted to indicates that testing has completed.
      */
    case class Finished(d: Duration) extends TestEvent
  }

}
