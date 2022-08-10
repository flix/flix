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
import ca.uwaterloo.flix.util.{InternalCompilerException, TimeOps}
import org.jline.terminal.{Terminal, TerminalBuilder}

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
      writer.println(s"Testing started... Total tests: ${tests.length}.")

      // Main event loop.
      var passed = 0
      var failed = 0

      var finished = false
      while (!finished) {
        queue.poll() match {
          case TestEvent.Success(sym, elapsed) =>
            passed = passed + 1
            writer.println(s"  - ${green(sym)} ${magenta(TimeOps.toMilliSeconds(elapsed) + "ms")}")
            terminal.flush()

          case TestEvent.Failure(sym, elapsed) =>
            failed = failed + 1
            writer.println(s"  - ${red(sym)} ${magenta(TimeOps.toMilliSeconds(elapsed) + "ms")}")
            terminal.flush()

          case TestEvent.Finished(elapsed) =>
            writer.println()
            writer.println(s"Finished. Passed: $passed, Failed: $failed. Elapsed: $elapsed ms.")
            terminal.flush()
            finished = true

          case _ => // nop
        }
      }
    }

    // TODO: Use flix.formatter
    private def green(s: AnyRef): String = Console.GREEN + s + Console.RESET

    // TODO: Use flix.formatter
    private def magenta(s: AnyRef): String = Console.MAGENTA + s + Console.RESET

    // TODO: Use flix.formatter
    private def red(s: AnyRef): String = Console.RED + s + Console.RESET
  }

  // TODO: DOC
  private class TestRunner(queue: ConcurrentLinkedQueue[TestEvent], tests: Vector[TestCase])(implicit flix: Flix) extends Thread {

    // TODO: DOC
    override def run(): Unit = {
      for (testCase <- tests) {
        runTest(testCase)
      }
      queue.add(TestEvent.Finished(0)) // TODO
    }

    // TODO: DOC
    private def runTest(testCase: TestCase): Unit = testCase match {
      case TestCase(sym, run) =>
        val start = System.nanoTime()
        try {
          queue.add(TestEvent.Before(sym))

          val result = run()

          val elapsed = System.nanoTime() - start // TODO

          result match {
            case java.lang.Boolean.TRUE =>
              queue.add(TestEvent.Success(sym, elapsed))
            case java.lang.Boolean.FALSE =>
              queue.add(TestEvent.Failure(sym, elapsed))
            case _ =>
              queue.add(TestEvent.Success(sym, elapsed))
          }
        } catch {
          case ex: Exception =>
            queue.add(TestEvent.Failure(sym, 0)) // TODO
        }
    }
  }

  /**
    * Returns all test cases from the given compilation `result`.
    */
  private def getTestCases(compilationResult: CompilationResult): Vector[TestCase] =
    compilationResult.getTests.toVector.sortBy(_._1.loc).map {
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
    case class Success(sym: Symbol.DefnSym, time: Long) extends TestEvent

    /**
      * A test event emitted to indicate that a test failed.
      */
    case class Failure(sym: Symbol.DefnSym, time: Long) extends TestEvent

    /**
      * A test event emitted to indicates that testing has completed.
      */
    case class Finished(time: Long) extends TestEvent
  }

}
