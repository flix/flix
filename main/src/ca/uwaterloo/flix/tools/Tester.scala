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
    val queue = new ConcurrentLinkedQueue[TestEvent]()
    val reporter = new TestReporter(queue, compilationResult)
    val runner = new TestRunner(queue, compilationResult)
    reporter.start()
    runner.start()

    reporter.join()
    runner.join()
  }

  // TODO: DOC
  private class TestReporter(queue: ConcurrentLinkedQueue[TestEvent], compilationResult: CompilationResult)(implicit flix: Flix) extends Thread {

    override def run(): Unit = {
      // Silence JLine warnings about terminal type.
      Logger.getLogger("org.jline").setLevel(Level.OFF)

      // Initialize the terminal.
      implicit val terminal: Terminal = TerminalBuilder
        .builder()
        .system(true)
        .build()

      var passed = 0
      var failed = 0

      while (true) {
        queue.poll() match {
          case TestEvent.Success(sym, elapsed) =>
            passed = passed + 1
            terminal.writer().println(s"  - ${green(sym.toString)} ${magenta(TimeOps.toMilliSeconds(elapsed) + "ms")}")
            terminal.flush()

          case TestEvent.Failure(sym, elapsed) =>
            failed = failed + 1
            terminal.writer().println(s"  - ${red(sym.toString)} ${magenta(TimeOps.toMilliSeconds(elapsed) + "ms")}")
            terminal.flush()

          case TestEvent.Finished(elapsed) =>
            terminal.writer().println()
            terminal.writer().println(s"Finished. Passed: ${passed}, Failed: ${failed}. Elapsed: $elapsed ")
            terminal.flush()
            return
          case _ => // nop
        }
      }
      throw InternalCompilerException("Unreachable")
    }

    // TODO: Use flix.formatter
    private def green(s: String): String = Console.GREEN + s + Console.RESET

    // TODO: Use flix.formatter
    private def magenta(s: String): String = Console.MAGENTA + s + Console.RESET

    // TODO: Use flix.formatter
    private def red(s: String): String = Console.RED + s + Console.RESET
  }

  // TODO: DOC
  private class TestRunner(queue: ConcurrentLinkedQueue[TestEvent], compilationResult: CompilationResult)(implicit flix: Flix) extends Thread {

    // TODO: DOC
    override def run(): Unit = {
      val results = compilationResult.getTests.toList.map {
        case (sym, defn) => runTest(sym, defn)
      }
      queue.add(TestEvent.Finished(0)) // TODO
    }

    // TODO: DOC
    private def runTest(sym: Symbol.DefnSym, defn: () => AnyRef): Unit = {
      val start = System.nanoTime()
      try {
        queue.add(TestEvent.Before(sym))

        val result = defn()

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
    * A common super-type for test events.
    */
  sealed trait TestEvent

  object TestEvent {

    /**
      * A test event emitted immediately before a test case is executed.
      */
    case class Before(sym: Symbol.DefnSym) extends TestEvent

    // TODO: DOC
    case class Success(sym: Symbol.DefnSym, time: Long) extends TestEvent

    // TODO: DOC
    case class Failure(sym: Symbol.DefnSym, time: Long) extends TestEvent

    // TODO: DOC
    case class Finished(time: Long) extends TestEvent
  }

}
