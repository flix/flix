/*
 * Copyright 2016 Magnus Madsen
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

package ca.uwaterloo.flix.runtime

import java.nio.file.StandardOpenOption._
import java.nio.file.{Files, Path}

import ca.uwaterloo.flix.api._
import ca.uwaterloo.flix.language.Compiler
import ca.uwaterloo.flix.language.ast.{ExecutableAst, PrettyPrinter}
import ca.uwaterloo.flix.util.{Options, Verbosity}

object DeltaSolver {

  /**
    * A common super-type to represent the result of running the solver.
    */
  sealed trait SolverResult

  object SolverResult {

    /**
      * The solver successfully computed the minimal model.
      */
    case object Success extends SolverResult

    /**
      * The solver failed with the *same* exception as the original exception.
      */
    case object FailSameException extends SolverResult

    /**
      * The solver failed with a *different* exception than the original exception.
      */
    case object FailDiffException extends SolverResult

  }

  /**
    * Runs the delta debugger on the given program.
    *
    * @param root    the program.
    * @param options the Flix options.
    * @param path    the path to write the minimized facts to.
    */
  def solve(root: ExecutableAst.Root, options: Options, path: Path): Unit = {
    val c = Compiler.ConsoleCtx

    /*
     * Refuse to overwrite existing file.
     */
    if (Files.exists(path)) {
      Console.err.println(s"Path `$path' already exists. Refusing to overwrite.")
      System.exit(1)
    }

    /*
     * Attempts to determine the exception the (original) program crashes with.
     */
    val exception = tryInit(root, options).getOrElse {
      Console.err.println(s"The program ran successfully. No need for delta debugging?")
      System.exit(1)
      null
    }

    /*
     * Print information about the caught exception.
     */
    Console.println(c.blue(s"Caught `${exception.getClass.getName}' with message:"))
    Console.println(c.blue(s"    `${exception.getMessage}'"))
    Console.println(c.blue(s"Delta Debugging Started. Trying to minimize ${root.facts.length} facts."))
    Console.println()

    /*
     * Repeatedly minimize and re-run the program.
     */
    var globalIteration = 1
    var globalFacts = root.facts.toSet
    var globalBlockSize = root.facts.length / 2
    val totalNumberOfFacts = globalFacts.size

    while (globalBlockSize >= 1) {
      Console.println(f"--- iteration: $globalIteration%3d, current facts: ${globalFacts.size}%5d, block size: $globalBlockSize%5d ---")

      // partition the facts into blocks of `size`.
      val blocks = globalFacts.grouped(globalBlockSize).toSet

      // a local variable to hold the current facts.
      var facts = blocks
      var round = 1
      for (block <- blocks) {
        // every fact except for those in the current block.
        facts = facts - block

        // try to solve the program.
        trySolve(root.copy(facts = facts.flatten.toArray), options, exception) match {
          case SolverResult.Success =>
            // the program successfully completed. Must backtrack.
            Console.println(c.red(f"    [block $round%3d] ${block.size}%5d fact(s) retained (program ran successfully)."))
            facts = facts + block // put the block back
          case SolverResult.FailDiffException =>
            // the program failed with a different exception. Must backtrack.
            Console.println(c.red(f"    [block $round%3d] ${block.size}%5d fact(s) retained (different exception)."))
            facts = facts + block // put the block back
          case SolverResult.FailSameException =>
            // the program failed with the same exception. Continue minimization.
            Console.println(c.green(f"    [block $round%3d] ${block.size}%5d fact(s) discarded."))
        }

        round += 1
      }

      // increase the iteration count.
      globalIteration += 1
      // update the global facts variable.
      globalFacts = facts.flatten
      // decrease the global block size.
      globalBlockSize = globalBlockSize / 2

      val numberOfFacts = globalFacts.size
      val percentage = 100.0 * numberOfFacts.toDouble / totalNumberOfFacts.toDouble
      Console.println(f"--- Progress: $numberOfFacts%4d out of $totalNumberOfFacts%4d facts ($percentage%2.1f%%) --- ")
      Console.println()

      writeFacts(globalFacts, path)
    }

    Console.println(c.green(s"    >>> Delta Debugging Complete! <<<"))
    Console.println(c.green(s"    >>> Output written to `$path'. <<<"))

    Console.println()
  }

  /**
    * Optionally returns the exception thrown by the original program.
    */
  def tryInit(root: ExecutableAst.Root, options: Options): Option[RuntimeException] = {
    try {
      runSolver(root, options)
      None
    } catch {
      case ex: RuntimeException => Some(ex)
    }
  }

  /**
    * Attempts to solve the given program expects `expectedException` to be thrown.
    */
  def trySolve(root: ExecutableAst.Root, options: Options, expectedException: RuntimeException): SolverResult = {
    try {
      // run the solver.
      runSolver(root, options)
      // the solver successfully completed, return `Success`.
      SolverResult.Success
    } catch {
      case actualException: RuntimeException =>
        // the solver crashed with an exception.
        if (sameException(expectedException, actualException)) {
          // the solver failed with the *same* exception.
          SolverResult.FailSameException
        } else {
          // the solver failed with a *different* exception.
          SolverResult.FailDiffException
        }
    }
  }

  /**
    * Returns `true` iff the two given exceptions `ex1` and `ex2` are the same.
    *
    * @param ex1 the 1st exception.
    * @param ex2 the 2nd exception.
    * @return `true` iff `ex1` is equal to `ex2`.
    */
  def sameException(ex1: RuntimeException, ex2: RuntimeException): Boolean = (ex1, ex2) match {
    case (MatchException(msg1, loc1), MatchException(msg2, loc2)) => msg1 == msg2 && loc1 == loc2
    case (RuleException(msg1, loc1), RuleException(msg2, loc2)) => msg1 == msg2 && loc1 == loc2
    case (SwitchException(msg1, loc1), SwitchException(msg2, loc2)) => msg1 == msg2 && loc1 == loc2
    case (TimeoutException(_, _), TimeoutException(_, _)) => true
    case (UserException(msg1, loc1), UserException(msg2, loc2)) => msg1 == msg2 && loc1 == loc2
    case _ => ex1.getClass == ex2.getClass
  }

  /**
    * Runs the solver.
    */
  private def runSolver(root: ExecutableAst.Root, options: Options): Unit = {
    // silence output from the solver.
    val opts = options.copy(verbosity = Verbosity.Silent)
    new Solver(root, opts).solve()
  }

  /**
    * Writes the given `facts` to the given `path`.
    */
  private def writeFacts(facts: Traversable[ExecutableAst.Constraint.Fact], path: Path): Unit = {
    val writer = Files.newBufferedWriter(path, WRITE, CREATE, TRUNCATE_EXISTING)
    for (fact <- facts) {
      val formattedFact = PrettyPrinter.fmt(fact, new StringBuilder).toString
      writer.write(formattedFact)
      writer.newLine()
    }
    writer.close()
  }

}
