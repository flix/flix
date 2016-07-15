/*
 * Copyright 2015-2016 Magnus Madsen
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

package ca.uwaterloo.flix

import java.io.File

import ca.uwaterloo.flix.api._
import ca.uwaterloo.flix.util._

import scala.concurrent.duration.Duration

/**
  * The main entry point for the Flix compiler and runtime.
  */
object Main {

  /**
    * The main method.
    */
  def main(argv: Array[String]): Unit = {

    // parse command line options.
    val cmdOpts: CmdOpts = parseCmdOpts(argv).getOrElse {
      Console.err.println("Unable to parse command line arguments. Will now exit.")
      System.exit(1)
      null
    }

    // check if the --tutorial flag was passed.
    if (cmdOpts.tutorial != null) {
      printTutorial(cmdOpts.tutorial)
      System.exit(0)
    }

    // check that some input files were passed.
    if (cmdOpts.files.isEmpty && !cmdOpts.pipe) {
      Console.err.println("No input. Try --help.")
      System.exit(1)
    }

    // construct flix options.
    val options = Options.Default.copy(
      debug = cmdOpts.xdebug,
      evaluation = if (cmdOpts.xinterpreter) Evaluation.Interpreted else Evaluation.Compiled,
      optimize = cmdOpts.optimize,
      monitor = cmdOpts.monitor,
      quickchecker = cmdOpts.quickchecker,
      timeout = cmdOpts.timeout,
      threads = if (cmdOpts.threads == -1) Options.Default.threads else cmdOpts.threads,
      verbosity = if (cmdOpts.verbose) Verbosity.Verbose else Verbosity.Normal,
      verifier = cmdOpts.verifier
    )

    // configure Flix and add the paths.
    val flix = new Flix()
    flix.setOptions(options)
    for (file <- cmdOpts.files) {
      flix.addPath(file.toPath)
    }

    // read input from standard-in.
    if (cmdOpts.pipe) {
      val s = StreamOps.readAll(Console.in)
      flix.addStr(s)
    }

    // check if we are running in delta debugging mode.
    if (cmdOpts.delta.nonEmpty) {
      flix.deltaSolve(cmdOpts.delta.get.toPath) match {
        case Validation.Success(_, errors) =>
          errors.foreach(e => println(e.message))
          System.exit(0)
        case Validation.Failure(errors) =>
          errors.foreach(e => println(e.message))
          System.exit(1)
      }
    }

    // compute the least model.
    try {
      flix.solve() match {
        case Validation.Success(model, errors) =>
          errors.foreach(e => println(e.message))

          val print = cmdOpts.print
          for (name <- print) {
            PrettyPrint.print(name, model)
          }
        case Validation.Failure(errors) =>
          errors.foreach(e => println(e.message))
      }
    } catch {
      case UserException(msg, loc) =>
        Console.err.println("User error " + loc.format)
        Console.err.println()
        Console.err.println(loc.underline(new AnsiConsole))
        System.exit(1)
      case MatchException(msg, loc) =>
        Console.err.println("Non-exhaustive match " + loc.format)
        Console.err.println()
        Console.err.println(loc.underline(new AnsiConsole))
        System.exit(1)
      case SwitchException(msg, loc) =>
        Console.err.println("Non-exhaustive switch " + loc.format)
        Console.err.println()
        Console.err.println(loc.underline(new AnsiConsole))
        System.exit(1)
      case RuleException(msg, loc) =>
        Console.err.println("Integrity rule violated " + loc.format)
        Console.err.println()
        Console.err.println(loc.underline(new AnsiConsole))
        System.exit(1)
    }

  }

  /**
    * A case class representing the parsed command line options.
    */
  case class CmdOpts(delta: Option[File] = None,
                     monitor: Boolean = false,
                     optimize: Boolean = false,
                     pipe: Boolean = false,
                     print: Seq[String] = Seq(),
                     quickchecker: Boolean = false,
                     threads: Int = -1,
                     timeout: Duration = Duration.Inf,
                     tutorial: String = null,
                     verbose: Boolean = false,
                     verifier: Boolean = false,
                     xdebug: Boolean = false,
                     xinterpreter: Boolean = false,
                     xinvariants: Boolean = false,
                     files: Seq[File] = Seq())

  /**
    * Parse command line options.
    *
    * @param args the arguments array.
    */
  def parseCmdOpts(args: Array[String]): Option[CmdOpts] = {
    val parser = new scopt.OptionParser[CmdOpts]("flix") {

      // Head
      head("The Flix Programming Language", Version.CurrentVersion.toString)

      // Delta.
      opt[File]("delta").action((f, c) => c.copy(delta = Some(f))).
        valueName("<file>").
        text("enables the delta debugger. Output facts to <file>.")

      // Help.
      help("help").text("prints this usage information.")

      // Monitor.
      opt[Unit]("monitor").action((_, c) => c.copy(monitor = true)).
        text("enables the debugger and profiler.")

      // Optimize.
      opt[Unit]("optimize").action((_, c) => c.copy(optimize = true))
        .text("enables compiler optimizations.")

      // Pipe.
      opt[Unit]("pipe").action((_, c) => c.copy(pipe = true)).
        text("reads from standard input.")

      // Print.
      opt[Seq[String]]("print").action((xs, c) => c.copy(print = xs)).
        valueName("<name>...").
        text("prints the named relations/lattices.")

      // Quickchecker.
      opt[Unit]("quickchecker").action((_, c) => c.copy(quickchecker = true)).
        text("enables the quickchecker.")

      // Timeout
      opt[Duration]("timeout").action((d, c) => c.copy(timeout = d)).
        valueName("<n>").
        text("sets the solver timeout (1ms, 1s, 1min, etc).")

      // Threads.
      opt[Int]("threads").action((i, c) => c.copy(threads = i)).
        validate(x => if (x > 0) success else failure("Value <n> must be at least 1.")).
        valueName("<n>").
        text("sets the number of threads to use.")

      // Tutorial.
      opt[String]("tutorial").action((f, c) => c.copy(tutorial = f)).
        valueName("<name>").
        text("prints the named tutorial to stdout. Try `--tutorial help'.")

      // Verbose.
      opt[Unit]("verbose").action((_, c) => c.copy(verbose = true))
        .text("enables verbose output.")

      // Verifier.
      opt[Unit]("verifier").action((_, c) => c.copy(verifier = true)).
        text("enables the verifier.")

      // Version.
      version("version").text("prints the version number.")

      // Experimental options:
      note("")
      note("The following options are experimental:")

      // Xdebug.
      opt[Unit]("Xdebug").action((_, c) => c.copy(xdebug = true)).
        text("[experimental] enables output of debugging information.")

      // Xinterpreter.
      opt[Unit]("Xinterpreter").action((_, c) => c.copy(xinterpreter = true)).
        text("[experimental] enables interpreted evaluation.")

      // Xinvariants.
      opt[Unit]("Xinvariants").action((_, c) => c.copy(xinvariants = true)).
        text("[experimental] enables compiler invariants.")

      note("")

      // Input files.
      arg[File]("<file>...").action((x, c) => c.copy(files = c.files :+ x))
        .optional()
        .unbounded()
        .text("input Flix source code files.")

    }

    parser.parse(args, CmdOpts())
  }

  /**
    * Prints the given tutorial to standard out.
    */
  def printTutorial(name: String): Unit = {
    val inputStream = name match {
      case "delta-debugging" => LocalResource.Tutorials.DeltaDebugging
      case "introduction" => LocalResource.Tutorials.Introduction
      case _ =>
        Console.println("No match. Available tutorials:")
        Console.println("  introduction")
        Console.println("  delta-debugging")
        System.exit(1)
        null
    }

    StreamOps.writeAll(inputStream, Console.out)
    inputStream.close()
  }

}
