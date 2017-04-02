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
import ca.uwaterloo.flix.runtime.{Benchmarker, Tester, Value}
import ca.uwaterloo.flix.util._
import ca.uwaterloo.flix.util.vt.VirtualString.{Code, Line, NewLine}
import ca.uwaterloo.flix.util.vt._

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

    // check if the --listen flag was passed.
    if (cmdOpts.listen.nonEmpty) {
      val rpcServer = new RpcServer(cmdOpts.listen.get)
      rpcServer.start()
      rpcServer.await()
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
      core = cmdOpts.xcore,
      debug = cmdOpts.xdebug,
      documentor = cmdOpts.documentor,
      evaluation = if (cmdOpts.xinterpreter) Evaluation.Interpreted else Evaluation.Compiled,
      impure = cmdOpts.ximpure,
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

    // check if a main function was given.
    val main = cmdOpts.main
    if (main.nonEmpty) {
      val name = main.get
      flix.addReachableRoot(name)
    }

    // the default color context.
    implicit val _ = TerminalContext.AnsiTerminal

    // check if we are running in delta debugging mode.
    if (cmdOpts.delta.nonEmpty) {
      flix.deltaSolve(cmdOpts.delta.get.toPath) match {
        case Validation.Success(_, errors) =>
          errors.foreach(e => println(e.message.fmt))
          System.exit(0)
        case Validation.Failure(errors) =>
          errors.foreach(e => println(e.message.fmt))
          System.exit(1)
      }
    }

    // compute the least model.
    try {
      flix.solve() match {
        case Validation.Success(model, errors) =>
          errors.foreach(e => println(e.message.fmt))

          val main = cmdOpts.main
          if (main.nonEmpty) {
            val name = main.get
            val timer = new Timer(model.getConstant(name))
            Console.println(s"$name returned `${Value.pretty(timer.getResult)}' (elapsed ${timer.fmt})")
          }

          if (cmdOpts.benchmark) {
            Benchmarker.benchmark(model)
          }

          if (cmdOpts.test) {
            val results = Tester.test(model)
            Console.println(results.getMessage.fmt)
          }

          val print = cmdOpts.print
          for (name <- print) {
            PrettyPrint.print(name, model)
          }
        case Validation.Failure(errors) =>
          errors.foreach(e => println(e.message.fmt))
      }
    } catch {
      case UserException(msg, loc) =>
        val vt = new VirtualTerminal()
        vt << Line("User Error", loc.source.format) << NewLine
        vt << Code(loc, msg) << NewLine
        Console.println(vt.fmt)
        System.exit(1)
      case MatchException(msg, loc) =>
        val vt = new VirtualTerminal()
        vt << Line("Non-exhaustive match", loc.source.format) << NewLine
        vt << Code(loc, msg) << NewLine
        Console.println(vt.fmt)
        System.exit(1)
      case SwitchException(msg, loc) =>
        val vt = new VirtualTerminal()
        vt << Line("Non-exhaustive switch", loc.source.format) << NewLine
        vt << Code(loc, msg) << NewLine
        Console.println(vt.fmt)
        System.exit(1)
      case RuleException(msg, loc) =>
        val vt = new VirtualTerminal()
        vt << Line("Integrity rule violated", loc.source.format) << NewLine
        vt << Code(loc, msg) << NewLine
        Console.println(vt.fmt)
        System.exit(1)
    }

  }

  /**
    * A case class representing the parsed command line options.
    */
  case class CmdOpts(benchmark: Boolean = false,
                     delta: Option[File] = None,
                     documentor: Boolean = false,
                     listen: Option[Int] = None,
                     main: Option[String] = None,
                     monitor: Boolean = false,
                     optimize: Boolean = false,
                     pipe: Boolean = false,
                     print: Seq[String] = Seq(),
                     quickchecker: Boolean = false,
                     threads: Int = -1,
                     test: Boolean = false,
                     timeout: Duration = Duration.Inf,
                     tutorial: String = null,
                     verbose: Boolean = false,
                     verifier: Boolean = false,
                     xcore: Boolean = false,
                     xdebug: Boolean = false,
                     ximpure: Boolean = false,
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

      // Benchmark.
      opt[Unit]("benchmark").action((_, c) => c.copy(benchmark = true)).
        text("runs benchmarks.")

      // Doc.
      opt[Unit]("doc").action((_, c) => c.copy(documentor = true)).
        text("generates HTML documentation.")

      // Delta.
      opt[File]("delta").action((f, c) => c.copy(delta = Some(f))).
        valueName("<file>").
        text("enables the delta debugger. Output facts to <file>.")

      // Help.
      help("help").text("prints this usage information.")

      // Listen.
      opt[Int]("listen").action((s, c) => c.copy(listen = Some(s))).
        valueName("<port>").
        text("listens on the given port.")

      // Main.
      opt[String]("main").action((s, c) => c.copy(main = Some(s))).
        valueName("<name>").
        text("evaluates the <name> function.")

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

      // Test.
      opt[Unit]("test").action((_, c) => c.copy(test = true)).
        text("runs unit tests.")

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

      // Xcore.
      opt[Unit]("Xcore").action((_, c) => c.copy(xcore = true)).
        text("[experimental] disables loading of all non-essential namespaces.")

      // Xdebug.
      opt[Unit]("Xdebug").action((_, c) => c.copy(xdebug = true)).
        text("[experimental] enables output of debugging information.")

      // Ximpure.
      opt[Unit]("Ximpure").action((_, c) => c.copy(ximpure = true)).
        text("[experimental] enables impure functions.")

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
      case "interpreter" => LocalResource.Tutorials.Interpreter
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
