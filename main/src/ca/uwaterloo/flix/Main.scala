/*
 * Copyright 2019 Magnus Madsen
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

import java.io.{File, PrintWriter}
import java.nio.file.Paths

import ca.uwaterloo.flix.api.{Flix, Version}
import ca.uwaterloo.flix.runtime.shell.Shell
import ca.uwaterloo.flix.tools.{Benchmarker, Packager, Tester}
import ca.uwaterloo.flix.util._
import ca.uwaterloo.flix.util.vt._
import flix.runtime.FlixError

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

    // the default color context.
    implicit val _ = TerminalContext.AnsiTerminal

    // compute the enabled optimizations.
    val optimizations = Optimization.All.filter {
      case Optimization.TailCalls => !cmdOpts.xnotailcalls
    }

    // construct flix options.
    val options = Options.Default.copy(
      core = cmdOpts.xcore,
      debug = cmdOpts.xdebug,
      documentor = cmdOpts.documentor,
      evaluation = if (cmdOpts.xinterpreter) Evaluation.Interpreted else Evaluation.Compiled,
      optimizations = optimizations,
      mode = if (cmdOpts.release) CompilationMode.Release else CompilationMode.Development,
      monitor = cmdOpts.monitor,
      quickchecker = cmdOpts.quickchecker,
      verbosity = if (cmdOpts.verbose) Verbosity.Verbose else Verbosity.Normal,
      verifier = cmdOpts.verifier,
      writeClassFiles = !cmdOpts.interactive
    )

    // check if command was passed.
    try {
      val cwd = Paths.get(".")

      cmdOpts.command match {
        case Command.None =>
        // nop, continue

        case Command.Init =>
          Packager.init(cwd, options)
          System.exit(0)

        case Command.Build =>
          Packager.build(cwd, options)
          System.exit(0)

        case Command.BuildJar =>
          Packager.buildJar(cwd, options)
          System.exit(0)

        case Command.BuildPkg =>
          Packager.buildPkg(cwd, options)
          System.exit(0)

        case Command.Run =>
          Packager.run(cwd, options)
          System.exit(0)

        case Command.Benchmark =>
          Packager.benchmark(cwd, options)
          System.exit(0)

        case Command.Test =>
          Packager.test(cwd, options)
          System.exit(0)
      }
    } catch {
      case ex: RuntimeException =>
        Console.println(ex.getMessage)
        System.exit(1)
    }

    // check if running in interactive mode.
    val interactive = cmdOpts.interactive || (cmdOpts.command == Command.None && cmdOpts.files.isEmpty)
    if (interactive) {
      val shell = new Shell(cmdOpts.files.toList.map(_.toPath), options)
      shell.loop()
      System.exit(0)
    }

    // configure Flix and add the paths.
    val flix = new Flix()
    flix.setOptions(options)
    for (file <- cmdOpts.files) {
      flix.addPath(file.toPath)
    }

    // compute the least model.
    try {
      val timer = new Timer(flix.compile())
      timer.getResult match {
        case Validation.Success(compilationResult) =>

          compilationResult.getMain match {
            case None => // nop
            case Some(m) =>
              val evalTimer = new Timer(compilationResult.evalToString("main"))
              options.verbosity match {
                case Verbosity.Normal => Console.println(evalTimer.getResult)
                case Verbosity.Verbose => Console.println(s"main returned `${evalTimer.getResult}' (compile: ${timer.getDuration.fmt}, execute: ${evalTimer.getDuration.fmt})")
                case Verbosity.Silent => // nop
              }
          }

          if (cmdOpts.benchmark) {
            Benchmarker.benchmark(compilationResult, new PrintWriter(System.out, true))
          }

          if (cmdOpts.test) {
            val results = Tester.test(compilationResult)
            Console.println(results.output.fmt)
          }
        case Validation.Failure(errors) =>
          errors.foreach(e => println(e.message.fmt))
      }
    } catch {
      case ex: FlixError =>
        Console.println(ex.getMessage)
        System.exit(1)
    }

  }

  /**
    * A case class representing the parsed command line options.
    */
  case class CmdOpts(command: Command = Command.None,
                     benchmark: Boolean = false,
                     documentor: Boolean = false,
                     interactive: Boolean = false,
                     listen: Option[Int] = None,
                     monitor: Boolean = false,
                     quickchecker: Boolean = false,
                     release: Boolean = false,
                     test: Boolean = false,
                     verbose: Boolean = false,
                     verifier: Boolean = false,
                     xcore: Boolean = false,
                     xdebug: Boolean = false,
                     xinterpreter: Boolean = false,
                     xinvariants: Boolean = false,
                     xnotailcalls: Boolean = false,
                     files: Seq[File] = Seq())

  /**
    * A case class representing possible commands.
    */
  sealed trait Command

  object Command {

    case object None extends Command

    case object Init extends Command

    case object Build extends Command

    case object BuildJar extends Command

    case object BuildPkg extends Command

    case object Run extends Command

    case object Benchmark extends Command

    case object Test extends Command

  }

  /**
    * Parse command line options.
    *
    * @param args the arguments array.
    */
  def parseCmdOpts(args: Array[String]): Option[CmdOpts] = {
    val parser = new scopt.OptionParser[CmdOpts]("flix") {

      // Head
      head("The Flix Programming Language", Version.CurrentVersion.toString)

      // Command
      cmd("init").action((_, c) => c.copy(command = Command.Init)).text("  create a new project in the current directory.")

      cmd("build").action((_, c) => c.copy(command = Command.Build)).text("  build the current project.")

      cmd("build-jar").action((_, c) => c.copy(command = Command.BuildJar)).text("  build a jar-file for the current project.")

      cmd("build-pkg").action((_, c) => c.copy(command = Command.BuildPkg)).text("  build a fpkg-file for the current project.")

      cmd("run").action((_, c) => c.copy(command = Command.Run)).text("  run main for the current project.")

      cmd("benchmark").action((_, c) => c.copy(command = Command.Benchmark)).text("  run benchmarks for the current project.")

      cmd("test").action((_, c) => c.copy(command = Command.Test)).text("  run tests for the current project.")

      note("")

      // Benchmark.
      opt[Unit]("benchmark").action((_, c) => c.copy(benchmark = true)).
        text("runs benchmarks.")

      // Doc.
      opt[Unit]("doc").action((_, c) => c.copy(documentor = true)).
        text("generates HTML documentation.")

      // Help.
      help("help").text("prints this usage information.")

      // Interactive.
      opt[Unit]("interactive").action((f, c) => c.copy(interactive = true)).
        text("enables interactive mode.")

      // Listen.
      opt[Int]("listen").action((s, c) => c.copy(listen = Some(s))).
        valueName("<port>").
        text("listens on the given port.")

      // Monitor.
      opt[Unit]("monitor").action((_, c) => c.copy(monitor = true)).
        text("enables the debugger and profiler.")

      // Quickchecker.
      opt[Unit]("quickchecker").action((_, c) => c.copy(quickchecker = true)).
        text("enables the quickchecker.")

      // Release.
      opt[Unit]("release").action((_, c) => c.copy(release = true)).
        text("enables release mode.")

      // Test.
      opt[Unit]("test").action((_, c) => c.copy(test = true)).
        text("runs unit tests.")

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

      // Xinterpreter.
      opt[Unit]("Xinterpreter").action((_, c) => c.copy(xinterpreter = true)).
        text("[experimental] enables interpreted evaluation.")

      // Xinvariants.
      opt[Unit]("Xinvariants").action((_, c) => c.copy(xinvariants = true)).
        text("[experimental] enables compiler invariants.")

      // Xno-tailcalls
      opt[Unit]("Xno-tailcalls").action((_, c) => c.copy(xnotailcalls = true)).
        text("[experimental] disables tail call elimination.")

      note("")

      // Input files.
      arg[File]("<file>...").action((x, c) => c.copy(files = c.files :+ x))
        .optional()
        .unbounded()
        .text("input Flix source code files.")

    }

    parser.parse(args, CmdOpts())
  }

}
