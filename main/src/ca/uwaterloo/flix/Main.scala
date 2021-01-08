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
import java.net.BindException
import java.nio.file.Paths

import ca.uwaterloo.flix.api.lsp.LanguageServer
import ca.uwaterloo.flix.api.{Flix, Version}
import ca.uwaterloo.flix.runtime.shell.Shell
import ca.uwaterloo.flix.tools._
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
      var successfulRun: Boolean = false
      while (!successfulRun) {
        try {
          val socketServer = new SocketServer(cmdOpts.listen.get)
          socketServer.run()
          successfulRun = true
        } catch {
          case ex: BindException =>
            Console.println(ex.getMessage)
            Console.println("Retrying in 10 seconds.")
            Thread.sleep(10_000)
        }
      }
      System.exit(0)
    }

    // check if the --lsp flag was passed.
    if (cmdOpts.lsp.nonEmpty) {
      try {
        val languageServer = new LanguageServer(cmdOpts.lsp.get)
        languageServer.run()
      } catch {
        case ex: BindException =>
          Console.println(ex.getMessage)
      }
      System.exit(0)
    }

    // the default color context.
    implicit val terminal = TerminalContext.AnsiTerminal

    // compute the enabled optimizations.
    val optimizations = Optimization.All.filter {
      case Optimization.TailCalls => !cmdOpts.xnotailcalls
    }

    // construct flix options.
    val options = Options.Default.copy(
      core = cmdOpts.xcore,
      debug = cmdOpts.xdebug,
      documentor = cmdOpts.documentor,
      optimizations = optimizations,
      mode = if (cmdOpts.release) CompilationMode.Release else CompilationMode.Development,
      quickchecker = cmdOpts.quickchecker,
      threads = cmdOpts.threads,
      verbosity = if (cmdOpts.verbose) Verbosity.Verbose else Verbosity.Normal,
      verifier = cmdOpts.verifier,
      writeClassFiles = !cmdOpts.interactive,
      xallowredundancies = cmdOpts.xallowredundancies,
      xlinter = cmdOpts.xlinter,
      xnoboolunification = cmdOpts.xnoboolunification,
      xnostratifier = cmdOpts.xnostratifier,
      xstatistics = cmdOpts.xstatistics
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

        case Command.Check =>
          Packager.check(cwd, options)
          System.exit(0)

        case Command.Build =>
          Packager.build(cwd, options, loadClasses = false)
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
          Packager.test(cwd, options) match {
            case Tester.OverallTestResult.NoTests | Tester.OverallTestResult.Success => System.exit(0)
            case Tester.OverallTestResult.Failure => System.exit(1)
          }
      }
    } catch {
      case ex: RuntimeException =>
        Console.println(ex.getMessage)
        System.exit(1)
    }

    // check if the -Xbenchmark-phases flag was passed.
    if (cmdOpts.xbenchmarkPhases) {
      BenchmarkCompiler.benchmarkPhases(options)
      System.exit(0)
    }

    // check if the -Xbenchmark-throughput flag was passed.
    if (cmdOpts.xbenchmarkThroughput) {
      BenchmarkCompiler.benchmarkThroughput(options)
      System.exit(0)
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

    // evaluate main.
    try {
      val timer = new Timer(flix.compile())
      timer.getResult match {
        case Validation.Success(compilationResult) =>

          compilationResult.getMain match {
            case None => // nop
            case Some(m) =>
              val args: Array[String] = argv // TODO: Pass actual arguments...
              val evalTimer = new Timer(m(args))
              options.verbosity match {
                case Verbosity.Normal => Console.println(evalTimer.getResult)
                case Verbosity.Verbose => Console.println(evalTimer.getResult)
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
          errors.sortBy(_.source.name).foreach(e => println(e.message.fmt))
          println()
          println(s"Compilation failed with ${errors.length} error(s).")
          System.exit(1)
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
                     lsp: Option[Int] = None,
                     quickchecker: Boolean = false,
                     release: Boolean = false,
                     test: Boolean = false,
                     threads: Option[Int] = None,
                     verbose: Boolean = false,
                     verifier: Boolean = false,
                     xallowredundancies: Boolean = false,
                     xbenchmarkPhases: Boolean = false,
                     xbenchmarkThroughput: Boolean = false,
                     xcore: Boolean = false,
                     xdebug: Boolean = false,
                     xinvariants: Boolean = false,
                     xnoboolunification: Boolean = false,
                     xlinter: Boolean = false,
                     xnostratifier: Boolean = false,
                     xnotailcalls: Boolean = false,
                     xstatistics: Boolean = false,
                     files: Seq[File] = Seq())

  /**
    * A case class representing possible commands.
    */
  sealed trait Command

  object Command {

    case object None extends Command

    case object Init extends Command

    case object Check extends Command

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

      cmd("check").action((_, c) => c.copy(command = Command.Check)).text("  type checks the current project.")

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
        text("starts the socket server and listens on the given port.")

      // LSP.
      opt[Int]("lsp").action((s, c) => c.copy(lsp = Some(s))).
        valueName("<port>").
        text("starts the LSP server and listens on the given port.")

      // Quickchecker.
      opt[Unit]("quickchecker").action((_, c) => c.copy(quickchecker = true)).
        text("enables the quickchecker.")

      // Release.
      opt[Unit]("release").action((_, c) => c.copy(release = true)).
        text("enables release mode.")

      // Test.
      opt[Unit]("test").action((_, c) => c.copy(test = true)).
        text("runs unit tests.")

      // Threads.
      opt[Int]("threads").action((n, c) => c.copy(threads = Some(n))).
        text("number of threads for compilation.")

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

      // Xallow-redundancies.
      opt[Unit]("Xallow-redundancies").action((_, c) => c.copy(xallowredundancies = true)).
        text("[experimental] disables the redundancies checker.")

      // Xbenchmark-phases
      opt[Unit]("Xbenchmark-phases").action((_, c) => c.copy(xbenchmarkPhases = true)).
        text("[experimental] benchmarks each individual compiler phase.")

      // Xbenchmark-throughput
      opt[Unit]("Xbenchmark-throughput").action((_, c) => c.copy(xbenchmarkThroughput = true)).
        text("[experimental] benchmarks the throughput of the entire compiler.")

      // Xcore.
      opt[Unit]("Xcore").action((_, c) => c.copy(xcore = true)).
        text("[experimental] disables loading of all non-essential namespaces.")

      // Xdebug.
      opt[Unit]("Xdebug").action((_, c) => c.copy(xdebug = true)).
        text("[experimental] enables output of debugging information.")

      // Xinvariants.
      opt[Unit]("Xinvariants").action((_, c) => c.copy(xinvariants = true)).
        text("[experimental] enables compiler invariants.")

      // Xlinter.
      opt[Unit]("Xlinter").action((_, c) => c.copy(xlinter = true)).
        text("[experimental] enables the semantic linter.")

      // Xno-effects
      opt[Unit]("Xno-bool-unification").action((_, c) => c.copy(xnoboolunification = true)).
        text("[experimental] disables bool unification.")

      // Xno-stratifier
      opt[Unit]("Xno-stratifier").action((_, c) => c.copy(xnostratifier = true)).
        text("[experimental] disables computation of stratification.")

      // Xno-tailcalls
      opt[Unit]("Xno-tailcalls").action((_, c) => c.copy(xnotailcalls = true)).
        text("[experimental] disables tail call elimination.")

      // Xstatistics
      opt[Unit]("Xstatistics").action((_, c) => c.copy(xstatistics = true)).
        text("[experimental] prints statistics about the compilation.")

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
