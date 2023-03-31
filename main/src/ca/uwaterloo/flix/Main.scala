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

import ca.uwaterloo.flix.api.lsp.LanguageServer
import ca.uwaterloo.flix.api.{Bootstrap, Flix, Version}
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.runtime.shell.Shell
import ca.uwaterloo.flix.tools._
import ca.uwaterloo.flix.util._

import java.io.File
import java.net.BindException
import java.nio.file.Paths

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
        val languageServer = new LanguageServer(cmdOpts.lsp.get, Options.Default)
        languageServer.run()
      } catch {
        case ex: BindException =>
          Console.println(ex.getMessage)
      }
      System.exit(0)
    }

    // compute the main entry point
    val entryPoint = cmdOpts.entryPoint match {
      case None => Options.Default.entryPoint
      case Some(s) => Some(Symbol.mkDefnSym(s))
    }

    // construct flix options.
    var options = Options(
      lib = cmdOpts.xlib,
      debug = cmdOpts.xdebug,
      documentor = cmdOpts.documentor,
      entryPoint = entryPoint,
      explain = cmdOpts.explain,
      incremental = Options.Default.incremental,
      json = cmdOpts.json,
      progress = true,
      installDeps = cmdOpts.installDeps,
      output = None,
      target = Options.Default.target,
      test = Options.Default.test,
      threads = cmdOpts.threads.getOrElse(Options.Default.threads),
      loadClassFiles = Options.Default.loadClassFiles,
      xallowredundancies = Options.Default.xallowredundancies,
      xbddthreshold = cmdOpts.xbddthreshold,
      xboolclassic = cmdOpts.xboolclassic,
      xnoboolcache = cmdOpts.xnoboolcache,
      xnoboolspecialcases = cmdOpts.xnoboolspecialcases,
      xnobooltable = cmdOpts.xnobooltable,
      xnoboolunif = cmdOpts.xnoboolunif,
      xnoqmc = cmdOpts.xnoqmc,
      xnounittests = cmdOpts.xnounittests,
      xstatistics = cmdOpts.xstatistics,
      xstrictmono = cmdOpts.xstrictmono,
      xnoseteffects = cmdOpts.xnoseteffects,
      xnobooleffects = cmdOpts.xnobooleffects,
      xnooptimizer = cmdOpts.xnooptimizer,
      xvirtualthreads = cmdOpts.xvirtualthreads,
      xprintasts = cmdOpts.xprintasts,
      xprintboolunif = cmdOpts.xprintboolunif,
      xflexibleregions = cmdOpts.xflexibleregions,
      xsummary = cmdOpts.xsummary
    )

    // Don't use progress bar if benchmarking.
    if (cmdOpts.benchmark || cmdOpts.xbenchmarkCodeSize || cmdOpts.xbenchmarkIncremental || cmdOpts.xbenchmarkPhases || cmdOpts.xbenchmarkFrontend || cmdOpts.xbenchmarkThroughput) {
      options = options.copy(progress = false)
    }

    // Don't use progress bar if not attached to a console.
    if (System.console() == null) {
      options = options.copy(progress = false)
    }

    val cwd = Paths.get(".").toAbsolutePath.normalize()

    // check if command was passed.
    try {
      val formatter = Formatter.getDefault

      cmdOpts.command match {
        case Command.None =>
          val result = SimpleRunner.run(cwd, cmdOpts, options)
          System.exit(getCode(result))

        case Command.Init =>
          val result = Bootstrap.init(cwd, options)(System.out)
          System.exit(getCode(result))

        case Command.Check =>
          Bootstrap.bootstrap(cwd)(System.out) match {
            case Result.Ok(bootstrap) =>
              val result = bootstrap.check(options)
              System.exit(getCode(result))
            case Result.Err(e) =>
              println(e.message(formatter))
              System.exit(1)
          }

        case Command.Build =>
          Bootstrap.bootstrap(cwd)(System.out) match {
            case Result.Ok(bootstrap) =>
              implicit val flix: Flix = new Flix().setFormatter(formatter)
              val result = bootstrap.build(options, loadClasses = false)
              System.exit(getCode(result))
            case Result.Err(e) =>
              println(e.message(formatter))
              System.exit(1)
          }

        case Command.BuildJar =>
          Bootstrap.bootstrap(cwd)(System.out) match {
            case Result.Ok(bootstrap) =>
              val result = bootstrap.buildJar(options)
              System.exit(getCode(result))
            case Result.Err(e) =>
              println(e.message(formatter))
              System.exit(1)
          }

        case Command.BuildPkg =>
          Bootstrap.bootstrap(cwd)(System.out) match {
            case Result.Ok(bootstrap) =>
              val result = bootstrap.buildPkg(options)
              System.exit(getCode(result))
            case Result.Err(e) =>
              println(e.message(formatter))
              System.exit(1)
          }

        case Command.Run =>
          Bootstrap.bootstrap(cwd)(System.out) match {
            case Result.Ok(bootstrap) =>
              val result = bootstrap.run(options)
              System.exit(getCode(result))
            case Result.Err(e) =>
              println(e.message(formatter))
              System.exit(1)
          }

        case Command.Benchmark =>
          val o = options.copy(progress = false)
          Bootstrap.bootstrap(cwd)(System.out) match {
            case Result.Ok(bootstrap) =>
              val result = bootstrap.benchmark(o)
              System.exit(getCode(result))
            case Result.Err(e) =>
              println(e.message(formatter))
              System.exit(1)
          }

        case Command.Test =>
          val o = options.copy(progress = false)
          Bootstrap.bootstrap(cwd)(System.out) match {
            case Result.Ok(bootstrap) =>
              val result = bootstrap.test(o)
              System.exit(getCode(result))
            case Result.Err(e) =>
              println(e.message(formatter))
              System.exit(1)
          }

        case Command.Repl =>
          if (cmdOpts.files.nonEmpty) {
            println("The `repl' command cannot be used with a list of files.")
            System.exit(1)
          }
          Bootstrap.bootstrap(cwd)(System.out) match {
            case Result.Ok(bootstrap) =>
              val shell = new Shell(bootstrap, options)
              shell.loop()
              System.exit(0)
            case Result.Err(e) =>
              println(e.message(formatter))
              System.exit(1)
          }

        case Command.Lsp(port) =>
          val o = options.copy(progress = false)
          try {
            val languageServer = new LanguageServer(port, o)
            languageServer.run()
          } catch {
            case ex: BindException =>
              throw new RuntimeException(ex)
          }
          System.exit(0)

      }
    } catch {
      case ex: RuntimeException =>
        ex.printStackTrace()
        System.exit(1)
    }
  }

  /**
    * Extracts the exit code from the given result.
    */
  private def getCode(result: Result[_, Int]): Int = result match {
    case Result.Ok(_) => 0
    case Result.Err(code) => code
  }

  /**
    * A case class representing the parsed command line options.
    */
  case class CmdOpts(command: Command = Command.None,
                     args: Option[String] = None,
                     benchmark: Boolean = false,
                     documentor: Boolean = false,
                     entryPoint: Option[String] = None,
                     explain: Boolean = false,
                     installDeps: Boolean = true,
                     json: Boolean = false,
                     listen: Option[Int] = None,
                     lsp: Option[Int] = None,
                     test: Boolean = false,
                     threads: Option[Int] = None,
                     xbenchmarkCodeSize: Boolean = false,
                     xbenchmarkIncremental: Boolean = false,
                     xbenchmarkPhases: Boolean = false,
                     xbenchmarkFrontend: Boolean = false,
                     xbenchmarkThroughput: Boolean = false,
                     xbddthreshold: Option[Int] = None,
                     xlib: LibLevel = LibLevel.All,
                     xdebug: Boolean = false,
                     xboolclassic: Boolean = false,
                     xnoboolcache: Boolean = false,
                     xnoboolspecialcases: Boolean = false,
                     xnobooltable: Boolean = false,
                     xnoboolunif: Boolean = false,
                     xnoqmc: Boolean = false,
                     xnounittests: Boolean = false,
                     xstatistics: Boolean = false,
                     xstrictmono: Boolean = false,
                     xnoseteffects: Boolean = false,
                     xnobooleffects: Boolean = false,
                     xnooptimizer: Boolean = false,
                     xvirtualthreads: Boolean = false,
                     xprintasts: Set[String] = Set.empty,
                     xprintboolunif: Boolean = false,
                     xflexibleregions: Boolean = false,
                     xsummary: Boolean = false,
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

    case object Repl extends Command

    case class Lsp(port: Int) extends Command

  }

  /**
    * Parse command line options.
    *
    * @param args the arguments array.
    */
  def parseCmdOpts(args: Array[String]): Option[CmdOpts] = {
    implicit val readInclusion: scopt.Read[LibLevel] = scopt.Read.reads {
      case "nix" => LibLevel.Nix
      case "min" => LibLevel.Min
      case "all" => LibLevel.All
      case arg => throw new IllegalArgumentException(s"'$arg' is not a valid library level. Valid options are 'all', 'min', and 'nix'.")
    }

    val parser = new scopt.OptionParser[CmdOpts]("flix") {

      // Head
      head("The Flix Programming Language", Version.CurrentVersion.toString)

      // Command
      cmd("init").action((_, c) => c.copy(command = Command.Init)).text("  creates a new project in the current directory.")

      cmd("check").action((_, c) => c.copy(command = Command.Check)).text("  checks the current project for errors.")

      cmd("build").action((_, c) => c.copy(command = Command.Build)).text("  builds (i.e. compiles) the current project.")

      cmd("build-jar").action((_, c) => c.copy(command = Command.BuildJar)).text("  builds a jar-file from the current project.")

      cmd("build-pkg").action((_, c) => c.copy(command = Command.BuildPkg)).text("  builds a fpkg-file from the current project.")

      cmd("run").action((_, c) => c.copy(command = Command.Run)).text("  runs main for the current project.")

      cmd("benchmark").action((_, c) => c.copy(command = Command.Benchmark)).text("  runs the benchmarks for the current project.")

      cmd("test").action((_, c) => c.copy(command = Command.Test)).text("  runs the tests for the current project.")

      cmd("repl").action((_, c) => c.copy(command = Command.Repl)).text("  starts a repl for the current project, or provided Flix source files.")

      cmd("lsp").text("  starts the LSP server and listens on the given port.")
        .children(
          arg[Int]("port").action((port, c) => c.copy(command = Command.Lsp(port)))
            .required()
        )

      note("")

      opt[String]("args").action((s, c) => c.copy(args = Some(s))).
        valueName("<a1, a2, ...>").
        text("arguments passed to main. Must be a single quoted string.")

      opt[Unit]("benchmark").action((_, c) => c.copy(benchmark = true)).
        text("runs benchmarks.")

      opt[Unit]("doc").action((_, c) => c.copy(documentor = true)).
        text("generates HTML documentation.")

      opt[String]("entrypoint").action((s, c) => c.copy(entryPoint = Some(s))).
        text("specifies the main entry point.")

      opt[Unit]("explain").action((_, c) => c.copy(explain = true)).
        text("provides suggestions on how to solve a problem")

      help("help").text("prints this usage information.")

      opt[Unit]("json").action((_, c) => c.copy(json = true)).
        text("enables json output.")

      opt[Int]("listen").action((s, c) => c.copy(listen = Some(s))).
        valueName("<port>").
        text("starts the socket server and listens on the given port.")

      opt[Int]("lsp").action((s, c) => c.copy(lsp = Some(s))).
        valueName("<port>").
        text("starts the LSP server and listens on the given port.")

      opt[Unit]("test").action((_, c) => c.copy(test = true)).
        text("runs unit tests.")

      opt[Int]("threads").action((n, c) => c.copy(threads = Some(n))).
        text("number of threads to use for compilation.")

      opt[Unit]("no-install").action((_, c) => c.copy(installDeps = false)).
        text("disables automatic installation of dependencies.")

      version("version").text("prints the version number.")

      // Experimental options:
      note("")
      note("The following options are experimental:")

      // Xbenchmark-code-size
      opt[Unit]("Xbenchmark-code-size").action((_, c) => c.copy(xbenchmarkCodeSize = true)).
        text("[experimental] benchmarks the size of the generated JVM files.")

      // Xbenchmark-incremental
      opt[Unit]("Xbenchmark-incremental").action((_, c) => c.copy(xbenchmarkIncremental = true)).
        text("[experimental] benchmarks the performance of each compiler phase in incremental mode.")

      // Xbenchmark-phases
      opt[Unit]("Xbenchmark-phases").action((_, c) => c.copy(xbenchmarkPhases = true)).
        text("[experimental] benchmarks the performance of each compiler phase.")

      // Xbenchmark-frontend
      opt[Unit]("Xbenchmark-frontend").action((_, c) => c.copy(xbenchmarkFrontend = true)).
        text("[experimental] benchmarks the performance of the frontend.")

      // Xbenchmark-throughput
      opt[Unit]("Xbenchmark-throughput").action((_, c) => c.copy(xbenchmarkThroughput = true)).
        text("[experimental] benchmarks the performance of the entire compiler.")

      // Xdebug.
      opt[Unit]("Xdebug").action((_, c) => c.copy(xdebug = true)).
        text("[experimental] enables compiler debugging output.")

      // Xflexible-regions
      opt[Unit]("Xflexible-regions").action((_, c) => c.copy(xflexibleregions = true)).
        text("[experimental] uses flexible variables for regions")

      // Xlib
      opt[LibLevel]("Xlib").action((arg, c) => c.copy(xlib = arg)).
        text("[experimental] controls the amount of std. lib. to include (nix, min, all).")

      // Xstatistics
      opt[Unit]("Xstatistics").action((_, c) => c.copy(xstatistics = true)).
        text("[experimental] prints compilation statistics.")

      // Xstrictmono
      opt[Unit]("Xstrictmono").action((_, c) => c.copy(xstrictmono = true)).
        text("[experimental] enables strict monomorphization.")

      // Xno-set-effects
      opt[Unit]("Xno-set-effects").action((_, c) => c.copy(xnoseteffects = true)).
        text("[experimental] disables set effects.")

      // Xno-bool-effects
      opt[Unit]("Xno-bool-effects").action((_, c) => c.copy(xnobooleffects = true)).
        text("[experimental] disables bool effects.")

      // Xno-optimizer
      opt[Unit]("Xno-optimizer").action((_, c) => c.copy(xnooptimizer = true)).
        text("[experimental] disables compiler optimizations")

      // Xvirtual-threads
      opt[Unit]("Xvirtual-threads").action((_, c) => c.copy(xvirtualthreads = true)).
        text("[experimental] enables virtual threads (requires Java 19 with `--enable-preview`.)")

      // xprint-asts
      opt[Seq[String]]("Xprint-asts").action((m, c) => c.copy(xprintasts = m.toSet))

      // Xprint-bool-unif
      opt[Unit]("Xprint-bool-unif").action((m, c) => c.copy(xprintboolunif = true)).
        text("[experimental] prints boolean unification queries")

      //
      // Boolean unification flags.
      //
      // Xbool-classic
      opt[Unit]("Xbool-classic").action((_, c) => c.copy(xboolclassic = true)).
        text("[experimental] enable classic Boolean unification (as published in 2020).")

      // Xbdd-threshold
      opt[Int]("Xbdd-threshold").action((n, c) => c.copy(xbddthreshold = Some(n))).
        text("[experimental] sets the threshold for when to use BDDs.")

      // Xno-bool-cache
      opt[Unit]("Xno-bool-cache").action((_, c) => c.copy(xnoboolcache = true)).
        text("[experimental] disables Boolean caches.")

      // Xno-bool-specialcases
      opt[Unit]("Xno-bool-specialcases").action((_, c) => c.copy(xnoboolspecialcases = true)).
        text("[experimental] disables hardcoded Boolean unification special cases.")

      // Xno-bool-table
      opt[Unit]("Xno-bool-table").action((_, c) => c.copy(xnobooltable = true)).
        text("[experimental] disables Boolean minimization via tabling.")

      // Xno-bool-unif
      opt[Unit]("Xno-bool-unif").action((_, c) => c.copy(xnoboolunif = true)).
        text("[experimental] disables Boolean unification. (DO NOT USE).")

      // Xno-unit-tests
      opt[Unit]("Xno-unit-tests").action((_, c) => c.copy(xnounittests = true)).
        text("[experimental] excludes unit tests from performance benchmarks.")

      // Xno-qmc
      opt[Unit]("Xno-qmc").action((_, c) => c.copy(xnoqmc = true)).
        text("[experimental] disables Quine McCluskey when using BDDs.")

      // Xsummary
      opt[Unit]("Xsummary").action((_, c) => c.copy(xsummary = true)).
        text("[experimental] prints a summary of the compiled modules.")

      note("")

      // Input files.
      arg[File]("<file>...").action((x, c) => c.copy(files = c.files :+ x))
        .optional()
        .unbounded()
        .text("input Flix source code files, Flix packages, and Java archives.")

    }

    parser.parse(args, CmdOpts())
  }

}
