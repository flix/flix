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
import ca.uwaterloo.flix.api.{Flix, Version}
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.runtime.shell.{Shell, SourceProvider}
import ca.uwaterloo.flix.tools._
import ca.uwaterloo.flix.util.Formatter.AnsiTerminalFormatter
import ca.uwaterloo.flix.util._

import java.io.{File, PrintWriter}
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

    // compute the main entry point
    val entryPoint = cmdOpts.entryPoint match {
      case None => Options.Default.entryPoint
      case Some(s) => Some(Symbol.mkDefnSym(s))
    }

    // construct flix options.
    var options = Options.Default.copy(
      lib = cmdOpts.xlib,
      debug = cmdOpts.xdebug,
      documentor = cmdOpts.documentor,
      entryPoint = entryPoint,
      explain = cmdOpts.explain,
      json = cmdOpts.json,
      output = cmdOpts.output.map(s => Paths.get(s)),
      progress = true,
      threads = cmdOpts.threads.getOrElse(Runtime.getRuntime.availableProcessors()),
      xnobooltable = cmdOpts.xnobooltable,
      xstatistics = cmdOpts.xstatistics,
      xstrictmono = cmdOpts.xstrictmono
    )

    // Don't use progress bar if not attached to a console.
    if (System.console() == null) {
      options = options.copy(progress = false)
    }

    val cwd = Paths.get(".").toAbsolutePath.normalize()

    // check if command was passed.
    try {
      cmdOpts.command match {
        case Command.Init =>
          val result = Packager.init(cwd, options)
          System.exit(getCode(result))

        case Command.Check =>
          val result = Packager.check(cwd, options)
          System.exit(getCode(result))

        case Command.Build =>
          val result = Packager.build(cwd, options, loadClasses = false)
          System.exit(getCode(result))

        case Command.BuildJar =>
          val result = Packager.buildJar(cwd, options)
          System.exit(getCode(result))

        case Command.BuildPkg =>
          val result = Packager.buildPkg(cwd, options)
          System.exit(getCode(result))

        case Command.Run =>
          val result = Packager.run(cwd, options)
          System.exit(getCode(result))

        case Command.Benchmark =>
          val o = options.copy(progress = false)
          val result = Packager.benchmark(cwd, o)
          System.exit(getCode(result))

        case Command.Test =>
          val o = options.copy(progress = false)
          val result = Packager.test(cwd, o)
          System.exit(getCode(result))

        case Command.Repl =>
          val source = if (cmdOpts.files.isEmpty) SourceProvider.ProjectPath(cwd) else SourceProvider.SourceFileList(cmdOpts.files)
          val shell = new Shell(source, options)
          shell.loop()
          System.exit(0)

        case Command.Install(project) =>
          val o = options.copy(progress = false)
          val result = Packager.install(project, cwd, o)
          System.exit(getCode(result))

        case Command.Doc =>
          val o = options.copy(documentor = true) // TODO documenting should be its own thing, not a phase
          val result = Packager.build(cwd, o)
          System.exit(getCode(result))

        case Command.Listen(port) =>
          var successfulRun: Boolean = false
          while (!successfulRun) {
            try {
              val socketServer = new SocketServer(port)
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

        case Command.Lsp(port) =>
          try {
            val languageServer = new LanguageServer(port)
            languageServer.run()
          } catch {
            case ex: BindException =>
              Console.println(ex.getMessage)
          }
          System.exit(0)

        case Command.XBenchmarkCodeSize =>
          BenchmarkCompiler.benchmarkCodeSize(options)
          System.exit(0)

        case Command.XBenchmarkIncremental =>
          BenchmarkCompiler.benchmarkIncremental(options)
          System.exit(0)

        case Command.XBenchmarkPhases =>
          BenchmarkCompiler.benchmarkPhases(options)
          System.exit(0)

        case Command.XBenchmarkThroughput =>
          BenchmarkCompiler.benchmarkThroughput(options)
          System.exit(0)

        case Command.None if cmdOpts.files.isEmpty =>
          val shell = new Shell(SourceProvider.ProjectPath(cwd), options)
          shell.loop()
          System.exit(0)

        case Command.None => ??? // MATT
      }
    } catch {
      case ex: RuntimeException =>
        Console.println(ex.getMessage)
        System.exit(1)
    }

    // configure Flix and add the paths.
    val flix = new Flix()
    flix.setOptions(options)
    for (file <- cmdOpts.files) {
      val ext = file.getName.split('.').last
      ext match {
        case "flix" => flix.addSourcePath(file.toPath)
        case "fpkg" => flix.addSourcePath(file.toPath)
        case "jar" => flix.addJar(file.toPath)
        case _ =>
          Console.println(s"Unrecognized file extension: '$ext'.")
          System.exit(1)
      }
    }
    if (Formatter.hasColorSupport)
      flix.setFormatter(AnsiTerminalFormatter)

    // evaluate main.
    val timer = new Timer(flix.compile())
    timer.getResult match {
      case Validation.Success(compilationResult) =>

        compilationResult.getMain match {
          case None => // nop
          case Some(m) =>
            // Compute the arguments to be passed to main.
            val args: Array[String] = cmdOpts.args match {
              case None => Array.empty
              case Some(a) => a.split(" ")
            }
            // Invoke main with the supplied arguments.
            m(args)

            // Exit.
            System.exit(0)
        }

      case Validation.Failure(errors) =>
        flix.mkMessages(errors.sortBy(_.source.name))
          .foreach(println)
        println()
        println(s"Compilation failed with ${errors.length} error(s).")
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
                     entryPoint: Option[String] = None,
                     explain: Boolean = false,
                     json: Boolean = false,
                     output: Option[String] = None,
                     threads: Option[Int] = None,
                     xlib: LibLevel = LibLevel.All,
                     xdebug: Boolean = false,
                     xnobooltable: Boolean = false,
                     xstatistics: Boolean = false,
                     xstrictmono: Boolean = false,
                     xeffects: Boolean = false,
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

    case class Install(project: String) extends Command

    case class Listen(port: Int) extends Command

    case object Doc extends Command

//    case object Help extends Command // MATT

    case class Lsp(port: Int) extends Command

//    case object Version extends Command // MATT

    case object XBenchmarkCodeSize extends Command

    case object XBenchmarkIncremental extends Command

    case object XBenchmarkPhases extends Command

    case object XBenchmarkThroughput extends Command

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

      cmd("install").text("  installs the Flix package from the given GitHub <owner>/<repo>.")
        .children(
          arg[String]("<project>").action((project, c) => c.copy(command = Command.Install(project)))
            .required()
        )

      cmd("doc").action((_, c) => c.copy(command = Command.Doc)).text("  generates JSON documentation.")

      cmd("listen").text("  starts the socket server and listens on the given port.")
        .children(
          arg[Int]("<port>").action((port, c) => c.copy(command = Command.Listen(port)))
            .required())

      cmd("lsp").text("  starts the LSP server and listens on the given port.")
        .children(
          arg[Int]("<port>").action((port, c) => c.copy(command = Command.Lsp(port)))
            .required())

      note("")

      opt[String]("args").action((s, c) => c.copy(args = Some(s))).
        valueName("<a1, a2, ...>").
        text("arguments passed to main. Must be a single quoted string.")

      opt[String]("entrypoint").action((s, c) => c.copy(entryPoint = Some(s))).
        text("specifies the main entry point.")

      opt[Unit]("explain").action((_, c) => c.copy(explain = true)).
        text("provides suggestions on how to solve a problem")

      help("help").text("prints this usage information.")

      opt[Unit]("json").action((_, c) => c.copy(json = true)).
        text("enables json output.")

      opt[String]("output").action((s, c) => c.copy(output = Some(s))).
        text("specifies the output directory for JVM bytecode.")

      opt[Int]("threads").action((n, c) => c.copy(threads = Some(n))).
        text("number of threads to use for compilation.")

      version("version").text("prints the version number.")

      // Experimental options:
      note("")
      note("The following commands and options are experimental:")

      // Xbenchmark-code-size
      cmd("Xbenchmark-code-size").action((_, c) => c.copy(command = Command.XBenchmarkCodeSize)).
        text("  [experimental] benchmarks the size of the generated JVM files.")

      // Xbenchmark-incremental
      cmd("Xbenchmark-incremental").action((_, c) => c.copy(command = Command.XBenchmarkIncremental)).
        text("  [experimental] benchmarks the performance of each compiler phase in incremental mode.")

      // Xbenchmark-phases
      cmd("Xbenchmark-phases").action((_, c) => c.copy(command = Command.XBenchmarkPhases)).
        text("  [experimental] benchmarks the performance of each compiler phase.")

      // Xbenchmark-throughput
      cmd("Xbenchmark-throughput").action((_, c) => c.copy(command = Command.XBenchmarkThroughput)).
        text("  [experimental] benchmarks the performance of the entire compiler.")

      note("")

      // Xdebug.
      opt[Unit]("Xdebug").action((_, c) => c.copy(xdebug = true)).
        text("[experimental] enables compiler debugging output.")

      // Xlib
      opt[LibLevel]("Xlib").action((arg, c) => c.copy(xlib = arg)).
        text("[experimental] controls the amount of std. lib. to include (nix, min, all).")

      // Xno-bool-table
      opt[Unit]("Xno-bool-table").action((_, c) => c.copy(xnobooltable = true)).
        text("[experimental] disables Boolean minimization via tabling.")

      // Xstatistics
      opt[Unit]("Xstatistics").action((_, c) => c.copy(xstatistics = true)).
        text("[experimental] prints compilation statistics.")

      // Xstrictmono
      opt[Unit]("Xstrictmono").action((_, c) => c.copy(xstrictmono = true)).
        text("[experimental] enable strict monomorphization.")

      opt[Unit]("Xeffects").action((_, c) => c.copy(xeffects = true)).
        text("[experimental] enable set effects")

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
