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

import ca.uwaterloo.flix.Main.Command.PlainLsp
import ca.uwaterloo.flix.api.lsp.{LspServer, VSCodeLspServer, Formatter as LspFormatter}
import ca.uwaterloo.flix.api.{Bootstrap, BootstrapError, Flix, Version}
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.language.phase.HtmlDocumentor
import ca.uwaterloo.flix.language.phase.unification.zhegalkin.ZhegalkinPerf
import ca.uwaterloo.flix.runtime.shell.Shell
import ca.uwaterloo.flix.tools.*
import ca.uwaterloo.flix.tools.pkg.PackageModules
import ca.uwaterloo.flix.util.*

import java.io.{File, PrintStream}
import java.net.BindException
import java.nio.file.Paths

object Main {

  def main(argv: Array[String]): Unit = {

    // retrieve the current working directory.
    val cwd = Paths.get(".").toAbsolutePath.normalize()

    // parse command line options.
    val cmdOpts: CmdOpts = parseCmdOpts(argv).getOrElse {
      Console.err.println("Unable to parse command line arguments. Will now exit.")
      System.exit(1)
      null
    }

    // get GitHub token
    val githubToken =
      cmdOpts.githubToken
        .orElse(FileOps.readLine(cwd.resolve("./.GITHUB_TOKEN").normalize()))
        .orElse(sys.env.get("GITHUB_TOKEN"))

    // compute the main entry point
    val entryPoint = cmdOpts.entryPoint match {
      case None => Options.Default.entryPoint
      case Some(s) => Some(Symbol.mkDefnSym(s))
    }

    // construct flix options.
    var options = Options(
      lib = cmdOpts.xlib,
      build = Build.Development,
      entryPoint = entryPoint,
      githubToken = githubToken,
      incremental = Options.Default.incremental,
      json = cmdOpts.json,
      progress = true,
      installDeps = cmdOpts.installDeps,
      outputJvm = false,
      outputPath = Options.Default.outputPath,
      threads = cmdOpts.threads.getOrElse(Options.Default.threads),
      loadClassFiles = Options.Default.loadClassFiles,
      assumeYes = cmdOpts.assumeYes,
      xprintphases = cmdOpts.xprintphases,
      xnodeprecated = cmdOpts.xnodeprecated,
      xsummary = cmdOpts.xsummary,
      xsubeffecting = cmdOpts.xsubeffecting,
      XPerfFrontend = cmdOpts.XPerfFrontend,
      XPerfPar = cmdOpts.XPerfPar,
      XPerfN = cmdOpts.XPerfN,
      xchaosMonkey = Options.Default.xchaosMonkey
    )

    // Don't use progress bar if benchmarking.
    if (cmdOpts.xbenchmarkCodeSize || cmdOpts.xbenchmarkIncremental || cmdOpts.xbenchmarkPhases || cmdOpts.xbenchmarkFrontend || cmdOpts.xbenchmarkThroughput) {
      options = options.copy(progress = false)
    }

    // Don't use progress bar if not attached to a console.
    if (System.console() == null) {
      options = options.copy(progress = false)
    }

    // check if command was passed.
    try {
      implicit val formatter: Formatter = Formatter.getDefault
      implicit val out: PrintStream = System.err

      cmdOpts.command match {
        case Command.None =>
          // check if the --listen flag was passed.
          if (cmdOpts.listen.nonEmpty) {
            SocketServer.listen(cmdOpts.listen.get)
            System.exit(0)
          }

          // check if the --Xbenchmark-code-size flag was passed.
          if (cmdOpts.xbenchmarkCodeSize) {
            BenchmarkCompilerOld.benchmarkCodeSize(options)
            System.exit(0)
          }

          // check if the --Xbenchmark-incremental flag was passed.
          if (cmdOpts.xbenchmarkIncremental) {
            BenchmarkCompilerOld.benchmarkIncremental(options)
            System.exit(0)
          }

          // check if the --Xbenchmark-phases flag was passed.
          if (cmdOpts.xbenchmarkPhases) {
            BenchmarkCompilerOld.benchmarkPhases(options)
            System.exit(0)
          }

          // check if the --Xbenchmark-frontend flag was passed.
          if (cmdOpts.xbenchmarkFrontend) {
            BenchmarkCompilerOld.benchmarkThroughput(options, frontend = true)
            System.exit(0)
          }

          // check if the --Xbenchmark-throughput flag was passed.
          if (cmdOpts.xbenchmarkThroughput) {
            BenchmarkCompilerOld.benchmarkThroughput(options, frontend = false)
            System.exit(0)
          }

          // check if we should start a REPL
          if (cmdOpts.files.isEmpty) {
            Bootstrap.bootstrap(cwd, options.githubToken) match {
              case Result.Ok(bootstrap) =>
                val shell = new Shell(bootstrap, options)
                shell.loop()
                System.exit(0)
              case Result.Err(error) =>
                println(error.message(formatter))
                System.exit(1)
            }
          }

          // configure Flix and add the paths.
          val flix = new Flix()
          flix.setOptions(options)
          implicit val sctx: SecurityContext = SecurityContext.Unrestricted
          for (file <- cmdOpts.files) {
            val ext = file.getName.split('.').last
            ext match {
              case "flix" => flix.addFile(file.toPath)
              case "fpkg" => flix.addPkg(file.toPath)
              case "jar" => flix.addJar(file.toPath)
              case _ =>
                Console.println(s"Unrecognized file extension: '$ext'.")
                System.exit(1)
            }
          }

          flix.setFormatter(formatter)

          // evaluate main.
          flix.compile().toResult match {
            case Result.Ok(compilationResult) =>
              compilationResult.getMain match {
                case None => // nop
                case Some(m) =>
                  // Invoke main with the supplied arguments.
                  m(cmdOpts.args.toArray)
              }
              System.exit(0)

            case Result.Err(errors) =>
              println(CompilationMessage.formatAll(errors.toList.sortBy(_.source.name)))
              System.exit(1)
          }

        case Command.Init =>
          if (cmdOpts.files.nonEmpty) {
            println("The 'init' command does not support file arguments.")
            System.exit(1)
          }
          exitOnResult(Bootstrap.init(cwd))

        case Command.Check =>
          if (cmdOpts.files.isEmpty) {
            exitOnResult {
              Bootstrap.bootstrap(cwd, options.githubToken).flatMap { bootstrap =>
                val flix = new Flix().setFormatter(formatter)
                flix.setOptions(options)
                bootstrap.check(flix)
              }
            }
          } else {
            val flix = mkFlixWithFiles(cmdOpts.files, options)
            val (_, errors) = flix.check()
            if (errors.isEmpty) System.exit(0)
            else exitWithErrors(flix, errors)
          }

        case Command.Build =>
          if (cmdOpts.files.nonEmpty) {
            println("The 'build' command does not support file arguments.")
            System.exit(1)
          }
          exitOnResult {
            Bootstrap.bootstrap(cwd, options.githubToken).flatMap { bootstrap =>
              val flix = new Flix().setFormatter(formatter)
              flix.setOptions(options.copy(loadClassFiles = false))
              bootstrap.build(flix)
            }
          }

        case Command.BuildJar =>
          if (cmdOpts.files.nonEmpty) {
            println("The 'build-jar' command does not support file arguments.")
            System.exit(1)
          }
          exitOnResult {
            Bootstrap.bootstrap(cwd, options.githubToken).flatMap { bootstrap =>
              val flix = new Flix().setFormatter(formatter)
              flix.setOptions(options.copy(loadClassFiles = false))
              bootstrap.buildJar(flix)
            }
          }

        case Command.BuildFatJar =>
          if (cmdOpts.files.nonEmpty) {
            println("The 'build-fatjar' command does not support file arguments.")
            System.exit(1)
          }
          exitOnResult {
            Bootstrap.bootstrap(cwd, options.githubToken).flatMap { bootstrap =>
              val flix = new Flix().setFormatter(formatter)
              flix.setOptions(options.copy(loadClassFiles = false))
              bootstrap.buildFatJar(flix)
            }
          }

        case Command.BuildPkg =>
          if (cmdOpts.files.nonEmpty) {
            println("The 'build-pkg' command does not support file arguments.")
            System.exit(1)
          }
          exitOnResult {
            Bootstrap.bootstrap(cwd, options.githubToken).flatMap { bootstrap =>
              bootstrap.buildPkg()
            }
          }

        case Command.Clean =>
          if (cmdOpts.files.nonEmpty) {
            println("The 'clean' command does not support file arguments.")
            System.exit(1)
          }
          exitOnResult {
            Bootstrap.bootstrap(cwd, options.githubToken).flatMap { bootstrap =>
              bootstrap.clean()
            }
          }

        case Command.Doc =>
          if (cmdOpts.files.isEmpty) {
            exitOnResult {
              Bootstrap.bootstrap(cwd, options.githubToken).flatMap { bootstrap =>
                val flix = new Flix().setFormatter(formatter)
                flix.setOptions(options)
                bootstrap.doc(flix)
              }
            }
          } else {
            val flix = mkFlixWithFiles(cmdOpts.files, options)
            val (optRoot, errors) = flix.check()
            if (errors.isEmpty) {
              HtmlDocumentor.run(optRoot.get, PackageModules.All)(flix)
              System.exit(0)
            } else exitWithErrors(flix, errors)
          }

        case Command.Format =>
          if (cmdOpts.files.isEmpty) {
            exitOnResult {
              Bootstrap.bootstrap(cwd, options.githubToken).flatMap { bootstrap =>
                val flix = new Flix().setFormatter(formatter)
                flix.setOptions(options)
                bootstrap.format(flix)
              }
            }
          }
          val flix = mkFlixWithFiles(cmdOpts.files, options)
          val (_, errors) = flix.check()
          if (errors.isEmpty) {
            val syntaxTree = flix.getParsedAst
            LspFormatter.formatFiles(syntaxTree, cmdOpts.files.map(_.toPath).toList)(flix)
            System.exit(0)
          }
          else exitWithErrors(flix, errors)


        case Command.Run =>
          if (cmdOpts.files.nonEmpty) {
            println("The 'run' command does not support file arguments.")
            System.exit(1)
          }
          exitOnResult {
            Bootstrap.bootstrap(cwd, options.githubToken).flatMap { bootstrap =>
              val flix = new Flix().setFormatter(formatter)
              flix.setOptions(options)
              bootstrap.run(flix, cmdOpts.args.toArray)
            }
          }

        case Command.Test =>
          if (cmdOpts.files.isEmpty) {
            exitOnResult {
              Bootstrap.bootstrap(cwd, options.githubToken).flatMap { bootstrap =>
                val flix = new Flix().setFormatter(formatter)
                flix.setOptions(options.copy(progress = false))
                bootstrap.test(flix)
              }
            }
          } else {
            val flix = mkFlixWithFiles(cmdOpts.files, options.copy(progress = false))
            flix.compile() match {
              case Validation.Success(compilationResult) =>
                Tester.run(Nil, compilationResult)(flix) match {
                  case Result.Ok(_) => System.exit(0)
                  case Result.Err(_) => System.exit(1)
                }
              case Validation.Failure(errors) => exitWithErrors(flix, errors.toList)
            }
          }

        case Command.Repl =>
          if (cmdOpts.files.nonEmpty) {
            println("The 'repl' command does not support file arguments.")
            System.exit(1)
          }
          Bootstrap.bootstrap(cwd, options.githubToken) match {
            case Result.Ok(bootstrap) =>
              val shell = new Shell(bootstrap, options)
              shell.loop()
              System.exit(0)
            case Result.Err(error) =>
              println(error.message(formatter))
              System.exit(1)
          }

        case PlainLsp =>
          if (cmdOpts.files.nonEmpty) {
            println("The 'lsp' command does not support file arguments.")
            System.exit(1)
          }
          LspServer.run(options)
          System.exit(0)

        case Command.VSCodeLsp(port) =>
          if (cmdOpts.files.nonEmpty) {
            println("The 'lsp-vscode' command does not support file arguments.")
            System.exit(1)
          }
          val o = options.copy(progress = false)
          try {
            val languageServer = new VSCodeLspServer(port, o)
            languageServer.run()
          } catch {
            case ex: BindException =>
              throw new RuntimeException(ex)
          }
          System.exit(0)

        case Command.Release =>
          if (cmdOpts.files.nonEmpty) {
            println("The 'release' command does not support file arguments.")
            System.exit(1)
          }
          exitOnResult {
            Bootstrap.bootstrap(cwd, options.githubToken).flatMap { bootstrap =>
              val flix = new Flix().setFormatter(formatter)
              flix.setOptions(options.copy(progress = false))
              bootstrap.release(flix)(System.err)
            }
          }

        case Command.Outdated =>
          if (cmdOpts.files.nonEmpty) {
            println("The 'outdated' command does not support file arguments.")
            System.exit(1)
          }
          Bootstrap.bootstrap(cwd, options.githubToken).flatMap {
            bootstrap =>
              val flix = new Flix().setFormatter(formatter)
              flix.setOptions(options.copy(progress = false))
              bootstrap.outdated(flix)(System.err)
          } match {
            case Result.Ok(false) =>
              // Up to date
              System.exit(0)
            case Result.Ok(true) =>
              // Contains outdated dependencies
              System.exit(1)
            case Result.Err(error) =>
              println(error.message(formatter))
              System.exit(1)
          }

        case Command.EffLock =>
          if (cmdOpts.files.nonEmpty) {
            println("The 'eff-lock' command does not support file arguments.")
            System.exit(1)
          }
          exitOnResult {
            Bootstrap.bootstrap(cwd, options.githubToken).flatMap {
              bootstrap =>
                val flix = new Flix().setFormatter(formatter)
                flix.setOptions(options.copy(progress = false))
                bootstrap.lockEffects(flix)
            }
          }

        case Command.CompilerPerf =>
          CompilerPerf.run(options)

        case Command.CompilerMemory =>
          CompilerMemory.run(options)

        case Command.Zhegalkin =>
          ZhegalkinPerf.run(options.XPerfN)

      }
    } catch {
      case ex: RuntimeException =>
        ex.printStackTrace()
        System.exit(1)
    }
  }

  /**
    * A case class representing the parsed command line options.
    */
  case class CmdOpts(command: Command = Command.None,
                     args: List[String] = Nil,
                     entryPoint: Option[String] = None,
                     installDeps: Boolean = true,
                     githubToken: Option[String] = None,
                     json: Boolean = false,
                     listen: Option[Int] = None,
                     threads: Option[Int] = None,
                     assumeYes: Boolean = false,
                     xbenchmarkCodeSize: Boolean = false,
                     xbenchmarkIncremental: Boolean = false,
                     xbenchmarkPhases: Boolean = false,
                     xbenchmarkFrontend: Boolean = false,
                     xbenchmarkThroughput: Boolean = false,
                     xnodeprecated: Boolean = false,
                     xlib: LibLevel = LibLevel.All,
                     xprintphases: Boolean = false,
                     xsummary: Boolean = false,
                     xsubeffecting: Set[Subeffecting] = Set.empty,
                     XPerfN: Option[Int] = None,
                     XPerfFrontend: Boolean = false,
                     XPerfPar: Boolean = false,
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

    case object BuildFatJar extends Command

    case object BuildPkg extends Command

    case object Clean extends Command

    case object Doc extends Command

    case object Format extends Command

    case object Run extends Command

    case object Test extends Command

    case object Repl extends Command

    case object PlainLsp extends Command

    case class VSCodeLsp(port: Int) extends Command

    case object Release extends Command

    case object Outdated extends Command

    case object EffLock extends Command

    case object CompilerPerf extends Command

    case object CompilerMemory extends Command

    case object Zhegalkin extends Command

  }

  /**
    * Parse command line options.
    *
    * @param args the arguments array.
    */
  def parseCmdOpts(args: Array[String]): Option[CmdOpts] = {
    // Split at "--" separator: arguments before are for flix, arguments after are for the program
    val separatorIndex = args.indexOf("--")
    val (flixArgs, progArgs) = if (separatorIndex >= 0) {
      (args.take(separatorIndex), args.drop(separatorIndex + 1))
    } else {
      (args, Array.empty[String])
    }

    implicit val readLibLevel: scopt.Read[LibLevel] = scopt.Read.reads {
      case "nix" => LibLevel.Nix
      case "min" => LibLevel.Min
      case "all" => LibLevel.All
      case arg => throw new IllegalArgumentException(s"'$arg' is not a valid library level. Valid options are 'all', 'min', and 'nix'.")
    }

    implicit val readSubEffectLevel: scopt.Read[Subeffecting] = scopt.Read.reads {
      case "mod-defs" => Subeffecting.ModDefs
      case "ins-defs" => Subeffecting.InsDefs
      case "lambdas" => Subeffecting.Lambdas
      case arg => throw new IllegalArgumentException(s"'$arg' is not a valid subeffecting option. Valid options are comma-separated combinations of 'mod-defs', 'ins-defs', and 'lambdas'.")
    }

    val parser = new scopt.OptionParser[CmdOpts]("flix") {

      // Head
      head("The Flix Programming Language", Version.CurrentVersion.toString)

      // Command
      cmd("init").action((_, c) => c.copy(command = Command.Init)).text("  creates a new project in the current directory.")

      cmd("check").action((_, c) => c.copy(command = Command.Check)).text("  checks the current project for errors.")

      cmd("build").action((_, c) => c.copy(command = Command.Build)).text("  builds (i.e. compiles) the current project.")

      cmd("build-jar").action((_, c) => c.copy(command = Command.BuildJar)).text("  builds a jar-file from the current project.")

      cmd("build-fatjar").action((_, c) => c.copy(command = Command.BuildFatJar)).text("  builds a fatjar-file from the current project.")

      cmd("build-pkg").action((_, c) => c.copy(command = Command.BuildPkg)).text("  builds a fpkg-file from the current project.")

      cmd("clean").action((_, c) => c.copy(command = Command.Clean)).text("  recursively removes class files from the build directory.")

      cmd("doc").action((_, c) => c.copy(command = Command.Doc)).text("  generates API documentation.")

      cmd("format").action((_, c) => c.copy(command = Command.Format)).text("  formats Flix source code files.")

      cmd("run").action((_, c) => c.copy(command = Command.Run)).text("  runs main for the current project.")

      cmd("test").action((_, c) => c.copy(command = Command.Test)).text("  runs the tests for the current project.")

      cmd("repl").action((_, c) => c.copy(command = Command.Repl)).text("  starts a repl for the current project, or provided Flix source files.")

      cmd("lsp").text("  starts the Plain-LSP server.")
        .action((_, c) => c.copy(command = Command.PlainLsp))

      cmd("lsp-vscode").text("  starts the VSCode-LSP server and listens on the given port.")
        .children(
          arg[Int]("port").action((port, c) => c.copy(command = Command.VSCodeLsp(port)))
            .required()
        )

      cmd("release").text("  releases a new version to GitHub.")
        .action((_, c) => c.copy(command = Command.Release))

      cmd("outdated").text("  shows dependencies which have newer versions available.")
        .action((_, c) => c.copy(command = Command.Outdated))

      cmd("eff-lock").text("  locks the current effect signatures.")
        .action((_, c) => c.copy(command = Command.EffLock))

      cmd("Xperf").action((_, c) => c.copy(command = Command.CompilerPerf)).children(
        opt[Unit]("frontend")
          .action((_, c) => c.copy(XPerfFrontend = true))
          .text("benchmark only frontend"),
        opt[Unit]("par")
          .action((_, c) => c.copy(XPerfPar = true))
          .text("benchmark only parallel evaluation"),
        opt[Int]("n")
          .action((v, c) => c.copy(XPerfN = Some(v)))
          .text("number of compilations")
      ).hidden()

      cmd("Xmemory").action((_, c) => c.copy(command = Command.CompilerMemory)).hidden()

      cmd("Xzhegalkin").action((_, c) => c.copy(command = Command.Zhegalkin)).children(
        opt[Int]("n")
          .action((v, c) => c.copy(XPerfN = Some(v)))
          .text("number of compilations")
      ).hidden()

      note("")

      opt[String]("entrypoint").action((s, c) => c.copy(entryPoint = Some(s))).
        text("specifies the main entry point.")

      opt[String]("github-token").action((s, c) => c.copy(githubToken = Some(s))).
        text("API key to use for GitHub dependency resolution.")

      help("help").text("prints this usage information.")

      opt[Unit]("json").action((_, c) => c.copy(json = true)).
        text("enables json output.")

      opt[Int]("listen").action((s, c) => c.copy(listen = Some(s))).
        valueName("<port>").
        text("starts the socket server and listens on the given port.")

      opt[Unit]("no-install").action((_, c) => c.copy(installDeps = false)).
        text("disables automatic installation of dependencies.")

      opt[Int]("threads").action((n, c) => c.copy(threads = Some(n))).
        text("number of threads to use for compilation.")

      opt[Unit]("yes").action((_, c) => c.copy(assumeYes = true)).
        text("automatically answer yes to all prompts.")

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

      // Xlib
      opt[LibLevel]("Xlib").action((arg, c) => c.copy(xlib = arg)).
        text("[experimental] controls the amount of std. lib. to include (nix, min, all).")

      // Xno-deprecated
      opt[Unit]("Xno-deprecated").action((_, c) => c.copy(xnodeprecated = true)).
        text("[experimental] disables deprecated features.")

      // Xprint-phase
      opt[Unit]("Xprint-phases").action((_, c) => c.copy(xprintphases = true)).
        text("[experimental] prints the ASTs after the each phase.")

      // Xsummary
      opt[Unit]("Xsummary").action((_, c) => c.copy(xsummary = true)).
        text("[experimental] prints a summary of the compiled modules.")

      // Xsubeffecting
      opt[Seq[Subeffecting]]("Xsubeffecting").action((subeffectings, c) => c.copy(xsubeffecting = subeffectings.toSet)).
        text("[experimental] enables sub-effecting in select places")

      note("")

      // Input files.
      arg[File]("<file>...").action((x, c) => c.copy(files = c.files :+ x))
        .optional()
        .unbounded()
        .text("input Flix source code files, Flix packages, and Java archives.")

    }

    parser.parse(flixArgs, CmdOpts()).map(_.copy(args = progArgs.toList))
  }

  /**
    * Creates a fresh Flix instance configured with the given options and source files.
    */
  private def mkFlixWithFiles(files: Seq[File], options: Options)(implicit formatter: Formatter): Flix = {
    val flix = new Flix().setFormatter(formatter)
    flix.setOptions(options)
    implicit val sctx: SecurityContext = SecurityContext.Unrestricted
    for (file <- files) {
      if (file.getName.endsWith(".flix")) {
        flix.addFile(file.toPath)
      } else {
        Console.println(s"Unrecognized file: '${file.getName}'. Only .flix files are supported.")
        System.exit(1)
      }
    }
    flix
  }

  /**
    * Prints compilation errors and exits with code 1.
    */
  private def exitWithErrors(flix: Flix, errors: List[CompilationMessage]): Unit = {
    println(CompilationMessage.formatAll(errors)(flix.getFormatter))
    System.exit(1)
  }

  /**
    * Exits with code 0 on success, or prints the error and exits with code 1 on failure.
    */
  private def exitOnResult[T](result: Result[T, BootstrapError])(implicit formatter: Formatter): Unit = {
    result match {
      case Result.Ok(_) => System.exit(0)
      case Result.Err(error) =>
        println(error.message(formatter))
        System.exit(1)
    }
  }

}
