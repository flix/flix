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
import ca.uwaterloo.flix.api.{Bootstrap, BootstrapError, BrowserRunSupport, Flix, Version, WasmRunSupport}
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.language.ast.{Symbol, TypedAst}
import ca.uwaterloo.flix.language.phase.HtmlDocumentor
import ca.uwaterloo.flix.language.phase.unification.zhegalkin.ZhegalkinPerf
import ca.uwaterloo.flix.runtime.shell.Shell
import ca.uwaterloo.flix.tools.*
import ca.uwaterloo.flix.tools.pkg.PackageModules
import ca.uwaterloo.flix.util.*

import java.io.{File, IOException, PrintStream}
import java.net.BindException
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.TimeUnit

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

    // construct base flix options. Target/stdlib are resolved later per command.
    var baseOptions = Options(
      lib = cmdOpts.xlib,
      stdlibProfile = if (cmdOpts.xstdlibProfileExplicit) cmdOpts.xstdlibProfile else StdlibProfile.Jvm,
      target = CompilationTarget.Jvm,
      artifactName = ArtifactNames.DefaultBaseName,
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
      baseOptions = baseOptions.copy(progress = false)
    }

    // Don't use progress bar if not attached to a console.
    if (System.console() == null) {
      baseOptions = baseOptions.copy(progress = false)
    }

    val options = baseOptions

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
            BenchmarkCompilerOld.benchmarkCodeSize(baseOptions)
            System.exit(0)
          }

          // check if the --Xbenchmark-incremental flag was passed.
          if (cmdOpts.xbenchmarkIncremental) {
            BenchmarkCompilerOld.benchmarkIncremental(baseOptions)
            System.exit(0)
          }

          // check if the --Xbenchmark-phases flag was passed.
          if (cmdOpts.xbenchmarkPhases) {
            BenchmarkCompilerOld.benchmarkPhases(baseOptions)
            System.exit(0)
          }

          // check if the --Xbenchmark-frontend flag was passed.
          if (cmdOpts.xbenchmarkFrontend) {
            BenchmarkCompilerOld.benchmarkThroughput(baseOptions, frontend = true)
            System.exit(0)
          }

          // check if the --Xbenchmark-throughput flag was passed.
          if (cmdOpts.xbenchmarkThroughput) {
            BenchmarkCompilerOld.benchmarkThroughput(baseOptions, frontend = false)
            System.exit(0)
          }

          // check if we should start a REPL
          if (cmdOpts.files.isEmpty) {
            Bootstrap.bootstrap(cwd, baseOptions.githubToken) match {
              case Result.Ok(bootstrap) =>
                val shell = new Shell(bootstrap, baseOptions)
                shell.loop()
                System.exit(0)
              case Result.Err(error) =>
                println(error.message(formatter))
                System.exit(1)
            }
          }

          // configure Flix and add the paths.
          val target = singleResolvedCliTarget(Command.Run, cmdOpts).getOrElse {
            System.exit(1)
            null
          }
          if (cmdOpts.runner.exists(r => !isRunnerSupported(Command.Run, target, r))) {
            println(s"Runner '${formatRunner(cmdOpts.runner.get)}' is not supported for command 'run' on target '${formatTarget(target)}'.")
            System.exit(1)
          }
          val options = optionsForTarget(baseOptions, cmdOpts, target)
          validateCommandPreflight(cmdOpts, options, runner = cmdOpts.runner).foreach { message =>
            println(message)
            System.exit(1)
          }

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
          flix.check() match {
            case (Some(root), Nil) =>
              target match {
                case CompilationTarget.LlvmWasm if cmdOpts.runner.contains(RunnerKind.Wasmtime) =>
                  flix.codeGen(root)
                  WasmRunSupport.runWasmtime(options, cwd, cmdOpts.args.toArray) match {
                    case Right(_) => ()
                    case Left(msg) =>
                      println(msg)
                      System.exit(1)
                  }
                case CompilationTarget.LlvmWasm if cmdOpts.runner.contains(RunnerKind.Browser) =>
                  flix.codeGen(root)
                  BrowserRunSupport.runBrowser(options, cwd, cmdOpts.args.toArray, headless = false) match {
                    case Right(_) => ()
                    case Left(msg) =>
                      println(msg)
                      System.exit(1)
                  }
                case _ =>
                  flix.codeGen(root).getMain match {
                    case None => // nop
                    case Some(m) =>
                      // Invoke main with the supplied arguments.
                      m(cmdOpts.args.toArray)
                  }
              }
              System.exit(0)
            case (optRoot, errors) =>
              println(CompilationMessage.formatAll(errors)(formatter, optRoot))
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
                resolveProjectTargets(Command.Check, cmdOpts, bootstrap).flatMap { targets =>
                  runCheckForTargets(targets, baseOptions, cmdOpts, bootstrap)
                }
              }
            }
          } else {
            exitOnResult(runFileCheckForTargets(cmdOpts, baseOptions))
          }

        case Command.Build =>
          if (cmdOpts.files.nonEmpty) {
            println("The 'build' command does not support file arguments.")
            System.exit(1)
          }
          Bootstrap.bootstrap(cwd, options.githubToken).flatMap { bootstrap =>
            resolveProjectTargets(Command.Build, cmdOpts, bootstrap).flatMap { targets =>
              runBuildForTargets(cwd, targets, baseOptions, cmdOpts, bootstrap)
            }
          } match {
            case Result.Ok(_) =>
              System.exit(0)
            case Result.Err(error) =>
              println(error.message(formatter))
              System.exit(1)
          }

        case Command.BuildJar =>
          if (cmdOpts.files.nonEmpty) {
            println("The 'build-jar' command does not support file arguments.")
            System.exit(1)
          }
          if (requestedTargets(cmdOpts).exists(_ != CompilationTarget.Jvm)) {
            println("Command 'build-jar' is JVM-only. Use 'build --target native|wasm' for non-JVM targets.")
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
          if (requestedTargets(cmdOpts).exists(_ != CompilationTarget.Jvm)) {
            println("Command 'build-fatjar' is JVM-only. Use 'build --target native|wasm' for non-JVM targets.")
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

        case Command.Doctor =>
          if (cmdOpts.files.nonEmpty) {
            println("The 'doctor' command does not support file arguments.")
            System.exit(1)
          }
          exitOnResult(runDoctor(cwd, cmdOpts, baseOptions))

        case Command.BindWasmEffects =>
          if (cmdOpts.files.nonEmpty) {
            println("The 'bind wasm-effects' command does not support file arguments.")
            System.exit(1)
          }
          val witDir = cmdOpts.bindWit.getOrElse {
            println("The 'bind wasm-effects' command requires --wit <dir>.")
            System.exit(1)
            null
          }
          val world = cmdOpts.bindWorld.getOrElse {
            println("The 'bind wasm-effects' command requires --world <name>.")
            System.exit(1)
            null
          }
          val outDir = cmdOpts.bindOut.getOrElse {
            println("The 'bind wasm-effects' command requires --out <dir>.")
            System.exit(1)
            null
          }
          WasmEffectBindingsTool.run(WasmEffectBindingsTool.Config(
            witDir = witDir,
            world = world,
            outDir = outDir,
            rootModule = cmdOpts.bindRootModule
          )) match {
            case Result.Ok(generated) =>
              println(s"Generated ${generated.flixFile.toAbsolutePath.normalize()}")
              println(s"Generated ${generated.bindingsFile.toAbsolutePath.normalize()}")
              println(s"Generated ${generated.jsFile.toAbsolutePath.normalize()}")
              println(s"Generated ${generated.dtsFile.toAbsolutePath.normalize()}")
              println(s"Generated ${generated.rustFile.toAbsolutePath.normalize()}")
              System.exit(0)
            case Result.Err(msg) =>
              println(msg)
              System.exit(1)
          }

        case Command.BindNative =>
          if (cmdOpts.files.nonEmpty) {
            println("The 'bind native' command does not support file arguments.")
            System.exit(1)
          }
          val header = cmdOpts.bindHeader.getOrElse {
            println("The 'bind native' command requires --header <file>.")
            System.exit(1)
            null
          }
          val outDir = cmdOpts.bindOut.getOrElse {
            println("The 'bind native' command requires --out <dir>.")
            System.exit(1)
            null
          }
          NativeBindingsTool.run(NativeBindingsTool.Config(
            header = header,
            outDir = outDir,
            rootModule = cmdOpts.bindNativeModule,
            spec = cmdOpts.bindSpec,
            includePaths = cmdOpts.bindIncludePaths,
            defines = cmdOpts.bindDefines,
            cflags = cmdOpts.bindCFlags,
          )) match {
            case Result.Ok(generated) =>
              println(s"Generated ${generated.flixFile.toAbsolutePath.normalize()}")
              generated.shimFile.foreach(p => println(s"Generated ${p.toAbsolutePath.normalize()}"))
              generated.shimHeaderFile.foreach(p => println(s"Generated ${p.toAbsolutePath.normalize()}"))
              if (generated.skipped.nonEmpty) {
                println(s"Generated ${generated.generatedDecls} of ${generated.totalDecls} extern declarations.")
                generated.skipped.foreach { skip =>
                  println(s"Skipped ${skip.symbol}: ${skip.reason}")
                }
              }
              System.exit(0)
            case Result.Err(msg) =>
              println(msg)
              System.exit(1)
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
            } else exitWithErrors(flix, errors, optRoot)
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
          val (optRoot, errors) = flix.check()
          if (errors.isEmpty) {
            val syntaxTree = flix.getParsedAst
            LspFormatter.formatFiles(syntaxTree, cmdOpts.files.map(_.toPath).toList)(flix)
            System.exit(0)
          }
          else exitWithErrors(flix, errors, optRoot)


        case Command.Run =>
          if (cmdOpts.files.nonEmpty) {
            println("The 'run' command does not support file arguments.")
            System.exit(1)
          }
          exitOnResult {
            Bootstrap.bootstrap(cwd, options.githubToken).flatMap { bootstrap =>
              resolveSingleProjectTarget(Command.Run, cmdOpts, bootstrap).flatMap { target =>
                resolveRunner(Command.Run, target, cmdOpts, bootstrap).flatMap { runner =>
                  val targetOptions = optionsForTarget(baseOptions, cmdOpts, target)
                  validateCommandPreflight(cmdOpts, targetOptions, runner = Some(runner)) match {
                    case Some(message) => Result.Err(BootstrapError.GeneralError(message))
                    case None =>
                      runner match {
                        case RunnerKind.Wasmtime =>
                          val flix = new Flix().setFormatter(formatter)
                          flix.setOptions(targetOptions)
                          bootstrap.build(flix).flatMap { _ =>
                            WasmRunSupport.runWasmtime(flix.options, cwd, cmdOpts.args.toArray) match {
                              case Right(_) => Result.Ok(())
                              case Left(msg) => Result.Err(BootstrapError.GeneralError(msg))
                            }
                          }
                        case RunnerKind.Browser =>
                          val flix = new Flix().setFormatter(formatter)
                          flix.setOptions(targetOptions)
                          bootstrap.build(flix).flatMap { _ =>
                            BrowserRunSupport.runBrowser(flix.options, cwd, cmdOpts.args.toArray, headless = false) match {
                              case Right(_) => Result.Ok(())
                              case Left(msg) => Result.Err(BootstrapError.GeneralError(msg))
                            }
                          }
                        case _ =>
                          val flix = new Flix().setFormatter(formatter)
                          flix.setOptions(targetOptions)
                          bootstrap.run(flix, cmdOpts.args.toArray)
                      }
                  }
                }
              }
            }
          }

        case Command.Test =>
          if (cmdOpts.files.isEmpty) {
            exitOnResult {
              Bootstrap.bootstrap(cwd, options.githubToken).flatMap { bootstrap =>
                resolveSingleProjectTarget(Command.Test, cmdOpts, bootstrap).flatMap { target =>
                  resolveRunner(Command.Test, target, cmdOpts, bootstrap).flatMap { runner =>
                    val targetOptions = optionsForTarget(baseOptions.copy(progress = false), cmdOpts, target)
                    validateCommandPreflight(cmdOpts, targetOptions, runner = Some(runner)) match {
                      case Some(message) => Result.Err(BootstrapError.GeneralError(message))
                      case None =>
                        val flix = new Flix().setFormatter(formatter)
                        flix.setOptions(targetOptions)
                        bootstrap.test(flix, Some(runner))
                    }
                  }
                }
              }
            }
          } else {
            val target = singleResolvedCliTarget(Command.Test, cmdOpts).getOrElse {
              System.exit(1)
              null
            }
            val runner = cmdOpts.runner.getOrElse(defaultRunner(target))
            if (!isRunnerSupported(Command.Test, target, runner)) {
              println(s"Runner '${formatRunner(runner)}' is not supported for command 'test' on target '${formatTarget(target)}'.")
              System.exit(1)
            }
            val targetOptions = optionsForTarget(baseOptions.copy(progress = false), cmdOpts, target)
            validateCommandPreflight(cmdOpts, targetOptions, runner = Some(runner)).foreach { message =>
              println(message)
              System.exit(1)
            }
            val flix = mkFlixWithFiles(cmdOpts.files, targetOptions)
            runFileTests(flix, target, runner)
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

        case Command.EffCheck =>
          if (cmdOpts.files.nonEmpty) {
            println("The 'eff-check' command does not support file arguments.")
            System.exit(1)
          }
          exitOnResult {
            Bootstrap.bootstrap(cwd, options.githubToken).flatMap { bootstrap =>
              val flix = new Flix().setFormatter(formatter)
              flix.setOptions(options.copy(progress = false))
              bootstrap.checkEffects(flix)
            }
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
  case class CmdOpts(
    command: Command = Command.None,
    args: List[String] = Nil,
    bindCFlags: List[String] = Nil,
    bindDefines: List[String] = Nil,
    bindHeader: Option[Path] = None,
    bindIncludePaths: List[Path] = Nil,
    bindNativeModule: String = "Native",
    bindOut: Option[Path] = None,
    bindSpec: Option[Path] = None,
    bindRootModule: String = "Wit",
    bindWit: Option[Path] = None,
    bindWorld: Option[String] = None,
    emits: List[EmitKind] = Nil,
    runner: Option[RunnerKind] = None,
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
    xstdlibProfile: StdlibProfile = StdlibProfile.Jvm,
    xstdlibProfileExplicit: Boolean = false,
    targets: List[CompilationTarget] = Nil,
    xprintphases: Boolean = false,
    xsummary: Boolean = false,
    xsubeffecting: Set[Subeffecting] = Set.empty,
    XPerfN: Option[Int] = None,
    XPerfFrontend: Boolean = false,
    XPerfPar: Boolean = false,
    files: Seq[File] = Seq()
  )

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

    case object Doctor extends Command

    case object BindNative extends Command

    case object BindWasmEffects extends Command

    case object Doc extends Command

    case object Format extends Command

    case object Run extends Command

    case object Test extends Command

    case object Repl extends Command

    case object PlainLsp extends Command

    case class VSCodeLsp(port: Int) extends Command

    case object Release extends Command

    case object Outdated extends Command

    case object EffCheck extends Command

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

    implicit val readStdlibProfile: scopt.Read[StdlibProfile] = scopt.Read.reads {
      case "jvm" => StdlibProfile.Jvm
      case "portable" => StdlibProfile.Portable
      case arg => throw new IllegalArgumentException(s"'$arg' is not a valid stdlib profile. Valid options are 'jvm' and 'portable'.")
    }

    implicit val readCompilationTarget: scopt.Read[CompilationTarget] = scopt.Read.reads {
      case "jvm" => CompilationTarget.Jvm
      case "native" => CompilationTarget.LlvmNative
      case "wasm" => CompilationTarget.LlvmWasm
      case arg => throw new IllegalArgumentException(s"'$arg' is not a valid compilation target. Valid options are 'jvm', 'native', and 'wasm'.")
    }

    implicit val readEmitKind: scopt.Read[EmitKind] = scopt.Read.reads {
      case "classes" => EmitKind.Classes
      case "jar" => EmitKind.Jar
      case "fatjar" => EmitKind.FatJar
      case "exe" => EmitKind.Exe
      case "staticlib" => EmitKind.StaticLib
      case "sharedlib" => EmitKind.SharedLib
      case "component" => EmitKind.Component
      case "js" => EmitKind.Js
      case arg => throw new IllegalArgumentException(s"'$arg' is not a valid build emit kind. Valid options are 'classes', 'jar', 'fatjar', 'exe', 'staticlib', 'sharedlib', 'component', and 'js'.")
    }

    implicit val readRunnerKind: scopt.Read[RunnerKind] = scopt.Read.reads {
      case "jvm" => RunnerKind.Jvm
      case "native" => RunnerKind.Native
      case "node" => RunnerKind.Node
      case "browser" => RunnerKind.Browser
      case "wasmtime" => RunnerKind.Wasmtime
      case arg => throw new IllegalArgumentException(s"'$arg' is not a valid runner. Valid options are 'jvm', 'native', 'node', 'browser', and 'wasmtime'.")
    }

    val parser = new scopt.OptionParser[CmdOpts]("flix") {

      // Head
      head("The Flix Programming Language", Version.CurrentVersion.toString)

      // Command
      cmd("init").action((_, c) => c.copy(command = Command.Init)).text("  creates a new project in the current directory.")

      cmd("check").action((_, c) => c.copy(command = Command.Check)).text("  checks the current project for errors.")

      cmd("build").action((_, c) => c.copy(command = Command.Build)).text("  builds (i.e. compiles) the current project.")

      cmd("build-jar").hidden().action((_, c) => c.copy(command = Command.BuildJar)).text("  builds a jar-file from the current project.")

      cmd("build-fatjar").hidden().action((_, c) => c.copy(command = Command.BuildFatJar)).text("  builds a fatjar-file from the current project.")

      cmd("package").action((_, c) => c.copy(command = Command.BuildPkg)).text("  builds a fpkg-file from the current project.")

      cmd("build-pkg").hidden().action((_, c) => c.copy(command = Command.BuildPkg)).text("  builds a fpkg-file from the current project.")

      cmd("clean").action((_, c) => c.copy(command = Command.Clean)).text("  recursively removes class files from the build directory.")

      cmd("doctor").action((_, c) => c.copy(command = Command.Doctor)).text("  checks toolchains and target configuration.")

      cmd("bind").text("  generates source bindings from external interface definitions.")
        .children(
          cmd("native").action((_, c) => c.copy(command = Command.BindNative))
            .text("  generates Flix native bindings from a curated C header."),
          cmd("wasm-effects").action((_, c) => c.copy(command = Command.BindWasmEffects))
            .text("  generates Flix async effect bindings from a WIT world.")
        )

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

      cmd("eff-check").text("  checks that dependencies respect the 'effects.lock' file.")
        .action((_, c) => c.copy(command = Command.EffCheck))

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

      opt[String]("wit").action((s, c) => c.copy(bindWit = Some(Paths.get(s)))).
        text("WIT package directory for binding generation.")

      opt[String]("header").action((s, c) => c.copy(bindHeader = Some(Paths.get(s)))).
        text("curated C header for native binding generation.")

      opt[String]("spec").action((s, c) => c.copy(bindSpec = Some(Paths.get(s)))).
        text("sidecar TOML spec for native binding generation semantics.")

      opt[String]("world").action((s, c) => c.copy(bindWorld = Some(s))).
        text("WIT world name for binding generation.")

      opt[String]("out").action((s, c) => c.copy(bindOut = Some(Paths.get(s)))).
        text("output directory for generated bindings.")

      opt[String]("root-module").action((s, c) => c.copy(bindRootModule = s)).
        text("root Flix module for generated WIT effect bindings.")

      opt[String]("native-module").action((s, c) => c.copy(bindNativeModule = s)).
        text("root Flix module for generated native bindings.")

      opt[String]("include").unbounded().action((s, c) => c.copy(bindIncludePaths = c.bindIncludePaths :+ Paths.get(s))).
        text("additional C include path for native binding generation.")

      opt[String]("define").unbounded().action((s, c) => c.copy(bindDefines = c.bindDefines :+ s)).
        text("preprocessor definition for native binding generation.")

      opt[String]("cflag").unbounded().action((s, c) => c.copy(bindCFlags = c.bindCFlags :+ s)).
        text("additional C compiler flag for native binding generation.")

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
      opt[CompilationTarget]("target").unbounded().action((arg, c) => c.copy(targets = c.targets :+ arg)).
        text("selects compilation target (jvm, native, wasm). Repeat for multi-target check/build.")

      opt[EmitKind]("emit").unbounded().action((arg, c) => c.copy(emits = c.emits :+ arg)).
        text("selects artifact kind for build (classes, jar, fatjar, exe, staticlib, sharedlib, component, js).")

      opt[RunnerKind]("runner").action((arg, c) => c.copy(runner = Some(arg))).
        text("selects runtime runner (jvm, native, node, browser, wasmtime).")

      opt[Unit]("jvm").action((_, c) => c.copy(targets = c.targets :+ CompilationTarget.Jvm)).
        text("alias for --target jvm.")

      opt[Unit]("native").action((_, c) => c.copy(targets = c.targets :+ CompilationTarget.LlvmNative)).
        text("alias for --target native.")

      opt[Unit]("wasm").action((_, c) => c.copy(targets = c.targets :+ CompilationTarget.LlvmWasm)).
        text("alias for --target wasm.")

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

      // Xstdlib-profile
      opt[StdlibProfile]("Xstdlib-profile").hidden().action((arg, c) => c.copy(xstdlibProfile = arg, xstdlibProfileExplicit = true)).
        text("[experimental] selects stdlib profile (jvm, portable). Default: jvm on JVM target, portable on LLVM targets.")

      // Xtarget
      opt[CompilationTarget]("Xtarget").hidden().action((arg, c) => c.copy(targets = c.targets :+ arg)).
        text("[experimental] selects compilation target (jvm, native, wasm).")

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
  private def exitWithErrors(flix: Flix, errors: List[CompilationMessage], root: Option[TypedAst.Root]): Unit = {
    println(CompilationMessage.formatAll(errors)(flix.getFormatter, root))
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

  private[flix] case class ToolRequirement(name: String, probe: List[String])

  private def requestedTargets(cmdOpts: CmdOpts): List[CompilationTarget] =
    cmdOpts.targets.distinct

  private[flix] def effectiveStdlibProfile(cmdOpts: CmdOpts, target: CompilationTarget): StdlibProfile =
    if (cmdOpts.xstdlibProfileExplicit) cmdOpts.xstdlibProfile
    else defaultStdlibProfile(target)

  private[flix] def defaultStdlibProfile(target: CompilationTarget): StdlibProfile = target match {
    case CompilationTarget.Jvm => StdlibProfile.Jvm
    case CompilationTarget.LlvmNative | CompilationTarget.LlvmWasm => StdlibProfile.Portable
  }

  private def optionsForTarget(baseOptions: Options, cmdOpts: CmdOpts, target: CompilationTarget): Options =
    baseOptions.copy(target = target, stdlibProfile = effectiveStdlibProfile(cmdOpts, target))

  private def singleResolvedCliTarget(command: Command, cmdOpts: CmdOpts): Option[CompilationTarget] = {
    requestedTargets(cmdOpts) match {
      case Nil => Some(CompilationTarget.Jvm)
      case target :: Nil => Some(target)
      case targets =>
        println(s"Command '${formatCommand(command)}' accepts exactly one target, but got: ${targets.map(formatTarget).mkString(", ")}")
        None
    }
  }

  private def resolveProjectTargets(command: Command,
                                    cmdOpts: CmdOpts,
                                    bootstrap: Bootstrap): Result[List[CompilationTarget], BootstrapError] = {
    val requested = requestedTargets(cmdOpts)

    command match {
      case Command.Check | Command.Build =>
        val targets = if (requested.nonEmpty) requested else bootstrap.buildTargets
        Result.Ok(targets)

      case Command.Run =>
        requested match {
          case Nil =>
            bootstrap.runTarget
              .orElse(bootstrap.buildTargets match {
                case target :: Nil => Some(target)
                case _ => None
              })
              .map(Result.Ok(_))
              .getOrElse(Result.Err(BootstrapError.GeneralError("No default run target configured. Add a [run] target to flix.toml or pass --target.")))
              .map(List(_))
          case target :: Nil => Result.Ok(List(target))
          case targets => Result.Err(BootstrapError.GeneralError(s"Command 'run' accepts exactly one target, but got: ${targets.map(formatTarget).mkString(", ")}"))
        }

      case Command.Test =>
        requested match {
          case Nil => Result.Ok(List(bootstrap.testTarget.getOrElse(CompilationTarget.Jvm)))
          case target :: Nil => Result.Ok(List(target))
          case targets => Result.Err(BootstrapError.GeneralError(s"Command 'test' accepts exactly one target, but got: ${targets.map(formatTarget).mkString(", ")}"))
        }

      case _ => Result.Ok(List(CompilationTarget.Jvm))
    }
  }

  private def resolveSingleProjectTarget(command: Command,
                                         cmdOpts: CmdOpts,
                                         bootstrap: Bootstrap): Result[CompilationTarget, BootstrapError] =
    resolveProjectTargets(command, cmdOpts, bootstrap).flatMap {
      case target :: Nil => Result.Ok(target)
      case Nil => Result.Err(BootstrapError.GeneralError(s"No target resolved for command '${formatCommand(command)}'."))
      case targets => Result.Err(BootstrapError.GeneralError(s"Command '${formatCommand(command)}' accepts exactly one target, but got: ${targets.map(formatTarget).mkString(", ")}"))
    }

  private def defaultRunner(target: CompilationTarget): RunnerKind = target match {
    case CompilationTarget.Jvm => RunnerKind.Jvm
    case CompilationTarget.LlvmNative => RunnerKind.Native
    case CompilationTarget.LlvmWasm => RunnerKind.Node
  }

  private val allRunners: List[RunnerKind] =
    List(RunnerKind.Jvm, RunnerKind.Native, RunnerKind.Node, RunnerKind.Browser, RunnerKind.Wasmtime)

  private def resolveRunner(command: Command,
                            target: CompilationTarget,
                            cmdOpts: CmdOpts,
                            bootstrap: Bootstrap): Result[RunnerKind, BootstrapError] = {
    val runner = cmdOpts.runner
      .orElse(command match {
        case Command.Run => bootstrap.runRunner
        case Command.Test => bootstrap.testRunner
        case _ => None
      })
      .getOrElse(defaultRunner(target))

    if (isRunnerSupported(command, target, runner)) Result.Ok(runner)
    else Result.Err(BootstrapError.GeneralError(s"Runner '${formatRunner(runner)}' is not supported for command '${formatCommand(command)}' on target '${formatTarget(target)}'."))
  }

  private def runDoctor(cwd: Path,
                        cmdOpts: CmdOpts,
                        baseOptions: Options)(implicit formatter: Formatter): Result[Unit, BootstrapError] = {
    val manifestPath = cwd.resolve("flix.toml").normalize()
    val manifestOptResult =
      if (Files.exists(manifestPath)) {
        ca.uwaterloo.flix.tools.pkg.ManifestParser.parse(manifestPath) match {
          case Result.Ok(m) => Result.Ok(Some(m))
          case Result.Err(e) => Result.Err(BootstrapError.ManifestParseError(e))
        }
      } else {
        Result.Ok(None)
      }

    manifestOptResult.map { manifestOpt =>
      val requested = requestedTargets(cmdOpts)
      val targets =
        if (requested.nonEmpty) requested
        else manifestOpt match {
          case Some(m) =>
            (m.buildConfig.targets ::: m.runConfig.target.toList ::: m.testConfig.target.toList).distinct
          case None =>
            List(CompilationTarget.Jvm, CompilationTarget.LlvmNative, CompilationTarget.LlvmWasm)
        }

      val lines = scala.collection.mutable.ListBuffer.empty[String]
      lines += s"Flix doctor for ${cwd.toAbsolutePath.normalize()}"
      lines += s"Manifest: ${if (manifestOpt.isDefined) "found" else "not found"}"
      lines += s"Artifact base name: ${manifestOpt.map(_.name).getOrElse(baseOptions.artifactName)}"

      manifestOpt.foreach { m =>
        lines += s"Build targets: ${m.buildConfig.targets.map(formatTarget).mkString(", ")}"
        lines += s"Run default: ${m.runConfig.target.map(formatTarget).getOrElse("<none>")} / ${m.runConfig.runner.map(formatRunner).getOrElse("<default>")}"
        lines += s"Test default: ${m.testConfig.target.map(formatTarget).getOrElse("<none>")} / ${m.testConfig.runner.map(formatRunner).getOrElse("<default>")}"
      }

      for (target <- targets) {
        val targetOptions = optionsForTarget(baseOptions, cmdOpts, target)
        val buildEmits = manifestOpt.flatMap(_.targetConfigs.emitFor(target)).getOrElse(defaultEmits(target))
        lines += ""
        lines += s"Target ${formatTarget(target)}"
        lines += s"  stdlib: ${formatStdlibProfile(targetOptions.stdlibProfile)}"
        lines += s"  default emits: ${formatDefaultEmits(target, buildEmits)}"

        val buildTools = doctorToolStatuses(Command.Build, targetOptions, runner = None)
        if (buildTools.nonEmpty) {
          lines += "  build tools:"
          buildTools.foreach(t => lines += s"    ${t.name}: ${if (t.ok) "ok" else "missing"}")
        }

        val supportedRunners = allRunners.filter(r => isRunnerSupported(Command.Run, target, r))
        if (supportedRunners.nonEmpty) {
          lines += "  run runners:"
          supportedRunners.foreach { runner =>
            val statuses = doctorToolStatuses(Command.Run, targetOptions, runner = Some(runner))
            val toolText =
              if (statuses.isEmpty) "n/a"
              else statuses.map(t => s"${t.name}=${if (t.ok) "ok" else "missing"}").mkString(", ")
            lines += s"    ${formatRunner(runner)}: $toolText"
          }
        }

        val testRunners = allRunners.filter(r => isRunnerSupported(Command.Test, target, r))
        if (testRunners.nonEmpty) {
          lines += "  test runners:"
          testRunners.foreach { runner =>
            val statuses = doctorToolStatuses(Command.Test, targetOptions, runner = Some(runner))
            val toolText =
              if (statuses.isEmpty) "n/a"
              else statuses.map(t => s"${t.name}=${if (t.ok) "ok" else "missing"}").mkString(", ")
            lines += s"    ${formatRunner(runner)}: $toolText"
          }
        }
      }

      println(lines.mkString(System.lineSeparator()))
    }
  }

  private case class DoctorToolStatus(name: String, ok: Boolean)

  private def doctorToolStatuses(command: Command,
                                 options: Options,
                                 runner: Option[RunnerKind]): List[DoctorToolStatus] = {
    val required = requiredTools(command, options, runner).map(t => DoctorToolStatus(t.name, hasCmd(t.probe)))
    (options.target, runner) match {
      case (CompilationTarget.LlvmWasm, Some(RunnerKind.Browser)) =>
        required :+ DoctorToolStatus("chrome", BrowserRunSupport.findChromeBinary().nonEmpty)
      case _ => required
    }
  }

  private def isRunnerSupported(command: Command, target: CompilationTarget, runner: RunnerKind): Boolean = (command, target, runner) match {
    case (_, CompilationTarget.Jvm, RunnerKind.Jvm) => true
    case (_, CompilationTarget.LlvmNative, RunnerKind.Native) => true
    case (Command.Run, CompilationTarget.LlvmWasm, RunnerKind.Node) => true
    case (Command.Run, CompilationTarget.LlvmWasm, RunnerKind.Browser) => true
    case (Command.Run, CompilationTarget.LlvmWasm, RunnerKind.Wasmtime) => true
    case (Command.Test, CompilationTarget.LlvmWasm, RunnerKind.Node) => true
    case (Command.Test, CompilationTarget.LlvmWasm, RunnerKind.Browser) => true
    case (Command.Test, CompilationTarget.LlvmWasm, RunnerKind.Wasmtime) => true
    case _ => false
  }

  private def runFileTests(flix: Flix,
                           target: CompilationTarget,
                           runner: RunnerKind)(implicit formatter: Formatter): Unit = target match {
    case CompilationTarget.Jvm =>
      flix.compile() match {
        case Validation.Success(compilationResult) =>
          Tester.run(Nil, compilationResult)(flix) match {
            case Result.Ok(_) => System.exit(0)
            case Result.Err(_) => System.exit(1)
          }
        case Validation.Failure(errors) => exitWithErrors(flix, errors.toList, None)
      }

    case CompilationTarget.LlvmNative | CompilationTarget.LlvmWasm =>
      implicit val sctx: SecurityContext = SecurityContext.Unrestricted
      val rootDir = Paths.get(".").toAbsolutePath.normalize()
      val driverPath = flix.options.outputPath.resolve("__FlixFileTestDriver.flix").normalize()
      flix.setOptions(flix.options.copy(entryPoint = None))

      flix.check() match {
        case (Some(root), Nil) =>
          flix.remVirtualPath(driverPath)
          flix.addVirtualPath(driverPath, ProjectTestDriver.mkDriverSource(ProjectTestDriver.collectProjectTests(root))(flix))
          flix.setOptions(flix.options.copy(entryPoint = Some(ProjectTestDriver.EntryPointSym)))
          flix.compile() match {
            case Validation.Success(compilationResult) =>
              val result = target match {
                case CompilationTarget.LlvmWasm if runner == RunnerKind.Wasmtime =>
                  WasmRunSupport.runWasmtime(flix.options, rootDir, Array.empty) match {
                    case Right(_) => Result.Ok(())
                    case Left(msg) => Result.Err(BootstrapError.GeneralError(msg))
                  }
                case CompilationTarget.LlvmWasm if runner == RunnerKind.Browser =>
                  BrowserRunSupport.runBrowser(flix.options, rootDir, Array.empty, headless = true) match {
                    case Right(_) => Result.Ok(())
                    case Left(msg) => Result.Err(BootstrapError.GeneralError(msg))
                  }
                case _ =>
                  compilationResult.getMain match {
                    case Some(main) =>
                      try {
                        main(Array.empty)
                        Result.Ok(())
                      } catch {
                        case ex: Throwable =>
                          Result.Err(BootstrapError.GeneralError(Option(ex.getMessage).getOrElse(ex.toString)))
                      }
                    case None =>
                      Result.Err(BootstrapError.GeneralError("Generated test driver has no main entry point."))
                  }
              }
              exitOnResult(result)
            case Validation.Failure(errors) =>
              exitWithErrors(flix, errors.toList, None)
          }

        case (optRoot, errors) =>
          exitWithErrors(flix, errors, optRoot)
      }
  }

  private def runCheckForTargets(targets: List[CompilationTarget],
                                 baseOptions: Options,
                                 cmdOpts: CmdOpts,
                                 bootstrap: Bootstrap)(implicit formatter: Formatter): Result[Unit, BootstrapError] =
    targets.foldLeft(Result.Ok(()): Result[Unit, BootstrapError]) {
      case (acc, target) =>
        acc.flatMap { _ =>
          val targetOptions = optionsForTarget(baseOptions, cmdOpts, target)
          validateCommandPreflight(cmdOpts, targetOptions)
            .map(msg => Result.Err[Unit, BootstrapError](BootstrapError.GeneralError(msg)))
            .getOrElse {
              val flix = new Flix().setFormatter(formatter)
              flix.setOptions(targetOptions)
              bootstrap.check(flix)
            }
        }
    }

  private def runFileCheckForTargets(cmdOpts: CmdOpts,
                                     baseOptions: Options)(implicit formatter: Formatter): Result[Unit, BootstrapError] = {
    val targets = requestedTargets(cmdOpts) match {
      case Nil => List(CompilationTarget.Jvm)
      case ts => ts
    }

    targets.foldLeft(Result.Ok(()): Result[Unit, BootstrapError]) {
      case (acc, target) =>
        acc.flatMap { _ =>
          val targetOptions = optionsForTarget(baseOptions, cmdOpts, target)
          validateCommandPreflight(cmdOpts, targetOptions)
            .map(msg => Result.Err[Unit, BootstrapError](BootstrapError.GeneralError(msg)))
            .getOrElse {
              val flix = mkFlixWithFiles(cmdOpts.files, targetOptions)
              val (optRoot, errors) = flix.check()
              if (errors.isEmpty) Result.Ok(())
              else Result.Err(BootstrapError.GeneralError(CompilationMessage.formatAll(errors)(flix.getFormatter, optRoot)))
            }
        }
    }
  }

  private def runBuildForTargets(cwd: Path,
                                 targets: List[CompilationTarget],
                                 baseOptions: Options,
                                 cmdOpts: CmdOpts,
                                 bootstrap: Bootstrap)(implicit formatter: Formatter): Result[Unit, BootstrapError] =
    validateBuildCliEmits(targets, cmdOpts.emits) match {
      case Some(message) => Result.Err(BootstrapError.GeneralError(message))
      case None =>
        targets.foldLeft(Result.Ok(()): Result[Unit, BootstrapError]) {
          case (acc, target) =>
            acc.flatMap { _ =>
              val targetOptions = optionsForTarget(baseOptions.copy(loadClassFiles = false), cmdOpts, target)
              val emits = resolveBuildEmits(target, cmdOpts, bootstrap)
              validateCommandPreflight(cmdOpts, targetOptions)
                .map(msg => Result.Err[Unit, BootstrapError](BootstrapError.GeneralError(msg)))
                .orElse(validateBuildEmits(target, emits).map(msg => Result.Err[Unit, BootstrapError](BootstrapError.GeneralError(msg))))
                .getOrElse(runBuildForTargetAndEmits(cwd, target, targetOptions, emits, bootstrap))
            }
        }
    }

  private def runBuildForTargetAndEmits(cwd: Path,
                                        target: CompilationTarget,
                                        options: Options,
                                        emits: List[EmitKind],
                                        bootstrap: Bootstrap)(implicit formatter: Formatter): Result[Unit, BootstrapError] = {
    val flix = new Flix().setFormatter(formatter)
    val requestedEmits = emits.distinct
    flix.setOptions(options.copy(emits = requestedEmits.toSet))

    (target, requestedEmits) match {
      case (CompilationTarget.Jvm, emits0) =>
        emits0.foldLeft(Result.Ok(()): Result[Unit, BootstrapError]) {
          case (acc, EmitKind.Classes) => acc.flatMap(_ => bootstrap.build(flix).map(_ => ()))
          case (acc, EmitKind.Jar) => acc.flatMap(_ => bootstrap.buildJar(flix))
          case (acc, EmitKind.FatJar) => acc.flatMap(_ => bootstrap.buildFatJar(flix))
          case (_, emit) => Result.Err(BootstrapError.GeneralError(s"Emit '${formatEmit(emit)}' is not supported for target '${formatTarget(target)}'."))
        }

      case (CompilationTarget.LlvmNative, _) =>
        bootstrap.build(flix).map { _ =>
          formatBuildArtifactsSummary(cwd, flix.options).foreach(println)
        }

      case (CompilationTarget.LlvmWasm, _) =>
        bootstrap.build(flix).map { _ =>
          formatBuildArtifactsSummary(cwd, flix.options).foreach(println)
        }
    }
  }

  private def defaultEmits(target: CompilationTarget): List[EmitKind] = target match {
    case CompilationTarget.Jvm => List(EmitKind.Classes)
    case CompilationTarget.LlvmNative => Nil
    case CompilationTarget.LlvmWasm => List(EmitKind.Component, EmitKind.Js)
  }

  private def formatDefaultEmits(target: CompilationTarget, emits: List[EmitKind]): String =
    if (emits.nonEmpty) emits.map(formatEmit).mkString(", ")
    else target match {
      case CompilationTarget.LlvmNative => "auto (exe if main, staticlib/sharedlib if exports)"
      case _ => "<none>"
    }

  private def resolveBuildEmits(target: CompilationTarget, cmdOpts: CmdOpts, bootstrap: Bootstrap): List[EmitKind] =
    if (cmdOpts.emits.nonEmpty) cmdOpts.emits.distinct
    else bootstrap.buildEmits(target).getOrElse(defaultEmits(target))

  private def validateBuildCliEmits(targets: List[CompilationTarget], emits: List[EmitKind]): Option[String] = {
    if (emits.isEmpty) None
    else if (targets.size != 1) {
      Some("Explicit --emit values currently require exactly one build target.")
    } else validateBuildEmits(targets.head, emits)
  }

  private def validateBuildEmits(target: CompilationTarget, emits: List[EmitKind]): Option[String] = {
    val invalid = emits.filterNot {
      case EmitKind.Classes | EmitKind.Jar | EmitKind.FatJar => target == CompilationTarget.Jvm
      case EmitKind.Exe | EmitKind.StaticLib | EmitKind.SharedLib => target == CompilationTarget.LlvmNative
      case EmitKind.Component | EmitKind.Js => target == CompilationTarget.LlvmWasm
    }

    invalid.headOption.map(emit => s"Emit '${formatEmit(emit)}' is not supported for target '${formatTarget(target)}'.")
  }

  private[flix] def validateCommandPreflight(cmdOpts: CmdOpts,
                                             options: Options,
                                             runner: Option[RunnerKind] = None,
                                             hasCmd: List[String] => Boolean = Main.hasCmd,
                                             hasUsableZig: => Boolean = ca.uwaterloo.flix.util.ZigToolchain.hasUsableCommand): Option[String] = {
    val effectiveCommand = cmdOpts.command match {
      case Command.None if cmdOpts.files.nonEmpty => Command.Run
      case other => other
    }

    if (!shouldValidatePreflight(effectiveCommand)) {
      None
    } else {
      validateTargetProfile(cmdOpts, options)
        .orElse(validateCommandSupport(effectiveCommand, options))
        .orElse(validateRequiredTools(effectiveCommand, options, runner, hasCmd, hasUsableZig))
        .orElse(validateBrowserRunner(options, runner))
    }
  }

  private def validateBrowserRunner(options: Options, runner: Option[RunnerKind]): Option[String] = (options.target, runner) match {
    case (CompilationTarget.LlvmWasm, Some(RunnerKind.Browser)) if BrowserRunSupport.findChromeBinary().isEmpty =>
      Some(
        s"""Missing required browser for target '${formatTarget(options.target)}': Chrome/Chromium
           |
           |Set:
           |  FLIX_CHROME=/path/to/chrome
           |or install `google-chrome` / `chromium`.
           |""".stripMargin
      )
    case _ => None
  }

  private[flix] def validateTargetProfile(cmdOpts: CmdOpts, options: Options): Option[String] = options.target match {
    case CompilationTarget.Jvm => None
    case CompilationTarget.LlvmNative | CompilationTarget.LlvmWasm if options.stdlibProfile != StdlibProfile.Portable =>
      Some(
        s"""Target '${formatTarget(options.target)}' uses the portable standard library profile.
           |
           |The explicit profile '${formatStdlibProfile(cmdOpts.xstdlibProfile)}' is not supported with this target.
           |
           |Re-run with:
           |  --target ${formatTarget(options.target)}
           |""".stripMargin
      )
    case _ => None
  }

  private[flix] def validateCommandSupport(command: Command, options: Options): Option[String] = (command, options.target) match {
    case (Command.BuildJar | Command.BuildFatJar, CompilationTarget.LlvmNative | CompilationTarget.LlvmWasm) =>
      Some(
        s"""Command '${formatCommand(command)}' is JVM-only.
           |
           |Use:
           |  build --target ${formatTarget(options.target)}
           |to produce LLVM artifacts instead.
           |""".stripMargin
      )
    case _ => None
  }

  private[flix] def validateRequiredTools(command: Command,
                                          options: Options,
                                          runner: Option[RunnerKind],
                                          hasCmd: List[String] => Boolean,
                                          hasUsableZig: => Boolean): Option[String] = {
    def toolAvailable(t: ToolRequirement): Boolean =
      if (t.name == "zig") hasUsableZig
      else hasCmd(t.probe)

    val missing = requiredTools(command, options, runner).filterNot(toolAvailable)
    if (missing.isEmpty) None
    else {
      val tools = missing.map(_.name).mkString(", ")
      val invocation = invocationHint(command, options)
      Some(
        s"""Missing required tool${if (missing.length == 1) "" else "s"} for target '${formatTarget(options.target)}': $tools
           |
           |Command:
           |  $invocation
           |""".stripMargin
      )
    }
  }

  private def requiredTools(command: Command, options: Options, runner: Option[RunnerKind]): List[ToolRequirement] = (command, options.target, runner) match {
    case (Command.Build | Command.Run | Command.Test, CompilationTarget.LlvmNative, _) =>
      List(ToolRequirement("zig", Nil))
    case (Command.Build, CompilationTarget.LlvmWasm, _) =>
      List(
        ToolRequirement("zig", Nil),
        ToolRequirement("wasm-tools", List("wasm-tools", "--version")),
        ToolRequirement("jco", List("jco", "--version"))
      )
    case (Command.Run | Command.Test, CompilationTarget.LlvmWasm, Some(RunnerKind.Wasmtime)) =>
      List(
        ToolRequirement("zig", Nil),
        ToolRequirement("wasm-tools", List("wasm-tools", "--version")),
        ToolRequirement("jco", List("jco", "--version")),
        ToolRequirement("cargo +stable", List("cargo", "+stable", "--version"))
      )
    case (Command.Run | Command.Test, CompilationTarget.LlvmWasm, _) =>
      List(
        ToolRequirement("zig", Nil),
        ToolRequirement("wasm-tools", List("wasm-tools", "--version")),
        ToolRequirement("jco", List("jco", "--version")),
        ToolRequirement("node", List("node", "--version"))
      )
    case _ => Nil
  }

  private def shouldValidatePreflight(command: Command): Boolean = command match {
    case Command.Check | Command.Build | Command.BuildJar | Command.BuildFatJar | Command.Run | Command.Test => true
    case _ => false
  }

  private def invocationHint(command: Command, options: Options): String = {
    val parts = List(formatCommand(command), "--target", formatTarget(options.target))
    parts.mkString(" ")
  }

  private def formatCommand(command: Command): String = command match {
    case Command.None => "<files>"
    case Command.Init => "init"
    case Command.Check => "check"
    case Command.Build => "build"
    case Command.BuildJar => "build-jar"
    case Command.BuildFatJar => "build-fatjar"
    case Command.BuildPkg => "package"
    case Command.Clean => "clean"
    case Command.Doctor => "doctor"
    case Command.BindNative => "bind native"
    case Command.BindWasmEffects => "bind wasm-effects"
    case Command.Doc => "doc"
    case Command.Format => "format"
    case Command.Run => "run"
    case Command.Test => "test"
    case Command.Repl => "repl"
    case Command.PlainLsp => "lsp"
    case Command.VSCodeLsp(_) => "lsp-vscode"
    case Command.Release => "release"
    case Command.Outdated => "outdated"
    case Command.EffCheck => "eff-check"
    case Command.EffLock => "eff-lock"
    case Command.CompilerPerf => "Xperf"
    case Command.CompilerMemory => "Xmemory"
    case Command.Zhegalkin => "Xzhegalkin"
  }

  private def formatTarget(target: CompilationTarget): String = target match {
    case CompilationTarget.Jvm => "jvm"
    case CompilationTarget.LlvmNative => "native"
    case CompilationTarget.LlvmWasm => "wasm"
  }

  private def formatStdlibProfile(profile: StdlibProfile): String = profile match {
    case StdlibProfile.Jvm => "jvm"
    case StdlibProfile.Portable => "portable"
  }

  private def formatEmit(emit: EmitKind): String = emit match {
    case EmitKind.Classes => "classes"
    case EmitKind.Jar => "jar"
    case EmitKind.FatJar => "fatjar"
    case EmitKind.Exe => "exe"
    case EmitKind.StaticLib => "staticlib"
    case EmitKind.SharedLib => "sharedlib"
    case EmitKind.Component => "component"
    case EmitKind.Js => "js"
  }

  private def formatRunner(runner: RunnerKind): String = runner match {
    case RunnerKind.Jvm => "jvm"
    case RunnerKind.Native => "native"
    case RunnerKind.Node => "node"
    case RunnerKind.Browser => "browser"
    case RunnerKind.Wasmtime => "wasmtime"
  }

  private def hasCmd(cmd: List[String]): Boolean = {
    try {
      val p = new ProcessBuilder(cmd: _*).redirectErrorStream(true).start()
      p.waitFor(2, TimeUnit.SECONDS) && p.exitValue() == 0
    } catch {
      case _: IOException => false
      case _: InterruptedException => false
    }
  }

  private def formatBuildArtifactsSummary(cwd: Path, options: Options): Option[String] = {
    val buildDir = Bootstrap.getBuildTargetDirectory(cwd, options.target).toAbsolutePath.normalize()
    val llvmDir = buildDir.resolve("llvm")

    options.target match {
      case CompilationTarget.Jvm => None
      case CompilationTarget.LlvmNative =>
        val paths = List(
          ca.uwaterloo.flix.language.phase.llvm.LlvmExportSdkWriter.nativeSdkDir(options.outputPath),
          ca.uwaterloo.flix.language.phase.llvm.LlvmExportSdkWriter.nativeManifestPath(options.outputPath),
          llvmDir.resolve("module.ll"),
          ca.uwaterloo.flix.language.phase.llvm.LlvmNativeDriver.executablePath(options.outputPath, options.artifactName),
          ca.uwaterloo.flix.language.phase.llvm.LlvmNativeDriver.staticLibraryPath(options.outputPath, options.artifactName),
          ca.uwaterloo.flix.language.phase.llvm.LlvmExportWriter.exportsHeaderPath(options.outputPath, options.artifactName),
          ca.uwaterloo.flix.language.phase.llvm.LlvmNativeDriver.sharedLibraryPath(options.outputPath, options.artifactName)
        ).filter(Files.exists(_))

        Some((s"Built ${formatTarget(options.target)} artifacts:" :: paths.map(p => s"  $p")).mkString(System.lineSeparator()))

      case CompilationTarget.LlvmWasm =>
        val paths = List(
          ca.uwaterloo.flix.language.phase.llvm.LlvmExportSdkWriter.wasmSdkDir(options.outputPath),
          ca.uwaterloo.flix.language.phase.llvm.LlvmExportSdkWriter.wasmManifestPath(options.outputPath),
          llvmDir.resolve("module.ll"),
          ca.uwaterloo.flix.language.phase.llvm.LlvmWasmExportWriter.manifestPath(options.outputPath, options.artifactName),
          ca.uwaterloo.flix.language.phase.llvm.LlvmWasmDriver.coreWasmPath(options.outputPath, options.artifactName),
          ca.uwaterloo.flix.language.phase.llvm.LlvmWasmDriver.componentWasmPath(options.outputPath, options.artifactName),
          ca.uwaterloo.flix.language.phase.llvm.LlvmWasmTypedExportsWriter.typedComponentPath(options.outputPath, options.artifactName),
          ca.uwaterloo.flix.language.phase.llvm.LlvmWasmDriver.componentJsPath(options.outputPath, options.artifactName),
          ca.uwaterloo.flix.language.phase.llvm.LlvmWasmTypedExportsWriter.typedComponentJsPath(options.outputPath, options.artifactName),
          ca.uwaterloo.flix.language.phase.llvm.LlvmWasmTypedExportsWriter.typedComponentTypesPath(options.outputPath, options.artifactName),
          ca.uwaterloo.flix.language.phase.llvm.LlvmWasmBindingWriter.bindingsJsPath(options.outputPath, options.artifactName),
          ca.uwaterloo.flix.language.phase.llvm.LlvmWasmBindingWriter.bindingsTypesPath(options.outputPath, options.artifactName),
          ca.uwaterloo.flix.language.phase.llvm.LlvmWasmTypedExportsWriter.typedWitDirPath(options.outputPath, options.artifactName),
          llvmDir.resolve("wasm").resolve("js")
        ).filter(Files.exists(_))

        Some((s"Built ${formatTarget(options.target)} artifacts:" :: paths.map(p => s"  $p")).mkString(System.lineSeparator()))
    }
  }

}
