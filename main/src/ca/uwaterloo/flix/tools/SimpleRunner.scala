/*
 * Copyright 2022 Matthew Lutze
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

import ca.uwaterloo.flix.Main.{CmdOpts, Command}
import ca.uwaterloo.flix.api.{Bootstrap, Flix}
import ca.uwaterloo.flix.runtime.shell.Shell
import ca.uwaterloo.flix.util._
import ca.uwaterloo.flix.util.collection.Chain

import java.nio.file.Path

/**
  * Manages the functionality of the compiler in non-packager mode.
  */
object SimpleRunner {

  def run(cwd: Path, cmdOpts: CmdOpts, options: Options): Result[Unit, Int] = {

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
    if (cmdOpts.command == Command.None && cmdOpts.files.isEmpty) {
      Bootstrap.bootstrap(cwd, options.githubToken)(System.out).toResult match {
        case Result.Ok((bootstrap, Nil)) =>
          val shell = new Shell(bootstrap, options)
          shell.loop()
          System.exit(0)
        case Result.Ok((_, failures)) =>
          failures.map(_.message(Formatter.getDefault)).foreach(println)
          System.exit(1)
        case Result.Err(failures) =>
          failures.map(_.message(Formatter.getDefault)).foreach(println)
          System.exit(1)
      }
    }

    // configure Flix and add the paths.
    val flix = new Flix()
    flix.setOptions(options)
    for (file <- cmdOpts.files) {
      val ext = file.getName.split('.').last
      ext match {
        case "flix" => flix.addFlix(file.toPath)
        case "fpkg" => flix.addPkg(file.toPath)
        case "jar" => flix.addJar(file.toPath)
        case _ =>
          Console.println(s"Unrecognized file extension: '$ext'.")
          System.exit(1)
      }
    }

    flix.setFormatter(Formatter.getDefault)

    // evaluate main.
    val timer = new Timer(flix.compile())
    timer.getResult.toResult match {
      case Result.Ok((compilationResult, Nil)) =>

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
        Result.Ok(())

      case Result.Ok((_, failures)) =>
        flix.mkMessages(Chain.from(failures.toSeq.sortBy(_.source.name)))
          .foreach(println)
        println()
        println(s"Compilation failed with ${failures.length} error(s).")
        Result.Err(1)
      case Result.Err(failures) =>
        flix.mkMessages(failures.toSeq.sortBy(_.source.name))
          .foreach(println)
        println()
        println(s"Compilation failed with ${failures.length} error(s).")
        Result.Err(1)
    }
  }
}
