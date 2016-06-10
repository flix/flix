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

import java.nio.file.{Files, InvalidPathException, Paths}

import ca.uwaterloo.flix.api._
import ca.uwaterloo.flix.util._

/**
  * The main entry point for the Flix compiler and runtime.
  */
object Main {

  /**
    * The main method.
    */
  def main(argv: Array[String]): Unit = {

    val paths = argv.filter(p => p.endsWith(".flix") || p.endsWith(".flix.zip")).toList
    val args = argv.filterNot(p => p.endsWith(".flix") || p.endsWith(".flix.zip")).toList

    // check that each path is valid.
    for (path <- paths) {
      if (!isValidPath(path)) {
        Console.println(s"Error: '$path' is not a valid path.")
        System.exit(1)
      }
    }

    // parse command line arguments.
    val options = parseArgs(args)

    // configure Flix and add the paths.
    val builder = new Flix()
    builder.setOptions(options)
    for (path <- paths) {
      builder.addPath(Paths.get(path))
    }

    // compute the least model.
    try {
      builder.solve() match {
        case Validation.Success(model, errors) =>
          errors.foreach(e => println(e.message))

          val print = options.print
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
    }

  }

  /**
    * Returns `true` iff the given string `s` is a path to a readable file.
    */
  private def isValidPath(s: String): Boolean = try {
    val path = Paths.get(s)
    Files.exists(path) && Files.isRegularFile(path) && Files.isReadable(path)
  } catch {
    case e: InvalidPathException => false
  }

  // TODO: Improve such that print is not part of the options.

  /**
    * Parses the given list of command line arguments `args`.
    */
  private def parseArgs(args: List[String]): Options = {
    def visit(ls: List[String], opts: Options): Options = ls match {
      case Nil => opts
      case "-d" :: xs => visit(xs, opts.copy(debugger = Debugger.Enabled))
      case "--debugger" :: xs => visit(xs, opts.copy(debugger = Debugger.Enabled))
      case "--parallel" :: xs => visit(xs, opts.copy(parallel = Parallel.Enable))
      case "-v" :: xs => visit(xs, opts.copy(verbosity = Verbosity.Verbose))
      case "--verbose" :: xs => visit(xs, opts.copy(verbosity = Verbosity.Verbose))
      case "-s" :: xs => visit(xs, opts.copy(verbosity = Verbosity.Silent))
      case "--silent" :: xs => visit(xs, opts.copy(verbosity = Verbosity.Silent))
      case "-Xinterpreter" :: xs => visit(xs, opts.copy(codegen = CodeGeneration.Disabled))
      case "-Xdump-bytecode" :: xs => visit(xs, opts.copy(debugBytecode = DebugBytecode.Enabled))
      case "--verify" :: xs => visit(xs, opts.copy(verify = Verify.Enabled))
      case ("-p" | "--print") :: xs =>
        val print = xs.takeWhile(s => !s.startsWith("-"))
        val rest = xs.drop(print.length)
        visit(rest, opts.copy(print = print))
      case arg =>
        Console.println(s"Error: '$arg' is not a valid argument.")
        ???
    }

    visit(args, Options.Default.copy(verbosity = Verbosity.Normal))
  }
}
