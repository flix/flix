/*
 * Copyright 2017 Magnus Madsen
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

package ca.uwaterloo.flix.runtime.shell

import ca.uwaterloo.flix.api.{Flix, Version}
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.{Ast, Symbol, TypedAst}
import ca.uwaterloo.flix.language.fmt.Audience
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.util.Formatter.AnsiTerminalFormatter
import ca.uwaterloo.flix.util._

import org.jline.reader.{EndOfFileException, LineReaderBuilder, UserInterruptException}
import org.jline.terminal.{Terminal, TerminalBuilder}

import java.nio.file._
import java.io.File
import java.util.concurrent.Executors
import java.util.logging.{Level, Logger}
import scala.collection.mutable
import scala.jdk.CollectionConverters._

class Shell(source: Either[Path, Seq[File]], options: Options) {

  /**
    * The audience is always external.
    */
  private implicit val audience: Audience = Audience.External

  /**
    * The mutable list of source code fragments.
    */
  private val fragments = mutable.Stack.empty[String]

  /**
    * The Flix instance (the same instance is used for incremental compilation).
    */
  private val flix: Flix = new Flix().setFormatter(AnsiTerminalFormatter)

  private val sourceFiles = new SourceFiles(source)

  /**
    * Continuously reads a line of input from the terminal, parses and executes it.
    */
  def loop(): Unit = {
    // Silence JLine warnings about terminal type.
    Logger.getLogger("org.jline").setLevel(Level.OFF)

    // Initialize the terminal.
    implicit val terminal: Terminal = TerminalBuilder
      .builder()
      .system(true)
      .build()

    // Initialize the terminal line reader.
    val reader = LineReaderBuilder
      .builder()
      .appName("flix")
      .terminal(terminal)
      .build()

    // Print the welcome banner.
    printWelcomeBanner()

    // Trigger a compilation of the source input files.
    execReload()

    try {
      // Repeatedly try to read an input from the line reader.
      while (!Thread.currentThread().isInterrupted) {
        // Try to read a command.
        val line = reader.readLine(prompt)

        // Parse the command.
        val cmd = Command.parse(line)
        try {
          // Try to execute the command. Catch any exception.
          execute(cmd)
        } catch {
          case e: Exception =>
            terminal.writer().print(e.getMessage)
            e.printStackTrace(terminal.writer())
        }
      }
    } catch {
      case _: UserInterruptException => // nop, exit gracefully.
      case _: EndOfFileException => // nop, exit gracefully.
    }

    // Print goodbye message.
    terminal.writer().println("Thanks, and goodbye.")
  }

  /**
    * Prints the welcome banner to the terminal.
    */
  private def printWelcomeBanner()(implicit terminal: Terminal): Unit = {
    val banner =
      """     __   _   _
        |    / _| | | (_)            Welcome to Flix __VERSION__
        |   | |_  | |  _  __  __
        |   |  _| | | | | \ \/ /     Enter an expression to have it evaluated.
        |   | |   | | | |  >  <      Type ':help' for more information.
        |   |_|   |_| |_| /_/\_\     Type ':quit' or press 'ctrl + d' to exit.
      """.stripMargin

    terminal.writer().println(banner.replaceAll("__VERSION__", Version.CurrentVersion.toString))
    terminal.flush()
  }

  /**
    * Returns the Flix prompt.
    */
  private def prompt: String = "flix> "

  /**
    * Executes the given command `cmd`.
    */
  private def execute(cmd: Command)(implicit terminal: Terminal): Unit = cmd match {
    case Command.Nop => // nop
    case Command.Reload => execReload()
    case Command.Quit => execQuit()
    case Command.Help => execHelp()
    case Command.Praise => execPraise()
    case Command.Eval(s) => execEval(s)
    case Command.Unknown(s) => execUnknown(s)
  }

  /**
    * Reloads every source path.
    */
  private def execReload()(implicit terminal: Terminal): Validation[CompilationResult, CompilationMessage] = {
    sourceFiles.addSourcesAndPackages(flix)
    fragments.clear()

    compile()
  }

  /**
    * Exits the shell.
    */
  private def execQuit()(implicit terminal: Terminal): Unit = {
    Thread.currentThread().interrupt()
  }

  /**
    * Executes the help command.
    */
  private def execHelp()(implicit terminal: Terminal): Unit = {
    val w = terminal.writer()

    w.println("  Command       Purpose")
    w.println()
    w.println("  :reload :r    Recompiles every source file.")
    w.println("  :quit :q      Terminates the Flix shell.")
    w.println("  :help :h :?   Shows this helpful information.")
    w.println()
  }

  /**
    * Executes the praise command.
    */
  private def execPraise()(implicit terminal: Terminal): Unit = {
    val w = terminal.writer()
    w.print(Toucan.leToucan())
  }

  /**
    * Evaluates the given source code.
    */
  private def execEval(s: String)(implicit terminal: Terminal): Unit = {
    val w = terminal.writer()

    //
    // Try to determine the category of the source line.
    //
    Category.categoryOf(s) match {
      case Category.Decl =>
        // The input is a declaration. Push it on the stack of fragments.
        fragments.push(s)

        // The name of the fragment is $n where n is the stack offset.
        val name = "$" + fragments.length

        // Add the source code fragment to Flix.
        flix.addSourceCode(name, s)

        // And try to compile!
        compile() match {
          case Validation.Success(_) =>
            // Compilation succeeded.
            w.println("Ok.")
          case Validation.Failure(_) =>
            // Compilation failed. Ignore the last fragment.
            fragments.pop()
            flix.remSourceCode(name)
            w.println("Error: Declaration ignored due to previous error(s).")
        }

      case Category.Expr =>
        // The input is an expression. Wrap it in main and run it.

        // The name of the generated main function.
        val main = Symbol.mkDefnSym("shell1")

        val src =
          s"""def ${main.name}(): Unit & Impure =
             |println($s)
             |""".stripMargin
        flix.addSourceCode("<shell>", src)
        run(main)
        // Remove immediately so it doesn't confuse subsequent compilations (e.g. reloads or declarations)
        flix.remSourceCode("<shell>")

      case Category.Unknown =>
        // The input is not recognized. Output an error message.
        w.println("Error: Input input cannot be parsed.")
    }
  }

  /**
    * Reports unknown command.
    */
  private def execUnknown(s: String)(implicit terminal: Terminal): Unit = {
    terminal.writer().println(s"Unknown command '$s'. Try `:help'.")
  }

  /**
    * Compile
    */
  private def compile(entryPoint: Option[Symbol.DefnSym] = None)(implicit terminal: Terminal): Validation[CompilationResult, CompilationMessage] = {

    this.flix.setOptions(options.copy(entryPoint = entryPoint))

    val result = this.flix.compile()
    result match {
      case Validation.Success(result) =>

      case Validation.Failure(errors) =>
        for (error <- errors) {
          terminal.writer().print(error)
        }
    }

    // Return the result.
    result
  }

  /**
    * Run the given main function
    */
  private def run(main: Symbol.DefnSym)(implicit terminal: Terminal): Unit = {
    // Recompile the program.
    compile(Some(main)) match {
      case Validation.Success(result) =>
        result.getMain match {
          case Some(m) => 
            // Evaluate the main function
            m(Array.empty)
          
          case None =>
        }

      case Validation.Failure(_) =>
    }
  }
}
