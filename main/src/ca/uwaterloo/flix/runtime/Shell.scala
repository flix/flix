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

package ca.uwaterloo.flix.runtime

import java.io.File

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.util.vt.TerminalContext
import ca.uwaterloo.flix.util._

class Shell(files: List[File], main: Option[String], options: Options) {

  /**
    * The default color context.
    */
  implicit val _ = TerminalContext.AnsiTerminal

  /**
    * The current model (if any).
    */
  var model: Model = _

  /**
    * A common super-type for input commands.
    */
  sealed trait Input

  object Input {

    /**
      * Does literally nothing.
      */
    case object Nop extends Input

    /**
      * Runs the current program.
      */
    case object Run extends Input

    /**
      * Prints information about the available commands.
      */
    case object Help extends Input

    /**
      * Prints the given constant, relation or lattice.
      */
    case class Print(name: String) extends Input

    /**
      * Gracefully terminates Flix.
      */
    case object Exit extends Input

    /**
      * Gracefully terminates Flix.
      */
    case object Quit extends Input

    /**
      * A command that was not unknown. Possibly a typo.
      */
    case class Unknown(line: String) extends Input

  }

  /**
    * Prints the welcome banner and starts the interactive shell.
    */
  def run(): Unit = {
    printWelcomeBanner()
    loop()
  }

  /**
    * Continuously reads a line of input from the input stream, parses and executes it.
    */
  def loop(): Unit = {
    while (!Thread.currentThread().isInterrupted) {
      Console.print(prompt)
      Console.flush()
      val line = scala.io.StdIn.readLine()
      val cmd = parse(line)
      try {
        execute(cmd)
      } catch {
        case e: Exception =>
          Console.println(e.getMessage)
          e.printStackTrace()
      }
    }
  }

  /**
    * Parses the string `line` into a command.
    */
  private def parse(line: String): Input = line match {
    case null => Input.Exit
    case "" => Input.Nop
    case "run" => Input.Run
    case "help" => Input.Help
    case "exit" => Input.Exit
    case "quit" => Input.Quit
    case s if s.startsWith("print") => Input.Print(s.substring("print ".length))
    case _ => Input.Unknown(line)
  }

  /**
    * Executes the given command `cmd`
    */
  private def execute(cmd: Input): Unit = cmd match {
    case Input.Nop => // nop

    case Input.Run =>
      // configure Flix and add the paths.
      val flix = new Flix()
      flix.setOptions(options)
      for (file <- files) {
        flix.addPath(file.toPath)
      }

      // check if a main function was given.
      if (main.nonEmpty) {
        val name = main.get
        flix.addReachableRoot(name)
      }

      // compute the least model.
      val timer = new Timer(flix.solve())
      timer.getResult match {
        case Validation.Success(m, errors) =>
          this.model = m
          if (main.nonEmpty) {
            val name = main.get
            val evalTimer = new Timer(m.getConstant(name))
            Console.println(s"$name returned `${Value.pretty(evalTimer.getResult)}' (compile: ${timer.fmt}, execute: ${evalTimer.fmt})")
          }
        case Validation.Failure(errors) =>
          errors.foreach(e => println(e.message.fmt))
      }

    case Input.Exit =>
      Thread.currentThread().interrupt()

    case Input.Quit =>
      Thread.currentThread().interrupt()

    case Input.Print(name) =>
      if (model == null)
        Console.println("Model not yet computed.")
      else
        PrettyPrint.print(name, model)

    case Input.Help =>
      Console.println("Available commands:")
      Console.println("  run        -- compile and run.")
      Console.println("  print      -- print a relation/lattice.")
      Console.println("  exit       -- graceful shutdown.")

    case Input.Unknown(s) => Console.println(s"Unknown command '$s'. Try `help'.")
  }

  /**
    * Prints the welcome banner to the console.
    */
  private def printWelcomeBanner(): Unit = {
    Console.println(s"Welcome to Flix ${Version.CurrentVersion}!  Type 'help' for more information.")
    Console.println(s"Enter a command and hit return. Type 'exit' or press ctrl+d to quit.")
  }

  /**
    * Prints the prompt.
    */
  private def prompt: String = "flix> "
}

