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

import org.jline.terminal.Terminal

/**
  * A common super-type for commands.
  */
sealed trait Command

object Command {

  /**
    * Does literally nothing.
    */
  case object Nop extends Command

  /**
    * Executes the main function.
    */
  case object Run extends Command

  /**
    * Shows the context for the given hole `fqn`.
    */
  case class Hole(fqnOpt: Option[String]) extends Command

  /**
    * Shows the definitions, relations, and lattices in the given namespace.
    */
  case class Browse(ns: Option[String]) extends Command

  /**
    * Show the documentation for the given fully-qualified name.
    */
  case class Doc(fqn: String) extends Command

  /**
    * Searches for a definition symbol which contains `needle` as part of its name.
    */
  case class Search(needle: String) extends Command

  /**
    * Reloads all source paths.
    */
  case object Reload extends Command

  /**
    * Runs all benchmarks in the program.
    */
  case object Benchmark extends Command

  /**
    * Runs all unit tests in the program.
    */
  case object Test extends Command

  /**
    * Warms up the compiler.
    */
  case object Warmup extends Command

  /**
    * Watches source paths for changes.
    */
  case object Watch extends Command

  /**
    * Unwatches source paths for changes.
    */
  case object Unwatch extends Command

  /**
    * Terminates the shell.
    */
  case object Quit extends Command

  /**
    * Prints helpful information about the available commands.
    */
  case object Help extends Command

  /**
    * Praise Le Toucan.
    */
  case object Praise extends Command

  /**
    * Unknown command.
    */
  case class Unknown(s: String) extends Command

  /**
    * Parses the given `input` into a command.
    */
  def parse(input: String)(implicit terminal: Terminal): Command = {
    //
    // Eof
    //
    if (input == null)
      return Command.Quit

    //
    // Nop
    //
    if (input.trim == "")
      return Command.Nop

    //
    // Run
    //
    if (input.startsWith(":run"))
      return Command.Run

    //
    // Hole
    //
    if (input.startsWith(":hole")) {
      val fqn = input.substring(":hole".length).trim
      if (fqn.isEmpty)
        return Command.Hole(None)
      else
        return Command.Hole(Some(fqn))
    }

    //
    // Browse
    //
    if (input.startsWith(":browse")) {
      if (input.trim == ":browse") {
        return Command.Browse(None)
      }
      val ns = input.substring(":browse".length).trim
      return Command.Browse(Some(ns))
    }

    //
    // Doc
    //
    if (input.startsWith(":doc ")) {
      val fqn = input.substring(":doc ".length).trim
      if (fqn.isEmpty) {
        terminal.writer().println("Missing argument for command :doc.")
        return Command.Nop
      }
      return Command.Doc(fqn)
    }

    //
    // Search
    //
    if (input.startsWith(":search ")) {
      val needle = input.substring(":search ".length).trim
      if (needle.isEmpty) {
        terminal.writer().println("Missing argument for command :search.")
        return Command.Nop
      }
      return Command.Search(needle)
    }

    //
    // Reload
    //
    if (input == ":r" || input == ":reload")
      return Command.Reload

    //
    // Benchmark
    //
    if (input == ":benchmark")
      return Command.Benchmark

    //
    // Test
    //
    if (input == ":test")
      return Command.Test

    //
    // Warmup
    //
    if (input == ":warmup")
      return Command.Warmup

    //
    // Watch
    //
    if (input == ":watch" || input == ":w")
      return Command.Watch

    //
    // Unwatch
    //
    if (input == ":unwatch")
      return Command.Unwatch

    //
    // Quit
    //
    if (input == ":quit" || input == ":q")
      return Command.Quit

    //
    // Help
    //
    if (input == ":help" || input == ":h" || input == ":?")
      return Command.Help

    //
    // Praise
    //
    if (input == ":praise")
      return Command.Praise

    Command.Unknown(input)
  }

}