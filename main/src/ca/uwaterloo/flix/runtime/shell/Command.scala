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

/**
  * A common super-type for commands.
  */
sealed trait Command

object Command {

  /**
    * End of input.
    */
  case object Eof extends Command

  /**
    * Prints helpful information about the available commands.
    */
  case object Help extends Command

  /**
    * Does literally nothing.
    */
  case object Nop extends Command

  /**
    * Lists all relations.
    */
  case object ListRel extends Command

  /**
    * Lists all lattices.
    */
  case object ListLat extends Command

  /**
    * Reload all source paths.
    */
  case object Reload extends Command

  /**
    * Terminates the shell.
    */
  case object Quit extends Command

  /**
    * Computes the least fixed point.
    */
  case object Solve extends Command

  /**
    * Watches source paths for changes.
    */
  case object Watch extends Command

  /**
    * Unwatches source paths for changes.
    */
  case object Unwatch extends Command

  /**
    * Browse the definitions in the optional namespace `ns`.
    */
  case class Browse(ns: Option[String]) extends Command

  /**
    * Evaluate the expression `s`.
    */
  case class Eval(s: String) extends Command

  /**
    * Add the path `p` to the set of source paths.
    */
  case class Load(p: String) extends Command

  /**
    * Remove the path `p` from the set of source paths.
    */
  case class Unload(s: String) extends Command

  /**
    * Searches for a symbol with name `needle`.
    */
  case class Search(needle: String) extends Command

  /**
    * Shows the rows in the relation `fqn` that matches the given `needle`.
    */
  case class ShowRel(fqn: String, needle: Option[String]) extends Command

  /**
    * Shows the rows in the lattice `fqn` that matches the given `needle`.
    */
  case class ShowLat(fqn: String, needle: Option[String]) extends Command

  /**
    * Unknown command.
    */
  case class Unknown(s: String) extends Command

  /**
    * Parses the given string `line` into a command.
    */
  def parse(line: String): Command = {
    //
    // Eof
    //
    if (line == null)
      return Command.Eof

    //
    // Nop
    //
    if (line.trim == "")
      return Command.Nop

    //
    // Help
    //
    if (line == ":help" || line == ":h" || line == ":?")
      return Command.Help

    //
    // ListRel
    //
    if (line == ":rel")
      return Command.ListRel

    //
    // ListLat
    //
    if (line == ":lat")
      return Command.ListLat

    //
    // Reload
    //
    if (line == ":r" || line == ":reload")
      return Command.Reload

    //
    // Quit
    //
    if (line == ":quit" || line == ":q")
      return Command.Quit

    //
    // Solve
    //
    if (line == ":solve")
      return Command.Solve

    //
    // Watch
    //
    if (line == ":watch" || line == ":w")
      return Command.Watch

    //
    // Unwatch
    //
    if (line == ":unwatch")
      return Command.Unwatch

    //
    // Browse
    //
    if (line.startsWith(":browse")) {
      if (line == ":browse") {
        return Command.Browse(None)
      }
      val ns = line.substring(":browse".length).trim
      return Command.Browse(Some(ns))
    }

    //
    // Load
    //
    if (line.startsWith(":load")) {
      val path = line.substring(":load".length).trim
      if (path.isEmpty) {
        Console.println("Missing argument for command :load.")
        return Command.Nop
      }
      return Command.Load(path)
    }

    //
    // Unload
    //
    if (line.startsWith(":unload")) {
      val path = line.substring(":unload".length).trim
      if (path.isEmpty) {
        Console.println("Missing argument for command :unload.")
        return Command.Nop
      }
      return Command.Unload(path)
    }

    //
    // Search
    //
    if (line.startsWith(":search")) {
      val needle = line.substring(":search".length).trim
      if (needle.isEmpty) {
        Console.println("Missing argument for command :search.")
        return Command.Nop
      }
      return Command.Search(needle)
    }

    //
    // ShowRel
    //
    if (line.startsWith(":rel")) {
      // Check if any arguments were passed.
      val args = line.substring(":rel".length).trim

      // Split the arguments into fqn and needle.
      val split = args.split(" ")
      if (args.length == 1)
        return Command.ShowRel(split(0), None)
      else
        return Command.ShowRel(split(0), Some(split(1)))
    }

    //
    // ShowLat
    //
    if (line.startsWith(":lat")) {
      // Check if any arguments were passed.
      val args = line.substring(":lat".length).trim

      // Split the arguments into fqn and needle.
      val split = args.split(" ")
      if (args.length == 1)
        return Command.ShowLat(split(0), None)
      else
        return Command.ShowLat(split(0), Some(split(1)))
    }

    //
    // Unknown
    //
    if (line.startsWith(":"))
      return Command.Unknown(line)

    //
    // Eval
    //
    Command.Eval(line)
  }

}