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
    * Does literally nothing.
    */
  case object Nop extends Command

  /**
    * End of input.
    */
  case object Eof extends Command

  /**
    * Evaluates the given expression `exp`.
    */
  case class Eval(exp: String) extends Command

  /**
    * Shows the type of the given expression `exp`.
    */
  case class TypeOf(exp: String) extends Command

  /**
    * Shows the kind of the given expression `exp`.
    */
  case class KindOf(exp: String) extends Command

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
    * Adds the given `path` to the set of source paths.
    */
  case class Load(path: String) extends Command

  /**
    * Removes the given `path` from the set of source paths.
    */
  case class Unload(path: String) extends Command

  /**
    * Reload all source paths.
    */
  case object Reload extends Command

  /**
    * Computes the least fixed point of the program.
    */
  case object Solve extends Command

  /**
    * Shows the rows in the relation `fqn` that matches the optional `needle`.
    */
  case class Rel(fqn: String, needle: Option[String]) extends Command

  /**
    * Shows the rows in the lattice `fqn` that matches the optional `needle`.
    */
  case class Lat(fqn: String, needle: Option[String]) extends Command

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
    * Unknown command.
    */
  case class Unknown(s: String) extends Command

  /**
    * Parses the given `input` into a command.
    */
  def parse(input: String): Command = {
    //
    // Eof
    //
    if (input == null)
      return Command.Eof

    //
    // Nop
    //
    if (input.trim == "")
      return Command.Nop

    //
    // Type
    //
    if (input.startsWith(":type")) {
      val exp = input.substring(":type".length).trim
      return Command.TypeOf(exp)
    }
    if (input.startsWith(":t")) {
      val exp = input.substring(":t".length).trim
      return Command.TypeOf(exp)
    }

    //
    // Kind
    //
    if (input.startsWith(":kind")) {
      val exp = input.substring(":kind".length).trim
      return Command.TypeOf(exp)
    }
    if (input.startsWith(":k")) {
      val exp = input.substring(":k".length).trim
      return Command.KindOf(exp)
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
    if (input.startsWith(":doc")) {
      val fqn = input.substring(":doc".length).trim
      if (fqn.isEmpty) {
        Console.println("Missing argument for command :doc.")
        return Command.Nop
      }
      return Command.Doc(fqn)
    }

    //
    // Search
    //
    if (input.startsWith(":search")) {
      val needle = input.substring(":search".length).trim
      if (needle.isEmpty) {
        Console.println("Missing argument for command :search.")
        return Command.Nop
      }
      return Command.Search(needle)
    }

    //
    // Load
    //
    if (input.startsWith(":load")) {
      val path = input.substring(":load".length).trim
      if (path.isEmpty) {
        Console.println("Missing argument for command :load.")
        return Command.Nop
      }
      return Command.Load(path)
    }

    //
    // Unload
    //
    if (input.startsWith(":unload")) {
      val path = input.substring(":unload".length).trim
      if (path.isEmpty) {
        Console.println("Missing argument for command :unload.")
        return Command.Nop
      }
      return Command.Unload(path)
    }

    //
    // Reload
    //
    if (input == ":r" || input == ":reload")
      return Command.Reload

    //
    // Solve
    //
    if (input == ":solve")
      return Command.Solve

    //
    // Rel
    //
    if (input.startsWith(":rel")) {
      // Check if any arguments were passed.
      val args = input.substring(":rel".length).trim
      if (args.isEmpty) {
        Console.println("Missing argument for command :rel.")
        return Command.Nop
      }

      // Split the arguments into fqn and needle.
      val split = args.split(" ")
      if (args.length == 1)
        return Command.Rel(split(0), None)
      else
        return Command.Rel(split(0), Some(split(1)))
    }

    //
    // Lat
    //
    if (input.startsWith(":lat")) {
      // Check if any arguments were passed.
      val args = input.substring(":lat".length).trim
      if (args.isEmpty) {
        Console.println("Missing argument for command :lat.")
        return Command.Nop
      }

      // Split the arguments into fqn and needle.
      val split = args.split(" ")
      if (args.length == 1)
        return Command.Lat(split(0), None)
      else
        return Command.Lat(split(0), Some(split(1)))
    }

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
    // Unknown
    //
    if (input.startsWith(":"))
      return Command.Unknown(input)

    //
    // Eval
    //
    Command.Eval(input)
  }

}