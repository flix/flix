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

}