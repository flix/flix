/*
 * Copyright 2017 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
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
    * Creates a new project in the current directory
    */
  case object Init extends Command

  /**
    * Builds the current project.
    */
  case object Build extends Command

  /**
    * Builds the current project and writes the class files to the build directory.
    */
  case object BuildClasses extends Command

  /**
    * Builds a jar file from the current project.
    */
  case object BuildJar extends Command

  /**
    * Builds a fatjar file from the current project.
    * Status: working on.
    */
  case object BuildFatJar extends Command

  /**
    * Builds an fpkg file from the current project.
    */
  case object BuildPkg extends Command

  /**
    * Publishes a release of the current project to GitHub.
    */
  case object Release extends Command

  /**
    * Checks the current project for errors.
    */
  case object Check extends Command

  /**
    * Generates API document for the current project.
    */
  case object Doc extends Command

  /**
    * Formats the current project source code.
   */
  case object Format extends Command

  /**
    * Runs the tests for the current project.
    */
  case object Test extends Command

  /**
    * Show dependencies which have newer versions available.
    */
  case object Outdated extends Command

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
    * Eval source code.
    */
  case class Eval(s: String) extends Command

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
      return Command.Quit

    if (input.trim == "")
      return Command.Nop

    if (input == ":init")
      return Command.Init

    if (input == ":build" || input == ":b")
      return Command.Build

    if (input == ":build-classes")
      return Command.BuildClasses

    if (input == ":check" || input == ":c")
      return Command.Check

    if (input == ":doc" || input == ":d")
      return Command.Doc

    if (input == ":format")
      return Command.Format

    if (input == ":build-jar")
      return Command.BuildJar

    if (input == ":build-fatjar")
      return Command.BuildFatJar

    if (input == ":build-pkg")
      return Command.BuildPkg

    if (input == ":release")
      return Command.Release

    if (input == ":test" || input == ":t")
      return Command.Test

    if (input == ":outdated")
      return Command.Outdated

    if (input == ":quit" || input == ":q")
      return Command.Quit

    if (input == ":help" || input == ":h" || input == ":?")
      return Command.Help

    if (input == ":praise")
      return Command.Praise

    //
    // Eval prefix?
    //
    if (input.startsWith(":eval "))
      return Command.Eval(input.substring(":eval ".length))

    //
    // Eval or Unknown?
    //
    if (input.startsWith(":"))
      Command.Unknown(input)
    else
      Command.Eval(input)
  }

}
