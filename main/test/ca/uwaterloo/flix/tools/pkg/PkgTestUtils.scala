/*
 * Copyright 2025 Jakob Schneider Villumsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.tools.pkg

import ca.uwaterloo.flix.api.{Bootstrap, Flix, Version}
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.util.{Formatter, Options}

import java.nio.file.Path

/**
  * Contains a test utilities for the package manager tests that rely heavily on I/O
  */
object PkgTestUtils {

  /**
    * GitHub token of the CI runner if available.
    */
  val gitHubToken: Option[String] = {
    val propValue = System.getenv("GITHUB_CI_RUNNER_TOKEN")
    if (propValue == null || propValue.isBlank || propValue.isEmpty)
      None
    else
      Some(propValue)
  }

  /**
    * Returns a new [[Flix]] object that has the GitHub token of the CI runner set if available.
    */
  def mkFlix: Flix = mkFlix(Nil)

  /**
    * Returns a new [[Flix]] object with the given packages that has the GitHub token of the CI runner set if available.
    */
  def mkFlix(pkgs: List[(Path, SecurityContext)]): Flix = {
    val flix = new Flix(pkgs = pkgs)
    flix.setOptions(flix.options.copy(githubToken = gitHubToken, progress = false))
  }

  /**
    * Returns a new [[Flix]] object for the given `bootstrap` that has the GitHub token of the CI runner set if available.
    */
  def mkFlix(bootstrap: Bootstrap): Flix =
    bootstrap.mkFlix(Options.Default.copy(githubToken = gitHubToken, progress = false), Formatter.NoFormatter)

  def mkTomlWithDeps(deps: String): String = {
    s"""
       |[package]
       |name = "test"
       |description = "test"
       |version = "0.1.0"
       |flix = "${Version.CurrentVersion}"
       |authors = ["flix"]
       |
       |[dependencies]
       |$deps
       |""".stripMargin
  }

}
