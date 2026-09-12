/*
 * Copyright 2025 Jakob Schneider Villumsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.tools.pkg

import ca.uwaterloo.flix.api.{Bootstrap, Flix, InstalledPackage, Version}
import ca.uwaterloo.flix.language.ast.shared.{Mountpoint, PackageId, SecurityContext}
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
    * The path a manifest parsed from a string is reported as coming from.
    *
    * A test holds a manifest as a literal rather than as a file, but an error still has to say
    * where the manifest came from, so it names the file a manifest normally lives in.
    */
  val ManifestPath: Path = Path.of("flix.toml")

  /**
    * The lock file of a project that has never been built, which records nothing and so checks
    * nothing. Used by the tests that are not about the lock file.
    */
  val NoLock: Lockfile = Lockfile(Map.empty)

  /**
    * Returns a new [[Flix]] object that has the GitHub token of the CI runner set if available.
    */
  def mkFlix: Flix = mkFlix(Nil)

  /**
    * Returns a new [[Flix]] object with the given packages that has the GitHub token of the CI runner set if available.
    */
  def mkFlix(pkgs: List[InstalledPackage]): Flix = mkFlix(pkgs, Map.empty)

  /**
    * Returns a new [[Flix]] object with the given packages and root mount table that has the
    * GitHub token of the CI runner set if available.
    */
  def mkFlix(pkgs: List[InstalledPackage], mounts: Map[Mountpoint, PackageId]): Flix = {
    val flix = new Flix(pkgs = pkgs, mounts = mounts)
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
       |version = "0.1.0"
       |flix = "${Version.CurrentVersion}"
       |
       |[dependencies]
       |$deps
       |""".stripMargin
  }

}
