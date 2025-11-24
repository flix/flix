package ca.uwaterloo.flix.tools.pkg

import ca.uwaterloo.flix.api.Flix

/**
  * Contains a test utilities for the package manager tests that rely heavily on I/O
  */
object PkgTestUtils {

  /**
    * GitHub token of the CI runner if available.
    */
  val gitHubToken: Option[String] = {
    val propValue = System.getProperty("GITHUB_CI_RUNNER_TOKEN")
    if (propValue == null || propValue.isBlank || propValue.isEmpty)
      None
    else
      Some(propValue)
  }

  /**
    * Returns a new [[Flix]] object that has the GitHub token of the CI runner set if available.
    */
  def mkFlix: Flix = {
    val flix = new Flix()
    flix.setOptions(flix.options.copy(githubToken = gitHubToken))
  }
}
