package ca.uwaterloo.flix.tools.pkg

import ca.uwaterloo.flix.api.Flix

/**
  * Contains a test utilities for the package manager tests that rely heavily on I/O
  */
object PkgTestUtils {

  /**
    * Returns the GitHub token of the CI runner if available.
    */
  def getGitHubToken: Option[String] = {
    val propValue = System.getProperty("GITHUB_CI_RUNNER_TOKEN")
    if (propValue.isBlank || propValue.isEmpty)
      None
    else
      Some(propValue)
  }

  /**
    * Returns a new [[Flix]] object that has the GitHub token of the CI runner set if available.
    */
  def mkFlixInstance: Flix = {
    val flix = new Flix()
    flix.setOptions(flix.options.copy(githubToken = getGitHubToken))
  }

  /**
    * Throttles `action` by waiting some amount of time before computing `action`
    * and waiting some other amount of time before returning the result.
    */
  def throttle[A](action: => A): A = {
    Thread.sleep(3000)
    val result = action
    Thread.sleep(1500)
    result
  }

}
