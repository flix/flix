package ca.uwaterloo.flix.tools.pkg

import ca.uwaterloo.flix.tools.pkg.github.GitHub
import org.scalatest.Suites

class PackageManagerSuite extends Suites(
  new TestBootstrap,
  new TestManifestParser,
  new TestFlixPackageManager
) {
  println(s"Total requests: ${GitHub.reqs.get()}\nTotal bytes: ${GitHub.totalData.get() * 16}")
}
