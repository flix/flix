package ca.uwaterloo.flix.tools.pkg

import org.scalatest.Suites

class PackageManagerSuite extends Suites(
  new TestBootstrap,
  new TestTrust,
  new TestManifestParser
)
