package ca.uwaterloo.flix.tools.pkg

import org.scalatest.DoNotDiscover
import org.scalatest.Suites

@DoNotDiscover
class PackageManagerSuite extends Suites(
  new TestBootstrap,
  new TestManifestParser,
  new TestFlixPackageManager,
  new TestJarPackageManager
)
