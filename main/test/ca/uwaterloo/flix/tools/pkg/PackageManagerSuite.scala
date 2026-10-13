package ca.uwaterloo.flix.tools.pkg

import ca.uwaterloo.flix.api.effectlock.TestEffectLock
import org.scalatest.DoNotDiscover
import org.scalatest.Suites

@DoNotDiscover
class PackageManagerSuite extends Suites(
  new TestBootstrap,
  new TestEffectLock,
  new TestManifestParser,
  new TestLockfileParser,
  new TestPackageSpec,
  new TestFlixPackageManager,
  new TestGitHub,
  new TestJarPackageManager
)
