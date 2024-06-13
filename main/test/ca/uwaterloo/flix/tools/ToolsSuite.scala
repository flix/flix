package ca.uwaterloo.flix.tools

import org.scalatest.Suites

class ToolsSuite extends Suites(
  new TestBootstrap,
  new TestPermission,
  new TestManifestParser
) {
  /* left empty */
}
