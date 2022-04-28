package ca.uwaterloo.flix.tools

import org.scalatest.Suites

class ToolsSuite extends Suites(
  new TestPackager,
  new TestPathComparator
) {
  /* left empty */
}
