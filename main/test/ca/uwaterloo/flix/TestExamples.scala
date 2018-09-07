package ca.uwaterloo.flix

import ca.uwaterloo.flix.util.FlixTest
import org.scalatest.Suites

class TestExamples extends Suites(

  // TODO: Compiled.
  // TODO: By including belnap those tests are run additional times.

  new FlixTest("TestBelnap", "examples/domains/Belnap.flix")(compiled = false),
  new FlixTest("TestConstant", "examples/domains/Constant.flix", "examples/domains/Belnap.flix")(compiled = false)



)
