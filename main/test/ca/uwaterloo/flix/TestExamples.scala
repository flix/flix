package ca.uwaterloo.flix

import ca.uwaterloo.flix.util.FlixTest
import org.scalatest.Suites

class TestExamples extends Suites(

  // TODO: Compiled.
  // TODO: By including belnap those tests are run additional times.

  new FlixTest("TestBelnap", "examples/domains/Belnap.flix")(compiled = false),
  new FlixTest("TestConstant", "examples/domains/Constant.flix", "examples/domains/Belnap.flix")(compiled = false),

  // TODO: Add explicit test cases for the remainder.
  new FlixTest("ConstantParity", "examples/domains/ConstantParity.flix", "examples/domains/Belnap.flix")(compiled = false),
  new FlixTest("ConstantSign", "examples/domains/ConstantSign.flix", "examples/domains/Belnap.flix")(compiled = false),
  new FlixTest("Interval", "examples/domains/Interval.flix", "examples/domains/Belnap.flix")(compiled = false),
  new FlixTest("IntervalAlt", "examples/domains/IntervalAlt.flix", "examples/domains/Belnap.flix")(compiled = false),
  // new FlixTest("IntervalInf", "examples/domains/IntervalInf.flix", "examples/domains/Belnap.flix")(compiled = false), // TODO: Broken
  new FlixTest("Mod3", "examples/domains/Mod3.flix", "examples/domains/Belnap.flix")(compiled = false),
  new FlixTest("Parity", "examples/domains/Parity.flix", "examples/domains/Belnap.flix")(compiled = false),
  new FlixTest("ParitySign", "examples/domains/ParitySign.flix", "examples/domains/Belnap.flix")(compiled = false),
  new FlixTest("PrefixSuffix", "examples/domains/PrefixSuffix.flix", "examples/domains/Belnap.flix")(compiled = false),
  new FlixTest("Sign", "examples/domains/Sign.flix", "examples/domains/Belnap.flix")(compiled = false),
  new FlixTest("StrictSign", "examples/domains/StrictSign.flix", "examples/domains/Belnap.flix")(compiled = false),

  new FlixTest("IFDS", "examples/analysis/IFDS.flix")(compiled = false),
  new FlixTest("IDE", "examples/analysis/IDE.flix")(compiled = false),
  new FlixTest("SUOpt", "examples/analysis/SUopt.flix")(compiled = false),
  new FlixTest("FloydWarshall", "examples/misc/FloydWarshall.flix")(compiled = false)

)
