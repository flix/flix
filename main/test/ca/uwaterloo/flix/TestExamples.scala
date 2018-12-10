package ca.uwaterloo.flix

import ca.uwaterloo.flix.util.FlixTest
import org.scalatest.Suites

class TestExamples extends Suites(

  // TODO: Cleanup

  new FlixTest("TestBelnap", "examples/domains/Belnap.flix"),
  new FlixTest("TestConstant", "examples/domains/Constant.flix", "examples/domains/Belnap.flix"),

  // TODO: Add explicit test cases for the remainder.
  new FlixTest("ConstantParity", "examples/domains/ConstantParity.flix", "examples/domains/Belnap.flix"),
  new FlixTest("ConstantSign", "examples/domains/ConstantSign.flix", "examples/domains/Belnap.flix"),
  new FlixTest("Interval", "examples/domains/Interval.flix", "examples/domains/Belnap.flix"),
  new FlixTest("IntervalAlt", "examples/domains/IntervalAlt.flix", "examples/domains/Belnap.flix"),
  // new FlixTest("IntervalInf", "examples/domains/IntervalInf.flix", "examples/domains/Belnap.flix")(compiled = false), // TODO: Broken
  new FlixTest("Mod3", "examples/domains/Mod3.flix", "examples/domains/Belnap.flix"),
  new FlixTest("Parity", "examples/domains/Parity.flix", "examples/domains/Belnap.flix"),
  new FlixTest("ParitySign", "examples/domains/ParitySign.flix", "examples/domains/Belnap.flix"),
  new FlixTest("PrefixSuffix", "examples/domains/PrefixSuffix.flix", "examples/domains/Belnap.flix"),
  new FlixTest("Sign", "examples/domains/Sign.flix", "examples/domains/Belnap.flix"),
  new FlixTest("StrictSign", "examples/domains/StrictSign.flix", "examples/domains/Belnap.flix"),

  new FlixTest("IFDS", "examples/analysis/IFDS.flix"),
  new FlixTest("IDE", "examples/analysis/IDE.flix"),
  new FlixTest("SUOpt", "examples/analysis/SUopt.flix"),
  new FlixTest("FloydWarshall", "examples/misc/FloydWarshall.flix")

)
