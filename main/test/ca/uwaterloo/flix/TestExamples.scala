package ca.uwaterloo.flix

import ca.uwaterloo.flix.util.FlixTest
import org.scalatest.Suites

class TestExamples extends Suites(

  // Webpage Examples
  new FlixTest("algebraic-data-types-and-pattern-matching", "examples/algebraic-data-types-and-pattern-matching.flix"),
  new FlixTest("lists-and-list-processing", "examples/lists-and-list-processing.flix"),
  new FlixTest("higher-order-functions.flix", "examples/higher-order-functions.flix"),
  new FlixTest("enums-and-parametric-polymorphism", "examples/enums-and-parametric-polymorphism.flix"),
  new FlixTest("function-composition-pipelines-and-currying", "examples/function-composition-pipelines-and-currying.flix"),
  new FlixTest("uniform-function-call-syntax", "examples/uniform-function-call-syntax.flix"),
  new FlixTest("mutual-recursion-with-full-tail-call-elimination", "examples/mutual-recursion-with-full-tail-call-elimination.flix"),
  new FlixTest("sending-and-receiving-on-channels", "examples/sending-and-receiving-on-channels.flix"),
  new FlixTest("using-channels-and-select", "examples/using-channels-and-select.flix"),
  new FlixTest("select-with-defaults-and-timers", "examples/select-with-defaults-and-timers.flix"),
  new FlixTest("fixpoint-computations-with-top-level-constraints", "examples/fixpoint-computations-with-top-level-constraints.flix"),
  new FlixTest("first-class-constraints-and-fixpoints", "examples/first-class-constraints-and-fixpoints.flix"),
  new FlixTest("polymorphic-first-class-constraints", "examples/polymorphic-first-class-constraints.flix"),
  new FlixTest("pipelines-of-fixpoint-computations", "examples/pipelines-of-fixpoint-computations.flix"),
  new FlixTest("an-interpreter-for-a-trivial-expression-language", "examples/an-interpreter-for-a-trivial-expression-language.flix"),

  // TODO: Cleanup
  new FlixTest("TestBelnap", "examples/domains/Belnap.flix"),
  new FlixTest("TestConstant", "examples/domains/Constant.flix", "examples/domains/Belnap.flix"),

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
