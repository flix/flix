package ca.uwaterloo.flix

import ca.uwaterloo.flix.util.{FlixTest, Options}
import org.scalatest.Suites

class TestExamples extends Suites(

  // Webpage Examples
  new FlixTest("algebraic-data-types-and-pattern-matching", "examples/algebraic-data-types-and-pattern-matching.flix")(Options.DefaultTest.copy(xallowredundancies = true)),
  new FlixTest("lists-and-list-processing", "examples/lists-and-list-processing.flix")(Options.DefaultTest.copy(xallowredundancies = true)),
  new FlixTest("higher-order-functions.flix", "examples/higher-order-functions.flix")(Options.DefaultTest.copy(xallowredundancies = true)),
  new FlixTest("enums-and-parametric-polymorphism", "examples/enums-and-parametric-polymorphism.flix")(Options.DefaultTest.copy(xallowredundancies = true)),
  new FlixTest("record-construction-and-use", "examples/record-construction-and-use.flix")(Options.DefaultTest.copy(xallowredundancies = true)),
  new FlixTest("polymorphic-record-update", "examples/polymorphic-record-update.flix")(Options.DefaultTest.copy(xallowredundancies = true)),
  new FlixTest("polymorphic-record-extension-and-restriction", "examples/polymorphic-record-extension-and-restriction.flix")(Options.DefaultTest.copy(xallowredundancies = true)),
  new FlixTest("function-composition-pipelines-and-currying", "examples/function-composition-pipelines-and-currying.flix")(Options.DefaultTest.copy(xallowredundancies = true)),
  new FlixTest("uniform-function-call-syntax", "examples/uniform-function-call-syntax.flix")(Options.DefaultTest.copy(xallowredundancies = true)),
  new FlixTest("pure-and-impure-functions", "examples/pure-and-impure-functions.flix")(Options.DefaultTest.copy(xallowredundancies = true)),
  new FlixTest("effect-polymorphic-functions", "examples/effect-polymorphic-functions.flix")(Options.DefaultTest.copy(xallowredundancies = true)),
  new FlixTest("opaque-types", "examples/opaque-types.flix")(Options.DefaultTest.copy(xallowredundancies = true)),
  new FlixTest("type-aliases", "examples/type-aliases.flix")(Options.DefaultTest.copy(xallowredundancies = true)),
  new FlixTest("reading-and-writing-a-text-file", "examples/reading-and-writing-a-text-file.flix")(Options.DefaultTest.copy(xallowredundancies = true)),
  new FlixTest("mutual-recursion-with-full-tail-call-elimination", "examples/mutual-recursion-with-full-tail-call-elimination.flix")(Options.DefaultTest.copy(xallowredundancies = true)),
  new FlixTest("sending-and-receiving-on-channels", "examples/sending-and-receiving-on-channels.flix")(Options.DefaultTest.copy(xallowredundancies = true)),
  new FlixTest("using-channels-and-select", "examples/using-channels-and-select.flix")(Options.DefaultTest.copy(xallowredundancies = true)),
  new FlixTest("select-with-defaults-and-timers", "examples/select-with-defaults-and-timers.flix")(Options.DefaultTest.copy(xallowredundancies = true)),
  new FlixTest("fixpoint-computations-on-relations", "examples/fixpoint-computations-on-relations.flix")(Options.DefaultTest.copy(xallowredundancies = true)),
  new FlixTest("fixpoint-computations-on-lattices", "examples/fixpoint-computations-on-lattices.flix")(Options.DefaultTest.copy(xallowredundancies = true)),
  new FlixTest("fixpoint-computations-with-stratified-negation", "examples/fixpoint-computations-with-stratified-negation.flix")(Options.DefaultTest.copy(xallowredundancies = true)),
  new FlixTest("first-class-constraints-and-fixpoints", "examples/first-class-constraints-and-fixpoints.flix")(Options.DefaultTest.copy(xallowredundancies = true)),
  new FlixTest("polymorphic-first-class-constraints", "examples/polymorphic-first-class-constraints.flix")(Options.DefaultTest.copy(xallowredundancies = true)),
  new FlixTest("pipelines-of-fixpoint-computations", "examples/pipelines-of-fixpoint-computations.flix")(Options.DefaultTest.copy(xallowredundancies = true)),
  new FlixTest("compiler-puzzle", "examples/compiler-puzzle.flix")(Options.DefaultTest.copy(xallowredundancies = true)),
  new FlixTest("an-interpreter-for-a-trivial-expression-language", "examples/an-interpreter-for-a-trivial-expression-language.flix")(Options.DefaultTest.copy(xallowredundancies = true)),
  new FlixTest("using-laziness-for-infinite-streams", "examples/using-laziness-for-infinite-streams.flix")(Options.DefaultTest.copy(xallowredundancies = true)),
  new FlixTest("using-laziness-for-logging", "examples/using-laziness-for-logging.flix")(Options.DefaultTest.copy(xallowredundancies = true)),
  new FlixTest("using-laziness-to-compute-fibonacci", "examples/using-laziness-to-compute-fibonacci.flix")(Options.DefaultTest.copy(xallowredundancies = true)),

  //new FlixTest("the-ast-typing-problem-with-polymorphic-records", "examples/the-ast-typing-problem-with-polymorphic-records.flix")(Options.DefaultTest.copy(xallowredundancies = true)), // TODO

  // Others
  new FlixTest("TestBelnap", "examples/domains/Belnap.flix")(Options.TestWithLibrary),
  new FlixTest("TestConstant", List("examples/domains/Constant.flix", "examples/domains/Belnap.flix"), Options.TestWithLibrary),
  new FlixTest("ConstantParity", List("examples/domains/ConstantParity.flix", "examples/domains/Belnap.flix"), Options.TestWithLibrary),
  new FlixTest("ConstantSign", List("examples/domains/ConstantSign.flix", "examples/domains/Belnap.flix"), Options.TestWithLibrary),
  new FlixTest("Interval", List("examples/domains/Interval.flix", "examples/domains/Belnap.flix"), Options.TestWithLibrary),
  new FlixTest("IntervalAlt", List("examples/domains/IntervalAlt.flix", "examples/domains/Belnap.flix"), Options.TestWithLibrary),
  new FlixTest("Mod3", List("examples/domains/Mod3.flix", "examples/domains/Belnap.flix"), Options.TestWithLibrary),
  new FlixTest("Parity", List("examples/domains/Parity.flix", "examples/domains/Belnap.flix"), Options.TestWithLibrary),
  new FlixTest("ParitySign", List("examples/domains/ParitySign.flix", "examples/domains/Belnap.flix"), Options.TestWithLibrary),
  new FlixTest("PrefixSuffix", List("examples/domains/PrefixSuffix.flix", "examples/domains/Belnap.flix"), Options.TestWithLibrary),
  new FlixTest("Sign", List("examples/domains/Sign.flix", "examples/domains/Belnap.flix"), Options.TestWithLibrary),
  new FlixTest("StrictSign", List("examples/domains/StrictSign.flix", "examples/domains/Belnap.flix"), Options.TestWithLibrary),

  new FlixTest("IFDS", "examples/analysis/IFDS.flix")(Options.TestWithLibrary),
  new FlixTest("IDE", "examples/analysis/IDE.flix")(Options.TestWithLibrary),
  new FlixTest("SUOpt", "examples/analysis/SUopt.flix")(Options.TestWithLibrary),
  new FlixTest("FloydWarshall", "examples/misc/FloydWarshall.flix")(Options.TestWithLibrary)

)
