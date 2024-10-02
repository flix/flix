package flix

import ca.uwaterloo.flix.util.{FlixSuite, Options}

class CompilerSuite extends FlixSuite(incremental = true) {
  implicit val options: Options = Options.TestWithLibAll

  mkTest("main/test/flix/Test.Exp.Let.Rec.flix")

}
