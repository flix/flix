package flix

import ca.uwaterloo.flix.util.{FlixSuite, Options}

class CompilerSuite extends FlixSuite(incremental = true) {
  implicit val options: Options = Options.TestWithLibAll

  mkTest("main/test/flix/Test.Exp.ArrayLit.flix")
  mkTest("main/test/flix/Test.Exp.ArrayLoad.flix")

}
