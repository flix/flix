package flix

import ca.uwaterloo.flix.util.{FlixSuite, Options}

class CompilerSuite extends FlixSuite(incremental = true) {
  implicit val options: Options = Options.TestWithLibAll

  // REVERT ME
  //mkTestDir("main/test/flix/")
  mkTest("main/test/flix/Test.Exp.Foreach.flix")

}
