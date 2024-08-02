package flix

import ca.uwaterloo.flix.util.{FlixSuite, Options}

class CompilerSuite extends FlixSuite(incremental = true) {
  implicit val options: Options = Options.TestWithLibAll

  mkTestDir("/Users/dghosef/flix/main/test/flix/Test.Exp.Struct.Get.flix")

}
