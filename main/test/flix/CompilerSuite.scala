package flix

import ca.uwaterloo.flix.util.{FlixSuite, Options}

class CompilerSuite extends FlixSuite(incremental = true) {
  implicit val options: Options = Options.TestWithLibAll

  mkTest("/Users/dghosef/flix/main/test/ca/uwaterloo/flix/library/TestBox.flix")

}
