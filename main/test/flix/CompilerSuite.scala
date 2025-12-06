package flix

import ca.uwaterloo.flix.util.{FlixSuite, Options}

class CompilerSuite extends FlixSuite(incremental = true) {
  implicit val options: Options = Options.TestWithLibAll

  mkTestDir("main/test/flix/", prelude = Some("main/test/flix/Prelude.flix"))

}
