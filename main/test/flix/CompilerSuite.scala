package flix

import ca.uwaterloo.flix.util.{FlixSuite, Options}
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CompilerSuite extends FlixSuite(incremental = true) {
  implicit val options: Options = Options.TestWithLibAll

  mkTestDir("main/test/flix/")

}
