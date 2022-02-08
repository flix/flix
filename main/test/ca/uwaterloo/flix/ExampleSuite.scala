package ca.uwaterloo.flix

import ca.uwaterloo.flix.util.{FlixSuite, Options}

class ExampleSuite extends FlixSuite {

  private implicit val TestOptions: Options = Options.TestWithLibAll.copy(xallowredundancies = true)

  override def incremental: Boolean = true

  mkTestDir("examples")
  mkTestDir("examples/analysis")
  mkTestDir("examples/datalog")
  mkTestDir("examples/koans")

}
