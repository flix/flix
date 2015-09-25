package ca.uwaterloo.flix

import ca.uwaterloo.flix.lang.ast._
import ca.uwaterloo.flix.lang.phase._
import org.scalatest.{ParallelTestExecution, Suites}

// NB: Run with -P to run in parallel.
class TestAll extends Suites(

  new TestTypedAst,
  new TestParser,
  new TestResolver,
  new TestTyper,
  new TestWeeder,
  new TestExamples) with ParallelTestExecution {

}
