package ca.uwaterloo.flix

import ca.uwaterloo.flix.lang.phases._
import org.scalatest.{ParallelTestExecution, Suites}

// NB: Run with -P to run in parallel.
class TestAll extends Suites(new TestPhases, new TestExamples) with ParallelTestExecution {

}
