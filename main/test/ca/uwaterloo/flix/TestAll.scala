package ca.uwaterloo.flix

import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.phase._
import ca.uwaterloo.flix.runtime.{TestSolver, TestValue, TestInterpreter}

import org.scalatest.{ParallelTestExecution, Suites}

// NB: Run with -P to run in parallel.
class TestAll extends Suites(
  new TestTypedAst,
  new TestParser,
  new TestResolver,
  new TestTyper,
  new TestWeeder,
  new TestInterpreter,
  new TestValue,
  new TestSolver,
  new TestExamples,
  new TestMicro) with ParallelTestExecution {

}
