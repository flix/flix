package ca.uwaterloo.flix

import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.backend.phase.TestCodegen
import ca.uwaterloo.flix.language.library.TestLibrary
import ca.uwaterloo.flix.language.phase._
import ca.uwaterloo.flix.runtime.{TestSolver, TestValue, TestInterpreter}
import ca.uwaterloo.flix.util.TestValidation

import org.scalatest.{ParallelTestExecution, Suites}

// TODO: re-organize

// NB: Run with -P to run in parallel.
class TestAll extends Suites(
  new TestTypedAst,
  new TestCodegen,
  new TestParser,
  new TestResolver,
  new TestTyper,
  new TestWeeder,
  new TestInterpreter,
  new TestSolver,
  new TestValue,
  new TestValidation,
  new TestLibrary,
  new TestExamples) with ParallelTestExecution {

}
