package ca.uwaterloo.flix

import ca.uwaterloo.flix.lang.phases.{TestTyper, TestWeeder, TestParser}
import org.scalatest.Suites

class TestAll extends Suites(new TestParser, new TestWeeder, new TestTyper) {

}
