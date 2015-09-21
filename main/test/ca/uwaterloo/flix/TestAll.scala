package ca.uwaterloo.flix

import ca.uwaterloo.flix.lang.phases._
import org.scalatest.Suites

class TestAll extends Suites(new TestPhases, new TestExamples) {

}
