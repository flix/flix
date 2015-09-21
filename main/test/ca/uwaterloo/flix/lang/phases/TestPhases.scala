package ca.uwaterloo.flix.lang.phases

import org.scalatest.Suites

class TestPhases extends Suites(new TestParser, new TestWeeder, new TestResolver, new TestTyper) {

}
