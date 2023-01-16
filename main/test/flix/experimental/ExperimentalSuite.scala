package flix.experimental

import org.scalatest.Suites

class ExperimentalSuite extends Suites(
  new ChooseSuite,
  new TestChoose
)
