package ca.uwaterloo.flix

object RunTest extends App {
  org.scalatest.run(new TestAll())
}
