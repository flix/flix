package ca.uwaterloo.flix.language.library

import ca.uwaterloo.flix.api.Flix

import org.scalatest.FunSuite

class TestLibrary extends FunSuite {

  test("Library.Prelude") {
    new Flix()
      .addPath("examples/library/Prelude.flix")
      .compile().get
  }

}
