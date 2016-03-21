package ca.uwaterloo.flix.language.library

import ca.uwaterloo.flix.api.Flix

import org.scalatest.FunSuite

class TestLibrary extends FunSuite {

  // TODO: Ensure that this includes the entire library.

  test("Library.Prelude") {
    intercept[scala.NotImplementedError] {
      new Flix().addPath("library/Prelude.flix")
        .compile()
        .get
    }
  }


  ignore("Library.Bounded") {
    new Flix()
      .addPath("library/Bounded.flix")
      .addPath("library/Float32.flix")
      .addPath("library/Float64.flix")
      .addPath("library/Int8.flix")
      .addPath("library/Int16.flix")
      .addPath("library/Int32.flix")
      .addPath("library/Int64.flix")
      .compile()
      .get
  }

  test("Library.Char") {
    intercept[scala.NotImplementedError] {
      new Flix().addPath("library/Char.flix")
        .compile()
        .get
    }
  }

  ignore("Library.Float32") {
    new Flix().addPath("library/Float32.flix")
      .compile()
      .get
  }

  ignore("Library.Float64") {
    new Flix().addPath("library/Float64.flix")
      .compile()
      .get
  }

  ignore("Library.Int8") {
    new Flix().addPath("library/Int8.flix")
      .compile()
      .get
  }

  ignore("Library.Int16") {
    new Flix().addPath("library/Int16.flix")
      .compile()
      .get
  }

  test("Library.Int32") {
    new Flix().addPath("library/Int32.flix")
      .compile()
      .get
  }

  ignore("Library.Int64") {
    new Flix().addPath("library/Int64.flix")
      .compile()
      .get
  }

  test("Library.Opt") {
    intercept[scala.NotImplementedError] {
      new Flix().addPath("library/Opt.flix")
        .compile()
        .get
    }
  }

}
