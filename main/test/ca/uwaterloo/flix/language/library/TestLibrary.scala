package ca.uwaterloo.flix.language.library

import ca.uwaterloo.flix.api.Flix

import org.scalatest.FunSuite

class TestLibrary extends FunSuite {

  // TODO: Ensure that this includes the entire library.

  test("Library.All") {
    intercept[scala.NotImplementedError] {
      new Flix()
        .addPath("library/Applicative.flix")
        .addPath("library/Bounded.flix")
        .addPath("library/BoundedLattice.flix")
        .addPath("library/Char.flix")
        .addPath("library/Eq.flix")
        .addPath("library/Flat.flix")
        .addPath("library/Float32.flix")
        .addPath("library/Float64.flix")
        .addPath("library/Foldable.flix")
        .addPath("library/Functor.flix")
        .addPath("library/Int8.flix")
        .addPath("library/Int16.flix")
        .addPath("library/Int32.flix")
        .addPath("library/Int64.flix")
        .addPath("library/JoinSemiLattice.flix")
        .addPath("library/Lattice.flix")
        .addPath("library/List.flix")
        .addPath("library/Map.flix")
        .addPath("library/MeetSemiLattice.flix")
        .addPath("library/Monad.flix")
        .addPath("library/Monoid.flix")
        .addPath("library/Narrowing.flix")
        .addPath("library/NatDec.flix")
        .addPath("library/NatInc.flix")
        .addPath("library/Norm.flix")
        .addPath("library/Opt.flix")
        .addPath("library/Ord.flix")
        .addPath("library/PartialOrder.flix")
        .addPath("library/Prelude.flix")
        .addPath("library/Traversable.flix")
        .addPath("library/Vector.flix")
        .addPath("library/Widening.flix")
        .compile()
        .get
    }
  }

  test("Library.Applicative") {
    intercept[scala.NotImplementedError] {
      new Flix().addPath("library/Applicative.flix")
        .compile()
        .get
    }
  }

  test("Library.Bounded") {
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

  test("Library.BoundedLattice") {
    intercept[scala.NotImplementedError] {
      new Flix().addPath("library/BoundedLattice.flix")
        .compile()
        .get
    }
  }

  test("Library.Char") {
    intercept[scala.NotImplementedError] {
      new Flix().addPath("library/Char.flix")
        .compile()
        .get
    }
  }

  test("Library.Eq") {
    intercept[scala.NotImplementedError] {
      new Flix().addPath("library/Eq.flix")
        .compile()
        .get
    }
  }

  test("Library.Flat") {
    intercept[scala.NotImplementedError] {
      new Flix().addPath("library/Flat.flix")
        .compile()
        .get
    }
  }

  test("Library.Float32") {
    new Flix().addPath("library/Float32.flix")
      .compile()
      .get
  }

  test("Library.Float64") {
    new Flix().addPath("library/Float64.flix")
      .compile()
      .get
  }

  test("Library.Foldable") {
    intercept[scala.NotImplementedError] {
      new Flix().addPath("library/Foldable.flix")
        .compile()
        .get
    }
  }

  test("Library.Functor") {
    new Flix().addPath("library/Functor.flix")
      .compile()
      .get
  }

  test("Library.Int8") {
    new Flix().addPath("library/Int8.flix")
      .compile()
      .get
  }

  test("Library.Int16") {
    new Flix().addPath("library/Int16.flix")
      .compile()
      .get
  }

  test("Library.Int32") {
    new Flix().addPath("library/Int32.flix")
      .compile()
      .get
  }

  test("Library.Int64") {
    new Flix().addPath("library/Int64.flix")
      .compile()
      .get
  }

  test("Library.JoinSemiLattice") {
    new Flix().addPath("library/JoinSemiLattice.flix")
      .compile()
      .get
  }

  test("Library.Lattice") {
    new Flix().addPath("library/Lattice.flix")
      .compile()
      .get
  }

  test("Library.Lift") {
    new Flix().addPath("library/Lift.flix")
      .compile()
      .get
  }

  test("Library.List") {
    intercept[scala.NotImplementedError] {
      new Flix().addPath("library/List.flix")
        .compile()
        .get
    }
  }

  test("Library.Map") {
    intercept[scala.NotImplementedError] {
      new Flix().addPath("library/Map.flix")
        .compile()
        .get
    }
  }

  test("Library.MeetSemiLattice") {
    intercept[scala.NotImplementedError] {
      new Flix().addPath("library/MeetSemiLattice.flix")
        .compile()
        .get
    }
  }

  test("Library.Monad") {
    intercept[scala.NotImplementedError] {
      new Flix().addPath("library/Monad.flix")
        .compile()
        .get
    }
  }

  test("Library.Monoid") {
    intercept[scala.NotImplementedError] {
      new Flix().addPath("library/Monoid.flix")
        .compile()
        .get
    }
  }

  test("Library.Narrowing") {
    intercept[scala.NotImplementedError] {
      new Flix().addPath("library/Narrowing.flix")
        .compile()
        .get
    }
  }

  test("Library.NatDec") {
    intercept[scala.NotImplementedError] {
      new Flix().addPath("library/NatDec.flix")
        .compile()
        .get
    }
  }

  test("Library.NatInc") {
    intercept[scala.NotImplementedError] {
      new Flix().addPath("library/NatInc.flix")
        .compile()
        .get
    }
  }

  test("Library.Norm") {
    intercept[scala.NotImplementedError] {
      new Flix().addPath("library/Norm.flix")
        .compile()
        .get
    }
  }

  test("Library.Opt") {
    intercept[scala.NotImplementedError] {
      new Flix().addPath("library/Opt.flix")
        .compile()
        .get
    }
  }

  test("Library.Ord") {
    intercept[scala.NotImplementedError] {
      new Flix().addPath("library/Ord.flix")
        .compile()
        .get
    }
  }

  test("Library.PartialOrder") {
    intercept[scala.NotImplementedError] {
      new Flix().addPath("library/PartialOrder.flix")
        .compile()
        .get
    }
  }

  test("Library.Prelude") {
    intercept[scala.NotImplementedError] {
      new Flix().addPath("library/Prelude.flix")
        .compile()
        .get
    }
  }

  test("Library.PreOrder") {
    intercept[scala.NotImplementedError] {
      new Flix().addPath("library/PreOrder.flix")
        .compile()
        .get
    }
  }

  test("Library.Traversable") {
    intercept[scala.NotImplementedError] {
      new Flix().addPath("library/Traversable.flix")
        .compile()
        .get
    }
  }

  test("Library.Vector") {
    intercept[scala.NotImplementedError] {
      new Flix().addPath("library/Vector.flix")
        .compile()
        .get
    }
  }

  test("Library.Widening") {
    intercept[scala.NotImplementedError] {
      new Flix().addPath("library/Widening.flix")
        .compile()
        .get
    }
  }

}
