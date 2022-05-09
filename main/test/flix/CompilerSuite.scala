package flix

import ca.uwaterloo.flix.util.{FlixSuite, Options}

class CompilerSuite extends FlixSuite(incremental = true) {
  implicit val options: Options = Options.TestWithLibAll


  //
  // Apply.
  //
  mkTest("main/test/flix/Test.Exp.Apply.Tail.flix")
  mkTest("main/test/flix/Test.Exp.Apply.Named.flix")

  //
  // Arrays
  //
  mkTest("main/test/flix/Test.Exp.ArrayLit.flix")
  mkTest("main/test/flix/Test.Exp.ArrayLoad.flix")
  mkTest("main/test/flix/Test.Exp.ArrayLength.flix")
  mkTest("main/test/flix/Test.Exp.ArrayNew.flix")
  mkTest("main/test/flix/Test.Exp.ArraySlice.flix")
  mkTest("main/test/flix/Test.Exp.ArraySliceNoEndIndex.flix")
  mkTest("main/test/flix/Test.Exp.ArraySliceNoStartIndex.flix")
  mkTest("main/test/flix/Test.Exp.ArraySliceCopy.flix")
  mkTest("main/test/flix/Test.Exp.ArrayStore.flix")

  //
  // Ascribe.
  //
  mkTest("main/test/flix/Test.Exp.Ascribe.flix")

  //
  // BigInt.
  //
  mkTest("main/test/flix/Test.Exp.BigInt.flix")

  //
  // Binary.
  //
  mkTest("main/test/flix/Test.Exp.Binary.Arithmetic.flix")
  mkTest("main/test/flix/Test.Exp.Binary.Bitwise.flix")
  mkTest("main/test/flix/Test.Exp.Binary.Comparison.flix")
  mkTest("main/test/flix/Test.Exp.Binary.Logic.flix")
  mkTest("main/test/flix/Test.Exp.Binary.Spaceship.flix")

  //
  // Fixpoint
  //
  mkTest("main/test/flix/Test.Exp.Fixpoint.Compose.flix")
  mkTest("main/test/flix/Test.Exp.Fixpoint.Constraint.flix")
  mkTest("main/test/flix/Test.Exp.Fixpoint.Lambda.flix")
  mkTest("main/test/flix/Test.Exp.Fixpoint.Project.flix")
  mkTest("main/test/flix/Test.Exp.Fixpoint.Query.flix")
  mkTest("main/test/flix/Test.Exp.Fixpoint.Solve.flix")
  mkTest("main/test/flix/Test.Exp.Fixpoint.Solve.Lattice.flix")

  //
  // Match.
  //
  mkTest("main/test/flix/Test.Exp.Match.Array.flix")
  mkTest("main/test/flix/Test.Exp.Match.Bool.flix")
  mkTest("main/test/flix/Test.Exp.Match.Char.flix")
  mkTest("main/test/flix/Test.Exp.Match.Guard.flix")
  mkTest("main/test/flix/Test.Exp.Match.Float32.flix")
  mkTest("main/test/flix/Test.Exp.Match.Float64.flix")
  mkTest("main/test/flix/Test.Exp.Match.Int8.flix")
  mkTest("main/test/flix/Test.Exp.Match.Int16.flix")
  mkTest("main/test/flix/Test.Exp.Match.Int32.flix")
  mkTest("main/test/flix/Test.Exp.Match.Int64.flix")
  mkTest("main/test/flix/Test.Exp.Match.List.flix")
  mkTest("main/test/flix/Test.Exp.Match.String.flix")
  mkTest("main/test/flix/Test.Exp.Match.Tag.flix")
  mkTest("main/test/flix/Test.Exp.Match.Unit.flix")
  mkTest("main/test/flix/Test.Exp.Match.Wild.flix")

  //
  // Record.
  //
  mkTest("main/test/flix/Test.Exp.Record.Extend.flix")
  mkTest("main/test/flix/Test.Exp.Record.Literal.flix")
  mkTest("main/test/flix/Test.Exp.Record.Multiple.flix")
  mkTest("main/test/flix/Test.Exp.Record.Polymorphism.flix")
  mkTest("main/test/flix/Test.Exp.Record.Restrict.flix")
  mkTest("main/test/flix/Test.Exp.Record.Select.flix")
  mkTest("main/test/flix/Test.Exp.Record.Update.flix")

}
