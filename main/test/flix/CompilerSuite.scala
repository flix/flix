package flix

import ca.uwaterloo.flix.util.{FlixSuite, Options}

class CompilerSuite extends FlixSuite(incremental = true) {
  implicit val options: Options = Options.TestWithLibAll

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
  // Apply.
  //
  mkTest("main/test/flix/Test.Exp.Apply.Tail.flix")
  mkTest("main/test/flix/Test.Exp.Apply.Named.flix")

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

}
