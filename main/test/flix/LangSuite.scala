package flix

import ca.uwaterloo.flix.util.FlixTest
import org.scalatest.Suites

class LangSuite extends Suites(

  //
  // Array.
  //
  new FlixTest("Test.Exp.ArrayLength", "main/test/flix/Test.Exp.ArrayLength.flix"),
  new FlixTest("Test.Exp.ArrayLit", "main/test/flix/Test.Exp.ArrayLit.flix"),
  new FlixTest("Test.Exp.ArrayLoad", "main/test/flix/Test.Exp.ArrayLoad.flix"),
  new FlixTest("Test.Exp.ArraySlice", "main/test/flix/Test.Exp.ArraySlice.flix"),
  new FlixTest("Test.Exp.ArraySliceNoEndIndex", "main/test/flix/Test.Exp.ArraySliceNoEndIndex.flix"),
  new FlixTest("Test.Exp.ArraySliceStartEndIndex", "main/test/flix/Test.Exp.ArraySliceNoStartIndex.flix"),
  new FlixTest("Test.Exp.ArraySliceCopy", "main/test/flix/Test.Exp.ArraySliceCopy.flix"),
  new FlixTest("Test.Exp.ArrayStore", "main/test/flix/Test.Exp.ArrayStore.flix"),
  new FlixTest("Test.Exp.ArrayNew", "main/test/flix/Test.Exp.ArrayNew.flix"),

  //
  // Ascribe.
  //
  new FlixTest("Test.Exp.Ascribe", "main/test/flix/Test.Exp.Ascribe.flix"),

  //
  // Cast.
  //
  new FlixTest("Test.Exp.Cast", "main/test/flix/Test.Exp.Cast.flix"),

  //
  // JVM.
  //
  new FlixTest("Test.Exp.Jvm.GetField", "main/test/flix/Test.Exp.Jvm.GetField.flix"),
  new FlixTest("Test.Exp.Jvm.GetStaticField", "main/test/flix/Test.Exp.Jvm.GetStaticField.flix"),
  new FlixTest("Test.Exp.Jvm.InvokeConstructor", "main/test/flix/Test.Exp.Jvm.InvokeConstructor.flix"),
  new FlixTest("Test.Exp.Jvm.InvokeMethod", "main/test/flix/Test.Exp.Jvm.InvokeMethod.flix"),
  new FlixTest("Test.Exp.Jvm.InvokeStaticMethod", "main/test/flix/Test.Exp.Jvm.InvokeStaticMethod.flix"),
  new FlixTest("Test.Exp.Jvm.PutField", "main/test/flix/Test.Exp.Jvm.PutField.flix"),
  new FlixTest("Test.Exp.Jvm.PutStaticField", "main/test/flix/Test.Exp.Jvm.PutStaticField.flix"),

  //
  // Reference.
  //
  new FlixTest("Test.Exp.Reference.Assign", "main/test/flix/Test.Exp.Reference.Assign.flix"),
  new FlixTest("Test.Exp.Reference.Deref", "main/test/flix/Test.Exp.Reference.Deref.flix"),
  new FlixTest("Test.Exp.Reference.Ref.flix", "main/test/flix/Test.Exp.Reference.Ref.flix"),
  new FlixTest("Test.Exp.Reference.Precedence", "main/test/flix/Test.Exp.Reference.Precedence.flix"),

  ///
  /// Spawn.
  ///
  new FlixTest("Test.Exp.Spawn", "main/test/flix/Test.Exp.Spawn.flix"),


)
