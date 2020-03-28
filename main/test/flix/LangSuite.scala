package flix

import ca.uwaterloo.flix.util.{FlixTest, Options}
import org.scalatest.Suites

class LangSuite extends Suites(

  //
  // Declarations.
  //
  new FlixTest("Test.Dec.Namespace", "main/test/flix/Test.Dec.Namespace.flix"),

  //
  // Effects.
  //
  new FlixTest("Test.Eff.Polymorphism", "main/test/flix/Test.Eff.Polymorphism.flix")(Options.TestWithLibrary),

  //
  // Equality.
  //
  new FlixTest("Test.Equality.Channel", "main/test/flix/Test.Equality.Channel.flix"),

  //
  // Array.
  //
  new FlixTest("Test.Exp.ArrayLength", "main/test/flix/Test.Exp.ArrayLength.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Exp.ArrayLit", "main/test/flix/Test.Exp.ArrayLit.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Exp.ArrayLoad", "main/test/flix/Test.Exp.ArrayLoad.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Exp.ArraySlice", "main/test/flix/Test.Exp.ArraySlice.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Exp.ArraySliceNoEndIndex", "main/test/flix/Test.Exp.ArraySliceNoEndIndex.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Exp.ArraySliceStartEndIndex", "main/test/flix/Test.Exp.ArraySliceNoStartIndex.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Exp.ArraySliceCopy", "main/test/flix/Test.Exp.ArraySliceCopy.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Exp.ArrayStore", "main/test/flix/Test.Exp.ArrayStore.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Exp.ArrayNew", "main/test/flix/Test.Exp.ArrayNew.flix")(Options.TestWithLibrary),

  //
  // Ascribe.
  //
  new FlixTest("Test.Exp.Ascribe", "main/test/flix/Test.Exp.Ascribe.flix")(Options.TestWithLibrary),

  //
  // Binary.Spaceship.
  //
  new FlixTest("Test.Exp.Binary.Spaceship", "main/test/flix/Test.Exp.Binary.Spaceship.flix")(Options.TestWithLibrary),

  //
  // Cast.
  //
  new FlixTest("Test.Exp.Cast", "main/test/flix/Test.Exp.Cast.flix")(Options.TestWithLibrary),

  ///
  /// Concurrency.
  ///
  new FlixTest("Test.Exp.Concurrency.Buffered", "main/test/flix/Test.Exp.Concurrency.Buffered.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Exp.Concurrency.NewChannel", "main/test/flix/Test.Exp.Concurrency.NewChannel.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Exp.Concurrency.Unbuffered", "main/test/flix/Test.Exp.Concurrency.Unbuffered.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Exp.Concurrency.Select", "main/test/flix/Test.Exp.Concurrency.Select.flix")(Options.DefaultTest.copy(xallowredundancies = true)),
  new FlixTest("Test.Exp.Concurrency.Spawn", "main/test/flix/Test.Exp.Concurrency.Spawn.flix")(Options.TestWithLibrary),

  //
  // JVM.
  //
  new FlixTest("Test.Exp.Jvm.GetField", "main/test/flix/Test.Exp.Jvm.GetField.flix"),
  new FlixTest("Test.Exp.Jvm.GetStaticField", "main/test/flix/Test.Exp.Jvm.GetStaticField.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Exp.Jvm.InvokeConstructor", "main/test/flix/Test.Exp.Jvm.InvokeConstructor.flix"),
  new FlixTest("Test.Exp.Jvm.InvokeMethod", "main/test/flix/Test.Exp.Jvm.InvokeMethod.flix"),
  new FlixTest("Test.Exp.Jvm.InvokeStaticMethod", "main/test/flix/Test.Exp.Jvm.InvokeStaticMethod.flix"),
  new FlixTest("Test.Exp.Jvm.PutField", "main/test/flix/Test.Exp.Jvm.PutField.flix"),
  new FlixTest("Test.Exp.Jvm.PutStaticField", "main/test/flix/Test.Exp.Jvm.PutStaticField.flix"),

  //
  // Let.
  //
  new FlixTest("Test.Exp.Let.MatchStar", "main/test/flix/Test.Exp.Let.MatchStar.flix")(Options.TestWithLibrary),

  //
  // Reference.
  //
  new FlixTest("Test.Exp.Reference.Assign", "main/test/flix/Test.Exp.Reference.Assign.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Exp.Reference.Deref", "main/test/flix/Test.Exp.Reference.Deref.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Exp.Reference.Ref.flix", "main/test/flix/Test.Exp.Reference.Ref.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Exp.Reference.Precedence", "main/test/flix/Test.Exp.Reference.Precedence.flix"),

  //
  // Tag.
  //
  new FlixTest("Test.Exp.Tag", "main/test/flix/Test.Exp.Tag.flix"),
  new FlixTest("Test.Exp.Tag.Lambda", "main/test/flix/Test.Exp.Tag.Lambda.flix")(Options.TestWithLibrary),

  //
  // Stm.
  //
  new FlixTest("Test.Exp.Stm", "main/test/flix/Test.Exp.Stm.flix"),

  //
  // Use.
  //
  new FlixTest("Test.Use.Def", "main/test/flix/Test.Use.Def.flix"),
  new FlixTest("Test.Use.Tag", "main/test/flix/Test.Use.Tag.flix"),
  new FlixTest("Test.Use.Type", "main/test/flix/Test.Use.Type.flix"),

)
