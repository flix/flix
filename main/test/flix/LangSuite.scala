package flix

import ca.uwaterloo.flix.util.{FlixTest, Options}
import org.scalatest.Suites

class LangSuite extends Suites(

  //
  // Effects.
  //
  new FlixTest("Test.Eff.Polymorphism", "main/test/flix/Test.Eff.Polymorphism.flix"),


  //
  // Equality.
  //
  new FlixTest("Test.Equality.Channel", "main/test/flix/Test.Equality.Channel.flix"),

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

  ///
  /// Concurrency.
  ///
  new FlixTest("Test.Exp.Concurrency.Buffered", "main/test/flix/Test.Exp.Concurrency.Buffered.flix"),
  new FlixTest("Test.Exp.Concurrency.NewChannel", "main/test/flix/Test.Exp.Concurrency.NewChannel.flix"),
  new FlixTest("Test.Exp.Concurrency.Unbuffered", "main/test/flix/Test.Exp.Concurrency.Unbuffered.flix"),
  new FlixTest("Test.Exp.Concurrency.Select", "main/test/flix/Test.Exp.Concurrency.Select.flix")(Options.DefaultTest.copy(xallowredundancies = true)),
  new FlixTest("Test.Exp.Concurrency.Spawn", "main/test/flix/Test.Exp.Concurrency.Spawn.flix"),

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

  //
  // Stm.
  //
  new FlixTest("Test.Exp.Stm", "main/test/flix/Test.Exp.Stm.flix"),

)
