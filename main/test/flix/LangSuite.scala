package flix

import ca.uwaterloo.flix.util.FlixTest
import org.scalatest.Suites

class LangSuite extends Suites(

  //
  // Casts.
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
  // References.
  //
  new FlixTest("Test.Exp.Reference.Assign", "main/test/flix/Test.Exp.Reference.Assign.flix"),
  new FlixTest("Test.Exp.Reference.Deref", "main/test/flix/Test.Exp.Reference.Deref.flix"),
  new FlixTest("Test.Exp.Reference.Ref.flix", "main/test/flix/Test.Exp.Reference.Ref.flix"),
  new FlixTest("Test.Exp.Reference.Precedence", "main/test/flix/Test.Exp.Reference.Precedence.flix"),


)
