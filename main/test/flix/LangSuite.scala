package flix

import ca.uwaterloo.flix.util.{Ignore, FlixTest, Options}
import org.scalatest.Suites

class LangSuite extends Suites(

  //
  // Currying.
  //
  new FlixTest("Test.Currying", "main/test/flix/Test.Currying.flix"),

  //
  // Declarations.
  //
  new FlixTest("Test.Dec.Class", "main/test/flix/Test.Dec.Class.flix"),
  new FlixTest("Test.Dec.Namespace", "main/test/flix/Test.Dec.Namespace.flix"),
  new FlixTest("Test.Dec.OpaqueType", "main/test/flix/Test.Dec.OpaqueType.flix"),
  new FlixTest("Test.Dec.RelAlias", "main/test/flix/Test.Dec.RelAlias.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Dec.TypeAlias", "main/test/flix/Test.Dec.TypeAlias.flix"),

  //
  // Definitions.
  //
  new FlixTest("Test.Def.KindInference", "main/test/flix/Test.Def.KindInference.flix"),

  //
  // Effects.
  //
  new FlixTest("Test.Eff.Advanced", "main/test/flix/Test.Eff.Advanced.flix"),
  new FlixTest("Test.Eff.Polymorphism", "main/test/flix/Test.Eff.Polymorphism.flix")(Options.TestWithLibrary),

  //
  // Equality.
  //
  new FlixTest("Test.Equality.BigInt", "main/test/flix/Test.Equality.BigInt.flix"),
  new FlixTest("Test.Equality.Bool", "main/test/flix/Test.Equality.Bool.flix"),
  new FlixTest("Test.Equality.Char", "main/test/flix/Test.Equality.Char.flix"),
  new FlixTest("Test.Equality.Float32", "main/test/flix/Test.Equality.Float32.flix"),
  new FlixTest("Test.Equality.Float64", "main/test/flix/Test.Equality.Float64.flix"),
  new FlixTest("Test.Equality.Int8", "main/test/flix/Test.Equality.Int8.flix"),
  new FlixTest("Test.Equality.Int16", "main/test/flix/Test.Equality.Int16.flix"),
  new FlixTest("Test.Equality.Int32", "main/test/flix/Test.Equality.Int32.flix"),
  new FlixTest("Test.Equality.Int64", "main/test/flix/Test.Equality.Int64.flix"),
  new FlixTest("Test.Equality.String", "main/test/flix/Test.Equality.String.flix"),
  new FlixTest("Test.Equality.Tag", "main/test/flix/Test.Equality.Tag.flix"),
  new FlixTest("Test.Equality.Tuple", "main/test/flix/Test.Equality.Tuple.flix"),
  new FlixTest("Test.Equality.Unit", "main/test/flix/Test.Equality.Unit.flix"),

  new FlixTest("Test.Equality.Map", "main/test/flix/Test.Equality.Map.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Equality.Set", "main/test/flix/Test.Equality.Set.flix")(Options.TestWithLibrary),

  //
  // Apply.Tail.
  //
  new FlixTest("Test.Exp.Apply.Tail", "main/test/flix/Test.Exp.Apply.Tail.flix"),

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
  // BigInt.
  //
  new FlixTest("Test.Exp.BigInt", "main/test/flix/Test.Exp.BigInt.flix"),

  //
  // Binary.
  //
  new FlixTest("Test.Exp.Binary.Arithmetic", "main/test/flix/Test.Exp.Binary.Arithmetic.flix"),
  new FlixTest("Test.Exp.Binary.Bitwise", "main/test/flix/Test.Exp.Binary.Bitwise.flix"),
  new FlixTest("Test.Exp.Binary.Comparison", "main/test/flix/Test.Exp.Binary.Comparison.flix"),
  new FlixTest("Test.Exp.Binary.Logic", "main/test/flix/Test.Exp.Binary.Logic.flix"),
  new FlixTest("Test.Exp.Binary.Spaceship", "main/test/flix/Test.Exp.Binary.Spaceship.flix")(Options.TestWithLibrary),

  //
  // Block.
  //
  new FlixTest("Test.Exp.Block", "main/test/flix/Test.Exp.Block.flix"),

  //
  // Cast.
  //
  new FlixTest("Test.Exp.Cast", "main/test/flix/Test.Exp.Cast.flix")(Options.TestWithLibrary),

  //
  // Char.
  //
  new FlixTest("Test.Exp.Char", "main/test/flix/Test.Exp.Char.flix"),

  //
  // Choose.
  //
  new FlixTest("Test.Exp.Choose", "main/test/flix/Test.Exp.Choose.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Exp.ChooseStar", "main/test/flix/Test.Exp.ChooseStar.flix")(Options.TestWithLibrary),

  //
  // Concurrency.
  //
  new FlixTest("Test.Exp.Concurrency.Buffered", "main/test/flix/Test.Exp.Concurrency.Buffered.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Exp.Concurrency.NewChannel", "main/test/flix/Test.Exp.Concurrency.NewChannel.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Exp.Concurrency.Unbuffered", "main/test/flix/Test.Exp.Concurrency.Unbuffered.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Exp.Concurrency.Select", "main/test/flix/Test.Exp.Concurrency.Select.flix")(Options.DefaultTest.copy(xallowredundancies = true)),
  new FlixTest("Test.Exp.Concurrency.Spawn", "main/test/flix/Test.Exp.Concurrency.Spawn.flix")(Options.TestWithLibrary),

  //
  // Default.
  //
  new FlixTest("Test.Exp.Default", "main/test/flix/Test.Exp.Default.flix"),

  //
  // Floats.
  //
  new FlixTest("Test.Exp.Float32", "main/test/flix/Test.Exp.Float32.flix"),
  new FlixTest("Test.Exp.Float64", "main/test/flix/Test.Exp.Float64.flix"),

  //
  // Fixpoint.
  //
  new FlixTest("Test.Exp.Fixpoint.Constraint", "main/test/flix/Test.Exp.Fixpoint.Constraint.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Exp.Fixpoint.Compose", "main/test/flix/Test.Exp.Fixpoint.Compose.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Exp.Fixpoint.Entails", "main/test/flix/Test.Exp.Fixpoint.Entails.flix")(Options.TestWithLibrary),
  new Ignore("Test.Exp.Fixpoint.Fold", "main/test/flix/Test.Exp.Fixpoint.Fold.flix")(Options.TestWithLibrary), // TODO
  new FlixTest("Test.Exp.Fixpoint.Project", "main/test/flix/Test.Exp.Fixpoint.Project.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Exp.Fixpoint.Solve", "main/test/flix/Test.Exp.Fixpoint.Solve.flix")(Options.TestWithLibrary),

  //
  // Force.
  //
  new FlixTest("Test.Exp.Force", "main/test/flix/Test.Exp.Force.flix"),

  //
  // Hole.
  //
  new FlixTest("Test.Exp.Hole", "main/test/flix/Test.Exp.Hole.flix")(Options.TestWithLibrary),

  //
  // IfThenElse.
  //
  new FlixTest("Test.Exp.IfThenElse", "main/test/flix/Test.Exp.IfThenElse.flix"),

  //
  // Infix.
  //
  new FlixTest("Test.Exp.Infix", "main/test/flix/Test.Exp.Infix.flix"),

  //
  // Int.
  //
  new FlixTest("Test.Exp.Int8", "main/test/flix/Test.Exp.Int8.flix"),
  new FlixTest("Test.Exp.Int16", "main/test/flix/Test.Exp.Int16.flix"),
  new FlixTest("Test.Exp.Int32", "main/test/flix/Test.Exp.Int32.flix"),
  new FlixTest("Test.Exp.Int64", "main/test/flix/Test.Exp.Int64.flix"),

  //
  // Interpolation.
  //
  new FlixTest("Test.Exp.Interpolation", "main/test/flix/Test.Exp.Interpolation.flix"),

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
  // Lambda.
  //
  new FlixTest("Test.Exp.Lambda.Match", "main/test/flix/Test.Exp.Lambda.Match.flix"),

  //
  // Lazy
  //
  new FlixTest("Test.Exp.Lazy", "main/test/flix/Test.Exp.Lazy.flix"),

  //
  // Let.
  //
  new FlixTest("Test.Exp.Let", "main/test/flix/Test.Exp.Let.flix"),
  new FlixTest("Test.Exp.Let.Match", "main/test/flix/Test.Exp.Let.Match.flix"),
  new FlixTest("Test.Exp.Let.MatchStar", "main/test/flix/Test.Exp.Let.MatchStar.flix")(Options.TestWithLibrary),

  //
  // List.
  //
  new FlixTest("Test.Exp.List", "main/test/flix/Test.Exp.List.flix"),

  //
  // Match.
  //
  new FlixTest("Test.Exp.Match.Array", "main/test/flix/Test.Exp.Match.Array.flix"),
  new FlixTest("Test.Exp.Match.Bool", "main/test/flix/Test.Exp.Match.Bool.flix"),
  new FlixTest("Test.Exp.Match.Char", "main/test/flix/Test.Exp.Match.Char.flix"),
  new FlixTest("Test.Exp.Match.Guard", "main/test/flix/Test.Exp.Match.Guard.flix"),
  new FlixTest("Test.Exp.Match.Float32", "main/test/flix/Test.Exp.Match.Float32.flix"),
  new FlixTest("Test.Exp.Match.Float64", "main/test/flix/Test.Exp.Match.Float64.flix"),
  new FlixTest("Test.Exp.Match.Int8", "main/test/flix/Test.Exp.Match.Int8.flix"),
  new FlixTest("Test.Exp.Match.Int16", "main/test/flix/Test.Exp.Match.Int16.flix"),
  new FlixTest("Test.Exp.Match.Int32", "main/test/flix/Test.Exp.Match.Int32.flix"),
  new FlixTest("Test.Exp.Match.Int64", "main/test/flix/Test.Exp.Match.Int64.flix"),
  new FlixTest("Test.Exp.Match.List", "main/test/flix/Test.Exp.Match.List.flix"),
  new FlixTest("Test.Exp.Match.String", "main/test/flix/Test.Exp.Match.String.flix"),
  new FlixTest("Test.Exp.Match.Tag", "main/test/flix/Test.Exp.Match.Tag.flix"),
  new FlixTest("Test.Exp.Match.Unit", "main/test/flix/Test.Exp.Match.Unit.flix"),
  new FlixTest("Test.Exp.Match.Wild", "main/test/flix/Test.Exp.Match.Wild.flix"),

  //
  // Null.
  //
  new FlixTest("Test.Exp.Null", "main/test/flix/Test.Exp.Null.flix"),

  //
  // Postfix.
  //
  new FlixTest("Test.Exp.Postfix", "main/test/flix/Test.Exp.Postfix.flix"),

  //
  // Record.
  //
  new FlixTest("Test.Exp.Record.Extend", "main/test/flix/Test.Exp.Record.Extend.flix"),
  new FlixTest("Test.Exp.Record.Literal", "main/test/flix/Test.Exp.Record.Literal.flix"),
  new FlixTest("Test.Exp.Record.Multiple", "main/test/flix/Test.Exp.Record.Multiple.flix"),
  new FlixTest("Test.Exp.Record.Polymorphism", "main/test/flix/Test.Exp.Record.Polymorphism.flix"),
  new FlixTest("Test.Exp.Record.Restrict", "main/test/flix/Test.Exp.Record.Restrict.flix"),
  new FlixTest("Test.Exp.Record.Select", "main/test/flix/Test.Exp.Record.Select.flix"),
  new FlixTest("Test.Exp.Record.Update", "main/test/flix/Test.Exp.Record.Update.flix"),

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
  // Tuple.
  //
  new FlixTest("Test.Exp.Tuple", "main/test/flix/Test.Exp.Tuple.flix"),

  //
  // Unary.
  //
  new FlixTest("Test.Exp.Unary.Arithmetic", "main/test/flix/Test.Exp.Unary.Arithmetic.flix"),
  new FlixTest("Test.Exp.Unary.Bitwise", "main/test/flix/Test.Exp.Unary.Bitwise.flix"),
  new FlixTest("Test.Exp.Unary.Logic", "main/test/flix/Test.Exp.Unary.Logic.flix"),

  //
  // Unit.
  //
  new FlixTest("Test.Exp.Unit", "main/test/flix/Test.Exp.Unit.flix"),

  //
  // Stm.
  //
  new FlixTest("Test.Exp.Stm", "main/test/flix/Test.Exp.Stm.flix"),

  //
  // String.
  //
  new FlixTest("Test.Exp.String", "main/test/flix/Test.Exp.String.flix"),

  //
  // Predicate.
  //
  new FlixTest("Test.Predicate.Filter", "main/test/flix/Test.Predicate.Filter.flix")(Options.TestWithLibrary),
  new Ignore("Test.Predicate.Guard", "main/test/flix/Test.Predicate.Guard.flix")(Options.TestWithLibrary), // TODO
  new FlixTest("Test.Predicate.Nullary", "main/test/flix/Test.Predicate.Nullary.flix")(Options.TestWithLibrary),
  new Ignore("Test.Predicate.Union", "main/test/flix/Test.Predicate.Union.flix")(Options.TestWithLibrary), // TODO

  //
  // Stratified Negation.
  //
  new FlixTest("Test.Stratification", "main/test/flix/Test.Stratification.flix")(Options.TestWithLibrary),

  //
  // Term.
  //
  new Ignore("Test.Term.Apply", "main/test/flix/Test.Term.Apply.flix")(Options.TestWithLibrary), // TODO
  new FlixTest("Test.Term.Lit", "main/test/flix/Test.Term.Lit.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Term.Lit.List", "main/test/flix/Test.Term.Lit.List.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Term.Lit.Option", "main/test/flix/Test.Term.Lit.Option.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Term.Lit.Result", "main/test/flix/Test.Term.Lit.Result.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Term.Lit.Set", "main/test/flix/Test.Term.Lit.Set.flix")(Options.TestWithLibrary),
  new Ignore("Test.Term.Var.CapturedVar", "main/test/flix/Test.Term.Var.CapturedVar.flix")(Options.TestWithLibrary), // TODO
  new FlixTest("Test.Term.Var.QuantVar", "main/test/flix/Test.Term.Var.QuantVar.flix")(Options.TestWithLibrary),
  new FlixTest("Test.Term.Var.WildVar", "main/test/flix/Test.Term.Var.WildVar.flix")(Options.TestWithLibrary), // TODO

  //
  // Type Alias.
  //
  new FlixTest("Test.TypeAlias.Rel", "main/test/flix/Test.TypeAlias.Rel.flix")(Options.TestWithLibrary),

  //
  // Unused.
  //
  new FlixTest("Test.Unused.Tag", "main/test/flix/Test.Unused.Tag.flix"),
  new FlixTest("Test.Unused.Var", "main/test/flix/Test.Unused.Var.flix"),

  //
  // Use.
  //
  new FlixTest("Test.Use.Def", "main/test/flix/Test.Use.Def.flix"),
  new FlixTest("Test.Use.Sig", "main/test/flix/Test.Use.Sig.flix"),
  new FlixTest("Test.Use.Tag", "main/test/flix/Test.Use.Tag.flix"),
  new FlixTest("Test.Use.Type", "main/test/flix/Test.Use.Type.flix"),

)
