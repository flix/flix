package flix

import ca.uwaterloo.flix.util.{FlixTest, Options}
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
  new FlixTest("Test.Dec.Effect", "main/test/flix/Test.Dec.Effect.flix"),
  new FlixTest("Test.Dec.Enum", "main/test/flix/Test.Dec.Enum.flix"),
  new FlixTest("Test.Dec.Namespace", "main/test/flix/Test.Dec.Namespace.flix"),
  new FlixTest("Test.Dec.OpaqueType", "main/test/flix/Test.Dec.OpaqueType.flix"),
  new FlixTest("Test.Dec.RelAlias", "main/test/flix/Test.Dec.RelAlias.flix")(Options.TestWithLibAll),
  new FlixTest("Test.Dec.TopLevel", "main/test/flix/Test.Dec.TopLevel.flix"),
  new FlixTest("Test.Dec.TypeAlias", "main/test/flix/Test.Dec.TypeAlias.flix"),

  //
  // Definitions.
  //
  new FlixTest("Test.Def.Op", "main/test/flix/Test.Def.Op.flix"),

  //
  // Derivations.
  //
  new FlixTest("Test.Derives.Eq", "main/test/flix/Test.Derives.Eq.flix"),
  new FlixTest("Test.Derives.Order", "main/test/flix/Test.Derives.Order.flix"),
  new FlixTest("Test.Derives.ToString", "main/test/flix/Test.Derives.ToString.flix"),

  //
  // Effects.
  //
  new FlixTest("Test.Eff.Advanced", "main/test/flix/Test.Eff.Advanced.flix"),
  new FlixTest("Test.Eff.Polymorphism", "main/test/flix/Test.Eff.Polymorphism.flix")(Options.TestWithLibAll),
  new FlixTest("Test.Eff.Simplification", "main/test/flix/Test.Eff.Simplification.flix"),

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

  new FlixTest("Test.Equality.Map", "main/test/flix/Test.Equality.Map.flix")(Options.TestWithLibAll),
  new FlixTest("Test.Equality.Set", "main/test/flix/Test.Equality.Set.flix")(Options.TestWithLibAll),

  //
  // Block.
  //
  new FlixTest("Test.Exp.Block", "main/test/flix/Test.Exp.Block.flix"),

  //
  // Cast.
  //
  new FlixTest("Test.Exp.Cast", "main/test/flix/Test.Exp.Cast.flix")(Options.TestWithLibAll),

  //
  // Char.
  //
  new FlixTest("Test.Exp.Char", "main/test/flix/Test.Exp.Char.flix"),

  //
  // Choose.
  //
  new FlixTest("Test.Exp.Choose", "main/test/flix/Test.Exp.Choose.flix")(Options.TestWithLibAll),
  new FlixTest("Test.Exp.ChooseStar", "main/test/flix/Test.Exp.ChooseStar.flix")(Options.TestWithLibAll),

  //
  // Concurrency.
  //
  new FlixTest("Test.Exp.Concurrency.Buffered", "main/test/flix/Test.Exp.Concurrency.Buffered.flix")(Options.TestWithLibAll),
  new FlixTest("Test.Exp.Concurrency.NewChannel", "main/test/flix/Test.Exp.Concurrency.NewChannel.flix")(Options.TestWithLibAll),
  new FlixTest("Test.Exp.Concurrency.Unbuffered", "main/test/flix/Test.Exp.Concurrency.Unbuffered.flix")(Options.TestWithLibAll),
  new FlixTest("Test.Exp.Concurrency.Spawn", "main/test/flix/Test.Exp.Concurrency.Spawn.flix")(Options.TestWithLibAll),
  new FlixTest("Test.Exp.Concurrency.Select", "main/test/flix/Test.Exp.Concurrency.Select.flix")(Options.DefaultTest.copy(xallowredundancies = true)),

  //
  // Default.
  //
  new FlixTest("Test.Exp.Default", "main/test/flix/Test.Exp.Default.flix"),

  //
  // Discard.
  //
  new FlixTest("Test.Exp.Discard", "main/test/flix/Test.Exp.Discard.flix"),

  //
  // Effects.
  //
  new FlixTest("Test.Exp.Effect", "main/test/flix/Test.Exp.Effect.flix"),

  //
  // Floats.
  //
  new FlixTest("Test.Exp.Float32", "main/test/flix/Test.Exp.Float32.flix"),
  new FlixTest("Test.Exp.Float64", "main/test/flix/Test.Exp.Float64.flix"),

  //
  // Force.
  //
  new FlixTest("Test.Exp.Force", "main/test/flix/Test.Exp.Force.flix"),

  //
  // Hole.
  //
  new FlixTest("Test.Exp.Hole", "main/test/flix/Test.Exp.Hole.flix")(Options.TestWithLibAll),

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
  new FlixTest("Test.Exp.Jvm.GetStaticField", "main/test/flix/Test.Exp.Jvm.GetStaticField.flix")(Options.TestWithLibAll),
  new FlixTest("Test.Exp.Jvm.GetFieldStaticInnerClass", "main/test/flix/Test.Exp.Jvm.GetFieldStaticInnerClass.flix"),
  new FlixTest("Test.Exp.Jvm.GetStaticFieldStaticInnerClass", "main/test/flix/Test.Exp.Jvm.GetStaticFieldStaticInnerClass.flix"),
  new FlixTest("Test.Exp.Jvm.GetFieldDoubleNestedClass", "main/test/flix/Test.Exp.Jvm.GetFieldDoubleNestedClass.flix"),
  new FlixTest("Test.Exp.Jvm.InvokeConstructor", "main/test/flix/Test.Exp.Jvm.InvokeConstructor.flix"),
  new FlixTest("Test.Exp.Jvm.InvokeMethod", "main/test/flix/Test.Exp.Jvm.InvokeMethod.flix"),
  new FlixTest("Test.Exp.Jvm.InvokeStaticMethod", "main/test/flix/Test.Exp.Jvm.InvokeStaticMethod.flix"),
  new FlixTest("Test.Exp.Jvm.PutField", "main/test/flix/Test.Exp.Jvm.PutField.flix"),
  new FlixTest("Test.Exp.Jvm.PutStaticField", "main/test/flix/Test.Exp.Jvm.PutStaticField.flix"),
  new FlixTest("Test.Exp.Jvm.TryCatch", "main/test/flix/Test.Exp.Jvm.TryCatch.flix")(Options.TestWithLibNix),

  //
  // Kind.
  //
  new FlixTest("Test.Kind.Class", "main/test/flix/Test.Kind.Class.flix"),
  new FlixTest("Test.Kind.Def", "main/test/flix/Test.Kind.Def.flix"),
  new FlixTest("Test.Kind.Enum", "main/test/flix/Test.Kind.Enum.flix"),
  new FlixTest("Test.Kind.Instance", "main/test/flix/Test.Kind.Instance.flix"),
  new FlixTest("Test.Kind.TypeAlias", "main/test/flix/Test.Kind.TypeAlias.flix"),

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
  new FlixTest("Test.Exp.Let.MatchStar", "main/test/flix/Test.Exp.Let.MatchStar.flix")(Options.TestWithLibAll),
  new FlixTest("Test.Exp.Let.Rec", "main/test/flix/Test.Exp.Let.Rec.flix")(Options.TestWithLibAll),

  //
  // List.
  //
  new FlixTest("Test.Exp.List", "main/test/flix/Test.Exp.List.flix"),

  //
  // Null.
  //
  new FlixTest("Test.Exp.Null", "main/test/flix/Test.Exp.Null.flix"),

  //
  // Reference.
  //
  new FlixTest("Test.Exp.Ref", List(
    "main/test/flix/Test.Exp.Ref.Assign.flix",
    "main/test/flix/Test.Exp.Ref.Ref.flix",
    "main/test/flix/Test.Exp.Ref.Deref.flix",
    "main/test/flix/Test.Exp.Ref.Precedence.flix",
  ), Options.TestWithLibAll),

  //
  // Reify.
  //
  new FlixTest("Test.Exp.Reify", "main/test/flix/Test.Exp.Reify.flix"),
  new FlixTest("Test.Exp.ReifyType", "main/test/flix/Test.Exp.ReifyType.flix"),

  //
  // Tag.
  //
  new FlixTest("Test.Exp.Tag", "main/test/flix/Test.Exp.Tag.flix"),
  new FlixTest("Test.Exp.Tag.Lambda", "main/test/flix/Test.Exp.Tag.Lambda.flix")(Options.TestWithLibAll),

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
  new FlixTest("Test.Predicate", List(
    "main/test/flix/Test.Predicate.Guard.flix",
    "main/test/flix/Test.Predicate.Nullary.flix",
  ), Options.TestWithLibAll),

  //
  // Stratified Negation.
  //
  new FlixTest("Test.Stratification", "main/test/flix/Test.Stratification.flix")(Options.TestWithLibAll),

  //
  // Term.
  //
  new FlixTest("Test.Term", List(
    "main/test/flix/Test.Term.Apply.flix",
    "main/test/flix/Test.Term.Lit.flix",
    "main/test/flix/Test.Term.Lit.Option.flix",
    "main/test/flix/Test.Term.Lit.Result.flix",
    "main/test/flix/Test.Term.Lit.List.flix",
    "main/test/flix/Test.Term.Lit.Set.flix",
    "main/test/flix/Test.Term.Var.CapturedVar.flix",
    "main/test/flix/Test.Term.Var.QuantVar.flix",
    "main/test/flix/Test.Term.Var.WildVar.flix"
  ), Options.TestWithLibAll),

  //
  // Type Alias.
  //
  new FlixTest("Test.TypeAlias.Rel", "main/test/flix/Test.TypeAlias.Rel.flix")(Options.TestWithLibAll),

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

  //
  // Integration.
  //
  new FlixTest("Test.Integ.Class.Schema", "main/test/flix/Test.Integ.Class.Schema.flix")(Options.TestWithLibAll),
  new FlixTest("Test.Integ.Fixpoint.TypeAlias", "main/test/flix/Test.Integ.Fixpoint.TypeAlias.flix")(Options.TestWithLibAll),
  new FlixTest("Test.Integ.Enum.TypeAlias", "main/test/flix/Test.Integ.Enum.TypeAlias.flix")
)
