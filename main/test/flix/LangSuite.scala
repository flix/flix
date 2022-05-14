package flix

import ca.uwaterloo.flix.util.{FlixTest, Options}
import org.scalatest.Suites

class LangSuite extends Suites(

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
  // Kind.
  //
  new FlixTest("Test.Kind.Class", "main/test/flix/Test.Kind.Class.flix"),
  new FlixTest("Test.Kind.Def", "main/test/flix/Test.Kind.Def.flix"),
  new FlixTest("Test.Kind.Enum", "main/test/flix/Test.Kind.Enum.flix"),
  new FlixTest("Test.Kind.Instance", "main/test/flix/Test.Kind.Instance.flix"),
  new FlixTest("Test.Kind.TypeAlias", "main/test/flix/Test.Kind.TypeAlias.flix"),

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
