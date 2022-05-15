package flix

import ca.uwaterloo.flix.util.{FlixSuite, Options}

class CompilerSuite extends FlixSuite(incremental = true) {
  implicit val options: Options = Options.TestWithLibAll

  //mkTestDir("main/test/flix/")

  //
  // Currying
  //
  mkTest("main/test/flix/Test.Currying.flix")

  //
  // Declarations
  //
  mkTest("main/test/flix/Test.Dec.Class.flix")
  mkTest("main/test/flix/Test.Dec.Effect.flix")
  mkTest("main/test/flix/Test.Dec.Enum.flix")
  mkTest("main/test/flix/Test.Dec.Namespace.flix")
  mkTest("main/test/flix/Test.Dec.OpaqueType.flix")
  mkTest("main/test/flix/Test.Dec.RelAlias.flix")
  mkTest("main/test/flix/Test.Dec.TopLevel.flix")
  mkTest("main/test/flix/Test.Dec.TypeAlias.flix")

  //
  // Definitions
  //
  mkTest("main/test/flix/Test.Def.Op.flix")

  //
  // Derivations
  //
  mkTest("main/test/flix/Test.Derives.Eq.flix")
  mkTest("main/test/flix/Test.Derives.Order.flix")
  mkTest("main/test/flix/Test.Derives.ToString.flix")

  //
  // Effects
  //
  mkTest("main/test/flix/Test.Eff.Advanced.flix")
  mkTest("main/test/flix/Test.Eff.Polymorphism.flix")
  mkTest("main/test/flix/Test.Eff.Simplification.flix")

  //
  // Equality
  //
  mkTest("main/test/flix/Test.Equality.BigInt.flix")
  mkTest("main/test/flix/Test.Equality.Bool.flix")
  mkTest("main/test/flix/Test.Equality.Char.flix")
  mkTest("main/test/flix/Test.Equality.Float32.flix")
  mkTest("main/test/flix/Test.Equality.Float64.flix")
  mkTest("main/test/flix/Test.Equality.Int8.flix")
  mkTest("main/test/flix/Test.Equality.Int16.flix")
  mkTest("main/test/flix/Test.Equality.Int32.flix")
  mkTest("main/test/flix/Test.Equality.Int64.flix")
  mkTest("main/test/flix/Test.Equality.String.flix")
  mkTest("main/test/flix/Test.Equality.Tag.flix")
  mkTest("main/test/flix/Test.Equality.Tuple.flix")
  mkTest("main/test/flix/Test.Equality.Unit.flix")

  mkTest("main/test/flix/Test.Equality.Map.flix")
  mkTest("main/test/flix/Test.Equality.Set.flix")

  //
  // Apply
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
  // Ascribe
  //
  mkTest("main/test/flix/Test.Exp.Ascribe.flix")

  //
  // BigInt
  //
  mkTest("main/test/flix/Test.Exp.BigInt.flix")

  //
  // Binary
  //
  mkTest("main/test/flix/Test.Exp.Binary.Arithmetic.flix")
  mkTest("main/test/flix/Test.Exp.Binary.Bitwise.flix")
  mkTest("main/test/flix/Test.Exp.Binary.Comparison.flix")
  mkTest("main/test/flix/Test.Exp.Binary.Logic.flix")
  mkTest("main/test/flix/Test.Exp.Binary.Spaceship.flix")

  //
  // Block
  //
  mkTest("main/test/flix/Test.Exp.Block.flix")

  //
  // Cast
  //
  mkTest("main/test/flix/Test.Exp.Cast.flix")

  //
  // Char
  //
  mkTest("main/test/flix/Test.Exp.Char.flix")

  //
  // Choose
  //
  mkTest("main/test/flix/Test.Exp.Choose.flix")
  mkTest("main/test/flix/Test.Exp.ChooseStar.flix")

  //
  // Concurrency
  //
  mkTest("main/test/flix/Test.Exp.Concurrency.Buffered.flix")
  mkTest("main/test/flix/Test.Exp.Concurrency.NewChannel.flix")
  mkTest("main/test/flix/Test.Exp.Concurrency.Unbuffered.flix")
  mkTest("main/test/flix/Test.Exp.Concurrency.Spawn.flix")
  mkTest("main/test/flix/Test.Exp.Concurrency.Select.flix")

  //
  // Default
  //
  mkTest("main/test/flix/Test.Exp.Default.flix")

  //
  // Discard
  //
  mkTest("main/test/flix/Test.Exp.Discard.flix")

  //
  // Effects
  //
  mkTest("main/test/flix/Test.Exp.Effect.flix")

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
  // Floats
  //
  mkTest("main/test/flix/Test.Exp.Float32.flix")
  mkTest("main/test/flix/Test.Exp.Float64.flix")

  //
  // Force
  //
  mkTest("main/test/flix/Test.Exp.Force.flix")

  //
  // Hole
  //
  mkTest("main/test/flix/Test.Exp.Hole.flix")

  //
  // IfThenElse
  //
  mkTest("main/test/flix/Test.Exp.IfThenElse.flix")

  //
  // Infix
  //
  mkTest("main/test/flix/Test.Exp.Infix.flix")

  //
  // Int
  //
  mkTest("main/test/flix/Test.Exp.Int8.flix")
  mkTest("main/test/flix/Test.Exp.Int16.flix")
  mkTest("main/test/flix/Test.Exp.Int32.flix")
  mkTest("main/test/flix/Test.Exp.Int64.flix")

  //
  // Interpolation
  //
  mkTest("main/test/flix/Test.Exp.Interpolation.flix")

  //
  // JVM
  //
  mkTest("main/test/flix/Test.Exp.Jvm.GetField.flix")
  mkTest("main/test/flix/Test.Exp.Jvm.GetStaticField.flix")
  mkTest("main/test/flix/Test.Exp.Jvm.GetFieldStaticInnerClass.flix")
  mkTest("main/test/flix/Test.Exp.Jvm.GetStaticFieldStaticInnerClass.flix")
  mkTest("main/test/flix/Test.Exp.Jvm.GetFieldDoubleNestedClass.flix")
  mkTest("main/test/flix/Test.Exp.Jvm.InvokeConstructor.flix")
  mkTest("main/test/flix/Test.Exp.Jvm.InvokeMethod.flix")
  mkTest("main/test/flix/Test.Exp.Jvm.InvokeStaticMethod.flix")
  mkTest("main/test/flix/Test.Exp.Jvm.PutField.flix")
  mkTest("main/test/flix/Test.Exp.Jvm.PutStaticField.flix")
  mkTest("main/test/flix/Test.Exp.Jvm.TryCatch.flix")


  //
  // Kind
  //
  mkTest("main/test/flix/Test.Kind.Class.flix")
  mkTest("main/test/flix/Test.Kind.Def.flix")
  mkTest("main/test/flix/Test.Kind.Enum.flix")
  mkTest("main/test/flix/Test.Kind.Instance.flix")
  mkTest("main/test/flix/Test.Kind.TypeAlias.flix")

  //
  // Lambda
  //
  mkTest("main/test/flix/Test.Exp.Lambda.Match.flix")

  //
  // Lazy
  //
  mkTest("main/test/flix/Test.Exp.Lazy.flix")

  //
  // Let
  //
  mkTest("main/test/flix/Test.Exp.Let.flix")
  mkTest("main/test/flix/Test.Exp.Let.Match.flix")
  mkTest("main/test/flix/Test.Exp.Let.MatchStar.flix")
  mkTest("main/test/flix/Test.Exp.Let.Rec.flix")

  //
  // List
  //
  mkTest("main/test/flix/Test.Exp.List.flix")

  //
  // Null
  //
  mkTest("main/test/flix/Test.Exp.Null.flix")

  //
  // Match
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
  // Record
  //
  mkTest("main/test/flix/Test.Exp.Record.Extend.flix")
  mkTest("main/test/flix/Test.Exp.Record.Literal.flix")
  mkTest("main/test/flix/Test.Exp.Record.Multiple.flix")
  mkTest("main/test/flix/Test.Exp.Record.Polymorphism.flix")
  mkTest("main/test/flix/Test.Exp.Record.Restrict.flix")
  mkTest("main/test/flix/Test.Exp.Record.Select.flix")
  mkTest("main/test/flix/Test.Exp.Record.Update.flix")

  //
  // Reference
  //
  mkTest("main/test/flix/Test.Exp.Ref.Assign.flix")
  mkTest("main/test/flix/Test.Exp.Ref.Ref.flix")
  mkTest("main/test/flix/Test.Exp.Ref.Deref.flix")
  mkTest("main/test/flix/Test.Exp.Ref.Precedence.flix")

  //
  // Reify
  //
  mkTest("main/test/flix/Test.Exp.Reify.flix")
  mkTest("main/test/flix/Test.Exp.ReifyType.flix")

  //
  // Tag
  //
  mkTest("main/test/flix/Test.Exp.Tag.flix")
  mkTest("main/test/flix/Test.Exp.Tag.Lambda.flix")

  //
  // Tuple
  //
  mkTest("main/test/flix/Test.Exp.Tuple.flix")

  //
  // Unary
  //
  mkTest("main/test/flix/Test.Exp.Unary.Arithmetic.flix")
  mkTest("main/test/flix/Test.Exp.Unary.Bitwise.flix")
  mkTest("main/test/flix/Test.Exp.Unary.Logic.flix")

  //
  // Unit
  //
  mkTest("main/test/flix/Test.Exp.Unit.flix")

  //
  // Stm
  //
  mkTest("main/test/flix/Test.Exp.Stm.flix")

  //
  // String
  //
  mkTest("main/test/flix/Test.Exp.String.flix")

  //
  // Predicate
  //
  mkTest("main/test/flix/Test.Predicate.Guard.flix")
  mkTest("main/test/flix/Test.Predicate.Nullary.flix")

  //
  // Stratified Negation
  //
  mkTest("main/test/flix/Test.Stratification.flix")

  //
  // Term
  //
  mkTest("main/test/flix/Test.Term.Apply.flix")
  mkTest("main/test/flix/Test.Term.Lit.flix")
  mkTest("main/test/flix/Test.Term.Lit.Option.flix")
  mkTest("main/test/flix/Test.Term.Lit.Result.flix")
  mkTest("main/test/flix/Test.Term.Lit.List.flix")
  mkTest("main/test/flix/Test.Term.Lit.Set.flix")
  mkTest("main/test/flix/Test.Term.Var.CapturedVar.flix")
  mkTest("main/test/flix/Test.Term.Var.QuantVar.flix")
  mkTest("main/test/flix/Test.Term.Var.WildVar.flix")

  //
  // Type Alias
  //
  mkTest("main/test/flix/Test.TypeAlias.Rel.flix")

  //
  // Unused
  //
  mkTest("main/test/flix/Test.Unused.Tag.flix")
  mkTest("main/test/flix/Test.Unused.Var.flix")

  //
  // Use
  //
  mkTest("main/test/flix/Test.Use.Def.flix")
  mkTest("main/test/flix/Test.Use.Sig.flix")
  mkTest("main/test/flix/Test.Use.Tag.flix")
  mkTest("main/test/flix/Test.Use.Type.flix")

  //
  // Integration
  //
  mkTest("main/test/flix/Test.Integ.Class.Schema.flix")
  mkTest("main/test/flix/Test.Integ.Fixpoint.TypeAlias.flix")
  mkTest("main/test/flix/Test.Integ.Enum.TypeAlias.flix")

}
