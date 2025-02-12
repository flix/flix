package ca.uwaterloo.flix.api.effectlock

import ca.uwaterloo.flix.language.ast.{SourceLocation, TypedAst}

/**
  * TODO maybe we can just remove this type entirely and return a list of expressions in TrustValidation
  * however this allows the caller to easily filter out the errors instead of pattern matching on the entire ast.
  */
sealed trait SuspiciousExpr {
  val expr: TypedAst.Expr
  val loc: SourceLocation
}

object SuspiciousExpr {

  case class InstanceOfUse(expr: TypedAst.Expr.InstanceOf, loc: SourceLocation) extends SuspiciousExpr

  case class CheckedCastUse(expr: TypedAst.Expr.CheckedCast, loc: SourceLocation) extends SuspiciousExpr

  case class UncheckedCastUse(expr: TypedAst.Expr.UncheckedCast, loc: SourceLocation) extends SuspiciousExpr

  case class UnsafeUse(expr: TypedAst.Expr.Unsafe, loc: SourceLocation) extends SuspiciousExpr

  case class TryCatchUse(expr: TypedAst.Expr.TryCatch, loc: SourceLocation) extends SuspiciousExpr

  case class ThrowUse(expr: TypedAst.Expr.Throw, loc: SourceLocation) extends SuspiciousExpr

  case class InvokeConstructorUse(expr: TypedAst.Expr.InvokeConstructor, loc: SourceLocation) extends SuspiciousExpr

  case class InvokeMethodUse(expr: TypedAst.Expr.InvokeMethod, loc: SourceLocation) extends SuspiciousExpr

  case class InvokeStaticMethodUse(expr: TypedAst.Expr.InvokeStaticMethod, loc: SourceLocation) extends SuspiciousExpr

  case class GetFieldUse(expr: TypedAst.Expr.GetField, loc: SourceLocation) extends SuspiciousExpr

  case class PutFieldUse(expr: TypedAst.Expr.PutField, loc: SourceLocation) extends SuspiciousExpr

  case class GetStaticFieldUse(expr: TypedAst.Expr.GetStaticField, loc: SourceLocation) extends SuspiciousExpr

  case class PutStaticFieldUse(expr: TypedAst.Expr.PutStaticField, loc: SourceLocation) extends SuspiciousExpr

  case class NewObjectUse(expr: TypedAst.Expr.NewObject, loc: SourceLocation) extends SuspiciousExpr

}
