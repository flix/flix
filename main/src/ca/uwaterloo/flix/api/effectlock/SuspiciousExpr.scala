package ca.uwaterloo.flix.api.effectlock

import ca.uwaterloo.flix.language.ast.{SourceLocation, TypedAst}

/**
  * TODO maybe we can just remove this type entirely and return a list of expressions in TrustValidation
  * however this allows the caller to easily filter out the errors instead of pattern matching on the entire ast.
  */
sealed trait SuspiciousExpr {
  val expr: TypedAst.Expr
}

object SuspiciousExpr {

  case class InstanceOfUse(expr: TypedAst.Expr.InstanceOf) extends SuspiciousExpr

  case class CheckedCastUse(expr: TypedAst.Expr.CheckedCast) extends SuspiciousExpr

  case class UncheckedCastUse(expr: TypedAst.Expr.UncheckedCast) extends SuspiciousExpr

  case class UnsafeUse(expr: TypedAst.Expr.Unsafe) extends SuspiciousExpr

  case class TryCatchUse(expr: TypedAst.Expr.TryCatch) extends SuspiciousExpr

  case class ThrowUse(expr: TypedAst.Expr.Throw) extends SuspiciousExpr

  case class InvokeConstructorUse(expr: TypedAst.Expr.InvokeConstructor) extends SuspiciousExpr

  case class InvokeMethodUse(expr: TypedAst.Expr.InvokeMethod) extends SuspiciousExpr

  case class InvokeStaticMethodUse(expr: TypedAst.Expr.InvokeStaticMethod) extends SuspiciousExpr

  case class GetFieldUse(expr: TypedAst.Expr.GetField) extends SuspiciousExpr

  case class PutFieldUse(expr: TypedAst.Expr.PutField) extends SuspiciousExpr

  case class GetStaticFieldUse(expr: TypedAst.Expr.GetStaticField) extends SuspiciousExpr

  case class PutStaticFieldUse(expr: TypedAst.Expr.PutStaticField) extends SuspiciousExpr

  case class NewObjectUse(expr: TypedAst.Expr.NewObject) extends SuspiciousExpr

}
