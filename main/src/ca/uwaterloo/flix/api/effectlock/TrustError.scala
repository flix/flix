package ca.uwaterloo.flix.api.effectlock

import ca.uwaterloo.flix.language.ast.{SourceLocation, TypedAst}

sealed trait TrustError

object TrustError {

  case class CheckedCastUse(exp: TypedAst.Expr.CheckedCast, loc: SourceLocation) extends TrustError

  case class UncheckedCastUse(exp: TypedAst.Expr.UncheckedCast, loc: SourceLocation) extends TrustError

  case class TryCatchUse(exp: TypedAst.Expr.TryCatch, loc: SourceLocation) extends TrustError

  case class ThrowUse(exp: TypedAst.Expr.Throw, loc: SourceLocation) extends TrustError

  case class InvokeConstructorUse(exp: TypedAst.Expr.InvokeConstructor, loc: SourceLocation) extends TrustError

  case class InvokeMethodUse(exp: TypedAst.Expr.InvokeMethod, loc: SourceLocation) extends TrustError

  case class InvokeStaticMethodUse(exp: TypedAst.Expr.InvokeStaticMethod, loc: SourceLocation) extends TrustError

  case class GetFieldUse(exp: TypedAst.Expr.GetField, loc: SourceLocation) extends TrustError

  case class PutFieldUse(exp: TypedAst.Expr.PutField, loc: SourceLocation) extends TrustError

  case class GetStaticFieldUse(exp: TypedAst.Expr.GetStaticField, loc: SourceLocation) extends TrustError

  case class PutStaticFieldUse(exp: TypedAst.Expr.PutStaticField, loc: SourceLocation) extends TrustError

  case class NewObjectUse(exp: TypedAst.Expr.NewObject, loc: SourceLocation) extends TrustError

}
