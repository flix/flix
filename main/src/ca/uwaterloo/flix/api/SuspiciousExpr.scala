package ca.uwaterloo.flix.api

import ca.uwaterloo.flix.language.ast.{SourceLocation, TypedAst}

sealed trait SuspiciousExpr {
  val expr: TypedAst.Expr
  val loc: SourceLocation
}

object SuspiciousExpr {
  case class InstanceOfUse(expr: TypedAst.Expr.InstanceOf) extends SuspiciousExpr {
    val loc: SourceLocation = expr.loc
  }

  case class CheckedCastUse(expr: TypedAst.Expr.CheckedCast) extends SuspiciousExpr {
    val loc: SourceLocation = expr.loc
  }

  case class UncheckedCastUse(expr: TypedAst.Expr.UncheckedCast) extends SuspiciousExpr {
    val loc: SourceLocation = expr.loc
  }

  case class UnsafeUse(expr: TypedAst.Expr.Unsafe) extends SuspiciousExpr {
    val loc: SourceLocation = expr.loc
  }

  case class TryCatchUse(expr: TypedAst.Expr.TryCatch) extends SuspiciousExpr {
    val loc: SourceLocation = expr.loc
  }

  case class ThrowUse(expr: TypedAst.Expr.Throw) extends SuspiciousExpr {
    val loc: SourceLocation = expr.loc
  }

  case class InvokeConstructorUse(expr: TypedAst.Expr.InvokeConstructor) extends SuspiciousExpr {
    val loc: SourceLocation = expr.loc
  }

  case class InvokeMethodUse(expr: TypedAst.Expr.InvokeMethod) extends SuspiciousExpr {
    val loc: SourceLocation = expr.loc
  }

  case class InvokeStaticMethodUse(expr: TypedAst.Expr.InvokeStaticMethod) extends SuspiciousExpr {
    val loc: SourceLocation = expr.loc
  }

  case class GetFieldUse(expr: TypedAst.Expr.GetField) extends SuspiciousExpr {
    val loc: SourceLocation = expr.loc
  }

  case class PutFieldUse(expr: TypedAst.Expr.PutField) extends SuspiciousExpr {
    val loc: SourceLocation = expr.loc
  }

  case class GetStaticFieldUse(expr: TypedAst.Expr.GetStaticField) extends SuspiciousExpr {
    val loc: SourceLocation = expr.loc
  }

  case class PutStaticFieldUse(expr: TypedAst.Expr.PutStaticField) extends SuspiciousExpr {
    val loc: SourceLocation = expr.loc
  }

  case class NewObjectUse(expr: TypedAst.Expr.NewObject) extends SuspiciousExpr {
    val loc: SourceLocation = expr.loc
  }
}
