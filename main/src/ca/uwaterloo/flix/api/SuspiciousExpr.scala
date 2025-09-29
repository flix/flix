/*
 * Copyright 2025 Jakob Schneider Villumsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
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
