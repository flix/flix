/*
 * Copyright 2024 Holger Dal Mogensen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast.shared

/**
  * A common super-type for syntactic contexts.
  *
  * A syntactic context is an estimate of the syntactic construct a specific source position is inside.
  */
sealed trait SyntacticContext

object SyntacticContext {

  sealed trait Decl extends SyntacticContext

  object Decl {
    case object Enum extends Decl

    case object Effect extends Decl

    case object Instance extends SyntacticContext

    case object Module extends Decl

    case object Struct extends Decl

    case object Trait extends Decl

    case object Type extends Decl
  }

  sealed trait Expr extends SyntacticContext

  object Expr {
    case object Constraint extends Expr

    case object MatchBody extends Expr

    case object OtherExpr extends Expr
  }

  case object Unknown extends SyntacticContext

}
