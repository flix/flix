/*
 * Copyright 2024 Holger Dal Mogensen
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
package ca.uwaterloo.flix.language.ast.shared

import ca.uwaterloo.flix.language.ast.Name
import ca.uwaterloo.flix.language.errors.ResolutionError

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

    case object Module extends Decl

    case object Struct extends Decl

    case object Trait extends Decl

    case object Type extends Decl
  }

  sealed trait Expr extends SyntacticContext

  object Expr {
    case object Constraint extends Expr

    case object OtherExpr extends Expr
  }

  case object Instance extends SyntacticContext

  case object Unknown extends SyntacticContext

}
