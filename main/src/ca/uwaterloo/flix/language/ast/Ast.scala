/*
 * Copyright 2015-2016 Magnus Madsen
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

package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.errors.ResolutionError

/**
  * A collection of AST nodes that are shared across multiple ASTs.
  */
object Ast {

  /**
    * A constructor for a type alias. (Not a valid type by itself).
    */
  case class AliasConstructor(sym: Symbol.TypeAliasSym, loc: SourceLocation)

  /**
    * A constructor for an associated type. (Not a valid type by itself).
    */
  case class AssocTypeConstructor(sym: Symbol.AssocTypeSym, loc: SourceLocation)

  /**
    * A use of a Flix symbol or import of a Java class.
    */
  sealed trait UseOrImport

  object UseOrImport {

    /**
      * A use of a Flix declaration symbol.
      */
    case class Use(sym: Symbol, alias: Name.Ident, loc: SourceLocation) extends UseOrImport

    /**
      * An import of a Java class.
      */
    case class Import(clazz: Class[?], alias: Name.Ident, loc: SourceLocation) extends UseOrImport
  }

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

      case object Instance extends Decl

      case object Module extends Decl

      case object Struct extends Decl

      case object Trait extends Decl

      case object Type extends Decl
    }

    sealed trait Expr extends SyntacticContext

    object Expr {
      case object Constraint extends Expr

      case class InvokeMethod(tpe: ca.uwaterloo.flix.language.ast.Type, name: Name.Ident) extends Expr

      case object New extends Expr

      case class StaticFieldOrMethod(e: ResolutionError.UndefinedJvmStaticField) extends Expr

      case class StructAccess(e: ResolutionError.UndefinedStructField) extends Expr

      case object OtherExpr extends Expr
    }

    case object Import extends SyntacticContext

    sealed trait Pat extends SyntacticContext

    object Pat {
      case object OtherPat extends Pat
    }

    sealed trait Type extends SyntacticContext

    object Type {
      case object Eff extends Type

      case object OtherType extends Type
    }

    case object Use extends SyntacticContext

    case object WithClause extends SyntacticContext

    case object WithHandler extends SyntacticContext

    case object Unknown extends SyntacticContext

  }

}
