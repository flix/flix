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

import ca.uwaterloo.flix.language.ast.shared.TraitConstraint
import ca.uwaterloo.flix.language.errors.ResolutionError

import java.util.Objects

/**
  * A collection of AST nodes that are shared across multiple ASTs.
  */
object Ast {

  /**
    * Represents that `cst[tpe1]` and `tpe2` are equivalent types.
    */
  case class EqualityConstraint(cst: Ast.AssocTypeConstructor, tpe1: Type, tpe2: Type, loc: SourceLocation)

  /**
    *
    * Represents that `tpe1` and `tpe2` are equivalent types.
    */
  case class BroadEqualityConstraint(tpe1: Type, tpe2: Type) // TODO ASSOC-TYPES not really an AST feature

  /**
    * Represents that an instance on type `tpe` has the type constraints `tconstrs`.
    */
  case class Instance(tpe: Type, tconstrs: List[TraitConstraint])

  /**
    * Represents the super traits and instances available for a particular traits.
    */
  case class TraitContext(superTraits: List[Symbol.TraitSym], instances: List[Ast.Instance])

  /**
    * Represents the definition of an associated type.
    * If this associated type is named `Assoc`, then
    * Assoc[arg] = ret.
    */
  case class AssocTypeDef(arg: Type, ret: Type)

  /**
    * Represents a derivation on an enum (e.g. `enum E with Eq`).
    */
  case class Derivation(trt: Symbol.TraitSym, loc: SourceLocation)

  /**
    * Represents a list of derivations with a source location.
    *
    * The source location spans the entire `with X, Y, Z` clause.
    *
    * If there is no `with`-clause then the source location has zero
    * length and is positioned right after the enum type. For example,
    * if the enum is `enum Color {` then the source position would point
    * to the position right after `r` and have zero width.
    */
  case class Derivations(traits: List[Derivation], loc: SourceLocation)

  /**
    * Represents the way a variable is bound.
    */
  sealed trait BoundBy

  object BoundBy {

    /**
      * Represents a variable that is bound by a formal parameter.
      */
    case object FormalParam extends BoundBy

    /**
      * Represents a variable that is bound by a let-binding.
      */
    case object Let extends BoundBy

    /**
      * Represents a variable that is bound by a pattern.
      */
    case object Pattern extends BoundBy

    /**
      * Represents a variable that is bound by a select.
      */
    case object SelectRule extends BoundBy

    /**
      * Represents a variable that is bound by a catch rule.
      */
    case object CatchRule extends BoundBy

    /**
      * Represents a variable that is bound by a constraint.
      */
    case object Constraint extends BoundBy

    /**
      * Represents a variable that is bound by a local def.
      */
    case object LocalDef extends BoundBy
  }

  /**
    * Represents the text of a variable.
    */
  sealed trait VarText {

    /**
      * A measure of precision of the text.
      */
    private def precision: Int = this match {
      case VarText.Absent => 0
      case VarText.SourceText(_) => 2
    }

    /**
      * Returns true if `this` VarText is less precise than `that` VarText.
      *
      * More precise text should be preferred when choosing a text to use when substituting.
      *
      */
    def isStrictlyLessPreciseThan(that: VarText): Boolean = this.precision < that.precision
  }

  object VarText {
    /**
      * The variable has no associated text.
      */
    case object Absent extends VarText

    /**
      * The variable is associated with the string `s` taken directly from the source code.
      */
    case class SourceText(s: String) extends VarText
  }

  /**
    * Enum representing whether a type is ascribed or inferred.
    */
  sealed trait TypeSource

  object TypeSource {
    /**
      * The type is ascribed (present in the source code).
      */
    case object Ascribed extends TypeSource

    /**
      * The type is inferred (absent in the source code).
      */
    case object Inferred extends TypeSource
  }

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
