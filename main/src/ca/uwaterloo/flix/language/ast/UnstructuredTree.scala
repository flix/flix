/*
 * Copyright 2023 Herluf Baggesen
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
import ca.uwaterloo.flix.language.errors.Parse2Error

object UnstructuredTree {

  sealed trait TreeKind {
    def debug_name: Option[String] = None
  }

  object TreeKind {
    case object Source extends TreeKind

    case object Doc extends TreeKind

    case object Comments extends TreeKind

    case object Annotations extends TreeKind

    case object Modifiers extends TreeKind

    case object Parameters extends TreeKind

    case object Parameter extends TreeKind

    case object Arguments extends TreeKind

    case object Argument extends TreeKind

    case object TypeParameters extends TreeKind

    case object TypeParameter extends TreeKind

    case object Kind extends TreeKind

    case object Operator extends TreeKind

    case object Case extends TreeKind

    sealed trait UsesOrImports extends TreeKind {
      override def debug_name: Option[String] = Some("UsesOrImports")
    }

    object UsesOrImports {
      case object UsesOrImports extends UsesOrImports

      case object Use extends UsesOrImports

      case object UseMany extends UsesOrImports

      case object Import extends UsesOrImports

      case object ImportMany extends UsesOrImports

      case object Alias extends UsesOrImports
    }

    /////// Declarations /////
    sealed trait Decl extends TreeKind {
      override def debug_name: Option[String] = Some("Decl")
    }

    object Decl {
      case object Module extends TreeKind

      case object Enum extends TreeKind

      case object RestrictableEnum extends TreeKind

      case object Law extends TreeKind

      case object Def extends TreeKind

      case object Class extends TreeKind

      case object Instance extends TreeKind

      case object Signature extends TreeKind

      case object AssociatedTypeSig extends TreeKind

      case object AssociatedTypeDef extends TreeKind

    }

    /////// NAMES ///////
    case object Ident extends TreeKind

    case object QName extends TreeKind

    /////// JVM /////
    sealed trait Pattern extends TreeKind {
      override def debug_name: Option[String] = Some("Pattern")
    }

    object Pattern {

      case object Pattern extends Pattern

      case object Variable extends Pattern

      case object Literal extends Pattern

      case object Tuple extends Pattern

      case object Tag extends Pattern

      case object Record extends Pattern

      case object RecordField extends Pattern

      case object FCons extends Pattern

    }

    /////// JVM /////
    sealed trait JvmOp extends TreeKind {
      override def debug_name: Option[String] = Some("JvmOp")
    }

    object JvmOp {

      case object JvmOp extends JvmOp

      case object Ascription extends JvmOp

      case object Signature extends JvmOp

      case object Method extends JvmOp

      case object Constructor extends JvmOp

      case object StaticMethod extends JvmOp

      case object GetField extends JvmOp

      case object PutField extends JvmOp

      case object StaticGetField extends JvmOp

      case object StaticPutField extends JvmOp

    }

    /////// EXPRESSIONS //////
    sealed trait Expr extends TreeKind {
      override def debug_name: Option[String] = Some("Expr")
    }

    object Expr {

      case object Expr extends Expr

      case object Ascribe extends Expr

      case object Literal extends Expr

      case object Tuple extends Expr

      case object Paren extends Expr

      case object Block extends Expr

      case object Binary extends Expr

      case object Unary extends Expr

      case object Call extends Expr

      case object Intrinsic extends Expr

      case object LetImport extends Expr

      case object LetRecDef extends Expr

      case object IfThenElse extends Expr

      case object UncheckedCast extends Expr

      case object UncheckedMaskingCast extends Expr

      case object CheckedTypeCast extends Expr

      case object CheckedEffectCast extends Expr

      case object Scope extends Expr

      case object LetMatch extends Expr

      case object Lambda extends Expr

      case object LambdaMatch extends Expr

      case object Hole extends Expr

      case object HoleVariable extends Expr

      case object Statement extends Expr

      case object Use extends Expr

      case object StringInterpolation extends Expr

      case object TypeMatch extends Expr

      case object TypeMatchRule extends Expr
    }

    ////// TYPES //////
    sealed trait Type extends TreeKind {
      override def debug_name: Option[String] = Some("Type")
    }

    object Type {

      case object Type extends Type

      case object Tuple extends Type

      case object Native extends Type

      case object Record extends Type

      case object Derivations extends Type

      case object Constraints extends Type

      case object Constraint extends Type

      case object RecordVariable extends Type

      case object RecordField extends Type

      case object Arguments extends Type

      case object Argument extends Type

      case object Apply extends Type

      case object Variable extends Type

      case object Function extends Type

      case object EffectSet extends Type

    }

    /**
     * A tree representing a parse-error.
     *
     * The actual error objects live in a list stored in the parser state for each source.
     *
     * @param error an index into an array of errors where the error message resides
     */
    case class ErrorTree(error: Parse2Error) extends TreeKind

  }

  case class Tree(kind: TreeKind, var loc: SourceLocation, var children: Array[Child]) {
    def toDebugString(nesting: Int = 1): String = {
      val kindName = kind.debug_name match {
        case Some(name) => s"$name.$kind"
        case None => s"$kind"
      }

      s"$kindName (${loc.beginLine}, ${loc.beginCol}) -> (${loc.endLine}, ${loc.endCol}) ${
        children.map {
          case Child.Token(token) => s"\n${"  " * nesting}'${token.text}'"
          case Child.Tree(tree) => s"\n${"  " * nesting}${tree.toDebugString(nesting + 1)}"
        }.mkString("")
      }"
    }
  }

  sealed trait Child

  object Child {
    case class Token(token: ca.uwaterloo.flix.language.ast.Token) extends Child

    case class Tree(tree: UnstructuredTree.Tree) extends Child
  }

}
