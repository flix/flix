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

    case object Def extends TreeKind

    case object Statement extends TreeKind

    case object Parameters extends TreeKind

    case object Parameter extends TreeKind

    case object Arguments extends TreeKind

    case object Argument extends TreeKind

    /////// NAMES ///////
    // NB: This naming follows [[Names]]
    case object Ident extends TreeKind

    case object QName extends TreeKind

    case object NName extends TreeKind

    case object JavaName extends TreeKind

    case object Pred extends TreeKind

    case object Label extends TreeKind

    /////// EXPRESSIONS //////
    sealed trait Expr extends TreeKind {
      override def debug_name: Option[String] = Some("Expr")
    }

    object Expr {

      case object Expr extends Expr

      case object Literal extends Expr

      case object Paren extends Expr

      case object Block extends Expr

      case object Binary extends Expr

      case object Unary extends Expr

      case object Call extends Expr
    }

    ////// TYPES //////
    sealed trait Type extends TreeKind {
      override def debug_name: Option[String] = Some("Type")
    }

    object Type {

      case object Type extends Type

      case object Tuple extends Type

      case object Record extends Type

      case object RecordVariable extends Type

      case object RecordField extends Type

      case object Arguments extends Type

      case object Argument extends Type

      case object Function extends Type

      case object Effect extends Type
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
