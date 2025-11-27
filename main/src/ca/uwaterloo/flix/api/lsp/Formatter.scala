/*
 * Copyright 2025 Din Jakupi
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
package ca.uwaterloo.flix.api.lsp

import ca.uwaterloo.flix.language.ast.SyntaxTree.TreeKind.Expr.Binary
import ca.uwaterloo.flix.language.ast.{SyntaxTree, Token}
import ca.uwaterloo.flix.language.ast.SyntaxTree.TreeKind.Operator

import scala.collection.mutable.ListBuffer

/**
  * A formatter for Flix syntax trees.
  */
object Formatter {

  /**
    * Formats the given syntax tree.
    *
    * @param root the syntax tree root
    * @param uri the file path of the syntax tree
    * @return a formatted source code as a list of text edits
    */
  def format(root: SyntaxTree.Root, uri: String): List[TextEdit] =
    findTreeBasedOnUri(root, uri).map(traverseTree).getOrElse(Nil)

  /**
    * Traverses the syntax tree and collects [[TextEdit]]s for formatting.
    * Only formats binary expressions that are on a single line.
    *
    * @param tree the syntax tree
    * @return a list of text edits to apply for formatting
    */
  private def traverseTree(tree: SyntaxTree.Tree): List[TextEdit] = {
    val editsHere = tree.kind match {
      case Binary if tree.loc.isSingleLine => formatBinaryExpression(tree)
      case _                               => Nil
    }

    val editsFromChildren = tree.children.flatMap {
      case childTree: SyntaxTree.Tree => traverseTree(childTree)
      case _                          => Nil
    }
    editsHere ++ editsFromChildren
  }

  /**
    * Formats binary expression by adding spaces around operators.
    * Adds a single space between tokens and operators in binary expressions on the same line.
    *
    * Example:
    *   Original source code
    *     def main(): Int32 = 1+2*3*5+4-6/2
    *   Formatted source code
    *     def main(): Int32 = 1 + 2 * 3 * 5 + 4 - 6 / 2
    * @param tree the binary expression tree
    * @return a list of text edits to apply for formatting
    */
  private def formatBinaryExpression(tree: SyntaxTree.Tree): List[TextEdit] = {
    val edits = ListBuffer.empty[TextEdit]
    val operatorTree = tree.children.collectFirst {
      case t: SyntaxTree.Tree if t.kind == Operator => t
    }

    operatorTree.foreach { opTree =>
      val opToken = collectTokens(opTree).headOption

      opToken.foreach { op =>
        val leftToken = tree.children.headOption.flatMap {
          case t: SyntaxTree.Tree => getRightmostToken(t)
          case token: Token => Some(token)
        }

        val rightToken = tree.children.lastOption.flatMap {
          case t: SyntaxTree.Tree => getLeftmostToken(t)
          case token: Token => Some(token)
        }

        (leftToken, rightToken) match {
          case (Some(left), Some(right)) =>
            val beforeRange = Range(
              Position(left.sp2.lineOneIndexed, left.sp2.colOneIndexed),
              Position(op.sp1.lineOneIndexed, op.sp1.colOneIndexed)
            )
            val afterRange = Range(
              Position(op.sp2.lineOneIndexed, op.sp2.colOneIndexed),
              Position(right.sp1.lineOneIndexed, right.sp1.colOneIndexed)
            )

            edits += TextEdit(beforeRange, " ")
            edits += TextEdit(afterRange, " ")
          case _ => // In case tokes are missing we do nothing.
        }
      }
    }

    edits.toList
  }

  private def getLeftmostToken(tree: SyntaxTree.Tree): Option[Token] = {
    tree.children.headOption.flatMap {
      case token: Token => Some(token)
      case t: SyntaxTree.Tree => getLeftmostToken(t)
    }
  }

  private def getRightmostToken(tree: SyntaxTree.Tree): Option[Token] = {
    tree.children.lastOption.flatMap {
      case token: Token => Some(token)
      case t: SyntaxTree.Tree => getRightmostToken(t)
    }
  }

  private def collectTokens(child: SyntaxTree.Child): Array[Token] = child match {
    case token: Token => Array(token)
    case tree: SyntaxTree.Tree =>
      tree.children.flatMap(collectTokens)
  }

  private def findTreeBasedOnUri(root: SyntaxTree.Root, uri: String): Option[SyntaxTree.Tree] = {
    root.units.collectFirst { case (source, tree) if source.name == uri => tree }
  }

}
