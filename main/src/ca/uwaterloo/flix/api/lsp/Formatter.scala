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
import ca.uwaterloo.flix.language.ast.SyntaxTree.TreeKind.ParameterList
import ca.uwaterloo.flix.language.ast.TokenKind.{Colon, Comma}
import ca.uwaterloo.flix.language.ast.{SyntaxTree, Token}

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
      case Binary if tree.loc.isSingleLine        => formatBinaryExpression(tree)
      case ParameterList if tree.loc.isSingleLine => formatParameterList(tree)
      case _                                      => Nil
    }

    val editsFromChildren = tree.children.flatMap {
      case childTree: SyntaxTree.Tree => traverseTree(childTree)
      case _                          => Nil
    }
    editsHere ++ editsFromChildren
  }

  /**
    * Formats parameter lists by adding spaces after commas.
    * Adds a single space after commas in a parameter list on the same line.
    *
    * @param tree the parameter list tree
    * @return a list of text edits to apply for formatting
    */
  private def formatParameterList(tree: SyntaxTree.Tree): List[TextEdit] = {
    val tokens = getTokensInOrder(tree)

      val edits = tokens
        .sliding(2)
        .collect {
          case List(prev, next) =>
            if (prev.kind == Comma || prev.kind == Colon) createSeparatorTextEdit(prev, next, " ")
            else createSeparatorTextEdit(prev, next, "")
        }
      edits.toList
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
    *
    * @param tree the binary expression tree
    * @return a list of text edits to apply for formatting
    */
  private def formatBinaryExpression(tree: SyntaxTree.Tree): List[TextEdit] = {
    val childBoundaries = getChildBoundaries(tree)

    val edits = childBoundaries
      .sliding(2)
      .collect {
        case List((_, prevLast), (nextFirst, _)) =>
          createSeparatorTextEdit(prevLast, nextFirst, " ")
      }
    edits.toList
  }

  /**
    * Gets the first and last tokens of each child of the given syntax tree.
    *
    * @param tree the syntax tree
    * @return a list of tuples containing the first and last tokens of each child
    */
  private def getChildBoundaries(tree: SyntaxTree.Tree) = {
    val childBoundaries = tree.children.flatMap {
      case t: SyntaxTree.Tree =>
        for {
          first <- getLeftMostToken(t)
          last <- getRightMostToken(t)
        } yield (first, last)
      case tok: Token =>
        Some((tok, tok))
    }.toList
    childBoundaries
  }

  private def createSeparatorTextEdit(prevLast: Token, nextFirst: Token, seperator: String) = {
    TextEdit(
      Range(
        Position.from(prevLast.sp2),
        Position.from(nextFirst.sp1)
      ),
      seperator
    )
  }

  private def getTokensInOrder(tree: SyntaxTree.Tree): List[Token] = {
    tree.children.flatMap {
      case t: SyntaxTree.Tree => getTokensInOrder(t)
      case tok: Token => List(tok)
    }.toList
  }

  /**
    * Recursively gets the left-most token in the syntax tree.
    *
    * @param tree the syntax tree
    * @return an option containing the left-most token if found
    */
  private def getLeftMostToken(tree: SyntaxTree.Tree): Option[Token] = {
    tree.children.headOption.flatMap {
      case t: SyntaxTree.Tree => getLeftMostToken(t)
      case tok: Token => Some(tok)
    }
  }

  /**
    * Recursively gets the right-most token in the syntax tree.
    *
    * @param tree the syntax tree
    * @return an option containing the right-most token if found
    */
  private def getRightMostToken(tree: SyntaxTree.Tree): Option[Token] = {
    tree.children.lastOption.flatMap {
      case t: SyntaxTree.Tree => getRightMostToken(t)
      case tok: Token => Some(tok)
    }
  }

  private def findTreeBasedOnUri(root: SyntaxTree.Root, uri: String): Option[SyntaxTree.Tree] = {
    root.units.collectFirst { case (source, tree) if source.name == uri => tree }
  }

}
