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

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

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
    * Formats the given syntax tree for multiple source paths.
    * Returns a list of text edits for all source paths.
    *
    * @param root the syntax tree root
    * @param sourcePaths the list of source file paths
    * @return a map of file paths to their corresponding list of text edits
    */
  def format(root: SyntaxTree.Root, sourcePaths: List[Path]): Map[Path, List[TextEdit]] = {
    sourcePaths.flatMap { path =>
      val uri = path.toString
      val edits = format(root, uri)
      Console.println(s"Formatted ${edits.length} edits for file: $uri")
      if (edits.nonEmpty) Some(path -> edits) else None
    }.toMap
  }

  /**
    * Formats the files at the given source paths based on the syntax tree.
    * Applies the text edits directly to the files.
    *
    * @param root the syntax tree root
    * @param sourcePaths the list of source file paths
    */
  def formatFiles(root: SyntaxTree.Root, sourcePaths: List[Path]): Unit = {
    // TODO: Better error handling strategy
    val edits = format(root, sourcePaths)
    edits.foreach {
      case (file, fileEdits) => applyTextEditsToFile(Some(file), fileEdits)
    }
  }

  /**
    * Applies the given text edits to the file at the specified path.
    *
    * @param file the file path
    * @param edits the list of text edits to apply
    */
  def applyTextEditsToFile(file: Option[Path], edits: List[TextEdit]): Unit = {
    try {
      val bytes = Files.readAllBytes(file.get)
      val src = new String(bytes, StandardCharsets.UTF_8)
      val updated = applyTextEditsToString(src, edits)
      Files.write(file.get, updated.getBytes(StandardCharsets.UTF_8))
    }
    catch {
      case io: java.io.IOException =>
        Console.err.println(s"Failed to apply text edits to file: ${file.get.toString}. Error: ${io.getMessage}")
    }
  }

  /**
    * Applies the given text edits to the source string.
    * Edits are applied in reverse order to maintain correct positions.
    *
    * @param src the source string
    * @param edits the list of text edits to apply
    * @return the updated source string after applying the edits
    */
  private def applyTextEditsToString(src: String, edits: List[TextEdit]): String = {
    val sortedEdits = edits.sortBy(e => (e.range.start.line, e.range.start.character)).reverse
    sortedEdits.foldLeft(src) { (text, edit) =>
      val lines = text.split("\n").toBuffer
      val lineIdx = edit.range.start.line - 1
      val line = lines(lineIdx)

      val updatedLine =
        line.substring(0, edit.range.start.character - 1) +
          edit.newText +
          line.substring(edit.range.end.character - 1)

      lines(lineIdx) = updatedLine
      lines.mkString("\n")
    }
  }

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
    * Example:
    *  Original source code
    *   def foo(a:Int,b:String,c:Bool): Unit = {}
    *  Formatted source code
    *   def foo(a: Int, b: String, c: Bool): Unit = {}
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

  /**
    * Creates a [[TextEdit]] to replace the text between two tokens with the given separator.
    *
    * @param prevLast the last token of the previous child
    * @param nextFirst the first token of the next child
    * @param separator the separator string to insert between the tokens
    * @return a text edit to apply for formatting
    */
  private def createSeparatorTextEdit(prevLast: Token, nextFirst: Token, separator: String) = {
    TextEdit(
      Range(
        Position(prevLast.sp2.lineOneIndexed, prevLast.sp2.colOneIndexed),
        Position(nextFirst.sp1.lineOneIndexed, nextFirst.sp1.colOneIndexed)
      ),
      separator
    )
  }

  /**
    * Recursively gets all tokens in the syntax tree in order.
    *
    * @param tree the syntax tree
    * @return a list of tokens in order
    */
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

  /**
    * Finds the syntax tree corresponding to the given URI.
    *
    * @param root the syntax tree root
    * @param uri the file path of the syntax tree
    * @return an option containing the syntax tree if found
    */
  private def findTreeBasedOnUri(root: SyntaxTree.Root, uri: String): Option[SyntaxTree.Tree] = {
    // TODO: This is a temporary solution. We need a better way to map URIs to syntax trees.
    root.units.find {
      case (path, _) => path.toString == uri
    }.map {
      case (_, tree) => tree
    }
  }
}
