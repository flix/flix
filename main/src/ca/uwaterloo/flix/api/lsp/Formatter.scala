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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.SyntaxTree.TreeKind.Expr.Binary
import ca.uwaterloo.flix.language.ast.SyntaxTree.TreeKind.ParameterList
import ca.uwaterloo.flix.language.ast.TokenKind.{Colon, Comma}
import ca.uwaterloo.flix.language.ast.{SyntaxTree, Token}
import ca.uwaterloo.flix.util.Result

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
    * @param uri  the file path of the syntax tree
    * @return a formatted source code as a list of text edits
    */
  def format(root: SyntaxTree.Root, uri: String): List[TextEdit] =
    findTreeBasedOnUri(root, uri).map(traverseTree).getOrElse(Nil)

  /**
    * Formats the given syntax tree for multiple source paths.
    * Returns a list of text edits for all source paths.
    *
    * @param root        the syntax tree root
    * @param sourcePaths the list of source file paths
    * @return a map of file paths to their corresponding list of text edits
    */
  def format(root: SyntaxTree.Root, sourcePaths: List[Path]): Map[Path, List[TextEdit]] = {
    sourcePaths.flatMap { path =>
      val uri = path.toString
      val edits = format(root, uri)
      if (edits.nonEmpty) Some(path -> edits) else None
    }.toMap
  }

  /**
    * Formats the files at the given source paths based on the syntax tree.
    * Applies the text edits directly to the files.
    *
    * @param root        the syntax tree root
    * @param sourcePaths the list of source file paths
    */
  def formatFiles(root: SyntaxTree.Root, sourcePaths: List[Path])(implicit flix: Flix): Unit = {
    // TODO: Better error handling strategy
    val edits = format(root, sourcePaths)
    edits.foreach {
      case (file, fileEdits) => applyTextEditsToFile(file, fileEdits)
    }
  }

  /**
    * Applies the given text edits to the file at the specified path.
    *
    * @param path  the file path
    * @param edits the list of text edits to apply
    */
  private def applyTextEditsToFile(path: Path, edits: List[TextEdit])(implicit flix: Flix): Unit = {
    isValidPath(path) match {
      case Result.Err(e: Throwable) => throw e
      case Result.Ok(()) =>
        val bytes = Files.readAllBytes(path)
        val src = new String(bytes, flix.defaultCharset)
        val updated = applyTextEditsToString(src, edits)
        Files.write(path, updated.getBytes(StandardCharsets.UTF_8))
    }
  }

  /**
    * Applies the given text edits to the source string.
    * Edits are applied in reverse order to maintain correct positions.
    *
    * @param src   the source string
    * @param edits the list of text edits to apply
    * @return the updated source string after applying the edits
    */
  private def applyTextEditsToString(src: String, edits: List[TextEdit]): String = {
    val sortedEdits = edits.sortBy(e => (e.range.start.line, e.range.start.character)).reverse
    val sb = new StringBuilder(src)
    val lineOffsets = computeLineOffsets(src) // start index of each line

    for (edit <- sortedEdits) {
      val start =
        lineOffsets(edit.range.start.line - 1) + (edit.range.start.character - 1)

      val end =
        lineOffsets(edit.range.end.line - 1) + (edit.range.end.character - 1)

      sb.replace(start, end, edit.newText)
    }

    sb.toString()
  }

  /**
    * Computes the starting offsets of each line in the source string.
    *
    * @param src the source string
    * @return an array of line starting offsets
    */
  private def computeLineOffsets(src: String): Array[Int] = {
    val lines = src.split("\n", -1)
    val offsets = new Array[Int](lines.length + 1)
    var currentOffset = 0
    for (i <- lines.indices) {
      offsets(i) = currentOffset
      currentOffset += lines(i).length + 1
    }
    offsets(lines.length) = currentOffset
    offsets
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
      case Binary if tree.loc.isSingleLine => formatBinaryExpression(tree)
      case ParameterList if tree.loc.isSingleLine => formatParameterList(tree)
      case _ => Nil
    }

    val editsFromChildren = tree.children.flatMap {
      case childTree: SyntaxTree.Tree => traverseTree(childTree)
      case _ => Nil
    }
    editsHere ++ editsFromChildren
  }

  /**
    * Formats parameter lists by adding spaces after commas.
    * Adds a single space after commas in a parameter list on the same line.
    *
    * Example:
    * Original source code
    * def foo(a:Int,b:String,c:Bool): Unit = {}
    * Formatted source code
    * def foo(a: Int, b: String, c: Bool): Unit = {}
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
    * Original source code
    * def main(): Int32 = 1+2*3*5+4-6/2
    * Formatted source code
    * def main(): Int32 = 1 + 2 * 3 * 5 + 4 - 6 / 2
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
    * @param prevLast  the last token of the previous child
    * @param nextFirst the first token of the next child
    * @param separator the separator string to insert between the tokens
    * @return a text edit to apply for formatting
    */
  private def createSeparatorTextEdit(prevLast: Token, nextFirst: Token, separator: String) = {
    TextEdit(
      Range(
        Position.from(prevLast.end),
        Position.from(nextFirst.start)
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
    * @param uri  the file path of the syntax tree
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

  /**
    * Validate that the given path can exist, is a regular file and is readable.
    *
    * @param path the file path
    * @return a result indicating success or an illegal argument exception
    */
  private def isValidPath(path: Path): Result[Unit, IllegalArgumentException] = {
    if (path == null) {
      return Result.Err(new IllegalArgumentException(s"'p' must be non-null."))
    }
    val pNorm = path.normalize()
    if (!Files.exists(pNorm)) {
      return Result.Err(new IllegalArgumentException(s"'$pNorm' must be a file."))
    }
    if (!Files.isRegularFile(pNorm)) {
      return Result.Err(new IllegalArgumentException(s"'$pNorm' must be a regular file."))
    }
    if (!Files.isReadable(pNorm)) {
      return Result.Err(new IllegalArgumentException(s"'$pNorm' must be a readable file."))
    }
    Result.Ok(())
  }
}
