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
import ca.uwaterloo.flix.language.ast.SyntaxTree
import ca.uwaterloo.flix.tools.fmt.PrettyPrinter
import ca.uwaterloo.flix.util.Result

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

/**
  * The FormatterLsp object provides functionality to format Flix source files using the [[PrettyPrinter]].
  */
object FormatterLsp {

  /**
    * Formats the files at the given source paths using [[PrettyPrinter]].
    * Applies a single [[TextEdit]] to each file.
    *
    * @param root        the syntax tree root
    * @param sourcePaths the list of source file paths
    */
  def formatFiles(root: SyntaxTree.Root, sourcePaths: List[Path])(implicit flix: Flix): Unit = {
    sourcePaths.foreach { path =>
      findTreeBasedOnUri(root, path.toString).foreach { tree =>
        applyTextEditsToFile(path, treeToTextEdits(tree))
      }
    }
  }

  /**
    * Formats the given syntax tree for the given URI using [[PrettyPrinter]].
    * Returns a single [[TextEdit]] replacing the entire document.
    *
    * @param root the syntax tree root
    * @param uri  the file URI
    * @return a list containing a single document text edit
    */
  def format(root: SyntaxTree.Root, uri: String): List[TextEdit] =
    findTreeBasedOnUri(root, uri).map(treeToTextEdits).getOrElse(Nil)

  private def treeToTextEdits(tree: SyntaxTree.Tree): List[TextEdit] = {
    val formatted = PrettyPrinter.format(tree)
    List(TextEdit(Range(Position(1, 1), Position(Int.MaxValue, 1)), formatted))
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
    val lineOffsets = computeLineOffsets(src)

    for (edit <- sortedEdits) {
      val start =
        lineOffsets(edit.range.start.line - 1) + (edit.range.start.character - 1)

      val endLineIdx = math.min(edit.range.end.line - 1, lineOffsets.length - 1)
      val end = math.min(
        lineOffsets(endLineIdx) + (edit.range.end.character - 1),
        sb.length()
      )

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
