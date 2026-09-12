/*
 * Copyright 2024 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast.shared

import ca.uwaterloo.flix.language.ast.{Name, SourcePosition}

/**
  * Companion object for [[AnchorPosition]].
  */
object AnchorPosition {
  /**
    * Returns the anchor position for the given namespace `name`.
    *
    * The anchor points to where `import`s and `use`s should be inserted.
    */
  def mkImportOrUseAnchor(name: Name.NName): AnchorPosition = {
    val sp = name.loc.start

    // We must consider two cases: whether the namespace is the root or is a proper module name.
    if (name.isRoot) {
      // If namespace is the root then the anchor position points to the start of the file and there are no spaces for indentation.
      AnchorPosition(1, 1, 0)
    } else {
      // Otherwise the anchor position points to the line after the module declaration with some amount of indentation. For example:
      //
      // mod Foo {
      //     mod Bar {
      // | <---- anchor point for "Bar" is here. Note col is 1, but spaces is 8.
      //
      AnchorPosition(sp.lineOneIndexed + 1, 1, spaces = (sp.colOneIndexed + 4 - 1).toShort)
    }
  }
}

/**
  * Represents an anchor position in a source file.
  *
  * An anchor (like an HTML anchor) points to some place in a source files where we may wish to insert source code.
  *
  * Hence, unlike [[SourcePosition]]s, [[AnchorPosition]] may point to "empty space" in a source file.
  *
  * Note: Do not construct the case class directly, but instead use the smart constructor [[AnchorPosition.mkImportOrUseAnchor]].
  *
  * @param line the line number. Must be one-indexed.
  * @param col  the column number. Must be one-indexed.
  */
case class AnchorPosition(line: Int, col: Short, spaces: Short)
