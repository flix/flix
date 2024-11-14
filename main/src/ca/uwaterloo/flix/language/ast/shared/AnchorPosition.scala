/*
 * Copyright 2024 Magnus Madsen
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
    val sp = name.loc.sp1

    // We must consider two cases: whether the namespace is the root or is a proper module name.
    if (name.isRoot) {
      // If namespace is the root then the anchor position points to the start of the file.
      AnchorPosition(1, 1)
    } else {
      // Otherwise the anchor position points to *the line after* the namespace source location.
      AnchorPosition(sp.line + 1, sp.col)
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
case class AnchorPosition(line: Int, col: Short)
