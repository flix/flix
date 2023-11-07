/*
 * Copyright 2023 Magnus Madsen
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
package ca.uwaterloo.flix.util

import ca.uwaterloo.flix.language.ast.SourceLocation
import org.json4s.JValue
import org.json4s.native.JsonMethods

import java.nio.file.{Files, LinkOption, Path}

object FileOps {

  /**
   * Writes the given string `s` to the given file path `p`.
   *
   * Creates the parent directory of `p` if needed.
   */
  def writeString(p: Path, s: String): Unit = {
    Files.createDirectories(p.getParent)

    // Check if the file already exists.
    if (Files.exists(p)) {
      // Check that the file is a regular file.
      if (!Files.isRegularFile(p, LinkOption.NOFOLLOW_LINKS)) {
        throw InternalCompilerException(s"Unable to write to non-regular file: '$p'.", SourceLocation.Unknown)
      }

      // Check if the file is writable.
      if (!Files.isWritable(p)) {
        throw InternalCompilerException(s"Unable to write to read-only file: '$p'.", SourceLocation.Unknown)
      }
    }

    Files.write(p, s.getBytes)
  }

  /**
   * Writes the given json `j` to the given file path `p`.
   *
   * Creates the parent directory of `p` if needed.
   */
  def writeJSON(p: Path, j: JValue): Unit = {
    FileOps.writeString(p, JsonMethods.pretty(JsonMethods.render(j)))
  }

}
