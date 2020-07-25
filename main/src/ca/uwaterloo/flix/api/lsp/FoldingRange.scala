/*
 * Copyright 2020 Magnus Madsen
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

import org.json4s.JsonDSL._
import org.json4s._

/**
  * Represents a `FoldingRange` in LSP.
  *
  * @param startLine      The zero-based line number from where the folded range starts.
  * @param startCharacter The zero-based character offset from where the folded range starts. If not defined, defaults to the length of the start line.
  */
case class FoldingRange(startLine: Int, startCharacter: Option[Int]) {
  def toJSON: JObject =
    ("startLine" -> startLine) ~
      ("startCharacter" -> startCharacter) ~


  //
  //    /**
  //      * The zero-based line number where the folded range ends.
  //      */
  //    endLine: number;
  //
  //    /**
  //      * The zero-based character offset before the folded range ends. If not defined, defaults to the length of the end line.
  //      */
  //    endCharacter?: number;
  //
  //    /**
  //      * Describes the kind of the folding range such as `comment` or `region`. The kind
  //      * is used to categorize folding ranges and used by commands like 'Fold all comments'. See
  //      * [FoldingRangeKind](#FoldingRangeKind) for an enumeration of standardized kinds.
  //      */
  //    kind?: string;
  //  }

}
