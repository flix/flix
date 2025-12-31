/*
 * Copyright 2025 Jakob Schneider Villumsen
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
package ca.uwaterloo.flix.api.effectlock

import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.util.Result

object EffectLock {

  /**
    * Deserializes `json` to a collection of defs and sigs.
    */
  def deserialize(json: String): Result[(List[TypedAst.Def], List[TypedAst.Sig]), String] = {
    try {
      implicit val formats: org.json4s.Formats = serialization.formats
      val serializableAST = org.json4s.native.Serialization.read[Map[String, serialization.DefOrSig]](json)
      ???
    } catch {
      case e: Exception => Result.Err(s"Unexpected JSON: ${e.getMessage}")
    }
  }

}
