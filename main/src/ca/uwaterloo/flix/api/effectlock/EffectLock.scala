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

import ca.uwaterloo.flix.api.effectlock.serialization.Deserialize
import ca.uwaterloo.flix.language.ast.{Scheme, Symbol, TypedAst}
import ca.uwaterloo.flix.util.Result

object EffectLock {

  /**
    * Deserializes `json` to a collection of schemes pointed to by either a def or sig.
    */
  def deserialize(json: String): Result[(Map[Symbol.DefnSym, Scheme], Map[Symbol.SigSym, Scheme]), String] = {
    try {
      implicit val formats: org.json4s.Formats = serialization.formats
      val serializableAST = org.json4s.native.Serialization.read[Map[String, serialization.DefOrSig]](json)
      val sdefs = serializableAST.collect {
        case (_, defn: serialization.SDef) => defn
      }
      val ssigs = serializableAST.collect {
        case (_, sig: serialization.SSig) => sig
      }
      val defs = sdefs.map(Deserialize.deserializeDef).toMap
      val sigs = ssigs.map(Deserialize.deserializeSig).toMap
      Result.Ok((defs, sigs))
    } catch {
      case e: Exception => Result.Err(s"Unexpected JSON: ${e.getMessage}")
    }
  }

}
