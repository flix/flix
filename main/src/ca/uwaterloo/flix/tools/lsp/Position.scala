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
package ca.uwaterloo.flix.tools.lsp

import org.json4s.JValue
import org.json4s.JsonAST.JInt

object Position {
  def parse(json: JValue): Position = {
    val line = json \\ "line" match {
      case JInt(i) => i.toInt
      case v => throw new RuntimeException(s"Unexpected value: '$v'.")
    }
    val col = json \\ "col" match {
      case JInt(i) => i.toInt
      case v => throw new RuntimeException(s"Unexpected value: '$v'.")
    }
    Position(line, col)
  }
}

case class Position(line: Int, col: Int)
