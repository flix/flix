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

import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol}
import ca.uwaterloo.flix.util.collection.MultiMap

/**
  * Represents a dependency graph.
  */
case class DependencyGraph(defs: MultiMap[Symbol.DefnSym, SourceLocation]
                          ) {

  def toInputMap: MultiMap[Input.Text, Input.Text] = {
    val m = scala.collection.mutable.Map.empty[Input.Text, Set[Input.Text]]

    for ((sym, loc) <- defs.m) {
      sym.loc.sp1.source.input match {
        case src: Input.Text =>
          val dsts = loc.map(_.sp1.source.input).collect({ case dst: Input.Text => dst })
          val s = m.getOrElse(src, Set.empty)
          m.put(src, s ++ dsts)

        case _ => // nop
      }
    }

    MultiMap(m.toMap)
  }

  override def toString: String = {
    val sb = new StringBuilder()
    val m = toInputMap

    for ((src, dsts) <- m.m) {
      for (dst <- dsts) {
        sb.append(f"${src.name} --> ${dst.name}\n")
      }
    }

    sb.toString()
  }

}
