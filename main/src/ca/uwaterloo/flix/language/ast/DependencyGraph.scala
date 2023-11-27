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
package ca.uwaterloo.flix.language.ast

import java.util.concurrent.ConcurrentHashMap

import scala.jdk.CollectionConverters._

class DependencyGraph(root: TypedAst.Root) {

  private val dependencies: ConcurrentHashMap[(Source, Target), ()] = new ConcurrentHashMap()

  def lookupEnumSym(sym: Symbol.EnumSym)(implicit src: Source): TypedAst.Enum = {
    val result = root.enums(sym)
    val target = Target.Enum(result.sym)
    dependencies.put((src, target), ())
    result
  }

  override def toString: String = {
    val sb = new StringBuilder()
    for (((src, target), _) <- dependencies.asScala) {
      sb.append(s"$src  -->  $target")
    }
    sb.toString()
  }
}

sealed trait Source

object Source {

  case class Def(sym: Symbol.DefnSym) extends Source

}

sealed trait Target

object Target {

  case class Enum(sym: Symbol.EnumSym) extends Target

}
