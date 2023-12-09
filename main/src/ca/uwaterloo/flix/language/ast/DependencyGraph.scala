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
import scala.collection.mutable
import scala.jdk.CollectionConverters._

object DependencyGraph {
  def empty: DependencyGraph = new DependencyGraph
}

class DependencyGraph {

  private val dependencies: ConcurrentHashMap[(Source, Target), Unit] = new ConcurrentHashMap()

  def lookupEnumSym(sym: Symbol.EnumSym)(implicit src: Source, root: TypedAst.Root): TypedAst.Enum = {
    val result = root.enums(sym)
    val target = Target.Enum(result.sym)
    dependencies.put((src, target), ())
    result
  }

  def staleDefs(changeSet: ChangeSet): Set[Symbol.DefnSym] = {
    val result = mutable.ArrayBuffer.empty[Symbol.DefnSym]
    for (((src, target), _) <- dependencies.asScala) {
      (src, target) match {
        case (Source.Def(defnSym), Target.Enum(enumSym)) if changeSet.isChanged(enumSym.loc) =>
          result += defnSym
        case _ => // nop
      }
    }
    result.toSet
  }

  override def toString: String = {
    val sb = new StringBuilder()
    for (((src, target), _) <- dependencies.asScala) {
      sb.append(f"$src%-50s -> $target%-20s")
      sb.append("\n")
    }
    sb.toString()
  }
}

sealed trait Source {
  final override def toString: String = this match {
    case Source.Def(sym) => sym.toString
    case Source.Sig(sym) => sym.toString
  }
}

object Source {

  case class Def(sym: Symbol.DefnSym) extends Source

  case class Sig(sym: Symbol.SigSym) extends Source

}

sealed trait Target {
  final override def toString: String = this match {
    case Target.Enum(sym) => sym.toString
  }
}

object Target {

  case class Enum(sym: Symbol.EnumSym) extends Target

}
