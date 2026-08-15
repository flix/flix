/*
 * Copyright 2026 Werner Stein
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
package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast.{SimpleType, Symbol}

import scala.collection.mutable

/**
  * Renders the identity of an erasure-time specialization as a string.
  *
  * [[Eraser]] specializes enums and structs by the erased types they are applied to,
  * keeping a cache on the pair `(symbol, targs)`. This renders that pair, so the
  * specialized symbol can be named after what it is rather than after how many symbols
  * preceded it.
  *
  * The counterpart for definitions is
  * [[ca.uwaterloo.flix.language.phase.monomorph.SpecializationKey]]. This one is simpler
  * because [[SimpleType]] is already erased: there are no type variables, no aliases, and
  * no effects to canonicalize.
  */
object ErasureKey {

  /**
    * Returns the key identifying the specialization of enum `sym` at `targs`.
    */
  def ofEnum(sym: Symbol.EnumSym, targs: List[SimpleType]): String =
    key(sym.toString, targs)

  /**
    * Returns the key identifying the specialization of struct `sym` at `targs`.
    */
  def ofStruct(sym: Symbol.StructSym, targs: List[SimpleType]): String =
    key(sym.toString, targs)

  /**
    * Returns the key for `name` applied to `targs`.
    */
  private def key(name: String, targs: List[SimpleType]): String = {
    val sb = new mutable.StringBuilder()
    sb.append(name).append('|')
    renderAll(targs, sb)
    sb.toString()
  }

  /**
    * Appends the rendering of `tpe` to `sb`.
    */
  private def render(tpe: SimpleType, sb: mutable.StringBuilder): Unit = tpe match {
    case SimpleType.Array(t) =>
      sb.append("Array(")
      render(t, sb)
      sb.append(')')

    case SimpleType.Lazy(t) =>
      sb.append("Lazy(")
      render(t, sb)
      sb.append(')')

    case SimpleType.Tuple(tpes) =>
      sb.append("Tuple(")
      renderAll(tpes, sb)
      sb.append(')')

    case SimpleType.Enum(sym, targs) =>
      sb.append("Enum(").append(sym)
      if (targs.nonEmpty) {
        sb.append(' ')
        renderAll(targs, sb)
      }
      sb.append(')')

    case SimpleType.Struct(sym, targs) =>
      sb.append("Struct(").append(sym)
      if (targs.nonEmpty) {
        sb.append(' ')
        renderAll(targs, sb)
      }
      sb.append(')')

    case SimpleType.Arrow(targs, result) =>
      sb.append("Arrow(")
      renderAll(targs, sb)
      sb.append("->")
      render(result, sb)
      sb.append(')')

    case SimpleType.RecordExtend(label, value, rest) =>
      sb.append("RecordExtend(").append(label).append(' ')
      render(value, sb)
      sb.append(' ')
      render(rest, sb)
      sb.append(')')

    case SimpleType.ExtensibleExtend(cons, tpes, rest) =>
      sb.append("ExtensibleExtend(").append(cons.name).append(' ')
      renderAll(tpes, sb)
      sb.append(' ')
      render(rest, sb)
      sb.append(')')

    case SimpleType.Native(clazz) =>
      sb.append("Native(").append(clazz.getName).append(')')

    case nullary: Product =>
      sb.append(nullary.productPrefix)

    case other =>
      sb.append(other.getClass.getSimpleName.stripSuffix("$"))
  }

  /**
    * Appends the rendering of `tpes`, comma separated, to `sb`.
    */
  private def renderAll(tpes: List[SimpleType], sb: mutable.StringBuilder): Unit = {
    var first = true
    tpes.foreach { t =>
      if (!first) sb.append(',')
      first = false
      render(t, sb)
    }
  }

}
