/*
 * Copyright 2020-2021 Jonathan Lindegaard Starup
 * Copyright 2017 Magnus Madsen
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

package ca.uwaterloo.flix.language.phase.sjvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ErasedAst.{Def, Root}
import ca.uwaterloo.flix.language.ast.{PType, Symbol}

object SjvmOps {

  /**
    * Performs name mangling on the given string `s` to avoid issues with special characters.
    */
    // TODO(JLS): missing |> i think, check for others
  def mangle(s: String): String = s.
    replace("+", JvmName.reservedDelimiter + "plus").
    replace("-", JvmName.reservedDelimiter + "minus").
    replace("*", JvmName.reservedDelimiter + "times").
    replace("/", JvmName.reservedDelimiter + "divide").
    replace("%", JvmName.reservedDelimiter + "modulo").
    replace("**", JvmName.reservedDelimiter + "exponentiate").
    replace("<", JvmName.reservedDelimiter + "lt").
    replace("<=", JvmName.reservedDelimiter + "le").
    replace(">", JvmName.reservedDelimiter + "gt").
    replace(">=", JvmName.reservedDelimiter + "ge").
    replace("==", JvmName.reservedDelimiter + "eq").
    replace("!=", JvmName.reservedDelimiter + "neq").
    replace("&&", JvmName.reservedDelimiter + "land").
    replace("||", JvmName.reservedDelimiter + "lor").
    replace("&", JvmName.reservedDelimiter + "band").
    replace("|", JvmName.reservedDelimiter + "bor").
    replace("^", JvmName.reservedDelimiter + "bxor").
    replace("<<", JvmName.reservedDelimiter + "lshift").
    replace(">>", JvmName.reservedDelimiter + "rshift")

  /**
    * Returns the namespace type for the given namespace `ns`.
    *
    * For example:
    *
    * <root>      =>  Ns
    * Foo         =>  Foo.Ns
    * Foo.Bar     =>  Foo.Bar.Ns
    * Foo.Bar.Baz =>  Foo.Bar.Baz.Ns
    */
  def getNamespaceClassType(ns: NamespaceInfo)(implicit root: Root, flix: Flix): JvmName = {
    val pkg = ns.ns
    val name = "Ns"
    JvmName(pkg, name)
  }

  /**
   * Returns `true` if the given definition `defn` is a law.
   */
  def nonLaw(defn: Def[_ <: PType]): Boolean = !defn.ann.isLaw

  /**
   * Returns the namespace info of the given definition symbol `sym`.
   */
  def getNamespace(sym: Symbol.DefnSym)(implicit root: Root, flix: Flix): NamespaceInfo = {
    NamespaceInfo(sym.namespace, Map.empty) // TODO: Magnus: Empty map.
  }

}
