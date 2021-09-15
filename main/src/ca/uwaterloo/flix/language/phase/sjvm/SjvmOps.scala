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
    replace("+", "$plus").
    replace("-", "$minus").
    replace("*", "$asterisk").
    replace("/", "$fslash").
    replace("\\", "$bslash").
    replace("%", "$percent").
    replace("<", "$less").
    replace(">", "$greater").
    replace("=", "$eq").
    replace("&", "$ampersand").
    replace("|", "$bar").
    replace("^", "$caret").
    replace("~", "$tilde").
    replace("!", "$exclamation").
    replace("#", "$hashtag").
    replace(":", "$colon").
    replace("?", "$question").
    replace("@", "$at")

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
    * @return `true` if the given definition `defn` is not a law.
    */
  def nonLaw(defn: Def[_ <: PType]): Boolean = !defn.ann.isLaw

  /**
    * Returns the namespace info of the given definition symbol `sym`.
    */
  def getNamespace(sym: Symbol.DefnSym)(implicit root: Root, flix: Flix): NamespaceInfo = {
    NamespaceInfo(sym.namespace, Map.empty)
  }

}
