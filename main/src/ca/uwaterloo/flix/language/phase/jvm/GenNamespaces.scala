/*
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

package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ExecutableAst.Root
import ca.uwaterloo.flix.language.ast.Type

object GenNamespaces {

  /**
    * Returns the set of namespaces classes for the given set of namespaces.
    */
  def gen(nss: Set[NamespaceInfo], root: Root)(implicit flix: Flix): Map[JvmName, JvmClass] = {
    //
    // Generate a namespace class for each namespace and collect the results in a map.
    //
    nss.foldLeft(Map.empty[JvmName, JvmClass]) {
      case (macc, ns) =>
        val clazz = genNamespaceClass(ns, root)
        macc + (clazz.name -> clazz)
    }
  }

  /**
    * Returns the namespace class for the given namespace `ns`.
    */
  private def genNamespaceClass(ns: NamespaceInfo, root: Root)(implicit flix: Flix): JvmClass = {
    ???
  }

}
