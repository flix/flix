/*
 * Copyright 2026 Magnus Madsen
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

object WasmImportInterface {

  case class Id(namespace: String, packageName: String, interfaceName: String, version: String) {
    def qualifiedInterface: String = s"$namespace:$packageName/$interfaceName@$version"

    def cFunctionName(func: String): String =
      s"${sanitize(namespace)}_${sanitize(packageName)}_${sanitize(interfaceName)}_${sanitize(func)}"

    def depFileName: String =
      s"${sanitize(namespace)}_${sanitize(packageName)}_${sanitize(version)}.wit"

    def jsModuleBaseName: String =
      s"${sanitize(namespace)}_${sanitize(packageName)}_${sanitize(interfaceName)}_${sanitize(version)}"

    def jsModuleFileName: String =
      s"$jsModuleBaseName.js"

    def jsSetterName: String =
      s"__set_${jsModuleBaseName}_impl"
  }

  private val InterfacePattern = raw"([a-z][a-z0-9-]*):([a-z][a-z0-9-]*)/([a-z][a-z0-9-]*)@([0-9]+\.[0-9]+\.[0-9]+)".r
  private val FuncPattern = raw"[a-z][a-z0-9-]*".r

  def parse(s: String): Option[Id] = s match {
    case InterfacePattern(ns, pkg, iface, version) => Some(Id(ns, pkg, iface, version))
    case _ => None
  }

  def isValidFuncName(s: String): Boolean = FuncPattern.matches(s)

  def sanitize(s: String): String = s.map {
    case c if c.isLetterOrDigit => c
    case _ => '_'
  }

  def jsName(s: String): String = {
    val parts = s.split("-").toList.filter(_.nonEmpty)
    parts match {
      case Nil => throw new IllegalStateException("empty wasm import name")
      case head :: tail => head + tail.map(capitalize).mkString
    }
  }

  private def capitalize(s: String): String =
    if (s.isEmpty) s else s"${s.head.toUpper}${s.tail}"
}
