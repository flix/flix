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

import java.nio.file.{Path, Paths}

/**
  * Companion object for the [[JvmName]] class.
  */
object JvmName {

  /**
    * The Flix Context class.
    */
  val Context: JvmName = JvmName(Nil, "Context")

  /**
    * The `java.lang.BigInteger` name.
    */
  val BigInteger: JvmName = JvmName(List("java", "math"), "BigInteger")

  /**
    * The `java.lang.Object` name.
    */
  val Object: JvmName = JvmName(List("java", "lang"), "Object")

  /**
    * The `java.lang.String` name.
    */
  val String: JvmName = JvmName(List("java", "lang"), "String")

}

/**
  * Represents the name of a Java class or interface.
  *
  * @param pkg  the package name.
  * @param name the class or interface name.
  */
case class JvmName(pkg: List[String], name: String) {
  /**
    * Returns the type descriptor of `this` Java name.
    */
  def toDescriptor: String = "L" + pkg.mkString("/") + name + ";"

  /**
    * Returns the internal name of `this` Java name.
    */
  def toInternalName: String = pkg.mkString("/") + "/" + name

  /**
    * Returns the relative path of `this` Java name.
    */
  def toPath: Path = Paths.get(pkg.mkString("/"), name + ".class")
}
