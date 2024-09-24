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

/**
  * Represents a Java class (or interface).
  *
  * @param name     the name of the class (or interface).
  * @param bytecode the bytecode of the class (or interface).
  */
case class JvmClass(name: JvmName, bytecode: Array[Byte]) {
  /** Returns the hashCode of `this` JvmClass. */
  override def hashCode(): Int = name.hashCode()

  /** Returns `true` if `obj` is a JvmClass with the same JvmName. */
  override def equals(obj: scala.Any): Boolean = obj match {
    case that: JvmClass => this.name == that.name
    case _ => false
  }

  /** Returns a string representation of `this` JvmClass. */
  override def toString: String = s"JvmClass($name, ${bytecode.length} bytes)"
}
