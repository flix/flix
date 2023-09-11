/*
 * Copyright 2023 Jonathan Lindegaard Starup, Jakob Schneider Villumsen
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
  * Represents all Flix types that are not object on the JVM including Void.
  */
trait VoidableType {
  /**
    * Returns a descriptor for the type. `Void` has descriptor `"V"`.
    */
  def toDescriptor: String
}

object VoidableType {
  case object Void extends VoidableType {
    override val toDescriptor: String = "V"

    /**
      * The erased string representation used in JVM names.
      */
    val toErasedString: String = "Void"
  }
}
