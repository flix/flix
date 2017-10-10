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
  * Loads generated JVM bytecode classes into a custom class loader.
  */
object BytecodeLoader {

  class FlixClassLoader extends ClassLoader {
    def define(name: String, bytes: Array[Byte]): Class[_] = {
      println(s"Define: $name")
      val clazz = defineClass(name, bytes, 0, bytes.length)
      println(s"Defined $name")
      clazz
    }
  }

  /**
    * Loads the given JVM `classes` using a custom class loader.
    */
  def loadAll(classes: Map[JvmName, JvmClass]): Map[JvmName, Class[_]] = {
    // Construct a fresh class loader.
    val loader = new FlixClassLoader

    // Load each class.
    val loadedClasses = classes.foldLeft(Map.empty[JvmName, Class[_]]) {
      case (macc, (jvmName, jvmClass)) =>
        val clazz = loader.define(jvmName.toInternalName, jvmClass.bytecode)
        macc + (jvmName -> clazz)
    }

    // TODO: Retrieve the method object for each Def$ and assign it to the appropriate AST.

    Map.empty // TODO
  }

}
