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
  * Loads generated JVM bytecode classes using a custom class loader.
  */
object BytecodeLoader {

  /**
    * Loads the given JVM `classes` using a custom class loader.
    */
  def loadAll(classes: Map[JvmName, JvmClass]): Map[JvmName, Class[_]] = {
    //
    // Compute a map from internal names (strings) to JvmClasses.
    //
    val m = classes.foldLeft(Map.empty[String, JvmClass]) {
      case (macc, (jvmName, jvmClass)) => macc + (jvmName.toInternalName -> jvmClass)
    }

    //
    // Instantiate the Flix class loader with this map.
    //
    val loader = new FlixClassLoader(m)

    //
    // Attempt to load each class using its internal name.
    //
    classes.foldLeft(Map.empty[JvmName, Class[_]]) {
      case (macc, (jvmName, jvmClass)) => macc + (jvmName -> loader.loadClass(jvmName.toInternalName))
    }
  }

}
