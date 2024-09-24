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

import scala.collection.mutable

/**
  * A custom class loader to load generated class files.
  *
  * @param classes A map from internal names (strings) to JvmClasses.
  *
  * We pass the platform class loader as the parent to avoid it delegating to the system classloader
  * (otherwise compiled Flix code has access to all classes within the compiler)
  */
class FlixClassLoader(classes: Map[String, JvmClass])(implicit flix: Flix) extends ClassLoader(ClassLoader.getPlatformClassLoader) {

  /** An internal cache of already loaded classes. */
  private val cache = mutable.Map.empty[String, Class[_]]

  /** Finds the class with the given binary `name`. */
  override def loadClass(name: String): Class[_] = try {
    // Lookup the internal name in the cache to see if the class was already defined.
    cache.get(name) match {
      case None =>
        // Case 1: The class was not defined. Lookup the bytecode.
        classes.get(name) match {
          case None =>
            // Case 1.1: The internal name does not exist. Try the external JAR loader.
            try {
              val clazz = flix.jarLoader.loadClass(name)
              cache.put(name, clazz)
              clazz
            } catch {
              case _: ClassNotFoundException =>
                // Case 1.1.1: Last attempt, use the VM
                val clazz = super.loadClass(name)
                cache.put(name, clazz)
                clazz
            }
          case Some(jvmClass) =>
            // Case 1.2: The internal name was found. Define the class using its byte code.
            val clazz = defineClass(name, jvmClass.bytecode, 0, jvmClass.bytecode.length)
            // Store it in the cache.
            cache.put(name, clazz)
            // And return it.
            clazz
        }
      // Case 2: The class was already defined. Simply return it.
      case Some(clazz) => clazz
    }
  } catch {
    case ex: ClassFormatError =>
      throw new RuntimeException(s"Unable to load: '$name' class due to class format error: ${ex.getMessage}")
    case ex: NoClassDefFoundError =>
      throw new RuntimeException(s"Unable to load: '$name' class not found: ${ex.getMessage}")
  }
}
