/*
 * Copyright 2017 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.language.phase.jvm.JvmClass

import scala.collection.mutable

/**
  * A custom class loader to load generated class files.
  *
  * @param classes   A map from binary names (strings) to JvmClasses.
  * @param jarLoader The class loader used to resolve classes from external JARs.
  *
  * We pass the platform class loader as the parent to avoid it delegating to the system classloader
  * (otherwise compiled Flix code has access to all classes within the compiler)
  */
class FlixClassLoader(classes: Map[String, JvmClass], jarLoader: ClassLoader) extends ClassLoader(ClassLoader.getPlatformClassLoader) {

  /**
    * An internal cache of already loaded classes.
    */
  private val cache = mutable.Map.empty[String, Class[?]]

  /**
    * Finds the class with the given binary `name`.
    */
  override def loadClass(name: String): Class[?] = try {
    // Lookup the binary name in the cache to see if the class was already defined.
    cache.get(name) match {
      case None =>
        // Case 1: The class was not defined. Lookup the bytecode.
        classes.get(name) match {
          case None =>
            // Case 1.1: The binary name does not exist. Try the external JAR loader.
            try {
              val clazz = jarLoader.loadClass(name)
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
            // Case 1.2: The binary name was found. Define the class using its byte code.
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
