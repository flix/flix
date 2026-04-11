/*
 * Copyright 2021 Matthew Lutze
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
package ca.uwaterloo.flix.util

import java.net.{URL, URLClassLoader}

/**
  * A class loader to which JARs can be added dynamically.
  *
  * We pass the platform class loader as the parent to avoid it delegating to the system classloader
  * (otherwise compiled Flix code has access to all classes within the compiler)
  */
class ExternalJarLoader extends URLClassLoader(Array.empty, ClassLoader.getPlatformClassLoader) {
  /**
    * Adds the URL to the class loader.
    */
  override def addURL(url: URL): Unit = {
    // just reimplements the superclass, but makes it public
    super.addURL(url)
  }

  override def findClass(name: String): Class[? <: Object] = {
    try {
      super.findClass(name)
    } catch {
      case e: ClassNotFoundException =>
        // Special case for compiler/runtime support classes under dev.flix.runtime.
        // These are intentionally visible to generated Flix code even though compiled
        // Flix programs do not otherwise inherit the compiler classpath.
        if (name == "dev.flix.runtime.Global" || name == "dev.flix.runtime.ChannelSupport" || name == "dev.flix.runtime.LockSupport" || name == "dev.flix.runtime.CancellationWakeup" || name == "dev.flix.runtime.RegionSupport")
          super.findSystemClass(name)
        // Special case for testing to allow us to load test classes
        else if (name.startsWith(("dev.flix.test.")))
          super.findSystemClass(name)
        else
          throw e
    }
  }
}
