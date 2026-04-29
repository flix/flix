/*
 * Copyright 2025 Magnus Madsen
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

import ca.uwaterloo.flix.language.ast.shared.ClassProvider

import java.net.URI
import java.nio.file.{FileSystems, Files, Path}
import scala.collection.mutable

/**
  * A [[ClassProvider]] backed by the Java Runtime Image (`jrt:/`).
  *
  * On construction, walks `jrt:/modules` to discover all `.class` file paths.
  * No bytes are read at construction time. Class models are parsed lazily
  * on the first call to [[getClassModel]] and cached for subsequent lookups.
  *
  * Note: All classfile API calls go through [[JrtClassFileHelper]] (a Java class)
  * to avoid a Scala 2.13 issue with JDK 24+ sealed interfaces.
  */
class JrtClassProvider private(classPaths: Map[String, Path]) extends ClassProvider {

  /** Cache of already-parsed class models (stored as Any). */
  private val cache: mutable.Map[String, Any] = mutable.Map.empty

  override def getClassModel(internalName: String): Option[Any] = {
    cache.get(internalName).orElse {
      classPaths.get(internalName).map { path =>
        val bytes = Files.readAllBytes(path)
        val cm: Any = JrtClassFileHelper.parse(bytes)
        cache(internalName) = cm
        cm
      }
    }
  }

  override def classNames: Iterable[String] = classPaths.keys

}

object JrtClassProvider {

  /**
    * Walks the `jrt:/modules` filesystem to discover all `.class` files
    * in the running Java runtime. Only collects paths — no bytes are read.
    */
  def initialize(): JrtClassProvider = {
    val jrtFs = FileSystems.getFileSystem(URI.create("jrt:/"))
    val modulesRoot = jrtFs.getPath("/modules")
    val result = mutable.Map.empty[String, Path]
    val prefix = "/modules/"

    Files.walk(modulesRoot).forEach { path =>
      val s = path.toString
      if (s.endsWith(".class") && !s.endsWith("module-info.class")) {
        // Path format: /modules/<module>/java/util/List.class
        // We want the internal name: java/util/List
        val afterModules = s.indexOf('/', prefix.length)
        if (afterModules > 0) {
          val internalName = s.substring(afterModules + 1, s.length - ".class".length)
          result(internalName) = path
        }
      }
    }

    new JrtClassProvider(result.toMap)
  }

}
