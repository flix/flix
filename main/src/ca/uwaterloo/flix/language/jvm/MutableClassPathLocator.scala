/*
 * Copyright 2026 Flix Authors
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
package ca.uwaterloo.flix.language.jvm

import net.bytebuddy.ClassFileVersion
import net.bytebuddy.dynamic.ClassFileLocator

import java.nio.file.{Files, Path}
import scala.collection.mutable

/**
  * A [[ClassFileLocator]] that reads class files from a growing set of JARs and directories.
  *
  * Entries are read directly from the archive rather than through a [[ClassLoader]]. A class
  * loader constructed at run time cannot serve resources inside a GraalVM native image, which
  * would make every class from a project's dependencies unresolvable.
  *
  * Entries are consulted in the order they were added.
  *
  * [[addPath]] is called while the compiler is being configured, before compilation starts.
  * [[locate]] is then called from the worker threads, which are created afterwards, so the
  * entries are safely published to them.
  */
final class MutableClassPathLocator extends ClassFileLocator {

  /**
    * The locators to consult, in order.
    */
  private val locators = mutable.ArrayBuffer.empty[ClassFileLocator]

  /**
    * Adds the JAR or class directory at `path`.
    *
    * Throws [[java.io.IOException]] if `path` cannot be opened.
    */
  def addPath(path: Path): Unit = {
    val version = ClassFileVersion.ofThisVm()
    val locator =
      if (Files.isDirectory(path)) ClassFileLocator.ForFolder.of(path.toFile, version)
      else ClassFileLocator.ForJarFile.of(path.toFile, version)
    locators += locator
  }

  /**
    * Returns the class file of `name` from the first entry that has it.
    */
  override def locate(name: String): ClassFileLocator.Resolution = {
    val it = locators.iterator
    while (it.hasNext) {
      val resolution = it.next().locate(name)
      if (resolution.isResolved) {
        return resolution
      }
    }
    new ClassFileLocator.Resolution.Illegal(name)
  }

  /**
    * Closes every entry.
    */
  override def close(): Unit = {
    locators.foreach(_.close())
    locators.clear()
  }

}
