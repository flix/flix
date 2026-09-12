/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.jvm

import net.bytebuddy.ClassFileVersion
import net.bytebuddy.dynamic.ClassFileLocator

import java.nio.file.{Files, Path}
import scala.collection.mutable

/**
  * The class path of a project's dependencies: a [[ClassFileLocator]] that reads class files
  * from a fixed list of JARs and class directories.
  *
  * Entries are read directly from the archive rather than through a [[ClassLoader]]. A class
  * loader constructed at run time cannot serve resources inside a GraalVM native image, which
  * would make every class from a project's dependencies unresolvable.
  *
  * Entries are consulted in the given order.
  *
  * Throws [[java.io.IOException]] if a path cannot be opened.
  */
final class DependencyClassPath(paths: List[Path]) extends ClassFileLocator {

  /**
    * The locators to consult, in order.
    */
  private val locators: List[ClassFileLocator] = {
    val version = ClassFileVersion.ofThisVm()
    val result = mutable.ArrayBuffer.empty[ClassFileLocator]
    for (path <- paths) {
      if (Files.isDirectory(path)) {
        result += ClassFileLocator.ForFolder.of(path.toFile, version)
      } else {
        result += ClassFileLocator.ForJarFile.of(path.toFile, version)
      }
    }
    result.toList
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
  }

}
