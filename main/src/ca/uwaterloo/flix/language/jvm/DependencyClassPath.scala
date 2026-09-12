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
  * from a growing set of JARs and class directories.
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
final class DependencyClassPath extends ClassFileLocator {

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
