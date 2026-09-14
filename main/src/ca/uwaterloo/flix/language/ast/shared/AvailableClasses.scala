/*
 * Copyright 2024 Chenhao Gao
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
package ca.uwaterloo.flix.language.ast.shared

import ca.uwaterloo.flix.util.ClassList
import ca.uwaterloo.flix.util.collection.MultiMap

object AvailableClasses {

  /**
    * The classes and interfaces of the Java platform (see [[ClassList]]), indexed once per JVM.
    */
  lazy val Platform: AvailableClasses = fromClassFiles(ClassList.TheList)

  /**
    * Returns the available classes given a list of class file names, e.g. `java/util/zip/ZipUtils.class`.
    */
  def fromClassFiles(l: List[String]): AvailableClasses = {
    val byPackage = l.foldLeft[MultiMap[List[String], String]](MultiMap.empty) {
      case (acc, clazz) =>
        // Given a string `java/util/zip/ZipUtils.class` we convert it to the list `java :: util :: zip :: ZipUtils`.
        // We strip both the ".class" and ".java" suffix. Order should not matter.
        val clazzPath = clazz.stripSuffix(".class").stripSuffix(".java").split('/').toList

        // Create a multimap from all package prefixes to their sub packages and classes.
        // For example, if we have `java.lang.String`, we want to compute:
        // Nil                  => {java}
        // List("java")         => {lang}
        // List("java", "lang") => {String}
        clazzPath.inits.foldLeft(acc) {
          // Case 1: Nonempty path: split prefix and package
          case (acc1, prefix :+ pkg) => acc1 + (prefix -> pkg)
          // Case 2: Empty path: skip it
          case (acc1, _) => acc1
        }
    }
    AvailableClasses(byPackage, byPackage2ByClass(byPackage))
  }

  /**
   * Returns the map from class names to package names given the multimap from package names to class names.
   *
   * Example:
   *   given byPackage: {["java", "util"] -> ["List", "Map"] ...}
   *   returns: {"List" -> ["java", "util"], "Map" -> ["java", "util"] ...}
   */
  private def byPackage2ByClass(byPackage: MultiMap[List[String], String]): MultiMap[String, List[String]] =
    byPackage.m.foldLeft(MultiMap.empty[String, List[String]]) {
      case (acc, (packageName, classNames)) =>
        classNames.foldLeft(acc) { (innerAcc, className) =>
          innerAcc + (className, packageName)
        }
    }
}

/**
  * Represents the classes (and interfaces) available to the Flix program.
  *
  * Available classes come from two sources: the Java JDK (i.e. from [[ClassList]]) and from the JARs loaded into the project.
  *
  * @param byPackage a map from a package name to a set of classes (and interfaces) in that package.
  * @param byClass a map from a class (or interface) to the packages that it occurs in.
  */
case class AvailableClasses(byPackage: MultiMap[List[String], String], byClass: MultiMap[String, List[String]]) {
  /**
    * Returns `this` extended with the classes of `that`.
    */
  def ++(that: AvailableClasses): AvailableClasses =
    AvailableClasses(byPackage ++ that.byPackage, byClass ++ that.byClass)
}
