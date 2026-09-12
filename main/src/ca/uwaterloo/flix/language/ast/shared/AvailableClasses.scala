/*
 * Copyright 2024 Chenhao Gao
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast.shared

import ca.uwaterloo.flix.util.collection.MultiMap

object AvailableClasses {
  /**
   * Returns the empty available classes.
   */
  def empty: AvailableClasses = AvailableClasses(MultiMap.empty, MultiMap.empty)

  /**
   * Returns the available classes given the multimap from package names to class names.
   */
  def apply(byPackage: MultiMap[List[String], String]): AvailableClasses =
    AvailableClasses(byPackage, byPackage2ByClass(byPackage))

  /**
   * Returns the map from class names to package names given the multimap from package names to class names.
   *
   * Example:
   *   given byPackage: {["java", "util"] -> ["List", "Map"] ...}
   *   returns: {"List" -> ["java", "util"], "Map" -> ["java", "util"] ...}
   */
  def byPackage2ByClass(byPackage: MultiMap[List[String], String]): MultiMap[String, List[String]] =
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
    * Returns `this` AvailableClasses extended with additional mappings from package names to class names.
    */
  def ++(newMapByPackage: MultiMap[List[String], String]): AvailableClasses = {
    val newMapByClass = AvailableClasses.byPackage2ByClass(newMapByPackage)
    AvailableClasses(byPackage ++ newMapByPackage, byClass ++ newMapByClass)
  }
}

