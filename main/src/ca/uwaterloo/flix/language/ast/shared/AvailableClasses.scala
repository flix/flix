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

import ca.uwaterloo.flix.util.collection.MultiMap

/**
 * @param byPackage a map from a package name to a set of classes (and interfaces) in that package.
 * @param byClass a map from a class (or interface) to the packages that it occurs in.
 */
case class AvailableClasses(byPackage: MultiMap[List[String], String], byClass: MultiMap[String, List[String]]){
  /**
   * Returns `this` AvailableClasses extended with additional mappings from package names to class names.
   */
  def ++(newMapByPackage: MultiMap[List[String], String]): AvailableClasses = {
    val newMapByClass = AvailableClasses.byPackage2ByClass(newMapByPackage)
    AvailableClasses(byPackage ++ newMapByPackage, byClass ++ newMapByClass)
  }
}

object AvailableClasses {
  /**
    * Returns the empty available classes.
    */
  def empty: AvailableClasses = AvailableClasses(MultiMap.empty, MultiMap.empty)

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

  /**
    * Returns the available classes given the multimap from package names to class names.
    */
  def apply(byPackage: MultiMap[List[String], String]): AvailableClasses =
    AvailableClasses(byPackage, byPackage2ByClass(byPackage))
}
