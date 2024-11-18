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

import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.util.collection.MultiMap

import scala.annotation.tailrec

/**
 * @param byPackage a map from a package name to a set of classes (and interfaces) in that package.
 * @param byClass a map from a class (or interface) to the packages that it occurs in.
 */
case class AvailableClasses(byPackage: MultiMap[List[String], String], byClass: MultiMap[String, List[String]])

object AvailableClasses {
  def apply(root: Root): AvailableClasses = {
    val byPackage = root.names
      val byClass = root.names.m.foldLeft(MultiMap.empty[String, List[String]]) {
        case (acc, (packageName, classNames)) =>
          classNames.foldLeft(acc) { (innerAcc, className) =>
            innerAcc + (className, packageName)
          }
      }
      AvailableClasses(byPackage, byClass)
  }
}
