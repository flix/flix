/*
 * Copyright 2024 Magnus Madsen
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

import ca.uwaterloo.flix.util.collection.MultiMap

/**
  * A list of classes available on the Java Platform.
  */
object ClassList {

  /**
   * The class list for Java 21.
   *
   * Computed as follows:
   *
   * {{{
   * $ git clone git@github.com:openjdk/jdk.git
   * $ cd src/java.base/share/classes
   * $ find . -name "*.java"|sort|grep -v "jdk/internal"|grep -v "module-info.java" | grep -v "package-info.java"
   * }}}
   */
  val TheList: List[String] = LocalResource.get("/src/ca/uwaterloo/flix/util/ClassList.txt").split('\n').map(_.trim).toList

  /**
   * The map from class name to class path, built from TheList
   *
   * Example:
   *  - List -> java.util.List
   *  - File -> java.io.File
   */
  val TheMap: MultiMap[String, String] =
    ClassList.TheList.foldLeft(MultiMap.empty[String, String]) { (map, path) =>
      val className = path.stripSuffix(".java").split("/").last
      val formattedPath = path.stripSuffix(".java").replace("/", ".")
      map + (className, formattedPath)
    }
}
