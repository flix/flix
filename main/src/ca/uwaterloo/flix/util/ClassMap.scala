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
package ca.uwaterloo.flix.util

/**
  * A map from available Java classes' name to their path
  */
object ClassMap {

  /**
    * The map from class name to class path, built from ClassList
    *
    * Example:
    *  - List -> java.util.List
    *  - File -> java.io.File
    */
  val TheMap: Map[String, String] = ClassList.TheList.map { path =>
    val className = path.split("/").last.stripSuffix(".java")
    val formattedPath = path.stripSuffix(".java").replace("/", ".")
    className -> formattedPath
  }.toMap
}
