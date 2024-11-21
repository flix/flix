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
    * $ git checkout jdk-21+0
    * $ cd src/java.base/share/classes
    * $ find . -name "*.java" | sort --ignore-case | grep -v "jdk/internal" | grep -v "module-info.java" | grep -v "package-info.java" | grep -v "com/sun/beans" | grep -v "com/sun/imageio" | grep -v "com/sun/java" | grep -v "com/sun/media" | grep -v "sun/awt" | grep -v "sun/font" | grep -v "sun/java2d" | grep -v "sun/print" | grep -v "sun/swing"
    * }}}
    *
    * Repeat the above for directories `src/java.desktop/share/classes` and `src/java.net.http/share/classes`.
    *
    * Finally, remove the `./` prefix, e.g., `./com/sun/...` should be `com/sun/...`.
    *
    */
  val TheList: List[String] = LocalResource.get("/src/ca/uwaterloo/flix/util/ClassList.txt").split('\n').map(_.trim).toList
}
