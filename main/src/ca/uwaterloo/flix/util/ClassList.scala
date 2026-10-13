/*
 * Copyright 2024 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.util

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
