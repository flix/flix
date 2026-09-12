/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.jvm

import java.lang.constant.ClassDesc

/**
  * The packages and [[ClassDesc]]s of the Flix classes that the compiler refers to by name.
  *
  * The classes and interfaces of the JDK live in [[JavaClasses]].
  */
object FlixClasses {

  /** The `dev.flix.runtime` package, which holds the classes of the Flix runtime. */
  val RuntimePackage: List[String] = List("dev", "flix", "runtime")

  /** The `dev.flix.test` package, which holds the Java classes used by the test suite. */
  val TestPackage: List[String] = List("dev", "flix", "test")

  /**
    * The `dev.flix.runtime.Global` class, which holds the global id counter and the
    * command line arguments.
    *
    * The compiler generates this class, but `main/src/dev/flix/runtime/Global.java` provides
    * a mock of it so that its method signatures can be checked at compile time.
    */
  val Global: ClassDesc = ClassDesc.of(RuntimePackage.mkString("."), "Global")

}
