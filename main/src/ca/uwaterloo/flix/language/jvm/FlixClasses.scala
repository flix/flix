/*
 * Copyright 2026 Magnus Madsen
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
