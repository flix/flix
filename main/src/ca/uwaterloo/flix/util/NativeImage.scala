/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.util

/**
  * Detects whether the compiler is running inside a GraalVM native image.
  */
object NativeImage {

  /**
    * `true` if the compiler is running inside a GraalVM native image.
    *
    * A native image sets the system property `org.graalvm.nativeimage.imagecode` to `"runtime"`.
    */
  val GraalEnabled: Boolean = System.getProperty("org.graalvm.nativeimage.imagecode") == "runtime"

}
