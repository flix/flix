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
package ca.uwaterloo.flix.util

/**
  * Detects whether the compiler is running inside a GraalVM native image.
  */
object NativeImage {

  /**
    * `true` if the compiler is running inside a GraalVM native image.
    *
    * A native image sets the system property `org.graalvm.nativeimage.imagecode` to `"runtime"`.
    * The property is `"buildtime"` while the image is being built and absent on a regular JVM.
    */
  val isRuntime: Boolean = System.getProperty("org.graalvm.nativeimage.imagecode") == "runtime"

}
