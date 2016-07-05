/*
 * Copyright 2015-2016 Magnus Madsen
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

import java.io.InputStream
import java.nio.file.{Files, Paths}

object LocalResource {

  val RootPath = "main/src"

  object Tutorials {

    def DeltaDebugging: InputStream = getInputStream("/tutorials/delta-debugging.flix")

    def Introduction: InputStream = getInputStream("/tutorials/introduction.flix")

  }

  /**
    * Returns the an input stream for the given relative path.
    */
  private def getInputStream(relativePath: String): InputStream = {
    val path = Paths.get(RootPath + relativePath)

    val inputStream = if (Files.exists(path))
      Files.newInputStream(path)
    else
      getClass.getResourceAsStream(relativePath)

    if (inputStream == null) {
      throw new RuntimeException("Tutorial not found. Corrupted JAR?")
    }
    inputStream
  }

}
