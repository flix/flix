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

  object Documentation {

    def JavaScript: InputStream = getInputStream("/resources/documentation/app.js")

    def StyleSheet: InputStream = getInputStream("/resources/documentation/app.css")

  }

  object Library {

    def Prelude: InputStream = getInputStream("/library/Prelude.flix")

    def BigInt: InputStream = getInputStream("/library/BigInt.flix")

    def Option: InputStream = getInputStream("/library/Option.flix")

    def Result: InputStream = getInputStream("/library/Result.flix")

    def List: InputStream = getInputStream("/library/List.flix")

    def Bounded: InputStream = getInputStream("/library/Bounded.flix")

    def PartialOrder: InputStream = getInputStream("/library/PartialOrder.flix")

    def JoinLattice: InputStream = getInputStream("/library/JoinLattice.flix")

    def MeetLattice: InputStream = getInputStream("/library/MeetLattice.flix")

    def Set: InputStream = getInputStream("/library/Set.flix")

    def Map: InputStream = getInputStream("/library/Map.flix")

  }

  object Tutorials {

    def DeltaDebugging: InputStream = getInputStream("/tutorials/delta-debugging.flix")

    def Introduction: InputStream = getInputStream("/tutorials/introduction.flix")

    def Interpreter: InputStream = getInputStream("/tutorials/interpreter.flix")

    // TODO: Add lambda calculus.

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
      throw new RuntimeException(s"Resource: '$relativePath' not found. Corrupted JAR?")
    }
    inputStream
  }

}
