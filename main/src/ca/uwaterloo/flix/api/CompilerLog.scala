/*
 * Copyright 2025 Magnus Madsen
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
package ca.uwaterloo.flix.api

import java.nio.file.{Files, Paths}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

object CompilerLog {

  /**
    * Logs the given message `m` to the `compiler.log` file if it exists.
    */
  def log(m: String): Unit = {
    val p = Paths.get("./compiler.log")
    if (Files.exists(p) && Files.isRegularFile(p) && Files.isWritable(p)) {
      val writer = Files.newBufferedWriter(p)
      writer.append(s"[$getTimeStamp] $m")
      writer.newLine()
      writer.close()
    }
  }

  /**
    * Returns the current time.
    */
  private def getTimeStamp(): String = {
    val now = LocalDateTime.now()
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")
    now.format(formatter)
  }

}
