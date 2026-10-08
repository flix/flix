/*
 * Copyright 2025 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api

import java.io.IOException
import java.nio.file.{Files, Paths, StandardOpenOption}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

object CompilerLog {

  /**
    * Appends the given message `m` to the `compiler.log` file, if it exists.
    */
  def log(m: String): Unit = {
    val p = Paths.get("./compiler.log")
    if (Files.exists(p) && Files.isRegularFile(p) && Files.isWritable(p)) {
      try {
        val writer = Files.newBufferedWriter(p, StandardOpenOption.APPEND)
        writer.append(s"[${getTimeStamp()}] $m")
        writer.newLine()
        writer.close()
      } catch {
        case ex: IOException => ex.printStackTrace()
      }
    }
  }

  /**
    * Returns the current time.
    */
  private def getTimeStamp(): String = {
    val now = LocalDateTime.now()
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
    now.format(formatter)
  }

}
