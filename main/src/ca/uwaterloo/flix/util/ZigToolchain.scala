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

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.concurrent.TimeUnit

/**
 * Resolves a usable Zig command prefix.
 *
 * A plain `zig` binary is not enough on hosts that use wrappers such as `anyzig`.
 * The contract here is stricter: a candidate must be able to run a trivial `zig test`
 * end to end.
 */
object ZigToolchain {

  lazy val probeCommands: List[List[String]] = candidateCommands

  private lazy val CachedUsableCommand: Option[List[String]] = detectUsableCommand0()

  def usableCommand: Option[List[String]] = CachedUsableCommand

  def hasUsableCommand: Boolean = CachedUsableCommand.nonEmpty

  private def detectUsableCommand0(): Option[List[String]] = {
    candidateCommands.find(canRunSmokeTestWith)
  }

  private def candidateCommands: List[List[String]] = {
    val envOverride = Option(System.getenv("FLIX_ZIG_CMD"))
      .map(_.trim)
      .filter(_.nonEmpty)
      .map(_.split("\\s+").toList)

    envOverride.toList ++ List(
      List("zig"),
      List("zig", "0.15.2"),
      List("zig", "master"),
    )
  }

  private def canRunSmokeTestWith(zigCmd: List[String]): Boolean = {
    try {
      val smokeDir = Files.createTempDirectory("flix-zig-smoke-")
      try {
        val smokeFile = smokeDir.resolve("smoke.zig")
        Files.writeString(
          smokeFile,
          """const std = @import("std");
            |test "smoke" { try std.testing.expect(true); }
            |""".stripMargin,
          StandardCharsets.UTF_8,
        )

        val cmd = zigCmd ++ List("test", smokeFile.toString)
        val p = new ProcessBuilder(cmd: _*)
          .directory(smokeDir.toFile)
          .redirectErrorStream(true)
          .start()

        p.waitFor(10, TimeUnit.SECONDS) && p.exitValue() == 0
      } finally {
        deleteRecursive(smokeDir)
      }
    } catch {
      case _: IOException => false
      case _: InterruptedException => false
    }
  }

  private def deleteRecursive(root: java.nio.file.Path): Unit = {
    if (!Files.exists(root)) return
    val stream = Files.walk(root)
    try {
      import scala.jdk.CollectionConverters.*
      stream.iterator().asScala.toList.sortBy(_.getNameCount).reverse.foreach(Files.deleteIfExists)
    } finally {
      stream.close()
    }
  }
}
