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

import ca.uwaterloo.flix.util.Result.{Err, Ok}

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.nio.file.{Path, Paths}

/**
 * Resolves compile and link flags for system libraries through `pkg-config`.
 *
 * We keep the result as raw ordered flags instead of trying to re-model all of
 * pkg-config's linker semantics inside Flix. The only normalization we do is to
 * absolutize path-bearing flags such as `-I`, `-L`, and `-F`.
 */
object PkgConfig {

  case class Resolution(compile: NativeCompileConfig = NativeCompileConfig(),
                        link: NativeLinkConfig = NativeLinkConfig())

  def resolve(packages: List[String], cwd: Path): Result[Resolution, String] = {
    val pkgs = packages.distinct
    if (pkgs.isEmpty) {
      Ok(Resolution())
    } else {
      for {
        compileFlags <- queryFlags("pkg-config --cflags", List("--cflags"), pkgs, cwd)
        linkFlags <- queryFlags("pkg-config --libs", List("--libs"), pkgs, cwd)
      } yield Resolution(
        compile = NativeCompileConfig(
          cflags = compileFlags
        ),
        link = NativeLinkConfig(
          flags = linkFlags
        )
      )
    }
  }

  private def queryFlags(label: String, args: List[String], packages: List[String], cwd: Path): Result[List[String], String] = {
    val cmd = List("pkg-config") ::: args ::: packages
    exec(cmd, cwd).flatMap { output =>
      tokenize(output).map(tokens => normalizePathFlags(tokens, cwd))
    }.mapErr(msg => s"$label failed for ${packages.mkString(", ")}: $msg")
  }

  private def exec(cmd: List[String], cwd: Path): Result[String, String] = {
    try {
      val pb = new ProcessBuilder(cmd*)
      pb.directory(cwd.toFile)
      pb.redirectErrorStream(true)
      val p = pb.start()
      val output = new String(p.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
      val exit = p.waitFor()
      if (exit == 0) Ok(output.trim)
      else Err(s"exit $exit\n$output".trim)
    } catch {
      case _: IOException =>
        Err("`pkg-config` is not available on PATH.")
      case _: InterruptedException =>
        Thread.currentThread().interrupt()
        Err("interrupted while invoking `pkg-config`.")
    }
  }

  /**
   * Tokenizes a shell-like flag string.
   *
   * pkg-config emits a flat string of compiler/linker flags. We only need basic
   * POSIX-style quoting and backslash escaping here.
   */
  private def tokenize(s: String): Result[List[String], String] = {
    sealed trait Mode
    case object Normal extends Mode
    case object SingleQuoted extends Mode
    case object DoubleQuoted extends Mode

    if (s.isBlank) return Ok(Nil)

    val result = scala.collection.mutable.ListBuffer.empty[String]
    val current = new StringBuilder

    var mode: Mode = Normal
    var escaping = false

    def flush(): Unit =
      if (current.nonEmpty) {
        result += current.result()
        current.clear()
      }

    s.foreach { ch =>
      if (escaping) {
        current.append(ch)
        escaping = false
      } else mode match {
        case Normal =>
          ch match {
            case '\\' => escaping = true
            case '\'' => mode = SingleQuoted
            case '"' => mode = DoubleQuoted
            case c if c.isWhitespace => flush()
            case c => current.append(c)
          }
        case SingleQuoted =>
          ch match {
            case '\'' => mode = Normal
            case c => current.append(c)
          }
        case DoubleQuoted =>
          ch match {
            case '\\' => escaping = true
            case '"' => mode = Normal
            case c => current.append(c)
          }
      }
    }

    if (escaping) {
      Err("unterminated escape sequence in pkg-config output.")
    } else mode match {
      case SingleQuoted => Err("unterminated single-quoted token in pkg-config output.")
      case DoubleQuoted => Err("unterminated double-quoted token in pkg-config output.")
      case Normal =>
        flush()
        Ok(result.toList)
    }
  }

  private def normalizePathFlags(tokens: List[String], cwd: Path): List[String] = {
    val normalized = scala.collection.mutable.ListBuffer.empty[String]
    val it = tokens.iterator
    while (it.hasNext) {
      val token = it.next()
      token match {
        case "-I" if it.hasNext =>
          normalized += "-I"
          normalized += normalizePath(it.next(), cwd)
        case "-L" if it.hasNext =>
          normalized += "-L"
          normalized += normalizePath(it.next(), cwd)
        case "-F" if it.hasNext =>
          normalized += "-F"
          normalized += normalizePath(it.next(), cwd)
        case flag if flag.startsWith("-I") && flag.length > 2 =>
          normalized += s"-I${normalizePath(flag.drop(2), cwd)}"
        case flag if flag.startsWith("-L") && flag.length > 2 =>
          normalized += s"-L${normalizePath(flag.drop(2), cwd)}"
        case flag if flag.startsWith("-F") && flag.length > 2 =>
          normalized += s"-F${normalizePath(flag.drop(2), cwd)}"
        case other =>
          normalized += other
      }
    }
    normalized.toList
  }

  private def normalizePath(raw: String, cwd: Path): String = {
    val path = Paths.get(raw)
    if (path.isAbsolute) path.normalize().toString
    else cwd.resolve(path).normalize().toString
  }
}
