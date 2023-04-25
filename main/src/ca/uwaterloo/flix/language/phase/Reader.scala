/*
 * Copyright 2017 Magnus Madsen
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

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.{Bootstrap, Flix}
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.Ast.{Input, Source}
import ca.uwaterloo.flix.language.ast.{Ast, ReadAst}
import ca.uwaterloo.flix.util.{StreamOps, Validation}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.collection.MultiMap

import java.io.IOException
import java.nio.file.{Files, Path}
import java.util.zip.ZipFile
import scala.collection.mutable
import scala.util.Using

/**
  * A phase to read inputs into memory.
  */
object Reader {

  /**
    * The name of the phase.
    */
  val phaseName = "Reader"

  /**
    * Reads the given source inputs into memory.
    */
  def run(inputs: List[Input])(implicit flix: Flix): Validation[ReadAst.Root, CompilationMessage] =
    flix.phase(phaseName) {

      val result = mutable.Map.empty[Source, Unit]
      for (input <- inputs) {
        input match {
          case Input.Text(name, text, stable) =>
            val src = Source(input, text.toCharArray, stable)
            result += (src -> ())

          case Input.TxtFile(path) =>
            val bytes = Files.readAllBytes(path)
            val str = new String(bytes, flix.defaultCharset)
            val arr = str.toCharArray
            val src = Source(input, arr, stable = false)
            result += (src -> ())

          case Input.PkgFile(path) =>
            for (src <- unpack(path)) {
              result += (src -> ())
            }
        }
      }

      val sources = result.toMap
      val names = findClasses()
      ReadAst.Root(sources, names).toSuccess
    }

  /**
    * Returns a list of sources extracted from the given flix package at path `p`.
    */
  private def unpack(p: Path)(implicit flix: Flix): List[Source] = {
    // Check that the path is a flix package.
    if (!Bootstrap.isPkgFile(p))
      throw new RuntimeException(s"The path '$p' is not a flix package.")

    // Open the zip file.
    Using(new ZipFile(p.toFile)) { zip =>
      // Collect all source and test files.
      val result = mutable.ListBuffer.empty[Source]
      val iterator = zip.entries()
      while (iterator.hasMoreElements) {
        val entry = iterator.nextElement()
        val name = entry.getName
        if (name.endsWith(".flix")) {
          val fullName = p.getFileName.toString + ":" + name
          val bytes = StreamOps.readAllBytes(zip.getInputStream(entry))
          val str = new String(bytes, flix.defaultCharset)
          val arr = str.toCharArray
          result += Source(Ast.Input.Text(fullName, str, stable = false), arr, stable = false)
        }
      }
      result.toList
    }.get // TODO Return a Result instead, see https://github.com/flix/flix/issues/3132
  }

  /**
    * Returns the java classes in the JDK classlist file.
    */
  private def findClasses(): MultiMap[List[String], String] = sys.env.get("JAVA_HOME") match {
    case None => MultiMap.empty
    case Some(home) =>
      val path = java.nio.file.Paths.get(home, "lib", "classlist")
      if (Files.exists(path) && Files.isRegularFile(path) && Files.isReadable(path)) {
        try {
          val fileContents = Files.readString(path)
          fileContents.linesIterator
            // Filter out comments
            .filter(line => !line.startsWith("#"))
            // Filter out inner classes
            .filter(clazz => !clazz.contains("$"))
            // Filter out lambda-invoke/lambda-proxy lines
            .filter(clazz => !clazz.contains("@"))
            // Create a multimap from all class path prefixes to the next packages/classes
            // I.e java.lang.string
            // [] => {java}
            // [java] => {lang}
            // [java, lang] => {String}
            .foldLeft[MultiMap[List[String], String]](MultiMap.empty) {
              case (acc, clazz) =>
                val clazzPath = clazz.split('/').toList

                clazzPath.inits.foldLeft(acc) {
                  // Case 1: Nonempty path: split prefix and package
                  case (acc1, prefix :+ pkg) => acc1 + (prefix -> pkg)
                  // Case 2: Empty path: skip it
                  case (acc1, _) => acc1
                }
            }
        } catch {
          // If any IO error occurs, i.e the file not existing in the users JDK, we return an empty map.
          case _: IOException => MultiMap.empty
        }
      } else {
        MultiMap.empty
      }
  }
}
