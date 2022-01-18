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

import java.io._
import java.nio.file.{Files, Path}
import java.util.zip.ZipInputStream

object StreamOps {

  /**
    * Reads an array of all bytes read from the given input stream `is`.
    */
  def readAllBytes(inputStream: InputStream): Array[Byte] = {
    val outputStream = new ByteArrayOutputStream()

    val buffer = new Array[Byte](0xFFFF)

    var read: Int = inputStream.read(buffer)
    while (read != -1) {
      outputStream.write(buffer, 0, read)
      read = inputStream.read(buffer)
    }

    outputStream.toByteArray
  }

  /**
    * Reads the `inputStream` into a string.
    */
  def readAll(inputStream: InputStream): String = {
    val reader = new BufferedReader(new InputStreamReader(inputStream, "UTF-8"))
    val result = readAll(reader)
    reader.close()
    result
  }

  /**
    * Reads the `reader` into a string.
    */
  def readAll(reader: BufferedReader): String = {
    val sb = new StringBuilder()
    var line = reader.readLine()
    while (line != null) {
      sb.append(line).append(System.lineSeparator())
      line = reader.readLine()
    }
    reader.close()
    sb.toString()
  }

  /**
    * Copies the `inputStream` to the `outputStream`.
    */
  def writeAll(inputStream: InputStream, outputStream: PrintStream): Unit = {
    val reader = new BufferedReader(new InputStreamReader(inputStream))
    var line = reader.readLine()
    while (line != null) {
      outputStream.println(line)
      line = reader.readLine()
    }
  }

  /**
    * Copies the `inputStream` to the `path`.
    */
  def writeAll(inputStream: InputStream, path: Path): Unit = {
    val outputStream = Files.newOutputStream(path)
    StreamOps.writeAll(inputStream, new PrintStream(outputStream))
  }

  /**
    * Copies the content of the current zip entry to the `path`.
    */
  def writeAll(zip: ZipInputStream, path: Path): Unit = {
    val BufferSize = 4096

    val os = Files.newOutputStream(path)
    val bf: Array[Byte] = new Array[Byte](BufferSize)
    var read = zip.read(bf)
    while (read != -1) {
      os.write(bf, 0, read)
      read = zip.read(bf)
    }
    os.close()
  }

}
