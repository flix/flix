/*
 * Copyright 2015-2016 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.util

import java.io.*

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

}
