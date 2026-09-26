/*
 * Copyright 2015-2016 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.util

import java.io.InputStream
import java.nio.file.{Files, Paths}

import scala.collection.mutable

object LocalResource {

  private val RootPath = "main"

  private val cache = mutable.Map.empty[String, String]

  /**
    * Returns the given relative path as a string.
    */
  def get(relativePath: String): String = cache.getOrElseUpdate(relativePath, {
    val inputStream = getInputStream(relativePath)
    val result = StreamOps.readAll(inputStream)
    inputStream.close()
    result
  })

  /**
    * Returns the an input stream for the given relative path.
    */
  def getInputStream(relativePath: String): InputStream = {
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
