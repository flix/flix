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

import java.io.InputStream
import java.nio.file.{Files, Path}
import java.security.MessageDigest

object Sha256 {

  /**
    * The name of the digest algorithm, as [[MessageDigest]] knows it.
    */
  private val Algorithm: String = "SHA-256"

  /**
    * The prefix that a rendered digest carries.
    *
    * A digest names the algorithm that produced it, so a file that records one can be read by a
    * version of Flix that computes a different digest, and can say so.
    */
  private val Prefix: String = "sha256:"

  /**
    * The number of hexadecimal characters in a SHA-256 digest.
    */
  private val HexLength: Int = 64

  /**
    * The hexadecimal digits, indexed by the value each one denotes.
    */
  private val HexDigits: String = "0123456789abcdef"

  /**
    * The size, in bytes, of the buffer used to read a stream.
    */
  private val BufferSize: Int = 65536

  /**
    * Returns the SHA-256 digest of `bytes`.
    */
  def ofBytes(bytes: Array[Byte]): Sha256 = {
    val digest = MessageDigest.getInstance(Algorithm)
    Sha256(toHex(digest.digest(bytes)))
  }

  /**
    * Returns the SHA-256 digest of the file at `p`.
    *
    * Throws an [[java.io.IOException]] if the file cannot be read.
    */
  def ofFile(p: Path): Sha256 = {
    val stream = Files.newInputStream(p)
    try {
      ofStream(stream)
    } finally {
      stream.close()
    }
  }

  /**
    * Returns the SHA-256 digest of the remaining bytes of `is`.
    *
    * Reads `is` to its end but does not close it: the stream belongs to the caller.
    *
    * Throws an [[java.io.IOException]] if the stream cannot be read.
    */
  def ofStream(is: InputStream): Sha256 = {
    val digest = MessageDigest.getInstance(Algorithm)
    val buffer = new Array[Byte](BufferSize)

    var read = is.read(buffer)
    while (read != -1) {
      digest.update(buffer, 0, read)
      read = is.read(buffer)
    }

    Sha256(toHex(digest.digest()))
  }

  /**
    * Returns the digest that `s` denotes, or `None` if `s` does not denote one.
    *
    * A digest is written as [[Prefix]] followed by exactly [[HexLength]] lowercase hexadecimal
    * characters, which is how [[Sha256.toString]] renders one. Nothing else is accepted: a digest
    * is written by Flix and read back by Flix, so another spelling is a mistake to report rather
    * than an alternative to support.
    */
  def parse(s: String): Option[Sha256] = {
    if (!s.startsWith(Prefix)) {
      return None
    }

    val hex = s.substring(Prefix.length)
    if (hex.length != HexLength || !hex.forall(isLowerHex)) {
      return None
    }

    Some(Sha256(hex))
  }

  /**
    * Returns `true` if `c` is a lowercase hexadecimal character.
    */
  private def isLowerHex(c: Char): Boolean =
    ('0' <= c && c <= '9') || ('a' <= c && c <= 'f')

  /**
    * Returns `bytes` as a string of lowercase hexadecimal characters.
    */
  private def toHex(bytes: Array[Byte]): String = {
    val sb = new StringBuilder(bytes.length * 2)
    for (b <- bytes) {
      sb.append(HexDigits((b >> 4) & 0xF))
      sb.append(HexDigits(b & 0xF))
    }
    sb.toString
  }

}

/**
  * The SHA-256 digest of a sequence of bytes.
  *
  * Used to tell that a file is no longer the one whose digest was recorded: a download that was
  * corrupted or tampered with, or a published artifact that has since been replaced. A digest is
  * not a signature. It shows that the bytes changed, not who changed them.
  *
  * Construct one with [[Sha256.ofBytes]], [[Sha256.ofFile]], [[Sha256.ofStream]], or
  * [[Sha256.parse]], each of which establishes the invariant on `hex`.
  *
  * @param hex the digest as exactly 64 lowercase hexadecimal characters.
  */
case class Sha256(hex: String) {

  /**
    * Returns the digest as `sha256:` followed by its 64 hexadecimal characters.
    */
  override def toString: String = s"${Sha256.Prefix}$hex"

}
