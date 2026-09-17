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

import org.scalatest.funsuite.AnyFunSuite

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

class TestSha256 extends AnyFunSuite {

  /**
    * The digest of the empty sequence of bytes.
    */
  private val EmptyHex: String = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"

  /**
    * The digest of `abc`, the first test vector of FIPS 180-4.
    */
  private val AbcHex: String = "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"

  /**
    * The digest of `flix`.
    */
  private val FlixHex: String = "837a79566eb24f6ee26bdf916de63931162856ac83c86ce439cc1343cdb3ee9e"

  /**
    * The digest of 100,000 `a` characters, which is more bytes than [[Sha256.ofStream]] reads
    * in one go.
    */
  private val LongHex: String = "6d1cf22d7cc09b085dfc25ee1a1f3ae0265804c607bc2074ad253bcc82fd81ee"

  /**
    * Returns `s` as UTF-8 bytes.
    */
  private def bytesOf(s: String): Array[Byte] = s.getBytes(StandardCharsets.UTF_8)

  /**
    * Returns the path to a new temporary file that holds `bytes`.
    */
  private def fileOf(bytes: Array[Byte]): Path = {
    val p = Files.createTempFile("flix-sha256", ".tmp")
    Files.write(p, bytes)
    p
  }

  test("ofBytes.01") {
    assert(Sha256.ofBytes(Array.emptyByteArray).hex == EmptyHex)
  }

  test("ofBytes.02") {
    assert(Sha256.ofBytes(bytesOf("abc")).hex == AbcHex)
  }

  test("ofBytes.03") {
    assert(Sha256.ofBytes(bytesOf("flix")).hex == FlixHex)
  }

  test("ofBytes.04") {
    assert(Sha256.ofBytes(bytesOf("a" * 100000)).hex == LongHex)
  }

  test("ofStream.01") {
    assert(Sha256.ofStream(new ByteArrayInputStream(Array.emptyByteArray)).hex == EmptyHex)
  }

  test("ofStream.02") {
    assert(Sha256.ofStream(new ByteArrayInputStream(bytesOf("abc"))).hex == AbcHex)
  }

  test("ofStream.03") {
    // More bytes than the read buffer holds, so the read loop runs more than once.
    assert(Sha256.ofStream(new ByteArrayInputStream(bytesOf("a" * 100000))).hex == LongHex)
  }

  test("ofStream.04") {
    // The stream belongs to the caller, so it must still be open afterwards.
    var closed = false
    val is = new ByteArrayInputStream(bytesOf("abc")) {
      override def close(): Unit = {
        closed = true
        super.close()
      }
    }
    Sha256.ofStream(is)
    assert(!closed)
  }

  test("ofFile.01") {
    assert(Sha256.ofFile(fileOf(Array.emptyByteArray)).hex == EmptyHex)
  }

  test("ofFile.02") {
    assert(Sha256.ofFile(fileOf(bytesOf("abc"))).hex == AbcHex)
  }

  test("ofFile.03") {
    assert(Sha256.ofFile(fileOf(bytesOf("a" * 100000))).hex == LongHex)
  }

  test("ofFile.04") {
    // The same bytes have the same digest, whether they are read from a file or from memory.
    assert(Sha256.ofFile(fileOf(bytesOf("flix"))) == Sha256.ofBytes(bytesOf("flix")))
  }

  test("toString.01") {
    assert(Sha256.ofBytes(bytesOf("abc")).toString == s"sha256:$AbcHex")
  }

  test("toString.02") {
    assert(Sha256.ofBytes(Array.emptyByteArray).toString == s"sha256:$EmptyHex")
  }

  test("parse.01") {
    assert(Sha256.parse(s"sha256:$AbcHex").contains(Sha256(AbcHex)))
  }

  test("parse.02") {
    // Every digest survives a round trip through its rendered form.
    val digest = Sha256.ofBytes(bytesOf("flix"))
    assert(Sha256.parse(digest.toString).contains(digest))
  }

  test("parse.03") {
    // No prefix.
    assert(Sha256.parse(AbcHex).isEmpty)
  }

  test("parse.04") {
    // The prefix of another algorithm.
    assert(Sha256.parse(s"sha1:$AbcHex").isEmpty)
  }

  test("parse.05") {
    // One character too few.
    assert(Sha256.parse(s"sha256:${AbcHex.dropRight(1)}").isEmpty)
  }

  test("parse.06") {
    // One character too many.
    assert(Sha256.parse(s"sha256:${AbcHex}0").isEmpty)
  }

  test("parse.07") {
    // The right digest, in uppercase.
    assert(Sha256.parse(s"sha256:${AbcHex.toUpperCase}").isEmpty)
  }

  test("parse.08") {
    // A character that is not hexadecimal.
    assert(Sha256.parse(s"sha256:${AbcHex.dropRight(1)}g").isEmpty)
  }

  test("parse.09") {
    assert(Sha256.parse("").isEmpty)
  }

  test("parse.10") {
    // The prefix alone.
    assert(Sha256.parse("sha256:").isEmpty)
  }

  test("equality.01") {
    assert(Sha256.ofBytes(bytesOf("abc")) == Sha256.ofBytes(bytesOf("abc")))
  }

  test("equality.02") {
    assert(Sha256.ofBytes(bytesOf("abc")) != Sha256.ofBytes(bytesOf("abd")))
  }

}
