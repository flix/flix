/*
 * Copyright 2026 Werner Stein
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

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

/**
  * Content-addressed suffixes for generated symbol names.
  *
  * Specialized definitions are today told apart by a [[ca.uwaterloo.flix.language.GenSym]]
  * counter, which leaks into JVM class and method names. The counter depends on how many
  * symbols happened to be minted before it, so an unrelated edit — or merely recompiling,
  * since specialization runs in parallel — renames the generated classes. Two consecutive
  * builds of identical source currently agree on only 3.5% of the id-bearing names.
  *
  * A suffix derived from what a symbol *is*, rather than from when it was created, removes
  * that dependence entirely.
  *
  * The suffix is only as stable as the key it is given: the caller must pass a canonical
  * form, in which types equal up to record field order, effect formula, associated types,
  * and aliases have already been made textually identical.
  */
object StableName {

  /**
    * The width of a suffix, in base-36 digits.
    *
    * Thirteen digits are needed to hold [[Bits]]: 36^12 is approximately 2^62.04, which
    * is too small, and 36^13 is approximately 2^67.2, which is not.
    */
  val Width: Int = 13

  /**
    * The number of digest bits retained.
    *
    * Sixty-four keeps a suffix computable with a single `Long` rather than a `BigInteger`.
    */
  val Bits: Int = 64

  /**
    * SHA-256 is used because its output is fixed by FIPS 180-4 and so cannot change
    * under us; a faster non-cryptographic hash would tie every generated name in every
    * Flix program to a third-party library's version. At roughly 2,000 specializations
    * per compile the cost is under a millisecond.
    *
    * [[MessageDigest]] is not thread-safe and specialization runs under
    * [[ParOps.parMapValues]], so each thread gets its own.
    */
  private val Digest: ThreadLocal[MessageDigest] =
    ThreadLocal.withInitial(() => MessageDigest.getInstance("SHA-256"))

  /**
    * Returns the stable id of the given canonical `key`.
    *
    * The value is fixed for a given key across runs, machines, and compiler versions. It
    * is the id a symbol carries; [[render]] turns it into the text that appears in a name.
    */
  def of(key: String): Long = {
    // The charset is explicit: `String.getBytes()` would use the platform default and make
    // a generated name depend on the locale of whoever compiled it.
    val digest = Digest.get().digest(key.getBytes(StandardCharsets.UTF_8))

    // The leading `Bits` of the digest, read big-endian and treated as unsigned.
    var truncated: Long = 0L
    var i = 0
    while (i < Bits / 8) {
      truncated = (truncated << 8) | (digest(i) & 0xffL)
      i = i + 1
    }
    truncated
  }

  /**
    * Returns the text for `id` as it appears in a generated name.
    *
    * Always exactly [[Width]] lowercase digits and letters. Rendering is kept apart from
    * the id itself so that a symbol stores 64 bits rather than a string that nothing
    * constrains to be well formed.
    */
  def render(id: Long): String = {
    // `toUnsignedString` renders base 36 in lowercase, which keeps names distinct on the
    // case-insensitive filesystems used by macOS and Windows.
    val encoded = java.lang.Long.toUnsignedString(id, 36)
    if (encoded.length >= Width) encoded else "0" * (Width - encoded.length) + encoded
  }

  /**
    * Returns the rendered stable suffix of the given canonical `key`.
    */
  def suffix(key: String): String = render(of(key))

}
