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
  * Content-addressed suffixes for generated symbol names: a SHA-256 digest of a
  * canonical key, truncated and rendered as lowercase base-36.
  *
  * `width` (in base-36 digits) is the only user-facing knob; see [[suffix]].
  */
object StableName {

  /** The default width: 12 base-36 digits (roughly 62 bits of digest entropy). */
  val DefaultWidth: Int = 12

  /**
    * The largest width this utility accepts: 25 base-36 digits, roughly 128 bits
    * of digest entropy. SHA-256 has 256 bits of digest to draw from in total (a
    * hard ceiling around 49 base-36 digits); requests are capped well below that
    * so generated names cannot grow unreasonably long.
    */
  val MaxWidth: Int = 25

  private val Log2_36: Double = math.log(36) / math.log(2)

  /** Returns the approximate number of bits of entropy a `width`-digit base-36 string carries. */
  def bitsFor(width: Int): Int = (width * Log2_36).toInt

  private val Digest: ThreadLocal[MessageDigest] =
    ThreadLocal.withInitial(() => MessageDigest.getInstance("SHA-256"))

  /**
    * Returns the stable id of `key`: its SHA-256 digest, read as an unsigned
    * integer and reduced modulo `36^width`.
    */
  def of(key: String, width: Int = DefaultWidth): BigInt = {
    require(width >= 1, s"width must be positive, got $width")
    require(width <= MaxWidth, s"width must be at most $MaxWidth, got $width")
    val digest = Digest.get().digest(key.getBytes(StandardCharsets.UTF_8))
    val full = BigInt(1, digest)
    // Modulo, not floored to whole bits: log2(36) is irrational, so flooring wastes up to
    // a bit of entropy per character depending on width. The resulting bias is negligible
    // (on the order of 2^-127 at the largest supported width).
    full % BigInt(36).pow(width)
  }

  /**
    * Renders `id` as lowercase base-36. Not left-padded: an id with a leading
    * zero digit renders shorter than the `width` it was computed with.
    */
  def render(id: BigInt): String = id.toString(36)

  /** Returns the stable, content-addressed suffix for `key` at the given `width`. */
  def suffix(key: String, width: Int = DefaultWidth): String = render(of(key, width))

}
