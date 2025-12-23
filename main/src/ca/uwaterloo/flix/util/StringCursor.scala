/*
 * Copyright 2025 Jonathan Lindegaard Starup
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

object StringCursor {

  /** The end-of-file character (`'\u0000'`) used methods like [[StringCursor.peek]] to avoid option types. */
  val EOF = '\u0000'

}

/**
  * Allows iteration through a character array `chars` via a mutable index (the cursor) into the array.
  * Relative indexing is allowed via [[StringCursor.nth]].
  */
final class StringCursor(val chars: Array[Char]) {

  /** The index pointing into [[chars]] (zero-indexed). */
  private var index: Int = 0

  /** Returns the current array index (zero-indexed). */
  def getIndex: Int = index

  /** Advances cursor one character forward
    *
    * Returns the character it was previously pointing to.
    *
    * Returns [[StringCursor.EOF]] and does not advance if the cursor is out of bounds.
    */
  def peekAndAdvance(): Char = {
    if (this.isInBounds) {
      val c = chars(index)
      index += 1
      c
    } else {
      StringCursor.EOF
    }
  }

  /**
    * Advances the cursor one character forward.
    *
    * If the cursor is already out of bounds, it is not advanced.
    */
  def advance(): Unit =
    if (this.isInBounds) {
      index += 1
    }

  /**
    * Advances cursor `n` chars forward.
    *
    * At each step, if the cursor is out of bounds, it will not be further advanced.
    */
  def advanceN(n: Int): Unit = {
    index += n
    if (index >= chars.length) {
      index = chars.length
    }
  }

  /**
    * Returns the character that is `n` characters ahead of the cursor if available.
    *
    * If `n` is negative then its the character that is `-n` characters behind (if available).
    */
  def nth(n: Int): Option[Char] = {
    val offset = index + n
    if (0 <= offset && offset < chars.length) {
      Some(chars(offset))
    } else {
      None
    }
  }

  /**
    * Returns the character of the cursor if inbounds.
    *
    * Returns [[StringCursor.EOF]] if the cursor out of bounds.
    */
  def peek: Char =
    if (this.isInBounds) {
      chars(index)
    } else {
      StringCursor.EOF
    }

  /**
    * Returns `p(this.peek)` if the cursor is inbounds.
    *
    * Returns `outOfBounds` if the cursor is out of bounds.
    */
  def peekIs(p: Char => Boolean, outOfBounds: Boolean): Boolean =
    if (this.isInBounds) {
      p(chars(index))
    } else {
      outOfBounds
    }

  /** Returns `true` if the cursor has moved past the end. */
  def isEof: Boolean = index >= chars.length

  /** Returns `true` if the cursor has not reached end of file. */
  def isInBounds: Boolean = index < chars.length

  /**
    * Advance the cursor past `s` if it matches the current content.
    *
    * Returns `true` if the cursor was advanced.
    *
    * If `s` is empty and the cursor is inbounds then `true` is returned but the cursor is not advanced.
    */
  def advanceIfMatch(s: String): Boolean = {
    if (chars.length < this.index + s.length) return false

    var sIndex = 0
    while (sIndex < s.length) {
      if (chars(this.index + sIndex) != s(sIndex)) return false
      sIndex += 1
    }

    advanceN(s.length)

    true
  }

  /**
    * Advance the cursor past `c` if it matches the character under the cursor.
    *
    * Returns `true` if the cursor was advanced.
    */
  def advanceIfMatch(c: Char): Boolean =
    if (this.isInBounds && chars(index) == c) {
      index += 1
      true
    } else {
      false
    }

  /** Continuously advance the cursor while `p` returns `true` for the character under the cursor. */
  def advanceWhile(p: Char => Boolean): Unit =
    while (this.isInBounds && p(chars(index))) {
      index += 1
    }

  /**
    * Continuously advance the cursor while `p` returns `true` for the character under the cursor.
    *
    * Returns the number of advances made.
    */
  def advanceWhileWithCount(p: Char => Boolean): Int = {
    val startingOffset = index
    advanceWhile(p)
    index - startingOffset
  }

  /**
    * Returns `p(this.nth(n).get)` if the cursor is inbounds.
    *
    * Returns `outOfBounds` if the cursor is out of bounds.
    */
  def nthIs(n: Int, p: Char => Boolean, outOfBounds: Boolean): Boolean = {
    val offset = index + n
    if (0 <= offset && offset < chars.length) {
      p(chars(offset))
    } else {
      outOfBounds
    }
  }

}
