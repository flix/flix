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

/**
 * A utility class for operations on bit vectors.
 */
object BitOps {

  /**
   * Returns `true` if the given `bit` is set in the bit vector `vec`.
   */
  def getBit(vec: Int, bit: Int): Boolean = (vec & (1 << bit)) != 0

  /**
   * Returns the given bit vector `vec` with the `bit` set.
   */
  def setBit(vec: Int, bit: Int): Int = vec | (1 << bit)

  /**
   * Returns the given bit vector `vec` with the `bit` unset.
   */
  def clearBit(vec: Int, bit: Int): Int = vec & ~(1 << bit)

  /**
   * Returns the given bit vector `vec` with the `bits` set.
   */
  def setBits(vec: Int, bits: Traversable[Int]): Int = {
    var result = vec
    for (bit <- bits) {
      result = setBit(result, bit)
    }
    result
  }

  /**
   * Returns the position (offset) of the least significant bit that is set in the given bit vector `vec`.
   */
  def positionOfLeastSignificantBit(vec: Int): Int = {
    31 - Integer.numberOfLeadingZeros(vec)
  }
}
