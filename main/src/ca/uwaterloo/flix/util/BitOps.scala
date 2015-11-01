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
   * Returns the given bit vector `vec` with the `bits` set.
   */
  def setBits(vec: Int, bits: Traversable[Int]): Int = {
    var result = vec
    for (bit <- bits) {
      result = setBit(result, bit)
    }
    result
  }
}
