namespace Char {

    external def toInt8(c: Char): Int8

    external def toInt16(c: Char): Int16

    external def toInt32(c: Char): Int32

    external def toInt64(c: Char): Int64

    /**
     * Alias for `toInt32`.
     */
    def toInt(c: Char): Int32 = toInt32(c)

}
