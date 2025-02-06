package ca.uwaterloo.flix.util

/**
  * A class to iterate through an array of characters while maintaining the line and column index
  * of the cursor.
  */
class StringCursor(val data: Array[Char]) {

  /** The cursor pointing into `data`. */
  private var offset: Int = 0

  /** The line index of `offset`. */
  private var line: Int = 0

  /** The column index of `offset` */
  private var column: Int = 0

  /** The max column index of the previous line or `0` if there is no previous line. */
  private var prevLineMaxColumn = 0

  /** Returns the current line index. */
  def getLine: Int = line

  /** Returns the current column index. */
  def getColumn: Int = column

  /** Returns the current source offset. */
  def getOffset: Int = offset

  /** Returns `(line, column)`. */
  def getPosition: (Int, Int) = (line, column)

  /**
    * Returns `(line, column)` where non-existent positions are preferred instead of the first
    * position of the next line.
    *
    * In this example, the current position is on 'v' (1,0) but this function will then return
    * the position just after 'Example' (0, 7). This is sometimes preferable for exclusive
    * end positions of ranges.
    *
    * {{{
    *   Example
    *   v
    * }}}
    */
  def getExclusiveEndPosition: (Int, Int) = {
    if (line <= 0 || column > 0) {
      (line, column)
    } else {
      (line - 1, prevLineMaxColumn + 1)
    }
  }

  /**
    * Advances cursor one char forward, returning the char it was previously sitting on.
    *
    * If the cursor has advanced past the content, EOF is returned (`'\u0000'`).
    */
  def advance(): Char = {
    if (offset >= data.length) {
      '\u0000'
    } else {
      val c = data(offset)
      if (c == '\n') {
        prevLineMaxColumn = column
        line += 1
        column = 0
      } else {
        column += 1
      }
      offset += 1
      c
    }
  }

  /** Peeks the character that is `n` characters ahead of the cursor if available. */
  def nth(n: Int): Option[Char] = {
    val index = offset + n
    if (0 <= index && index < data.length) {
      Some(data(index))
    } else {
      None
    }
  }

  /** Peeks the previous character if available. */
  def previous: Option[Char] = nth(-1)

  /**
    * Peeks the character that cursor is currently sitting on without advancing.
    *
    * If the cursor has advanced past the content, EOF is returned (`'\u0000'`).
    */
  def peek: Char = {
    if (offset < data.length) {
      data(offset)
    } else {
      '\u0000' // EOF char
    }
  }

  /** Returns true if the cursor has moved past the end. */
  def eof: Boolean = offset >= data.length

  /** Continuously advance past whitespace characters. */
  def advanceWhitespace(): Unit = {
    // This is finite since EOF is not whitespace.
    while (this.peek.isWhitespace) {
      advance()
    }
  }

  /** Returns a copy of `this`, pointing to the same underlying array. */
  def copy(): StringCursor = {
    val sc = new StringCursor(data)
    sc.offset = offset
    sc.line = line
    sc.column = column
    sc.prevLineMaxColumn = prevLineMaxColumn
    sc
  }

}
