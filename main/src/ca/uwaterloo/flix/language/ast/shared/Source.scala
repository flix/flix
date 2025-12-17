/*
 * Copyright 2024 Magnus Madsen
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
package ca.uwaterloo.flix.language.ast.shared

import ca.uwaterloo.flix.language.ast.Sourceable
import ca.uwaterloo.flix.util.FileLines

import scala.annotation.tailrec

object Source {
  /** An unknown source. */
  val Unknown: Source = new Source(Input.Unknown, Array.emptyCharArray)
}

/**
 * A source is a name and an array of character data.
 */
case class Source(input: Input, data: Array[Char]) extends Sourceable {

  val lines: FileLines = FileLines.fromChars(data)

  def name: String = input match {
    case Input.RealFile(path, _) => path.toString
    case Input.VirtualFile(name, _, _) => name.toString
    case Input.VirtualUri(name, _, _) => name.toString
    case Input.PkgFile(path, _) => path.toString
    case Input.FileInPackage(_, virtualPath, _, _) => virtualPath
    case Input.Unknown => "unknown"
  }

  def src: Source = this

  override def equals(o: scala.Any): Boolean = o match {
    case that: Source => this.input == that.input
  }

  override def hashCode(): Int = input.hashCode()

  override def toString: String = name

  /**
    * Return the characters between `start` (inclusive) and `end` (exclusive).
    *
    * Returns "!bad source lookup!" if `start` or `end` is out of bounds.
    */
  def getData(start: Int, end: Int): String = {
    if (start < 0 || start >= data.length || end < 0 || end > data.length) return "!bad source lookup!"
    new String(data.slice(start, end))
  }

  /**
   * Gets a line of text from the source as a string (without newline characters).
   *
    * If `line` is out of bounds, "!bad source line lookup!" is returned.
   */
  def getLine(line: Int): String = {
    lines.nthLineInfo(line) match {
      case Some(FileLines.LineInfo(index, realLength)) => new String(data.slice(index, realLength))
      case None => "!bad source line lookup!"
    }
  }
}
