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

package ca.uwaterloo.flix.language

import java.nio.file.Path

import org.parboiled2.ParserInput

package object ast {

  // TODO: Move into Ast.

  /**
    * A common super-type for inputs.
    */
  sealed trait Input

  object Input {

    /**
      * A source that is backed by an internal resource.
      */
    case class Internal(name: String, text: String) extends Input

    /**
      * A source that is backed by a regular string.
      */
    case class Str(text: String) extends Input

    /**
      * A source that is backed by a regular file.
      */
    case class TxtFile(path: Path) extends Input

    /**
      * A source that is backed by a zip file.
      */
    case class ZipFile(path: Path) extends Input

  }

  /**
    * A source is a name and an array of character data.
    */
  case class Source(name: String, data: Array[Char]) {
    def format: String = name

    override def equals(o: scala.Any): Boolean = o match {
      case that: Source => this.name == that.name
    }

    override def hashCode(): Int = name.hashCode
  }


  object SourcePosition {
    /**
      * Represents an unknown source position.
      */
    val Unknown: SourcePosition = SourcePosition(Source("", Array.emptyCharArray), 0, 0, None)
  }

  /**
    * A class that represent a physical source position inside a source input.
    *
    * @param line  the line number.
    * @param col   the column number.
    * @param input the parser input.
    */
  case class SourcePosition(source: Source, line: Int, col: Int, input: Option[ParserInput])

  /**
    * Companion object for the [[SourceLocation]] class.
    */
  object SourceLocation {
    /**
      * Represents a generated source location.
      */
    val Generated: SourceLocation = mk(SourcePosition.Unknown, SourcePosition.Unknown)

    /**
      * Represents an unknown source location.
      */
    val Unknown: SourceLocation = mk(SourcePosition.Unknown, SourcePosition.Unknown)

    def mk(b: SourcePosition, e: SourcePosition): SourceLocation = {
      assert(b.source == e.source)
      val lineAt = b.input match {
        case None => (i: Int) => ""
        case Some(input) => (i: Int) => input.getLine(i)
      }
      SourceLocation(b.source, b.line, b.col, e.line, e.col, lineAt)
    }

    implicit object Order extends Ordering[SourceLocation] {

      import scala.math.Ordered.orderingToOrdered

      def compare(x: SourceLocation, y: SourceLocation): Int =
        (x.source.format, x.beginLine, x.beginCol) compare(y.source.format, y.beginLine, y.beginCol)
    }

  }

  /**
    * A class that represents the physical source location of some parsed syntactic entity.
    *
    * @param source    the source input.
    * @param beginLine the line number where the entity begins.
    * @param beginCol  the column number where the entity begins.
    * @param endLine   the line number where the entity ends.
    * @param endCol    the column number where the entity ends.
    * @param lineAt    a closure which returns the text at the given line offset.
    */
  case class SourceLocation(source: Source, beginLine: Int, beginCol: Int, endLine: Int, endCol: Int, lineAt: Int => String) {

    /**
      * Returns a formatted string representation of `this` source location.
      */
    def format: String = s"${source.format}:$beginLine:$beginCol"

  }

  object Time {
    val Default: Time = Time(
      reader = 0,
      parser = 0,
      weeder = 0,
      namer = 0,
      resolver = 0,
      typer = 0,
      effects = 0,
      patsExhaustive = 0,
      documentor = 0,
      stratifier = 0,
      monomorph = 0,
      synthesize = 0,
      simplifier = 0,
      uncurrier = 0,
      propertyGen = 0,
      verifier = 0,
      lambdaLift = 0,
      tailrec = 0,
      inliner = 0,
      treeshaker = 0,
      varNumbering = 0,
      enumGen = 0,
      codeGen = 0,
      tupleGen = 0,
      loadByteCode = 0,
      solver = 0)
  }

  /**
    * A class that tracks the amount of time spent in each phase of the compiler.
    */
  case class Time(reader: Long,
                  parser: Long,
                  weeder: Long,
                  namer: Long,
                  resolver: Long,
                  typer: Long,
                  effects: Long,
                  patsExhaustive: Long,
                  documentor: Long,
                  stratifier: Long,
                  monomorph: Long,
                  synthesize: Long,
                  simplifier: Long,
                  uncurrier: Long,
                  propertyGen: Long,
                  verifier: Long,
                  lambdaLift: Long,
                  tailrec: Long,
                  inliner: Long,
                  treeshaker: Long,
                  varNumbering: Long,
                  enumGen: Long,
                  codeGen: Long,
                  tupleGen: Long,
                  loadByteCode: Long,
                  solver: Long)

}
