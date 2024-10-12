package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.util.Result
import org.json4s.JValue
import org.json4s.JsonDSL.pair2Assoc
import org.json4s.jvalue2monadic

object Range {
  /**
    * Tries to parse the given `json` value as a [[Range]].
    */
  def parse(json: JValue): Result[Range, String] = {
    val startResult = Position.parse(json \\ "start")
    val endResult = Position.parse(json \\ "end")
    for {
      start <- startResult
      end <- endResult
    } yield Range(start, end)
  }

  implicit object Order extends Ordering[Range] {
    override def compare(x: Range, y: Range): Int =
      (x.start, x.end).compare(y.start, y.end)
  }
}

case class Range(start: Position, end: Position) {
  def contains(that: Range): Boolean =
    this.start <= that.start && that.end <= this.end

  def contains(pos: Position): Boolean =
    this.start <= pos && pos < this.end

  def toJSON: JValue = ("start" -> start.toJSON) ~ ("end" -> end.toJSON)
}
