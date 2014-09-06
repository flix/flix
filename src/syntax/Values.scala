package syntax

import impl.logic.Value
import syntax.Symbols._
import syntax.Terms._

/**
 * Embedded DSL syntax for values.
 */
object Values {

  /**
   * Rich Values.
   */
  implicit class RichValue(v: Value) {
    def fmt: String = v match {
      case Value.Unit => "unit"
      case Value.Bool(b) => b.toString
      case Value.Int(i) => i.toString
      case Value.Str(s) => "\"" + s + "\""
      case Value.Set(xs) => "{" + xs.map(x => x.fmt).mkString(", ") + "}"

      case Value.Abs(x, typ, t) => "λ" + x.fmt + " " + t.fmt

      case Value.Tag(s, Value.Unit, typ) =>  ":" + s.fmt
      case Value.Tag(s, v1, typ) =>          ":" + s.fmt + " " + v1.fmt
      case Value.Tuple2(v1, v2) =>              "(" + v1.fmt + "," + v2.fmt + ")"
      case Value.Tuple3(v1, v2, v3) =>          "(" + v1.fmt + "," + v2.fmt + "," + v3.fmt + ")"
      case Value.Tuple4(v1, v2, v3, v4) =>      "(" + v1.fmt + "," + v2.fmt + "," + v3.fmt + "," + v4.fmt + ")"
      case Value.Tuple5(v1, v2, v3, v4, v5) =>  "(" + v1.fmt + "," + v2.fmt + "," + v3.fmt + "," + v4.fmt + "," + v5.fmt + ")"
    }
  }

}
