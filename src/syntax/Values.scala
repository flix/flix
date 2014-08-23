package syntax

import syntax.Symbols._
import impl.logic.Value

/**
 * Embedded DSL syntax for values.
 */
object Values {

  /**
   * Rich Values.
   */
  implicit class RichValue(v: Value) {
    def fmt: String = v match {
      case Value.Bool(b) => b.toString
      case Value.Int(i) => i.toString
      case Value.Str(s) => "\"" + s + "\""
      case Value.Constructor0(s) => s.fmt
      case Value.Constructor1(s, v1) => s.fmt + "(" + v1.fmt + ")"
      case Value.Constructor2(s, v1, v2) => s.fmt + "(" + v1.fmt + "," + v2.fmt + ")"
      case Value.Constructor3(s, v1, v2, v3) => s.fmt + "(" + v1.fmt + "," + v2.fmt + "," + v3.fmt + ")"
      case Value.Constructor4(s, v1, v2, v3, v4) => s.fmt + "(" + v1.fmt + "," + v2.fmt + "," + v3.fmt + "," + v4.fmt + ")"
      case Value.Constructor5(s, v1, v2, v3, v4, v5) => s.fmt + "(" + v1.fmt + "," + v2.fmt + "," + v3.fmt + "," + v4.fmt + "," + v5.fmt + ")"
    }
  }

}
