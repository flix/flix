package syntax

import impl.logic.Pattern

/**
 * Embedded DSL syntax for patterns.
 */
object Patterns {

  /**
   * Rich Values.
   */
  implicit class RichPattern(p: Pattern) {
    def fmt: String = p match {
      case Pattern.Wildcard => "_"
      case Pattern.Var(x) => x.s
      case Pattern.Unit => "Unit"
      case Pattern.Bool(b) => b.toString
      case Pattern.Int(i) => i.toString
      case Pattern.Str(s) => "\"" + s + "\""
      case Pattern.Tag(n, Pattern.Unit) => n.s
      case Pattern.Tag(n, p1) => n + " " + p1.fmt
      case Pattern.Tuple2(p1, p2) => "(" + p1.fmt + ", " + p2.fmt + ")"
      case Pattern.Tuple3(p1, p2, p3) => "(" + p1.fmt + ", " + p2.fmt + ", " + p3.fmt + ")"
      case Pattern.Tuple4(p1, p2, p3, p4) => "(" + p1.fmt + ", " + p2.fmt + ", " + p3.fmt + ", " + p4.fmt + ")"
      case Pattern.Tuple5(p1, p2, p3, p4, p5) => "(" + p1.fmt + ", " + p2.fmt + ", " + p3.fmt + ", " + p4.fmt + ", " + p5.fmt + ")"
    }
  }

}
