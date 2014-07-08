package syntax

import impl.logic.Term
import syntax.Symbols._
import syntax.Values._

/**
 * Embedded DSL syntax for terms.
 */
object Terms {

  /**
   * Rich Terms
   */
  implicit class RichTerm(t: Term) {
    def fmt: String = t match {
      case Term.Constant(v) => v.fmt
      case Term.Variable(s) => s.fmt
      case Term.Apply(s, ts) => s.fmt + "(" + ts.map(t => t.fmt).mkString(",") + ")"
      case Term.Constructor0(s) => s.fmt
      case Term.Constructor1(s, t1) => s.fmt + "(" + t1.fmt + ")"
      case Term.Constructor2(s, t1, t2) => s.fmt + "(" + t1.fmt + "," + t2.fmt + ")"
      case Term.Constructor3(s, t1, t2, t3) => s.fmt + "(" + t1.fmt + "," + t2.fmt + "," + t3.fmt + ")"
      case Term.Constructor4(s, t1, t2, t3, t4) => s.fmt + "(" + t1.fmt + "," + t2.fmt + "," + t3.fmt + "," + t4.fmt + ")"
      case Term.Constructor5(s, t1, t2, t3, t4, t5) => s.fmt + "(" + t1.fmt + "," + t2.fmt + "," + t3.fmt + "," + t4.fmt + "," + t5.fmt + ")"
    }
  }

}
