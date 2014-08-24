package syntax

import impl.logic.Term
import syntax.Operators._
import syntax.Symbols._

/**
 * Embedded DSL syntax for terms.
 */
object Terms {

  /**
   * Rich Terms
   */
  implicit class RichTerm(t: Term) {
    def fmt: String = t match {
      case Term.Bool(b) => b.toString
      case Term.Int(i) => i.toString
      case Term.Str(s) => s
      case Term.Variable(s) => s.fmt
      case Term.Apply(s, ts) => s.fmt + "(" + ts.map(t => t.fmt).mkString(",") + ")"
      case Term.Abs(s, t1) => "λ" + s.fmt + ". " + t1.fmt
      case Term.App(t1, t2) => t1.fmt + " " + t2.fmt
      case Term.UnaryOp(op, t1) => op.fmt + " " + t1.fmt
      case Term.BinaryOp(op, t1, t2) => t1.fmt + " " + op.fmt + " " + t2.fmt
      case Term.Ite(t1, t2, t3) => "if " + t1.fmt + " then " + t2.fmt + " else " + t3.fmt
      case Term.Constructor0(s) => s.fmt
      case Term.Constructor1(s, t1) => s.fmt + "(" + t1.fmt + ")"
      case Term.Constructor2(s, t1, t2) => s.fmt + "(" + t1.fmt + "," + t2.fmt + ")"
      case Term.Constructor3(s, t1, t2, t3) => s.fmt + "(" + t1.fmt + "," + t2.fmt + "," + t3.fmt + ")"
      case Term.Constructor4(s, t1, t2, t3, t4) => s.fmt + "(" + t1.fmt + "," + t2.fmt + "," + t3.fmt + "," + t4.fmt + ")"
      case Term.Constructor5(s, t1, t2, t3, t4, t5) => s.fmt + "(" + t1.fmt + "," + t2.fmt + "," + t3.fmt + "," + t4.fmt + "," + t5.fmt + ")"
    }
  }

}
