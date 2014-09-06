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
      case Term.Unit => "unit"
      case Term.Bool(b) => b.toString
      case Term.Int(i) => i.toString
      case Term.Str(s) => s
      case Term.Set(xs) => "{" + xs.map(x => x.fmt).mkString(", ") + "}"

      case Term.Var(s) => s.fmt
      case Term.Abs(x, typ, t1) => "λ" + x.fmt + " " + t1.fmt
      case Term.App(t1, t2) => t1.fmt + " " + t2.fmt

      case Term.UnaryOp(op, t1) => op.fmt + " " + t1.fmt
      case Term.BinaryOp(op, t1, t2) => t1.fmt + " " + op.fmt + " " + t2.fmt
      case Term.Match(t1, rules) => t1.fmt + " " + rules.map {
        case (p, t2) => "case (" + p + ") => " + t2.fmt
      }.mkString(" | ")

      case Term.Tag(s, Term.Unit, _) => s.fmt
      case Term.Tag(s, t1, _) => s.fmt + " " + t1.fmt
      case Term.Tuple2(t1, t2) =>              "(" + t1.fmt + "," + t2.fmt + ")"
      case Term.Tuple3(t1, t2, t3) =>          "(" + t1.fmt + "," + t2.fmt + "," + t3.fmt + ")"
      case Term.Tuple4(t1, t2, t3, t4) =>      "(" + t1.fmt + "," + t2.fmt + "," + t3.fmt + "," + t4.fmt + ")"
      case Term.Tuple5(t1, t2, t3, t4, t5) =>  "(" + t1.fmt + "," + t2.fmt + "," + t3.fmt + "," + t4.fmt + "," + t5.fmt + ")"
    }
  }

}
