import impl.logic.{Symbol, Value}
import syntax.Symbols._

package object syntax {

  /**
   * A string interpolator which takes symbols into account.
   */
  implicit class SmtSyntaxInterpolator(sc: StringContext) {
    def smt(args: Any*): String = {
      def format(a: Any): String = a match {
        case x: Value => x match {
          case _ => x.toString
        }
        case x: Symbol => x.fmt
        case x => x.toString
      }

      val pi = sc.parts.iterator
      val ai = args.iterator
      val bldr = new java.lang.StringBuilder(pi.next())
      while (ai.hasNext) {
        bldr append format(ai.next())
        bldr append pi.next()
      }
      bldr.toString
    }
  }

}
