package impl.logic

trait Declaration

object Declaration {
  /**
   * A declaration of the bottom element for the type `typ`.
   */
  case class DeclareBot(v: Value, typ: Type) extends Declaration

  /**
   * A declaration of the less-than-equal function `t` for the type `typ`.
   */
  case class DeclareLeq(t: Term.Abs, typ: Type) extends Declaration

  /**
   * A declaration of the least-upper-bound function `t` for the type `typ`.
   */
  case class DeclareLub(t: Term.Abs, typ: Type) extends Declaration

  /**
   * A declaration of the height function `t` for the type `typ`.
   */
  case class DeclareHeight(t: Term.Abs, typ: Type) extends Declaration
}