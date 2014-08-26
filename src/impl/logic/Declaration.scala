package impl.logic

/**
 * A declaration is a top-level statement.
 */
trait Declaration

object Declaration {

  case class DeclareConstraint(constraint: String) extends Declaration

  case class DeclareFun(name: Symbol.FunctionSymbol, t: Term.Abs, typ: Type) extends Declaration

  case class DeclareBot(v: Value, typ: Type) extends Declaration

  case class DeclareLeq(t: Term.Abs, typ: Type) extends Declaration

  /**
   * A declaration of the least-upper-bound function `t` for the type `typ`.
   */
  case class DeclareLub(t: Term.Abs, typ: Type) extends Declaration
}