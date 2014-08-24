package impl.logic

/**
 * A logical program is a sequence of declarations.
 */
case class ConstraintSystem(decl: List[Declaration])

/**
 * A declaration is a top-level statement.
 */
trait Declaration

object Declaration {

  case class DeclareClause(constraint: Constraint) extends Declaration

  case class DeclareFun(name: Symbol.FunctionSymbol, t: Term.Abs, typ: Type) extends Declaration

  case class DeclareBot(v: Value, typ: Type) extends Declaration

  case class DeclareLeq(t: Term.Abs, typ: Type) extends Declaration

  /**
   * A declaration of the least-upper-bound function `t` for the type `typ`.
   */
  case class DeclareLub(t: Term.Abs, typ: Type) extends Declaration
}

trait Constraint

object Constraint {

  case class Fact(head: Predicate) extends Constraint

  case class Rule(head: Predicate, body: List[Predicate]) extends Constraint

}
