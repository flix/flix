package impl.logic

/**
 *
 */
case class ConstraintSystem(decl: List[Declaration])

trait Declaration

object Declaration {

  case class DeclareClause(constraint: Constraint) extends Declaration

  case class DeclareFun(name: Symbol.FunctionSymbol, t: Term.Abs, typ: Type) extends Declaration

  case class DeclareBot(v: Value, typ: Type) extends Declaration

  case class DeclareLeq(t: Term.Abs, typ: Type) extends Declaration

  case class DeclareLub(t: Term.Abs, typ: Type) extends Declaration
}

trait Constraint

object Constraint {

  case class Fact(head: Predicate) extends Constraint

  case class Rule(head: Predicate, body: List[Predicate]) extends Constraint

}
