package impl.logic

case class ConstraintSystem(decl: List[Declaration])

trait Declaration

object Declaration {

  case class DeclareType(name: Symbol.NamedSymbol, typ: Type) extends Declaration

  case class DeclareBot(name: Symbol.NamedSymbol, v: Value, typ: Type) extends Declaration

  case class DeclareLeq(name: Symbol.FunctionSymbol, t: Term.Abs, typ: Type) extends Declaration

  case class DeclareLub(name: Symbol.FunctionSymbol, t: Term.Abs, typ: Type) extends Declaration

  case class DeclareFun(name: Symbol.FunctionSymbol, t: Term.Abs, typ: Type) extends Declaration

  case class DeclareClause(clause: Clause) extends Declaration

}

trait Clause

object Clause {

  case class Fact(head: Predicate) extends Clause

  case class Rule(head: Predicate, body: List[Predicate]) extends Clause

}
