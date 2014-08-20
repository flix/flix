package impl.ast

import impl.logic.{Symbol, Term, Type, Value}

sealed trait Decl

object Decl {

  case class DeclareType(name: Symbol.NamedSymbol, typ: Type) extends Decl

  case class DeclareBot(name: Symbol.NamedSymbol, value: Value) extends Decl

  case class DeclareLeq(name: Symbol.NamedSymbol, term: Term) extends Decl

  case class DeclareLub(name: Symbol.NamedSymbol, term: Term) extends Decl

  case class DeclareFn(name: Symbol.NamedSymbol, term: Term) extends Decl

  case class DeclareRule() extends Decl

}


