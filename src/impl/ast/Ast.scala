package impl.ast

import impl.logic.Symbol

trait Ast

/**
 * The top level is a list of declarations:
 *
 * (decl_1 ...)
 * ...
 * (decl_n ...)
 */
case class TopLevel(declarations: List[Decl])


sealed trait Decl

object Decl {

  case class DeclareType(name: Symbol.NamedSymbol) extends Decl

  case class DeclareBot(name: Symbol.NamedSymbol, typ: String, exp: Exp) extends Decl

  case class DeclareLeq(name: Symbol.NamedSymbol) extends Decl

  case class DeclareLub(name: Symbol.NamedSymbol) extends Decl

  case class DeclareFn(name: Symbol.NamedSymbol) extends Decl

  case class DeclareRule() extends Decl

}

sealed trait Exp

object Exp {

  case class FunctionExp(formals: List[(String, String)], body: Exp) extends Exp

  case class Variable(s: String) extends Exp

  case class Match(e: Exp, cases: List[(Exp, Exp)]) extends Exp

  case class Constructor0(s: String) extends Exp

  case class Constructor1(s: String, e1: Exp) extends Exp

  case class Constructor2(s: String, e1: Exp, e2: Exp) extends Exp

  case class Constructor3(s: String, e1: Exp, e2: Exp, e3: Exp) extends Exp

  case class Constructor4(s: String, e1: Exp, e2: Exp, e3: Exp, e4: Exp) extends Exp

  case class Constructor5(s: String, e1: Exp, e2: Exp, e3: Exp, e5: Exp) extends Exp


  case class Tuple2(e1: Exp, e2: Exp) extends Exp

}
