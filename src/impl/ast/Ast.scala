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
case class TopLevel(declarations: List[Declaration])

sealed trait Declaration

object Declaration {

  case class DeclareType(name: Symbol.NamedSymbol) extends Declaration

  /**
   * Declaration of the bottom element for a named type.
   *
   * (def-bot name exp)
   */
  case class DeclareBot(name: Symbol.TypeSymbol, exp: Exp) extends Declaration

  /**
   * Declaration of the leq relation for a named type.
   *
   * (def-leq name (arg_1 type_1 ... arg_n type_n) exp)
   */
  case class DeclareLeq(name: Symbol.NamedSymbol, args: List[String], exp: Exp) extends Declaration

  case class DeclareLub(name: Symbol.NamedSymbol) extends Declaration

  case class DeclareFn(name: Symbol.NamedSymbol) extends Declaration

  case class DeclareRule() extends Declaration

}

sealed trait Exp

object Exp {

  case class FunctionExp(formals: List[(String, String)], body: Exp) extends Exp

  case class Variable(s: String) extends Exp


  case class Tuple2(e1: Exp, e2: Exp) extends Exp

}
