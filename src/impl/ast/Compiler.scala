package impl.ast

import impl.logic.{Symbol, Type, Program, Term}

object Compiler {

  def compile(ast: Ast): Program = ???

  def compile(e: Exp): Term = e match {
    case Exp.FunctionExp(formals, body) =>
      (compile(body) /: formals) {
        case (exp, (arg, typ)) => Term.Abs(symbolOf(arg), typeOf(typ), exp)
      }
  }

  def symbolOf(s: String): Symbol.VariableSymbol = ???

  def typeOf(s: String): Type = ???

}
