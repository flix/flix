package impl.ast

import impl.logic.{Program, Term}

object Compiler {

  def compile(ast: Ast): Program = ???

  def compile(e: Exp): Term = e match {
    case Exp.FunctionExp(formals, body) => ???
  }



}
