package impl.ast

import impl.logic.{ConstraintSystem, Declaration, Type, Value}

object Compiler {

  def parse(e: SExp): ConstraintSystem = ???

  def parseDeclaration(e: SExp): Declaration = e.head match {
    case "def-type" => ???
    case "def-bot" => {
      val List(typ, value) = e.body
      Declaration.DeclareBot(parseValue(value), parseType(typ))
    }
    case "def-leq" => ???
    case "def-lub" => ???
    case "def-fun" => ???
    case "constraint" => ???
  }

  def parseType(e: SExp): Type = ???


  def parseValue(e: SExp): Value = ???


}
