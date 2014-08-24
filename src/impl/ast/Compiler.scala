package impl.ast

import impl.logic.{ConstraintSystem, Declaration, Type, Value}

object Compiler {

  def parse(e: SExp): ConstraintSystem = ???

  def parseDeclaration(e: SExp): Declaration = e match {
    case SExp.Lst(SExp.Str("def-type") :: Nil) => ???
//    case "def-bot" => {
//      val List(typ, value) = ???
//      Declaration.DeclareBot(parseValue(???), parseType(???))
//    }
//    case "def-leq" => ???
//    case "def-lub" => ???
//    case "def-fun" => ???
//    case "constraint" => ???
  }

  def parseType(e: SExp): Type = ???


  def parseValue(e: SExp): Value = ???


}
