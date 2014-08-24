package impl.ast

import impl.logic._

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

  def parseValue(e: SExp): Value = e match {
    case SExp.Bool(b) => Value.Bool(b)
    case SExp.Int(i) => Value.Int(i)
    case SExp.Str(s) => Value.Str(s)
    case SExp.Lst(SExp.Name(s) :: e1 :: Nil) => Value.Tagged(Symbol.NamedSymbol(s), parseValue(e1), ???)

    case SExp.Lst(a :: b :: Nil) => ???
  }
}
