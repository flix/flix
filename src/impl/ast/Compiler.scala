package impl.ast

import java.io.File

import impl.logic._

object Compiler {

  def main(args: Array[String]): Unit = {
    val file = new File(args(0))
    val ast = Parser.parse(file)
    println(ast)

    //parse(ast)
  }

  def parse(e: SExp): ConstraintSystem = ???

  def parseDeclaration(e: SExp): Declaration = e match {
    case SExp.Lst(Keyword.DefBot :: Nil) => ???
    case SExp.Lst(Keyword.DefLeq :: Nil) => ???
    //    case "def-leq" => ???
    //    case "def-lub" => ???
    //    case "def-fun" => ???
    //    case "rule" => ???

  }

  def parseType(e: SExp): Type = e match {
    case SExp.Name(s) => ???
  }

  def parseTerm(e: SExp): Term = ???

  def parseValue(e: SExp): Value = e match {
    case Literal.Bool(b) => Value.Bool(b)
    case Literal.Int(i) => Value.Int(i)
    case Literal.Str(s) => Value.Str(s)
    case SExp.Lst(SExp.Name(s) :: e1 :: Nil) => Value.Tagged(Symbol.NamedSymbol(s), parseValue(e1), ???)

    case SExp.Lst(a :: b :: Nil) => ???
  }
}
