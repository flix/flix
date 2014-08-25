package impl.ast

import java.io.File

import impl.logic._

object Compiler {

  def main(args: Array[String]): Unit = {
    val file = new File(args(0))
    val ast = Parser.parse(file)
    println(ast)

    println(parse(ast))
  }

  def parse(e: List[SExp]): Unit = {
    e map parseDeclaration
  }

  def parseDeclaration(e: SExp): Unit = e match {
    case SExp.Lst(SExp.Keyword("def-type") :: Literal.Name(n) :: typ :: Nil) => parseType(typ)
    case SExp.Lst(SExp.Keyword("def-bot") :: Literal.Name(n) :: v :: Nil) => parseValue(v)
    case SExp.Lst(SExp.Keyword("def-leq") :: Literal.Name(n) :: args :: body :: Nil) =>
    case SExp.Lst(SExp.Keyword("def-lub") :: Literal.Name(n) :: args :: body :: Nil) =>
    case SExp.Lst(SExp.Keyword("def-height") :: Literal.Name(n) :: args :: body :: Nil) =>
    case SExp.Lst(SExp.Keyword("def-fun") :: Literal.Name(n) :: args :: body :: Nil) =>
    case SExp.Lst(SExp.Keyword("rule") :: head :: tail) =>
  }

  def parseType(e: SExp): Type = e match {
    case Literal.Name(s) => Type.Tagged(Symbol.NamedSymbol(s), Type.Unit)
    case SExp.Lst(xs) => Type.Sum(xs.map(parseType))
  }

  def parseTerm(e: SExp): Term = ???

  def parseValue(e: SExp): Value = e match {
    case Literal.Name(n) => Value.Tagged(Symbol.NamedSymbol(n), Value.Unit, Type.Sum(List.empty))
    case Literal.Bool(b) => Value.Bool(b)
    case Literal.Int(i) => Value.Int(i)
    case Literal.Str(s) => Value.Str(s)
    case SExp.Lst(Literal.Name(s) :: e1 :: Nil) => Value.Tagged(Symbol.NamedSymbol(s), parseValue(e1), ???)

    case SExp.Lst(a :: b :: Nil) => ???
  }
}
