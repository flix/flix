package impl.ast

import java.io.File

import impl.logic._

object Compiler {

  def main(args: Array[String]): Unit = {
    val file = new File(args(0))
    val ast = Parser.parse(file)
    println(ast)

    println(compile(ast))
  }

  def compile(e: List[SExp]): Unit = {
    e map compileDeclaration
  }

  def compileDeclaration(e: SExp): Unit = e match {
    case SExp.Lst(SExp.Keyword("def-type") :: SExp.Name(n) :: typ :: Nil) => compileType(typ)
    case SExp.Lst(SExp.Keyword("def-bot") :: SExp.Name(n) :: v :: Nil) => compileValue(v)
    case SExp.Lst(SExp.Keyword("def-leq") :: SExp.Name(n) :: args :: body :: Nil) =>
    case SExp.Lst(SExp.Keyword("def-lub") :: SExp.Name(n) :: args :: body :: Nil) =>
    case SExp.Lst(SExp.Keyword("def-height") :: SExp.Name(n) :: args :: body :: Nil) => compileTerm(body)
    case SExp.Lst(SExp.Keyword("def-fun") :: SExp.Name(n) :: args :: body :: Nil) =>
    case SExp.Lst(SExp.Keyword("rule") :: head :: tail) =>
  }

  def compileType(e: SExp): Type = e match {
    case SExp.Name(s) => Type.Tagged(Symbol.NamedSymbol(s), Type.Unit)
    case SExp.Lst(xs) => Type.Sum(xs.map(compileType))
  }

  def compileTerm(e: SExp): Term = e match {
    case SExp.Lst(SExp.Keyword("match") :: exp :: cases :: Nil) => ???
  }

  def compilePattern(s: SExp): Pattern = ???


  def compileValue(e: SExp): Value = e match {
    case SExp.Unit => Value.Unit
    case SExp.Bool(b) => Value.Bool(b)
    case SExp.Int(i) => Value.Int(i)
    case SExp.Str(s) => Value.Str(s)
    case SExp.Name(n) => Value.Tagged(Symbol.NamedSymbol(n), Value.Unit, Type.Sum(List.empty)) // TODO: Type
    case SExp.Lst(List(SExp.Name(s), e1)) => Value.Tagged(Symbol.NamedSymbol(s), compileValue(e1), ???) // TODO: Type
    case SExp.Lst(List(e1, e2)) => Value.Tuple2(compileValue(e1), compileValue(e2))
    case SExp.Lst(List(e1, e2, e3)) => Value.Tuple3(compileValue(e1), compileValue(e2), compileValue(e3))
    case SExp.Lst(List(e1, e2, e3, e4)) => Value.Tuple4(compileValue(e1), compileValue(e2), compileValue(e3), compileValue(e4))
    case SExp.Lst(List(e1, e2, e3, e4, e5)) => Value.Tuple5(compileValue(e1), compileValue(e2), compileValue(e3), compileValue(e4), compileValue(e5))
  }
}
