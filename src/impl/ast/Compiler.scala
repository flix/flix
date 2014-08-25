package impl.ast

import impl.logic._

import scala.collection.mutable
import java.io.File

// TODO: Rename: def-type to def lattice?

object Compiler {

  val types = mutable.Map.empty[SExp.Name, Type]
  val funcs = mutable.Map.empty[SExp.Name, Term]

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
    case SExp.Lst(SExp.Keyword("def-bot") :: SExp.Name(n) :: v :: Nil) => compileTerm(v)
    case SExp.Lst(SExp.Keyword("def-leq") :: SExp.Name(n) :: args :: body :: Nil) =>
    case SExp.Lst(SExp.Keyword("def-lub") :: SExp.Name(n) :: args :: body :: Nil) =>
    case SExp.Lst(SExp.Keyword("def-height") :: SExp.Name(n) :: args :: body :: Nil) => compileTerm(body)
    case SExp.Lst(SExp.Keyword("def-fun") :: SExp.Name(n) :: args :: body :: Nil) =>
    case SExp.Lst(SExp.Keyword("rule") :: head :: tail) => compileRule(head, tail)
  }

  def compileRule(head: SExp, body: List[SExp]): Unit = ???

  def compilePredicate(e: SExp): Unit = ???

  def compileTerm(e: SExp): Term = e match {
    case SExp.Lst(SExp.Keyword("match") :: exp :: cases) => compileTerm(exp); Term.Unit // TODO

    case SExp.Unit => Term.Unit
    case SExp.Bool(b) => Term.Bool(b)
    case SExp.Int(i) => Term.Int(i)
    case SExp.Str(s) => Term.Str(s)
    case SExp.Name(n) => Term.Tagged(Symbol.NamedSymbol(n), Term.Unit, Type.Sum(List.empty)) // TODO: Type
    case SExp.Lst(List(SExp.Name(s), e1)) => Term.Tagged(Symbol.NamedSymbol(s), compileTerm(e1), ???) // TODO: Type
    case SExp.Lst(List(e1, e2)) => Term.Tuple2(compileTerm(e1), compileTerm(e2))
    case SExp.Lst(List(e1, e2, e3)) => Term.Tuple3(compileTerm(e1), compileTerm(e2), compileTerm(e3))
    case SExp.Lst(List(e1, e2, e3, e4)) => Term.Tuple4(compileTerm(e1), compileTerm(e2), compileTerm(e3), compileTerm(e4))
    case SExp.Lst(List(e1, e2, e3, e4, e5)) => Term.Tuple5(compileTerm(e1), compileTerm(e2), compileTerm(e3), compileTerm(e4), compileTerm(e5))

    case _ => throw new RuntimeException(s"Unexpected term: $e")
  }

  def compilePattern(e: SExp): Pattern = e match {
    case SExp.Var(x) => Pattern.Var(Symbol.VariableSymbol(x))
    case SExp.Lst(List(e1, e2)) => Pattern.Tuple2(compilePattern(e1), compilePattern(e2))
    case SExp.Lst(List(e1, e2, e3)) => Pattern.Tuple3(compilePattern(e1), compilePattern(e2), compilePattern(e3))
    case SExp.Lst(List(e1, e2, e3, e4)) => Pattern.Tuple4(compilePattern(e1), compilePattern(e2), compilePattern(e3), compilePattern(e4))
    case SExp.Lst(List(e1, e2, e3, e4, e5)) => Pattern.Tuple5(compilePattern(e1), compilePattern(e2), compilePattern(e3), compilePattern(e4), compilePattern(e5))
    case _ => throw new RuntimeException(s"Unexpected pattern: $e")
  }

  def compileType(e: SExp): Type = e match {
    case SExp.Name(s) => Type.Tagged(Symbol.NamedSymbol(s), Type.Unit)
    case SExp.Lst(types) => Type.Sum(types.map(compileType))
    case _ => throw new RuntimeException(s"Unexpected type: $e")
  }

}
