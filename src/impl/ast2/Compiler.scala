package impl.ast2

import impl.logic._

import scala.collection.immutable

object Compiler {

  def compile(ast: Ast.Root): Ast.Root = {
    val ast2 = Desugaring.desugar(ast)
    Linking.link(ast2)
  }

  object Desugaring {

    /**
     * Desugars the entire ast.
     */
    def desugar(ast: Ast.Root): Ast.Root = ast match {
      case Ast.Root(decls) => Ast.Root(decls map desugar)
    }

    /**
     * Desugars the given ast declaration.
     */
    def desugar(ast: Ast.Declaration): Ast.Declaration = ast match {
      case Ast.Declaration.TypeDecl(name, typ) => Ast.Declaration.TypeDecl(name, desugar(typ))
      case _ => ast; // TODO
    }

    /**
     * Desugars the given ast type.
     */
    def desugar(ast: Ast.Type): Ast.Type = ast match {
      case Ast.Type.NameRef(Seq("Unit")) => Ast.Type.Unit
      case Ast.Type.NameRef(Seq("Bool")) => Ast.Type.Bool
      case Ast.Type.NameRef(Seq("Int")) => Ast.Type.Int
      case Ast.Type.NameRef(Seq("Str")) => Ast.Type.Str

      case Ast.Type.Tuple(elms) => Ast.Type.Tuple(elms map desugar)
      case Ast.Type.Set(elms) => Ast.Type.Set(desugar(elms))
      case Ast.Type.Rel(elms) => ???
      case Ast.Type.Map(elms) => Ast.Type.Map(elms map desugar)

      case Ast.Type.Function(typ1, typ2) => Ast.Type.Function(desugar(typ1), desugar(typ2))

      case _ => ast // TODO
    }
  }


  // TODO: Check
  // -unresolved references
  // -ambigious decls
  // -patterns with the same variable
  // -recursive types, calls, etc.


  object Linking {

    def link(ast: Ast.Root): Ast.Root = {
      ???
    }


    def visit(ast: Ast.Root): Map[List[String], Ast.Type] = ast match {
      case Ast.Root(decls) => merge(decls map (d => visit(Nil, d)))
    }

    def visit(namespace: List[String], ast: Ast.Declaration): Map[List[String], Ast.Type] = ast match {
      case Ast.Declaration.TypeDecl(name, typ) => Map((namespace ::: List(name)) -> typ)
    }

    def merge(xs: Seq[Map[List[String], Ast.Type]]): Map[List[String], Ast.Type] = ???

    def merge(m1: Map[List[String], Ast.Type], m2: Map[List[String], Ast.Type]): Map[List[String], Ast.Type] = ???


  }

  object Translation {

    def compile(decl: Ast.Declaration) = ???

    def compile(exp: Ast.Expression): Term = exp match {
      case Ast.Expression.Var(x) => Term.Var(Symbol.VariableSymbol(x))

      case Ast.Expression.IfThenElse(e1, e2, e3) => ???

      case Ast.Expression.Match(e, rules) => ???

      case Ast.Expression.Unary(op, e1) =>
        // Eliminates unary plus and minus.
        val t1 = compile(e1)
        op match {
          case UnaryOperator.Not => Term.UnaryOp(UnaryOperator.Not, t1)
          case UnaryOperator.UnaryPlus => t1
          case UnaryOperator.UnaryMinus => Term.BinaryOp(BinaryOperator.Minus, Term.Int(0), t1)
        }

      case Ast.Expression.Binary(e1, op, e2) =>
        val t1 = compile(e1)
        val t2 = compile(e2)
        op match {
          case BinaryOperator.Plus => Term.BinaryOp(op, t1, t2)
          case BinaryOperator.Minus => Term.BinaryOp(op, t1, t2)
          case BinaryOperator.Times => Term.BinaryOp(op, t1, t2)
          case BinaryOperator.Divide => Term.BinaryOp(op, t1, t2)

          case BinaryOperator.Equal => Term.BinaryOp(op, t1, t2)
          case BinaryOperator.NotEqual => Term.UnaryOp(UnaryOperator.Not, Term.BinaryOp(BinaryOperator.Equal, t1, t2))

          case BinaryOperator.Greater => Term.BinaryOp(op, t1, t2)
          //case BinaryOperator.GreaterEqual => Term.BinaryOp
          case BinaryOperator.Less => Term.BinaryOp(BinaryOperator.Greater, t2, t1)

        }
    }

    /**
     * Compiles an ast pattern to a core pattern.
     */
    private def compile(pattern: Ast.Pattern): Pattern = pattern match {
      case Ast.Pattern.Wildcard => Pattern.Wildcard
      case Ast.Pattern.Var(name) => Pattern.Var(Symbol.VariableSymbol(name))
      case Ast.Pattern.Bool(literal) => Pattern.Bool(literal)
      case Ast.Pattern.Int(literal) => Pattern.Int(literal)
      case Ast.Pattern.Str(literal) => Pattern.Str(literal)
      //case Ast.Pattern.Tag(name, p1) => Pattern.Tag(compile(name), compile(p1))
      case Ast.Pattern.Tuple(Seq(p1, p2)) => Pattern.Tuple2(compile(p1), compile(p2))
      case Ast.Pattern.Tuple(Seq(p1, p2, p3)) => Pattern.Tuple3(compile(p1), compile(p2), compile(p3))
      case Ast.Pattern.Tuple(Seq(p1, p2, p3, p4)) => Pattern.Tuple4(compile(p1), compile(p2), compile(p3), compile(p4))
      case Ast.Pattern.Tuple(Seq(p1, p2, p3, p4, p5)) => Pattern.Tuple5(compile(p1), compile(p2), compile(p3), compile(p4), compile(p5))
      case Ast.Pattern.Tuple(elms) => throw new CompilerException("Tuples with more than 5 elements are not yet supported.")
    }


    /**
     * Compiles an ast type to a core type.
     */
    private def compile(typ: Ast.Type): Type = typ match {
      // case Ast.Type.Tag(name) => Type.Tag(), TODO
      case Ast.Type.Set(typ1) => Type.Set(compile(typ1))
      //case Ast.Type.Map(keys, values) => throw CompilerException("Map types are currently not supported.")
      case Ast.Type.NameRef(name) => throw CompilerException(s"Unresolved named type: $name.")
    }

    /**
     * Compiles an ast name into a named symbol.
     */
  }


  case class CompilerException(msg: String) extends RuntimeException(msg)

}
