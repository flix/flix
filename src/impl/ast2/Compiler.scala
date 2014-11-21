package impl.ast2

import impl.logic._

class Compiler(ast: Ast.Root) {

  def run(): Unit = {
    //println(typenv(ast))
    // phases: 1 desugar
    // phase2: environments
    // phase3: linking
    // phase4: translate
  }

  // environments
  // Typ Env
  // ValEnv
  // FunEnv
  // etc. etc.

  /**
   * Returns a map from (fully qualified) names to types.
   */
  def typenv(root: Ast.Root): Map[Ast.Name, Ast.Type] = {


    def visit(name: Ast.Name, node: Ast): Map[Ast.Name, Ast.Type] = ???

    ???
  }

  // TODO: Check
  // -unresolved references
  // -ambigious decls
  // -patterns with the same variable
  // -recursive types, calls, etc.

  // TODO: Deal with ambiquity


  // linking

  def link(ast: Ast.Type, typenv: Map[Ast.Name, Ast.Type]): Ast.Type = ast match {
    case Ast.Type.Bool => Ast.Type.Bool
    // ...
    case Ast.Type.NameRef(name) => typenv.getOrElse(name, ???)
  }

  // TODO: Need map.
  // TODO: Need fold.
  def visit[A](f: Ast => A): A = ???


  object TranslationPhase {

    def compile(exp: Ast.Expression): Term = exp match {
      case Ast.Expression.Unary(op, e1) =>
        // Eliminates unary plus and minus.
        val t1 = compile(e1)
        op match {
          case UnaryOperator.Not => Term.UnaryOp(UnaryOperator.Not, t1)
          case UnaryOperator.UnaryPlus => t1
          case UnaryOperator.UnaryMinus => Term.BinaryOp(BinaryOperator.Minus, Term.Int(0), t1)
        }

      case Ast.Expression.Binary(op, e1, e2) =>
        val t1 = compile(e1)
        val t2 = compile(e2)
        op match {
          case BinaryOperator.Plus => Term.BinaryOp(op, t1, t2)
          case BinaryOperator.Minus => Term.BinaryOp(op, t1, t2)
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
      case Ast.Pattern.Tag(name, p1) => Pattern.Tag(compile(name), compile(p1))
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
      case Ast.Type.Unit => Type.Unit
      case Ast.Type.Bool => Type.Bool
      case Ast.Type.Int => Type.Int
      case Ast.Type.Str => Type.Str
      // case Ast.Type.Tag(name) => Type.Tag(), TODO
      case Ast.Type.Set(typ1) => Type.Set(compile(typ1))
      case Ast.Type.Map(keys, values) => throw CompilerException("Map types are currently not supported.")
      case Ast.Type.NameRef(name) => throw CompilerException(s"Unresolved named type: $name.")
    }

    /**
     * Compiles an ast name into a named symbol.
     */
    private def compile(name: Ast.Name): Symbol.NamedSymbol = ???
  }


  case class CompilerException(msg: String) extends RuntimeException(msg)

}
