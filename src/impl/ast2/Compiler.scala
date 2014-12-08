package impl.ast2

import impl.logic._

import scala.collection.immutable

object Compiler {

  def compile(ast: Ast.Root): Ast.Root = {
    val ast2 = Desugaring.desugar(ast)
    val env = Environments.visit(ast);

    println(env)

    ast
  }

  /**
   * An environment which performs desugaring.
   */
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

  /**
   * A compiler-phases which constructs environments (i.e. the symbol table).
   */
  object Environments {

    /**
     * A (fully qualified) name is a list of strings.
     */
    type Name = List[String]

    /**
     * An environment is map from names to ast declaractions.
     *
     * An environment may contain multiple declaractions for the same names:
     *
     * (1) Names may be overloaded for values, types, etc.
     * (2) Names may be ambiguous.
     */
    type Environment = MultiMap[Name, Ast.Declaration]

    /**
     * The empty environment.
     */
    val Empty = MultiMap.empty[Name, Ast.Declaration]

    /**
     * Returns an environment with the given mapping.
     */
    def environmentOf(kv: (Name, Ast.Declaration)): Environment = MultiMap(kv)

    /**
     * Returns a map from fully qualified names to ast declarations.
     */
    def visit(ast: Ast.Root): Environment = ast match {
      case Ast.Root(decls) => (decls foldLeft Empty) {
        case (env, decl) => env ++ visit(Nil, decl)
      }
    }

    /**
     * Returns a map from fully qualified names to ast declaractions assuming the declarations reside under the given namespace.
     */
    def visit(namespace: Name, ast: Ast.Declaration): Environment = ast match {
      case Ast.Declaration.NameSpace(name, body) => (body foldLeft Empty) {
        case (env, decl) => env ++ visit(withSuffix(namespace, name), decl)
      }
      case decl: Ast.Declaration.TypeDecl => environmentOf(withSuffix(namespace, decl.name) -> decl)
      case decl: Ast.Declaration.Val => environmentOf(withSuffix(namespace, decl.name) -> decl)
      case decl: Ast.Declaration.Var => environmentOf(withSuffix(namespace, decl.name) -> decl)
      case decl: Ast.Declaration.Fun => environmentOf(withSuffix(namespace, decl.name) -> decl)
      case decl: Ast.Declaration.Lattice => Empty
      case decl: Ast.Declaration.Fact => Empty
      case decl: Ast.Declaration.Rule => Empty
    }

    /**
     * Returns `name` . `suffix`.
     */
    def withSuffix(name: Name, suffix: String): Name = name ::: List(suffix)

    /**
     * Returns `name` . `suffix`.
     */
    def withSuffix(name: Name, suffix: Seq[String]): Name = name ::: suffix.toList
  }

  object Linking {
    // replaces all names by their actuals.

    def link(ast: Ast.Root, env: MultiMap[List[String], Ast.Declaration]): Ast.Root = ???

    def link(ast: Ast.Expression, env: MultiMap[List[String], Ast.Declaration], bound: Set[String]): Ast.Expression = ast match {
      case Ast.Expression.VarOrNameRef(name) => ??? // todo lookup
    }

    def link(ast: Ast.Type, namespace: List[String], env: Map[List[String], Ast.Declaration]): Ast.Type = ast match {
      case Ast.Type.NameRef(name) => ???
    }

    def lookupType(name: List[String], namespace: List[String], env: MultiMap[List[String], Ast.Declaration]): Ast.Type = ???

  }

  // TODO: Move somewhere appropiate.
  object MultiMap {
    def empty[K, V]: MultiMap[K, V] = new MultiMap[K, V](Map.empty[K, Set[V]])

    def apply[K, V](kv: (K, V)): MultiMap[K, V] = new MultiMap[K, V](Map[K, Set[V]](kv._1 -> Set(kv._2)))
  }

  class MultiMap[K, V](val m: Map[K, Set[V]]) {
    def ++(that: MultiMap[K, V]): MultiMap[K, V] = new MultiMap(
      (that.m foldLeft this.m) {
        case (acc, (thatKey, thatValues)) =>
          val thisValues = acc.getOrElse(thatKey, Set.empty)
          acc + (thatKey -> (thisValues ++ thatValues))
      }
    )

    override def toString: String = m.toString()
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
