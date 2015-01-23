package impl.ast2

import impl.ast2.Ast.Predicate

object Compiler {

  def compile(ast: Ast.Root): Ast.Root = {
    println(ast)
    val env = Symbols.visit(ast)
    Disambiguation.disambiguate(ast, env)

  }

  /**
   * A compiler-phases which constructs environments (i.e. the symbol table).
   */
  object Symbols {

    /**
     * A (fully qualified) name is a list of strings.
     */
    // TODO Move into Ast and change to Seq.
    type Name = List[String]

    /**
     * An environment is map from names to ast declaractions.
     *
     * An environment may contain multiple declaractions for the same names:
     *
     * (1) Names may be overloaded for values, types, etc.
     * (2) Names may be ambiguous.
     */
    // Todo: Move into some abstract compiler trait.
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
      case decl: Ast.Declaration.Tpe => environmentOf(withSuffix(namespace, decl.name) -> decl)
      case decl: Ast.Declaration.Val => environmentOf(withSuffix(namespace, decl.name) -> decl)
      case decl: Ast.Declaration.Var => environmentOf(withSuffix(namespace, decl.name) -> decl)
      case decl: Ast.Declaration.Fun => environmentOf(withSuffix(namespace, decl.name) -> decl)
      case decl: Ast.Declaration.Enum =>
        val init = environmentOf(withSuffix(namespace, decl.name) -> decl)
        decl.tpe.elms.foldLeft(init) {
          case (env, tag) => env ++ environmentOf(withSuffix(namespace, tag.name) -> decl)
        }
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


  // TODO: Check
  // -unresolved references
  // -ambigious decls
  // -patterns with the same variable
  // -recursive types, calls, etc.
  /**
   * A compiler-phase which replaces name references by their actuals.
   */
  object Disambiguation {

    import Symbols._

    /**
     * Disambiguates the given `ast` using the given environment `env`.
     */
    def disambiguate(ast: Ast.Root, env: Environment): Ast.Root = Ast.Root(ast.decls map {
      case decl => disambiguate(decl, Nil, env)
    })


    /**
     * Disambiguates the given `ast` declaraction in the `namespace` using the given environment `env`.
     */
    def disambiguate(ast: Ast.Declaration, namespace: Name, env: Environment): Ast.Declaration = ast match {
      case Ast.Declaration.NameSpace(name, body) => Ast.Declaration.NameSpace(name, body map {
        case decl => disambiguate(decl, withSuffix(namespace, name), env)
      })

      case Ast.Declaration.Tpe(name, tpe) =>
        Ast.Declaration.Tpe(name, disambiguate(tpe, namespace, env))

      case Ast.Declaration.Enum(name, tpe) =>
        Ast.Declaration.Enum(name, disambiguate(tpe, namespace, env).asInstanceOf[Ast.Type.Enum])

      case Ast.Declaration.Val(name, tpe, exp) =>
        val tpe2 = disambiguate(tpe, namespace, env)
        val exp2 = disambiguate(namespace, exp, env, Set.empty)
        Ast.Declaration.Val(name, tpe2, exp2)

      case Ast.Declaration.Var(name, lat) => Ast.Declaration.Var(name, disambiguate(lat, namespace, env))

      case Ast.Declaration.Fun(annotations, name, args, tpe, exp) =>
        val args2 = args.map {
          case (argName, argType) => (argName, disambiguate(argType, namespace, env))
        }
        val bound = args.map(_._1).toSet
        val returnTpe = disambiguate(tpe, namespace, env)
        val bodyExp = disambiguate(namespace, exp, env, bound)
        Ast.Declaration.Fun(annotations, name, args2, returnTpe, bodyExp)

      case decl: Ast.Declaration.Lattice => decl.copy(record = disambiguate(namespace, decl.record, env, Set.empty))

      case Ast.Declaration.Fact(head) => Ast.Declaration.Fact(disambiguate(head, namespace, env))

      case Ast.Declaration.Rule(head, body) => ast // TODO
    }

    /**
     * Disambiguates the given expression `ast`.
     */
    // TODO: Change order of arguments
    def disambiguate(namespace: Name, ast: Ast.Expression, env: Environment, bound: Set[String]): Ast.Expression = ast match {
      case Ast.Expression.AmbiguousName(name) => name match {
        case Seq(simple) if bound contains simple => Ast.Expression.Var(simple)
        case _ => lookupExp(namespace, name.toList, env)
      }

      case Ast.Expression.Var(name) => ???
      case Ast.Expression.Lit(literal) => ast
      case Ast.Expression.Unary(op, e) => Ast.Expression.Unary(op, disambiguate(namespace, e, env, bound))
      case Ast.Expression.Binary(e1, op, e2) => Ast.Expression.Binary(disambiguate(namespace, e1, env, bound), op, disambiguate(namespace, e2, env, bound))
      case Ast.Expression.Infix(e1, name, e2) => ???
      case Ast.Expression.Let(name, value, body) => ???
      case Ast.Expression.IfThenElse(e1, e2, e3) =>
        val a1 = disambiguate(namespace, e1, env, bound)
        val a2 = disambiguate(namespace, e2, env, bound)
        val a3 = disambiguate(namespace, e3, env, bound)
        Ast.Expression.IfThenElse(a1, a2, a3)

      case Ast.Expression.Match(exp, rules) =>
        val dexp = disambiguate(namespace, exp, env, bound)
        val drules = rules map {
          case (p, e) => (p, disambiguate(namespace, e, env, bound)) // TODO: Variables bound by pattern.
        }
        Ast.Expression.Match(dexp, drules)

      case Ast.Expression.AmbiguousCall(name, args) =>
        val args2 = args map (a => disambiguate(namespace, a, env, bound))
        Ast.Expression.AmbiguousCall(name, args2)

      case Ast.Expression.Tag(name, e) => ???

      case Ast.Expression.Set(elms) => ???

      case Ast.Expression.Tuple(elms) =>
        val delms = elms map (e => disambiguate(namespace, e, env, bound))
        Ast.Expression.Tuple(delms)

      case Ast.Expression.Record(elms) =>
        val delms = elms map {
          case (name, e) => (name, disambiguate(namespace, e, env, bound))
        }
        Ast.Expression.Record(delms)

      case Ast.Expression.Error => Ast.Expression.Error
    }

    import Ast._

    /**
     * Disambiguates the given type `ast`.
     */
    def disambiguate(ast: Ast.Type, namespace: Name, env: Environment): Ast.Type = ast match {
      // Primitives
      case Type.Unit => Type.Unit
      case Type.Bool => Type.Bool
      case Type.Int => Type.Int
      case Type.Str => Type.Str
      // Ambiguous
      case Type.AmbiguousName(Seq("Unit")) => Type.Unit
      case Type.AmbiguousName(Seq("Bool")) => Type.Bool
      case Type.AmbiguousName(Seq("Int")) => Type.Int
      case Type.AmbiguousName(Seq("Str")) => Type.Str
      case Type.AmbiguousName(name) => lookupType(namespace, name.toList, env)

      // Compound
      case Type.Tag(name) => Type.Tag(name)
      case Type.Tuple(elms) => Type.Tuple(elms map (e => disambiguate(e, namespace, env)))
      case Type.Set(elms) => Type.Set(disambiguate(elms, namespace, env))
      //case Type.Map(elms) => ???
      case Type.Enum(elms) =>
        val elms2 = elms map (e => disambiguate(e, namespace, env).asInstanceOf[Ast.Type.Tag])
        Type.Enum(elms2)

      case Type.Function(t1, t2) => ???
    }

    def disambiguate(predicate: Predicate, name: Name, env: Environment): Predicate =
      ???




    def lookupType(namespace: Name, name: Name, env: Environment): Ast.Type = {
      lookupType(namespace ::: name, env).
        orElse(lookupType(name, env)).getOrElse(throw new CompilerException(s"Name not found $name"))
    }

    def lookupType(name: Name, env: Environment): Option[Ast.Type] = {
      val candidates = env.get(name).collect {
        case d: Ast.Declaration.Tpe => d.typ
        case d: Ast.Declaration.Enum => d.tpe
      }
      if (candidates.size > 1) {
        throw new CompilerException(s"Ambiguous name: $name")
      }
      candidates.headOption
    }


    // TODO: Messy. Rewrite.
    def lookupExp(namespace: Name, name: Name, env: Environment): Ast.Expression = {
      lookupExp(namespace ::: name, env).
        orElse(lookupExp(name, env)).getOrElse(throw new CompilerException(s"Name not found $name"))
    }

    def lookupExp(name: Name, env: Environment): Option[Ast.Expression] = {
      val candidates = env.get(name).collect {
        case d: Ast.Declaration.Val => d.exp
        case d: Ast.Declaration.Fun => d.body
        case d: Ast.Declaration.Enum => d.tpe.elms.find(tag => tag.name == name.last).map(tag => Ast.Expression.Tag(tag.name, Ast.Expression.Lit(Ast.Literal.Unit))).get
      }
      if (candidates.size > 1) {
        throw new CompilerException(s"Ambiguous name: $name")
      }
      candidates.headOption
    }

  }

  // TODO: Introduce bound methods for pattern and function.

  case class CompilerException(msg: String) extends RuntimeException(msg)


  // TODO: Move somewhere appropiate.
  object MultiMap {
    def empty[K, V]: MultiMap[K, V] = new MultiMap[K, V](Map.empty[K, Set[V]])

    def apply[K, V](kv: (K, V)): MultiMap[K, V] = new MultiMap[K, V](Map[K, Set[V]](kv._1 -> Set(kv._2)))
  }

  class MultiMap[K, V](val m: Map[K, Set[V]]) {
    def get(k: K): Set[V] = m.getOrElse(k, Set.empty[V])

    def ++(that: MultiMap[K, V]): MultiMap[K, V] = new MultiMap(
      (that.m foldLeft this.m) {
        case (acc, (thatKey, thatValues)) =>
          val thisValues = acc.getOrElse(thatKey, Set.empty)
          acc + (thatKey -> (thisValues ++ thatValues))
      }
    )

    override def toString: String = m.toString()
  }

}
