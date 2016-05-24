package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast.{NamedAst, SourceLocation, Symbol, WeededAst}
import ca.uwaterloo.flix.language.{CompilationError, Compiler}
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

import scala.collection.mutable

// TODO: Cleanup TODOs and docs before committing.

object Namer {

  import NamerError._

  /**
    * A common super-type for naming errors.
    */
  sealed trait NamerError extends CompilationError

  object NamerError {

    implicit val consoleCtx = Compiler.ConsoleCtx

    /**
      * An error raised to indicate that the given `name` is used for multiple definitions.
      *
      * @param name the name.
      * @param loc1 the location of the first definition.
      * @param loc2 the location of the second definition.
      */
    case class DuplicateDefinition(name: String, loc1: SourceLocation, loc2: SourceLocation) extends NamerError {
      val message =
        s"""${consoleCtx.blue(s"-- NAMING ERROR -------------------------------------------------- ${loc1.source.format}")}
           |
           |${consoleCtx.red(s">> Duplicate definition of the name '$name'.")}
           |
           |First definition was here:
           |${loc1.underline}
           |Second definition was here:
           |${loc2.underline}
           |Tip: Consider renaming or removing one of the definitions.
         """.stripMargin
    }

  }

  /**
    * Performs naming on the given `program`.
    */
  def namer(program: WeededAst.Program)(implicit genSym: GenSym): Validation[NamedAst.Program, NamerError] = {

    val prog = NamedAst.Program(Map.empty, program.hooks, program.time)

    for (root <- program.roots; decl <- root.decls) {
      Declarations.namer(decl, prog)
    }

    NamedAst.Program(Map.empty, program.hooks, program.time).toSuccess // TODO
  }

  object Declarations {

    /**
      * Performs naming on the given declaration `decl0` under the given (partial) program `prog0`.
      */
    def namer(decl0: WeededAst.Declaration, prog0: NamedAst.Program)(implicit genSym: GenSym): Validation[NamedAst.Program, NamerError] = decl0 match {
      /*
       * Namespace.
       */
      case WeededAst.Declaration.Namespace(name, decls, loc) => Validation.fold(decls, prog0) {
        case (prog, decl) => namer(decl, prog)
      }

      /*
       * Definition.
       */
      case WeededAst.Declaration.Definition(ann, idents, params, exp, tpe, loc) =>
        // TODO: Lookup defn
        Expressions.namer(exp, Map.empty) map {
          case e => prog0
        }

      /*
       * Signature.
       */
      case WeededAst.Declaration.Signature(ident, params, tpe, loc) => ???

      /*
       * External.
       */
      case WeededAst.Declaration.External(ident, params, tpe, loc) => ???

      /*
       * Law.
       */
      case WeededAst.Declaration.Law(ident, tparams, params, tpe, exp, loc) => ???

      /*
       * Enum.
       */
      case WeededAst.Declaration.Enum(ident, cases, loc) => ???

      /*
       * Class.
       */
      case WeededAst.Declaration.Class(ident, tparams, decls, loc) => ???

      /*
       * Impl.
       */
      case WeededAst.Declaration.Impl(ident, tparams, decls, loc) => ???

      /*
       * Fact.
       */
      case WeededAst.Declaration.Fact(head, loc) => ???

      /*
       * Rule.
       */
      case WeededAst.Declaration.Rule(head, body, loc) => ???

      /*
       * Index.
       */
      case WeededAst.Declaration.Index(ident, indexes, loc) => ???

      /*
       * BoundedLattice (deprecated).
       */
      case WeededAst.Declaration.BoundedLattice(tpe, bot, top, leq, lub, glb, loc) => ???

      /*
       * Relation.
       */
      case WeededAst.Table.Relation(ident, attr, loc) => ???

      /*
       * Lattice.
       */
      case WeededAst.Table.Lattice(ident, keys, value, loc) => ???

    }

  }

  object Expressions {

    /**
      * Performs naming on the given expression `exp0` under the given environment `env0`.
      */
    def namer(exp0: WeededAst.Expression, env0: Map[String, Symbol.VarSym])(implicit genSym: GenSym): Validation[NamedAst.Expression, NamerError] = exp0 match {
      /*
       * Variables.
       */
      case WeededAst.Expression.Wild(loc) => NamedAst.Expression.Wild(id(), loc).toSuccess

      case WeededAst.Expression.Var(name, loc) if name.isUnqualified =>
        // lookup the variable name in the environment.
        env0.get(name.ident.name) match {
          case None =>
            // Case 1: reference.
            NamedAst.Expression.Ref(id(), name, loc).toSuccess
          case Some(sym) =>
            // Case 2: variable.
            NamedAst.Expression.Var(id(), sym, loc).toSuccess
        }

      case WeededAst.Expression.Var(name, loc) =>
        NamedAst.Expression.Ref(id(), name, loc).toSuccess

      /*
       * Literals.
       */
      case WeededAst.Expression.Unit(loc) => NamedAst.Expression.Unit(id(), loc).toSuccess

      case WeededAst.Expression.True(loc) => NamedAst.Expression.True(id(), loc).toSuccess

      case WeededAst.Expression.False(loc) => NamedAst.Expression.False(id(), loc).toSuccess

      case WeededAst.Expression.Char(lit, loc) => NamedAst.Expression.Char(id(), lit, loc).toSuccess

      case WeededAst.Expression.Float32(lit, loc) => NamedAst.Expression.Float32(id(), lit, loc).toSuccess

      case WeededAst.Expression.Float64(lit, loc) => NamedAst.Expression.Float64(id(), lit, loc).toSuccess

      case WeededAst.Expression.Int8(lit, loc) => NamedAst.Expression.Int8(id(), lit, loc).toSuccess

      case WeededAst.Expression.Int16(lit, loc) => NamedAst.Expression.Int16(id(), lit, loc).toSuccess

      case WeededAst.Expression.Int32(lit, loc) => NamedAst.Expression.Int32(id(), lit, loc).toSuccess

      case WeededAst.Expression.Int64(lit, loc) => NamedAst.Expression.Int64(id(), lit, loc).toSuccess

      case WeededAst.Expression.BigInt(lit, loc) => NamedAst.Expression.BigInt(id(), lit, loc).toSuccess

      case WeededAst.Expression.Str(lit, loc) => NamedAst.Expression.Str(id(), lit, loc).toSuccess

      case WeededAst.Expression.Apply(lambda, args, loc) =>
        val lambdaVal = namer(lambda, env0)
        val argsVal = @@(args map (a => namer(a, env0)))
        @@(lambdaVal, argsVal) map {
          case (e, es) => NamedAst.Expression.Apply(id(), e, es, loc)
        }

      case WeededAst.Expression.Lambda(params, exp, loc) =>
        // make a fresh variable symbol for each for parameter.
        val syms = params map (ident => Symbol.mkVarSym(ident))
        val env1 = (params zip syms) map {
          case (ident, sym) => ident.name -> sym
        }
        namer(exp, env0 ++ env1) map {
          case e => NamedAst.Expression.Lambda(id(), syms, e, loc)
        }

      case WeededAst.Expression.Unary(op, exp, loc) => namer(exp, env0) map {
        case e => NamedAst.Expression.Unary(id(), op, e, loc)
      }

      case WeededAst.Expression.Binary(op, exp1, exp2, loc) =>
        @@(namer(exp1, env0), namer(exp2, env0)) map {
          case (e1, e2) => NamedAst.Expression.Binary(id(), op, e1, e2, loc)
        }

      case WeededAst.Expression.IfThenElse(exp1, exp2, exp3, loc) =>
        @@(namer(exp1, env0), namer(exp2, env0), namer(exp3, env0)) map {
          case (e1, e2, e3) => NamedAst.Expression.IfThenElse(id(), e1, e2, e3, loc)
        }

      case WeededAst.Expression.Let(ident, exp1, exp2, loc) =>
        // make a fresh variable symbol for the local variable.
        val sym = Symbol.mkVarSym(ident)
        @@(namer(exp1, env0), namer(exp2, env0 + (ident.name -> sym))) map {
          case (e1, e2) => NamedAst.Expression.Let(id(), sym, e1, e2, loc)
        }

      case WeededAst.Expression.Match(exp, rules, loc) =>
        val expVal = namer(exp, env0)
        val rulesVal = rules map {
          case (pat, body) =>
            // extend the environment with every variable occurring in the pattern
            // and perform naming on the rule body under the extended environment.
            val (p, env1) = Patterns.namer(pat)
            namer(body, env0 ++ env1) map {
              case b => p -> b
            }
        }
        @@(expVal, @@(rulesVal)) map {
          case (e, rs) => NamedAst.Expression.Match(id(), e, rs, loc)
        }

      case WeededAst.Expression.Switch(rules, loc) => @@(rules map {
        case (cond, body) => @@(namer(cond, env0), namer(body, env0))
      }) map {
        case rs => NamedAst.Expression.Switch(id(), rs, loc)
      }

      case WeededAst.Expression.Tag(enum, tag, exp, loc) => namer(exp, env0) map {
        case e => NamedAst.Expression.Tag(id(), enum, tag, e, loc)
      }

      case WeededAst.Expression.Tuple(elms, loc) =>
        @@(elms map (e => namer(e, env0))) map {
          case es => NamedAst.Expression.Tuple(id(), es, loc)
        }

      case WeededAst.Expression.FNone(loc) => NamedAst.Expression.FNone(id(), loc).toSuccess

      case WeededAst.Expression.FSome(exp, loc) => namer(exp, env0) map {
        case e => NamedAst.Expression.FSome(id(), e, loc)
      }

      case WeededAst.Expression.FNil(loc) => NamedAst.Expression.FNil(id(), loc).toSuccess

      case WeededAst.Expression.FList(hd, tl, loc) =>
        @@(namer(hd, env0), namer(tl, env0)) map {
          case (e1, e2) => NamedAst.Expression.FList(id(), e1, e2, loc)
        }

      case WeededAst.Expression.FVec(elms, loc) =>
        @@(elms map (e => namer(e, env0))) map {
          case es => NamedAst.Expression.FVec(id(), es, loc)
        }

      case WeededAst.Expression.FSet(elms, loc) =>
        @@(elms map (e => namer(e, env0))) map {
          case es => NamedAst.Expression.FSet(id(), es, loc)
        }

      case WeededAst.Expression.FMap(elms, loc) => @@(elms map {
        case (key, value) => @@(namer(key, env0), namer(value, env0))
      }) map {
        case es => NamedAst.Expression.FMap(id(), es, loc)
      }

      case WeededAst.Expression.GetIndex(exp1, exp2, loc) =>
        @@(namer(exp1, env0), namer(exp2, env0)) map {
          case (e1, e2) => NamedAst.Expression.GetIndex(id(), e1, e2, loc)
        }

      case WeededAst.Expression.PutIndex(exp1, exp2, exp3, loc) =>
        @@(namer(exp1, env0), namer(exp2, env0)) map {
          case (e1, e2) => NamedAst.Expression.GetIndex(id(), e1, e2, loc)
        }

      case WeededAst.Expression.Existential(params, exp, loc) =>
        namer(exp, env0) map {
          case e => NamedAst.Expression.Existential(id(), params.toList, e, loc)
        }

      case WeededAst.Expression.Universal(params, exp, loc) => namer(exp, env0) map {
        case e => NamedAst.Expression.Universal(id(), params.toList, e, loc)
      }

      case WeededAst.Expression.Ascribe(exp, tpe, loc) => namer(exp, env0) map {
        case e => NamedAst.Expression.Ascribe(id(), e, tpe, loc)
      }

      case WeededAst.Expression.UserError(loc) => NamedAst.Expression.UserError(id(), loc).toSuccess
    }

  }

  object Patterns {

    /**
      * Names the given pattern `pat0` and returns map from variable names to variable symbols.
      */
    def namer(pat0: WeededAst.Pattern)(implicit genSym: GenSym): (NamedAst.Pattern, Map[String, Symbol.VarSym]) = {
      val m = mutable.Map.empty[String, Symbol.VarSym]
      def visit(p: WeededAst.Pattern): NamedAst.Pattern = p match {
        case WeededAst.Pattern.Wild(loc) => NamedAst.Pattern.Wild(loc)
        case WeededAst.Pattern.Var(ident, loc) =>
          // make a fresh variable symbol for the local variable.
          val sym = Symbol.mkVarSym(ident)
          m += (ident.name -> sym)
          NamedAst.Pattern.Var(sym, loc)
        case WeededAst.Pattern.Unit(loc) => NamedAst.Pattern.Unit(loc)
        case WeededAst.Pattern.True(loc) => NamedAst.Pattern.True(loc)
        case WeededAst.Pattern.False(loc) => NamedAst.Pattern.False(loc)
        case WeededAst.Pattern.Char(lit, loc) => NamedAst.Pattern.Char(lit, loc)
        case WeededAst.Pattern.Float32(lit, loc) => NamedAst.Pattern.Float32(lit, loc)
        case WeededAst.Pattern.Float64(lit, loc) => NamedAst.Pattern.Float64(lit, loc)
        case WeededAst.Pattern.Int8(lit, loc) => NamedAst.Pattern.Int8(lit, loc)
        case WeededAst.Pattern.Int16(lit, loc) => NamedAst.Pattern.Int16(lit, loc)
        case WeededAst.Pattern.Int32(lit, loc) => NamedAst.Pattern.Int32(lit, loc)
        case WeededAst.Pattern.Int64(lit, loc) => NamedAst.Pattern.Int64(lit, loc)
        case WeededAst.Pattern.BigInt(lit, loc) => NamedAst.Pattern.BigInt(lit, loc)
        case WeededAst.Pattern.Str(lit, loc) => NamedAst.Pattern.Str(lit, loc)
        case WeededAst.Pattern.Tag(enum, tag, pat, loc) => NamedAst.Pattern.Tag(enum, tag, visit(pat), loc)
        case WeededAst.Pattern.Tuple(elms, loc) => NamedAst.Pattern.Tuple(elms map visit, loc)
        case WeededAst.Pattern.FNone(loc) => NamedAst.Pattern.FNone(loc)
        case WeededAst.Pattern.FSome(pat, loc) => NamedAst.Pattern.FSome(visit(pat), loc)
        case WeededAst.Pattern.FNil(loc) => NamedAst.Pattern.FNil(loc)
        case WeededAst.Pattern.FList(hd, tl, loc) => NamedAst.Pattern.FList(visit(hd), visit(tl), loc)
        case WeededAst.Pattern.FVec(elms, rest, loc) => NamedAst.Pattern.FVec(elms map visit, rest map visit, loc)
        case WeededAst.Pattern.FSet(elms, rest, loc) => NamedAst.Pattern.FSet(elms map visit, rest map visit, loc)
        case WeededAst.Pattern.FMap(elms, rest, loc) =>
          val kvs = elms map {
            case (k, v) => visit(k) -> visit(v)
          }
          NamedAst.Pattern.FMap(kvs, rest map visit, loc)
      }

      (visit(pat0), m.toMap)
    }

  }

  /**
    * Short hand for genSym.freshId.
    */
  @inline
  private def id()(implicit genSym: GenSym): Int = genSym.freshId()

}
