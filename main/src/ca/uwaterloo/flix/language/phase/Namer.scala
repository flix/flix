package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast.{NamedAst, Symbol, WeededAst}
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

  }

  /**
    * Performs naming on the given `program`.
    */
  def namer(program: WeededAst.Program)(implicit genSym: GenSym): Validation[NamedAst.Program, NamerError] = {
    program.roots.map(namer)

    NamedAst.Program(Nil, program.hooks, program.time).toSuccess // TODO
  }

  /**
    * Performs naming on the given `root`.
    */
  def namer(root: WeededAst.Root)(implicit genSym: GenSym): Validation[NamedAst.Root, NamerError] = {
    for (decl <- root.decls) {
      Declarations.namer(decl)
    }

    NamedAst.Root(Nil).toSuccess // TODO
  }

  object Declarations {

    def namer(decl0: WeededAst.Declaration)(implicit genSym: GenSym): Unit = {
      decl0 match {
        case WeededAst.Declaration.Namespace(name, decls, loc) =>
          decls.foreach(d => namer(d))

        case WeededAst.Declaration.Definition(ann, idents, params, exp, tpe, loc) =>
          Expressions.namer(exp, Map.empty)
        case _ => // TODO
      }
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

      case WeededAst.Expression.Apply(lambda, args, loc) => ???

      case WeededAst.Expression.Lambda(params, exp, loc) => ???

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
            val p = Patterns.namer(pat)
            val env = env0 ++ Patterns.symbolsOf(p)
            namer(body, env) map {
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
      * Returns the named pattern corresponding to the given pattern `pat0`.
      */
    def namer(pat0: WeededAst.Pattern)(implicit genSym: GenSym): NamedAst.Pattern = pat0 match {
      case WeededAst.Pattern.Wild(loc) => NamedAst.Pattern.Wild(loc)
      case WeededAst.Pattern.Var(ident, loc) =>
        // make a fresh variable symbol for the local variable.
        val sym = Symbol.mkVarSym(ident)
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
      case WeededAst.Pattern.Tag(enum, tag, pat, loc) => NamedAst.Pattern.Tag(enum, tag, namer(pat), loc)
      case WeededAst.Pattern.Tuple(elms, loc) => NamedAst.Pattern.Tuple(elms map namer, loc)
      case WeededAst.Pattern.FNone(loc) => NamedAst.Pattern.FNone(loc)
      case WeededAst.Pattern.FSome(pat, loc) => NamedAst.Pattern.FSome(namer(pat), loc)
      case WeededAst.Pattern.FNil(loc) => NamedAst.Pattern.FNil(loc)
      case WeededAst.Pattern.FList(hd, tl, loc) => NamedAst.Pattern.FList(namer(hd), namer(tl), loc)
      case WeededAst.Pattern.FVec(elms, rest, loc) => NamedAst.Pattern.FVec(elms map namer, rest map namer, loc)
      case WeededAst.Pattern.FSet(elms, rest, loc) => NamedAst.Pattern.FSet(elms map namer, rest map namer, loc)
      case WeededAst.Pattern.FMap(elms, rest, loc) =>
        val kvs = elms map {
          case (k, v) => namer(k) -> namer(v)
        }
        NamedAst.Pattern.FMap(kvs, rest map namer, loc)
    }

    /**
      * Returns a map from variable names to variable symbols in the given pattern `pat0`.
      */
    // TODO :Consider whether this is better done functionally or as part of the above function
    def symbolsOf(pat0: NamedAst.Pattern): Map[String, Symbol.VarSym] = {
      val m = mutable.Map.empty[String, Symbol.VarSym]

      def visit(p: NamedAst.Pattern): Unit = p match {
        case NamedAst.Pattern.Wild(loc) => // nop
        case NamedAst.Pattern.Var(ident, loc) =>
        case NamedAst.Pattern.Unit(loc) => // nop
        case NamedAst.Pattern.True(loc) => // nop
        case NamedAst.Pattern.False(loc) => // nop
        case NamedAst.Pattern.Char(lit, loc) => // nop
        case NamedAst.Pattern.Float32(lit, loc) => // nop
        case NamedAst.Pattern.Float64(lit, loc) => // nop
        case NamedAst.Pattern.Int8(lit, loc) => // nop
        case NamedAst.Pattern.Int16(lit, loc) => // nop
        case NamedAst.Pattern.Int32(lit, loc) => // nop
        case NamedAst.Pattern.Int64(lit, loc) => // nop
        case NamedAst.Pattern.BigInt(lit, loc) => // nop
        case NamedAst.Pattern.Str(lit, loc) => // nop
        case NamedAst.Pattern.Tag(enum, tag, pat, loc) =>
          visit(pat)
        case NamedAst.Pattern.Tuple(elms, loc) => elms foreach visit
        case NamedAst.Pattern.FNone(loc) => // nop
        case NamedAst.Pattern.FSome(pat, loc) => visit(pat)
        case NamedAst.Pattern.FNil(loc) => // nop
        case NamedAst.Pattern.FList(hd, tl, loc) => visit(hd); visit(tl)
        case NamedAst.Pattern.FVec(elms, rest, loc) =>
          elms foreach visit; rest foreach visit
        case NamedAst.Pattern.FSet(elms, rest, loc) =>
          elms foreach visit; rest foreach visit
        case NamedAst.Pattern.FMap(elms, rest, loc) =>
          ??? // TODO
      }

      visit(pat0)
      m.toMap
    }

  }

  /**
    * Short hand for genSym.freshId.
    */
  @inline
  private def id()(implicit genSym: GenSym): Int = genSym.freshId()

}
