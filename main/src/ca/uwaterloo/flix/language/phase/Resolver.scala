/*
 *  Copyright 2017 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{Name, NamedAst, ResolvedAst}
import ca.uwaterloo.flix.language.errors.ResolutionError
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

// TODO: DOC
object Resolver extends Phase[NamedAst.Program, NamedAst.Program] { // TODO: Change types

  // TODO: DOC
  def run(p: NamedAst.Program)(implicit flix: Flix): Validation[NamedAst.Program, ResolutionError] = {


    val definitionsVal = p.definitions.flatMap {
      case (ns, defs) => defs.map {
        case (name, defn) => Declarations.resolve(defn, ns, p) // TODO: Need ns, name?
      }
    }

    val propertiesVal = p.properties.map {
      case (ns, properties) => Properties.resolve(properties, ns, p) map {
        case ps => ns -> ps
      }
    }

    val time = p.time // TODO

    for {
      definitions <- seqM(definitionsVal)
      properties <- seqM(propertiesVal)
    } yield {
      ResolvedAst.Program(???, ???, ???, ???, ???, ???, p.hooks, properties.toMap, p.reachable, time)
    }

    ???
  }

  object Constraints {

    /**
      * Performs name resolution on the given `constraints` in the given namespace `ns0`.
      */
    def resolve(constraints: List[NamedAst.Constraint], ns0: Name.NName, prog0: NamedAst.Program): Validation[List[ResolvedAst.Constraint], ResolutionError] = {
      seqM(constraints.map(c => resolve(c, ns0, prog0)))
    }

    /**
      * Performs name resolution on the given constraint `c0` in the given namespace `ns0`.
      */
    def resolve(c0: NamedAst.Constraint, ns0: Name.NName, prog0: NamedAst.Program): Validation[ResolvedAst.Constraint, ResolutionError] = {
      for {
        h <- Predicates.Head.resolve(c0.head, ns0, prog0)
        bs <- seqM(c0.body.map(b => Predicates.Body.resolve(b, ns0, prog0)))
      } yield ResolvedAst.Constraint(???, h, bs, c0.loc)
    }

  }

  object Declarations {

    def resolve(decl: NamedAst.Declaration.Definition, ns0: Name.NName, p: NamedAst.Program): Validation[ResolvedAst.Declaration.Definition, ResolutionError] = {
      Expressions.resolve(decl.exp, ns0, p)
      ???
    }


  }

  object Expressions {

    def resolve(exp0: NamedAst.Expression, ns0: Name.NName, prog0: NamedAst.Program): Validation[ResolvedAst.Expression, ResolutionError] = {

      def visit(e0: NamedAst.Expression): Validation[ResolvedAst.Expression, ResolutionError] = e0 match {
        case NamedAst.Expression.Wild(tpe, loc) => ResolvedAst.Expression.Wild(tpe, loc).toSuccess

        case NamedAst.Expression.Var(sym, loc) => ResolvedAst.Expression.Var(sym, loc).toSuccess

        case NamedAst.Expression.Ref(ref, tvar, loc) =>
          ResolvedAst.Expression.Ref(ref, tvar, loc).toSuccess
        //          Disambiguation.lookupRef(ref, ns0, prog0) match {
        //            case Ok(RefTarget.Defn(ns, defn)) => ??? // TODO
        //            case Ok(RefTarget.Hook(hook)) => ??? // TODO
        //            case Err(e) => ??? // TODO
        //          }

        case NamedAst.Expression.Unit(loc) => ResolvedAst.Expression.Unit(loc).toSuccess

        case NamedAst.Expression.True(loc) => ResolvedAst.Expression.True(loc).toSuccess

        case NamedAst.Expression.False(loc) => ResolvedAst.Expression.False(loc).toSuccess

        case NamedAst.Expression.Char(lit, loc) => ResolvedAst.Expression.Char(lit, loc).toSuccess

        case NamedAst.Expression.Float32(lit, loc) => ResolvedAst.Expression.Float32(lit, loc).toSuccess

        case NamedAst.Expression.Float64(lit, loc) => ResolvedAst.Expression.Float64(lit, loc).toSuccess

        case NamedAst.Expression.Int8(lit, loc) => ResolvedAst.Expression.Int8(lit, loc).toSuccess

        case NamedAst.Expression.Int16(lit, loc) => ResolvedAst.Expression.Int16(lit, loc).toSuccess

        case NamedAst.Expression.Int32(lit, loc) => ResolvedAst.Expression.Int32(lit, loc).toSuccess

        case NamedAst.Expression.Int64(lit, loc) => ResolvedAst.Expression.Int64(lit, loc).toSuccess

        case NamedAst.Expression.BigInt(lit, loc) => ResolvedAst.Expression.BigInt(lit, loc).toSuccess

        case NamedAst.Expression.Str(lit, loc) => ResolvedAst.Expression.Str(lit, loc).toSuccess

        case NamedAst.Expression.Apply(lambda, args, tvar, loc) =>
          for {
            e <- visit(lambda)
            es <- seqM(args map visit)
          } yield ResolvedAst.Expression.Apply(e, es, tvar, loc)

        case NamedAst.Expression.Lambda(params, exp, tvar, loc) =>
          for {
            e <- visit(exp)
          } yield ResolvedAst.Expression.Lambda(params, e, tvar, loc)

        case NamedAst.Expression.Unary(op, exp, tvar, loc) =>
          for {
            e <- visit(exp)
          } yield ResolvedAst.Expression.Unary(op, e, tvar, loc)

        case NamedAst.Expression.Binary(op, exp1, exp2, tvar, loc) =>
          for {
            e1 <- visit(exp1)
            e2 <- visit(exp2)
          } yield ResolvedAst.Expression.Binary(op, e1, e2, tvar, loc)

        case NamedAst.Expression.IfThenElse(exp1, exp2, exp3, tvar, loc) =>
          for {
            e1 <- visit(exp1)
            e2 <- visit(exp2)
            e3 <- visit(exp3)
          } yield ResolvedAst.Expression.IfThenElse(e1, e2, e3, tvar, loc)

        case NamedAst.Expression.Let(sym, exp1, exp2, tvar, loc) =>
          for {
            e1 <- visit(exp1)
            e2 <- visit(exp2)
          } yield ResolvedAst.Expression.Let(sym, e1, e2, tvar, loc)

        case NamedAst.Expression.Match(exp, rules, tvar, loc) =>
          val rulesVal = rules map {
            case NamedAst.MatchRule(pat, guard, body) =>
              for {
                p <- Patterns.resolve(pat, ns0, prog0)
                g <- visit(guard)
                b <- visit(body)
              } yield ResolvedAst.MatchRule(p, g, b)
          }

          for {
            e <- visit(exp)
            rs <- seqM(rulesVal)
          } yield ResolvedAst.Expression.Match(e, rs, tvar, loc)

        case NamedAst.Expression.Switch(rules, tvar, loc) =>
          val rulesVal = rules map {
            case (cond, body) => @@(visit(cond), visit(body))
          }
          seqM(rulesVal) map {
            case rs => ResolvedAst.Expression.Switch(rs, tvar, loc)
          }

        case NamedAst.Expression.Tag(enum, tag, exp, tvar, loc) =>
          // TODO: Perform lookup
          for {
            e <- visit(exp)
          } yield ResolvedAst.Expression.Tag(enum, tag, e, tvar, loc)

        case NamedAst.Expression.Tuple(elms, tvar, loc) =>
          for {
            es <- seqM(elms map visit)
          } yield ResolvedAst.Expression.Tuple(es, tvar, loc)

        case NamedAst.Expression.Existential(fparam, exp, loc) =>
          for {
            fp <- Params.resolve(fparam, ns0, prog0)
            e <- visit(exp)
          } yield ResolvedAst.Expression.Existential(fp, e, loc)

        case NamedAst.Expression.Universal(fparam, exp, loc) =>
          for {
            fp <- Params.resolve(fparam, ns0, prog0)
            e <- visit(exp)
          } yield ResolvedAst.Expression.Universal(fp, e, loc)

        case NamedAst.Expression.Ascribe(exp, tpe, loc) =>
          for {
            e <- visit(exp)
            t <- Types.resolve(tpe, ns0, prog0)
          } yield ResolvedAst.Expression.Ascribe(e, t, loc)

        case NamedAst.Expression.NativeConstructor(constructor, args, tpe, loc) =>
          for {
            es <- seqM(args map visit)
          } yield ResolvedAst.Expression.NativeConstructor(constructor, es, tpe, loc)

        case NamedAst.Expression.NativeField(field, tpe, loc) => ResolvedAst.Expression.NativeField(field, tpe, loc).toSuccess

        case NamedAst.Expression.NativeMethod(method, args, tpe, loc) =>
          for {
            es <- seqM(args map visit)
          } yield ResolvedAst.Expression.NativeMethod(method, es, tpe, loc)

        case NamedAst.Expression.UserError(tvar, loc) => ResolvedAst.Expression.UserError(tvar, loc).toSuccess
      }

      visit(exp0)
    }

  }

  object Patterns {

    /**
      * Performs name resolution on the given pattern `pat0` in the namespace `ns0`.
      */
    def resolve(pat0: NamedAst.Pattern, ns0: Name.NName, p: NamedAst.Program): Validation[ResolvedAst.Pattern, ResolutionError] = {

      def visit(p0: NamedAst.Pattern): Validation[ResolvedAst.Pattern, ResolutionError] = p0 match {
        case NamedAst.Pattern.Wild(tvar, loc) => ResolvedAst.Pattern.Wild(tvar, loc).toSuccess

        case NamedAst.Pattern.Var(sym, tvar, loc) => ResolvedAst.Pattern.Var(sym, tvar, loc).toSuccess

        case NamedAst.Pattern.Unit(loc) => ResolvedAst.Pattern.Unit(loc).toSuccess

        case NamedAst.Pattern.True(loc) => ResolvedAst.Pattern.True(loc).toSuccess

        case NamedAst.Pattern.False(loc) => ResolvedAst.Pattern.False(loc).toSuccess

        case NamedAst.Pattern.Char(lit, loc) => ResolvedAst.Pattern.Char(lit, loc).toSuccess

        case NamedAst.Pattern.Float32(lit, loc) => ResolvedAst.Pattern.Float32(lit, loc).toSuccess

        case NamedAst.Pattern.Float64(lit, loc) => ResolvedAst.Pattern.Float64(lit, loc).toSuccess

        case NamedAst.Pattern.Int8(lit, loc) => ResolvedAst.Pattern.Int8(lit, loc).toSuccess

        case NamedAst.Pattern.Int16(lit, loc) => ResolvedAst.Pattern.Int16(lit, loc).toSuccess

        case NamedAst.Pattern.Int32(lit, loc) => ResolvedAst.Pattern.Int32(lit, loc).toSuccess

        case NamedAst.Pattern.Int64(lit, loc) => ResolvedAst.Pattern.Int64(lit, loc).toSuccess

        case NamedAst.Pattern.BigInt(lit, loc) => ResolvedAst.Pattern.BigInt(lit, loc).toSuccess

        case NamedAst.Pattern.Str(lit, loc) => ResolvedAst.Pattern.Str(lit, loc).toSuccess

        case NamedAst.Pattern.Tag(enum, tag, pat, tvar, loc) =>
          Disambiguation.lookupEnumByTag(enum, tag, ns0, p) match {
            case Ok(decl) =>
              // TODO: Use decl
              for {
                p <- visit(pat)
              } yield ResolvedAst.Pattern.Tag(enum, tag, p, tvar, loc)
            case Err(e) => ???
          }

        case NamedAst.Pattern.Tuple(elms, tvar, loc) =>
          for {
            es <- seqM(elms map visit)
          } yield ResolvedAst.Pattern.Tuple(es, tvar, loc)
      }

      visit(pat0)
    }

  }

  object Predicates {

    object Head {
      /**
        * Performs name resolution on the given head predicate `h0` in the given namespace `ns0`.
        */
      def resolve(h0: NamedAst.Predicate.Head, ns0: Name.NName, prog0: NamedAst.Program): Validation[ResolvedAst.Predicate.Head, ResolutionError] = h0 match {
        case NamedAst.Predicate.Head.True(loc) => ResolvedAst.Predicate.Head.True(loc).toSuccess

        case NamedAst.Predicate.Head.False(loc) => ResolvedAst.Predicate.Head.False(loc).toSuccess

        case NamedAst.Predicate.Head.Positive(name, terms, loc) =>
          for {
            ts <- seqM(terms.map(t => Expressions.resolve(t, ns0, prog0)))
          } yield ResolvedAst.Predicate.Head.Positive(name, ts, loc)

        case NamedAst.Predicate.Head.Negative(name, terms, loc) =>
          for {
            ts <- seqM(terms.map(t => Expressions.resolve(t, ns0, prog0)))
          } yield ResolvedAst.Predicate.Head.Negative(name, ts, loc)
      }
    }

    object Body {
      /**
        * Performs name resolution on the given body predicate `b0` in the given namespace `ns0`.
        */
      def resolve(b0: NamedAst.Predicate.Body, ns0: Name.NName, prog0: NamedAst.Program): Validation[ResolvedAst.Predicate.Body, ResolutionError] = b0 match {
        case NamedAst.Predicate.Body.Positive(name, terms, loc) =>
          for {
            ts <- seqM(terms.map(t => Patterns.resolve(t, ns0, prog0)))
          } yield ResolvedAst.Predicate.Body.Positive(name, ts, loc)

        case NamedAst.Predicate.Body.Negative(name, terms, loc) =>
          for {
            ts <- seqM(terms.map(t => Patterns.resolve(t, ns0, prog0)))
          } yield ResolvedAst.Predicate.Body.Negative(name, ts, loc)

        case NamedAst.Predicate.Body.Filter(name, terms, loc) =>
          for {
            ts <- seqM(terms.map(t => Expressions.resolve(t, ns0, prog0)))
          } yield ResolvedAst.Predicate.Body.Filter(name, ts, loc)

        case NamedAst.Predicate.Body.Loop(pat, term, loc) =>
          for {
            p <- Patterns.resolve(pat, ns0, prog0)
            t <- Expressions.resolve(term, ns0, prog0)
          } yield ResolvedAst.Predicate.Body.Loop(p, t, loc)
      }
    }

  }

  object Properties {

    /**
      * Performs name resolution on each of the given `properties` in the given namespace `ns0`.
      */
    def resolve(properties: List[NamedAst.Property], ns0: Name.NName, prog0: NamedAst.Program): Validation[List[ResolvedAst.Property], ResolutionError] = {
      seqM(properties.map(p => resolve(p, ns0, prog0)))
    }

    /**
      * Performs name resolution on the given property `p0` in the given namespace `ns0`.
      */
    def resolve(p0: NamedAst.Property, ns0: Name.NName, prog0: NamedAst.Program): Validation[ResolvedAst.Property, ResolutionError] = {
      for {
        e <- Expressions.resolve(p0.exp, ns0, prog0)
      } yield ResolvedAst.Property(p0.law, p0.defn, e, p0.loc)
    }

  }

  object Types {

    /**
      * Performs name resolution on the given type `tpe0` in the given namespace `ns0`.
      */
    def resolve(tpe0: NamedAst.Type, ns0: Name.NName, prog0: NamedAst.Program): Validation[ResolvedAst.Type, ResolutionError] = {
      /**
        * Local visitor.
        */
      def visit(tpe: NamedAst.Type): Validation[ResolvedAst.Type, ResolutionError] = tpe match {
        case NamedAst.Type.Var(tvar, loc) => ResolvedAst.Type.Var(tvar, loc).toSuccess

        case NamedAst.Type.Unit(loc) => ResolvedAst.Type.Unit(loc).toSuccess

        case NamedAst.Type.Ref(name, loc) => ResolvedAst.Type.Ref(name, loc).toSuccess

        case NamedAst.Type.Enum(name) => ResolvedAst.Type.Enum(name).toSuccess

        case NamedAst.Type.Tuple(elms, loc) =>
          for {
            es <- seqM(elms map visit)
          } yield ResolvedAst.Type.Tuple(es, loc)

        case NamedAst.Type.Arrow(params, ret, loc) =>
          for {
            ps <- seqM(params map visit)
            r <- visit(ret)
          } yield ResolvedAst.Type.Arrow(ps, r, loc)

        case NamedAst.Type.Apply(base, tparams, loc) =>
          for {
            b <- visit(base)
            ps <- seqM(tparams map visit)
          } yield ResolvedAst.Type.Apply(b, ps, loc)
      }

      visit(tpe0)
    }
  }

  object Params {

    /**
      * Performs name resolution on the given formal parameter `fparam0` in the given namespace `ns0`.
      */
    def resolve(fparam0: NamedAst.FormalParam, ns0: Name.NName, prog0: NamedAst.Program): Validation[ResolvedAst.FormalParam, ResolutionError] = {
      for {
        t <- Types.resolve(fparam0.tpe, ns0, prog0)
      } yield ResolvedAst.FormalParam(fparam0.sym, t, fparam0.loc)
    }


  }

}
