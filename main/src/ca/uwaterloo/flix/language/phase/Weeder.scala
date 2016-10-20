/*
 * Copyright 2015-2016 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ca.uwaterloo.flix.language.phase

import java.math.BigInteger

import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.errors.WeederError
import ca.uwaterloo.flix.language.errors.WeederError._
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

import scala.collection.immutable.Seq
import scala.collection.mutable

/**
  * The Weeder phase performs simple syntactic checks and rewritings.
  */
object Weeder {

  /**
    * Weeds the whole program.
    */
  def weed(program: ParsedAst.Program, hooks: Map[Name.NName, Map[String, Ast.Hook]]): Validation[WeededAst.Program, WeederError] = {
    val b = System.nanoTime()
    @@(program.roots map weed) map {
      case roots =>
        val e = System.nanoTime() - b
        WeededAst.Program(roots, hooks, program.time.copy(weeder = e))
    }
  }

  /**
    * Weeds the given abstract syntax tree.
    */
  private def weed(root: ParsedAst.Root): Validation[WeededAst.Root, WeederError] = {
    @@(root.decls map Declarations.weed) map {
      case decls => WeededAst.Root(decls)
    }
  }

  object Declarations {

    /**
      * Compiles the given parsed declaration `past` to a weeded declaration.
      */
    def weed(decl: ParsedAst.Declaration): Validation[WeededAst.Declaration, WeederError] = decl match {
      case ParsedAst.Declaration.Namespace(sp1, name, decls, sp2) =>
        @@(decls.map(weed)) map {
          case ds => WeededAst.Declaration.Namespace(name, ds, mkSL(sp1, sp2))
        }

      case ParsedAst.Declaration.Definition(docOpt, ann, sp1, ident, tparams0, paramsOpt, tpe, exp, sp2) =>
        val doc = docOpt.map(d => Ast.Documentation(d.text.mkString(" "), mkSL(d.sp1, d.sp2)))
        val sl = mkSL(ident.sp1, ident.sp2)
        val annVal = Annotations.weed(ann)
        val expVal = Expressions.weed(exp)
        val tparams = tparams0.toList.map(_.ident)

        /*
         * Check for `IllegalParameterList`.
         */
        paramsOpt match {
          case None => @@(annVal, expVal) flatMap {
            case (as, e) =>
              val t = WeededAst.Type.Arrow(Nil, Types.weed(tpe), sl)
              WeededAst.Declaration.Definition(doc, as, ident, tparams, Nil, e, t, sl).toSuccess
          }
          case Some(Nil) => IllegalParameterList(sl).toFailure
          case Some(params) =>
            /*
             * Check for `DuplicateFormal`.
             */
            val formalsVal = checkDuplicateFormal(params)
            @@(annVal, formalsVal, expVal) map {
              case (as, fs, e) =>
                val t = WeededAst.Type.Arrow(fs map (_.tpe), Types.weed(tpe), sl)
                WeededAst.Declaration.Definition(doc, as, ident, tparams, fs, e, t, sl)
            }
        }

      case ParsedAst.Declaration.Signature(docOpt, sp1, ident, paramsOpt, tpe, sp2) =>
        val doc = docOpt.map(d => Ast.Documentation(d.text.mkString(" "), mkSL(d.sp1, d.sp2)))

        /*
         * Check for `IllegalParameterList`.
         */
        paramsOpt match {
          case None => WeededAst.Declaration.Signature(doc, ident, Nil, Types.weed(tpe), mkSL(sp1, sp2)).toSuccess
          case Some(Nil) => IllegalParameterList(mkSL(sp1, sp2)).toFailure
          case Some(params) =>
            /*
             * Check for `DuplicateFormal`.
             */
            checkDuplicateFormal(params) map {
              case ps => WeededAst.Declaration.Signature(doc, ident, ps, Types.weed(tpe), mkSL(sp1, sp2))
            }
        }

      case ParsedAst.Declaration.External(docOpt, sp1, ident, paramsOpt, tpe, sp2) =>
        val doc = docOpt.map(d => Ast.Documentation(d.text.mkString(" "), mkSL(d.sp1, d.sp2)))

        /*
         * Check for `IllegalParameterList`.
         */
        paramsOpt match {
          case None => WeededAst.Declaration.External(doc, ident, Nil, Types.weed(tpe), mkSL(sp1, sp2)).toSuccess
          case Some(Nil) => IllegalParameterList(mkSL(sp1, sp2)).toFailure
          case Some(params) =>
            /*
             * Check for `DuplicateFormal`.
             */
            checkDuplicateFormal(params) map {
              case ps => WeededAst.Declaration.External(doc, ident, ps, Types.weed(tpe), mkSL(sp1, sp2))
            }
        }

      case ParsedAst.Declaration.Law(docOpt, sp1, ident, tparams, paramsOpt, tpe, exp, sp2) =>
        val doc = docOpt.map(d => Ast.Documentation(d.text.mkString(" "), mkSL(d.sp1, d.sp2)))

        /*
         * Check for `IllegalParameterList`.
         */
        Expressions.weed(exp) flatMap {
          case e => paramsOpt match {
            case None => WeededAst.Declaration.Law(doc, ident, tparams.toList, Nil, Types.weed(tpe), e, mkSL(sp1, sp2)).toSuccess
            case Some(Nil) => IllegalParameterList(mkSL(sp1, sp2)).toFailure
            case Some(params) =>
              /*
               * Check for `DuplicateFormal`.
               */
              checkDuplicateFormal(params) map {
                case ps => WeededAst.Declaration.Law(doc, ident, tparams.toList, ps, Types.weed(tpe), e, mkSL(sp1, sp2))
              }
          }
        }

      case ParsedAst.Declaration.Enum(docOpt, sp1, ident, tparams0, cases, sp2) =>
        val doc = docOpt.map(d => Ast.Documentation(d.text.mkString(" "), mkSL(d.sp1, d.sp2)))
        val tparams = tparams0.toList.map(_.ident)
        /*
         * Check for `DuplicateTag`.
         */
        Validation.fold[ParsedAst.Case, Map[String, WeededAst.Case], WeederError](cases, Map.empty) {
          case (macc, caze: ParsedAst.Case) =>
            val tag = caze.ident.name
            macc.get(tag) match {
              case None => (macc + (tag -> WeededAst.Case(ident, caze.ident, Types.weed(caze.tpe)))).toSuccess
              case Some(otherTag) => DuplicateTag(tag, otherTag.tag.loc, mkSL(caze.sp1, caze.sp2)).toFailure
            }
        } map {
          case m => WeededAst.Declaration.Enum(doc, ident, tparams, m, mkSL(sp1, sp2))
        }

      case ParsedAst.Declaration.Class(docOpt, sp1, ident, tparams, bounds, decls, sp2) =>
        val doc = docOpt.map(d => Ast.Documentation(d.text.mkString(" "), mkSL(d.sp1, d.sp2)))
        @@(decls.map(weed)) map {
          case ds => WeededAst.Declaration.Class(doc, ident, tparams.toList.map(Types.weed), ds, mkSL(sp1, sp2))
        }

      case ParsedAst.Declaration.Impl(docOpt, sp1, ident, tparams, bounds, decls, sp2) =>
        val doc = docOpt.map(d => Ast.Documentation(d.text.mkString(" "), mkSL(d.sp1, d.sp2)))
        @@(decls.map(weed)) map {
          case ds => WeededAst.Declaration.Impl(doc, ident, tparams.toList.map(Types.weed), ds, mkSL(sp1, sp2))
        }

      case ParsedAst.Declaration.Relation(docOpt, sp1, ident, attrs, sp2) =>
        val doc = docOpt.map(d => Ast.Documentation(d.text.mkString(" "), mkSL(d.sp1, d.sp2)))

        /*
         * Check for `EmptyRelation`
         */
        if (attrs.isEmpty)
          return EmptyRelation(mkSL(sp1, sp2)).toFailure

        /*
         * Check for `DuplicateAttribute`.
         */
        checkDuplicateAttribute(attrs) map {
          case as => WeededAst.Table.Relation(doc, ident, as, mkSL(sp1, sp2))
        }

      case ParsedAst.Declaration.Lattice(docOpt, sp1, ident, attrs, sp2) =>
        val doc = docOpt.map(d => Ast.Documentation(d.text.mkString(" "), mkSL(d.sp1, d.sp2)))

        /*
         * Check for `EmptyLattice`.
         */
        if (attrs.isEmpty)
          return EmptyLattice(mkSL(sp1, sp2)).toFailure

        /*
         * Check for `DuplicateAttribute`.
         */
        checkDuplicateAttribute(attrs) map {
          case as =>
            // Split the attributes into keys and element.
            WeededAst.Table.Lattice(doc, ident, as.init, as.last, mkSL(sp1, sp2))
        }

      case ParsedAst.Declaration.Fact(sp1, head, sp2) =>
        Predicate.Head.weed(head) map {
          case p => WeededAst.Declaration.Fact(p, mkSL(sp1, sp2))
        }

      case ParsedAst.Declaration.Rule(sp1, head, body, sp2) =>
        val headVal = Predicate.Head.weed(head)
        val bodyVal = @@(body.map(Predicate.Body.weed))

        @@(headVal, bodyVal) map {
          case (h, b) => WeededAst.Declaration.Rule(h, b, mkSL(sp1, sp2))
        }

      case ParsedAst.Declaration.Index(sp1, qname, indexes, sp2) =>
        /*
         * Check for `EmptyIndex` and `IllegalIndex`.
         */
        val sl = mkSL(sp1, sp2)
        if (indexes.isEmpty)
          EmptyIndex(sl).toFailure
        else if (indexes.exists(_.isEmpty))
          IllegalIndex(sl).toFailure
        else
          WeededAst.Declaration.Index(qname, indexes.toList.map(_.toList), sl).toSuccess

      case ParsedAst.Declaration.BoundedLattice(sp1, tpe, elms, sp2) =>
        val elmsVal = @@(elms.toList.map(e => Expressions.weed(e)))
        elmsVal flatMap {
          case List(bot, top, leq, lub, glb) => WeededAst.Declaration.BoundedLattice(Types.weed(tpe), bot, top, leq, lub, glb, mkSL(sp1, sp2)).toSuccess
          case _ => IllegalLattice(mkSL(sp1, sp2)).toFailure
        }

    }

  }

  object Expressions {

    /**
      * Translates the given literal to an expression.
      */
    def toExp(lit0: ParsedAst.Literal): Validation[WeededAst.Expression, WeederError] = lit0 match {
      case ParsedAst.Literal.Unit(sp1, sp2) =>
        WeededAst.Expression.Unit(mkSL(sp1, sp2)).toSuccess

      case ParsedAst.Literal.True(sp1, sp2) =>
        WeededAst.Expression.True(mkSL(sp1, sp2)).toSuccess

      case ParsedAst.Literal.False(sp1, sp2) =>
        WeededAst.Expression.False(mkSL(sp1, sp2)).toSuccess

      case ParsedAst.Literal.Char(sp1, lit, sp2) =>
        WeededAst.Expression.Char(lit(0), mkSL(sp1, sp2)).toSuccess

      case ParsedAst.Literal.Float32(sp1, sign, before, after, sp2) =>
        toFloat32(sign, before, after, mkSL(sp1, sp2)) map {
          case lit => WeededAst.Expression.Float32(lit, mkSL(sp1, sp2))
        }

      case ParsedAst.Literal.Float64(sp1, sign, before, after, sp2) =>
        toFloat64(sign, before, after, mkSL(sp1, sp2)) map {
          case lit => WeededAst.Expression.Float64(lit, mkSL(sp1, sp2))
        }

      case ParsedAst.Literal.Int8(sp1, sign, digits, sp2) =>
        toInt8(sign, digits, mkSL(sp1, sp2)) map {
          case lit => WeededAst.Expression.Int8(lit, mkSL(sp1, sp2))
        }

      case ParsedAst.Literal.Int16(sp1, sign, digits, sp2) =>
        toInt16(sign, digits, mkSL(sp1, sp2)) map {
          case lit => WeededAst.Expression.Int16(lit, mkSL(sp1, sp2))
        }

      case ParsedAst.Literal.Int32(sp1, sign, digits, sp2) =>
        toInt32(sign, digits, mkSL(sp1, sp2)) map {
          case lit => WeededAst.Expression.Int32(lit, mkSL(sp1, sp2))
        }

      case ParsedAst.Literal.Int64(sp1, sign, digits, sp2) =>
        toInt64(sign, digits, mkSL(sp1, sp2)) map {
          case lit => WeededAst.Expression.Int64(lit, mkSL(sp1, sp2))
        }

      case ParsedAst.Literal.BigInt(sp1, sign, digits, sp2) =>
        toBigInt(sign, digits, mkSL(sp1, sp2)) map {
          case lit => WeededAst.Expression.BigInt(lit, mkSL(sp1, sp2))
        }

      case ParsedAst.Literal.Str(sp1, lit, sp2) =>
        WeededAst.Expression.Str(lit, mkSL(sp1, sp2)).toSuccess
    }

    /**
      * Weeds the given expression.
      */
    def weed(exp0: ParsedAst.Expression, allowWildcards: Boolean = false): Validation[WeededAst.Expression, WeederError] = {
      /**
        * Inner visitor.
        */
      def visit(e0: ParsedAst.Expression): Validation[WeededAst.Expression, WeederError] = e0 match {
        case ParsedAst.Expression.Wild(sp1, sp2) =>
          if (allowWildcards)
            WeededAst.Expression.Wild(mkSL(sp1, sp2)).toSuccess
          else
            IllegalWildcard(mkSL(sp1, sp2)).toFailure

        case ParsedAst.Expression.SName(sp1, ident, sp2) =>
          val qname = Name.QName(sp1, Name.RootNS, ident, sp2)
          WeededAst.Expression.VarOrRef(qname, mkSL(sp1, sp2)).toSuccess

        case ParsedAst.Expression.QName(sp1, qname, sp2) =>
          WeededAst.Expression.VarOrRef(qname, mkSL(sp1, sp2)).toSuccess

        case ParsedAst.Expression.Lit(sp1, lit, sp2) => toExp(lit)

        case ParsedAst.Expression.Apply(lambda, args, sp2) =>
          val sp1 = leftMostSourcePosition(lambda)
          @@(visit(lambda), @@(args map visit)) flatMap {
            case (e, as) => WeededAst.Expression.Apply(e, as, mkSL(sp1, sp2)).toSuccess
          }

        case ParsedAst.Expression.Infix(exp1, name, exp2, sp2) =>
          /*
           * Rewrites infix expressions to apply expressions.
           */
          @@(visit(exp1), visit(exp2)) map {
            case (e1, e2) =>
              val loc = mkSL(leftMostSourcePosition(exp1), sp2)
              val e3 = WeededAst.Expression.VarOrRef(name, loc)
              WeededAst.Expression.Apply(e3, List(e1, e2), loc)
          }

        case ParsedAst.Expression.Lambda(sp1, params, exp, sp2) =>
          /*
           * Check for `DuplicateFormal`.
           */
          checkDuplicateFormal2(params) flatMap {
            case ps =>
              visit(exp) map {
                case e => WeededAst.Expression.Lambda(params.toList, e, mkSL(sp1, sp2))
              }
          }

        case ParsedAst.Expression.Unary(sp1, op, exp, sp2) => visit(exp) map {
          case e => WeededAst.Expression.Unary(op, e, mkSL(sp1, sp2))
        }

        case ParsedAst.Expression.Binary(exp1, op, exp2, sp2) =>
          @@(visit(exp1), visit(exp2)) map {
            case (e1, e2) => WeededAst.Expression.Binary(op, e1, e2, mkSL(leftMostSourcePosition(exp1), sp2))
          }

        case ParsedAst.Expression.ExtendedBinary(exp1, op, exp2, sp2) =>
          /*
           * Rewrites extended binary expressions to apply expressions.
           */
          @@(visit(exp1), visit(exp2)) map {
            case (e1, e2) =>
              op match {
                case ExtBinaryOperator.Leq =>
                  val sp1 = leftMostSourcePosition(exp1)
                  val loc = mkSL(sp1, sp2)
                  val ident = Name.Ident(sp1, "⊑", sp2)
                  val namespace = Name.NName(sp1, List.empty, sp2)
                  val name = Name.QName(sp1, namespace, ident, sp2)
                  val lambda = WeededAst.Expression.VarOrRef(name, loc)
                  WeededAst.Expression.Apply(lambda, List(e1, e2), loc)

                case ExtBinaryOperator.Lub =>
                  val sp1 = leftMostSourcePosition(exp1)
                  val loc = mkSL(sp1, sp2)
                  val ident = Name.Ident(sp1, "⊔", sp2)
                  val namespace = Name.NName(sp1, List.empty, sp2)
                  val name = Name.QName(sp1, namespace, ident, sp2)
                  val lambda = WeededAst.Expression.VarOrRef(name, loc)
                  WeededAst.Expression.Apply(lambda, List(e1, e2), loc)

                case ExtBinaryOperator.Glb =>
                  val sp1 = leftMostSourcePosition(exp1)
                  val loc = mkSL(sp1, sp2)
                  val ident = Name.Ident(sp1, "⊓", sp2)
                  val namespace = Name.NName(sp1, List.empty, sp2)
                  val name = Name.QName(sp1, namespace, ident, sp2)
                  val lambda = WeededAst.Expression.VarOrRef(name, loc)
                  WeededAst.Expression.Apply(lambda, List(e1, e2), loc)

                case ExtBinaryOperator.Widen =>
                  val sp1 = leftMostSourcePosition(exp1)
                  val loc = mkSL(sp1, sp2)
                  val ident = Name.Ident(sp1, "▽", sp2)
                  val namespace = Name.NName(sp1, List.empty, sp2)
                  val name = Name.QName(sp1, namespace, ident, sp2)
                  val lambda = WeededAst.Expression.VarOrRef(name, loc)
                  WeededAst.Expression.Apply(lambda, List(e1, e2), loc)

                case ExtBinaryOperator.Narrow =>
                  val sp1 = leftMostSourcePosition(exp1)
                  val loc = mkSL(sp1, sp2)
                  val ident = Name.Ident(sp1, "△", sp2)
                  val namespace = Name.NName(sp1, List.empty, sp2)
                  val name = Name.QName(sp1, namespace, ident, sp2)
                  val lambda = WeededAst.Expression.VarOrRef(name, loc)
                  WeededAst.Expression.Apply(lambda, List(e1, e2), loc)
              }
          }

        case ParsedAst.Expression.IfThenElse(sp1, exp1, exp2, exp3, sp2) =>
          @@(visit(exp1), visit(exp2), visit(exp3)) map {
            case (e1, e2, e3) => WeededAst.Expression.IfThenElse(e1, e2, e3, mkSL(sp1, sp2))
          }

        case ParsedAst.Expression.LetMatch(sp1, pat, exp1, exp2, sp2) =>
          /*
           * Rewrites a let-match to a regular let-binding or a full-blown pattern match.
           */
          @@(Patterns.weed(pat), visit(exp1), visit(exp2)) map {
            case (WeededAst.Pattern.Var(ident, loc), value, body) =>
              // Let-binding
              WeededAst.Expression.Let(ident, value, body, mkSL(sp1, sp2))
            case (pattern, value, body) =>
              // Full-blown pattern match.
              val rules = List(pattern -> body)
              WeededAst.Expression.Match(value, rules, mkSL(sp1, sp2))
          }

        case ParsedAst.Expression.Match(sp1, exp, rules, sp2) =>
          val rulesVal = rules map {
            case (pat, body) => @@(Patterns.weed(pat), visit(body))
          }
          @@(visit(exp), @@(rulesVal)) map {
            case (e, rs) => WeededAst.Expression.Match(e, rs, mkSL(sp1, sp2))
          }

        case ParsedAst.Expression.Switch(sp1, rules, sp2) =>
          val rulesVal = rules map {
            case (cond, body) => @@(visit(cond), visit(body))
          }
          @@(rulesVal) map {
            case rs => WeededAst.Expression.Switch(rs, mkSL(sp1, sp2))
          }

        case ParsedAst.Expression.Tag(sp1, enum, tag, o, sp2) =>
          /*
           * Introduce implicit unit, if needed.
           */
          o match {
            case None =>
              val loc = mkSL(sp1, sp2)
              val exp = WeededAst.Expression.Unit(loc)
              WeededAst.Expression.Tag(enum, tag, exp, loc).toSuccess
            case Some(exp) => visit(exp) map {
              case e => WeededAst.Expression.Tag(enum, tag, e, mkSL(sp1, sp2))
            }
          }

        case ParsedAst.Expression.Tuple(sp1, elms, sp2) =>
          /*
           * Rewrites empty tuples to Unit and eliminate single-element tuples.
           */
          @@(elms map visit) map {
            case Nil =>
              val loc = mkSL(sp1, sp2)
              WeededAst.Expression.Unit(loc)
            case x :: Nil => x
            case xs => WeededAst.Expression.Tuple(xs, mkSL(sp1, sp2))
          }

        case ParsedAst.Expression.FNil(sp1, sp2) =>
          WeededAst.Expression.FNil(mkSL(sp1, sp2)).toSuccess

        case ParsedAst.Expression.FList(hd, tl, sp2) =>
          val sp1 = leftMostSourcePosition(hd)
          @@(visit(hd), visit(tl)) map {
            case (e1, e2) => WeededAst.Expression.FList(e1, e2, mkSL(sp1, sp2))
          }

        case ParsedAst.Expression.FVec(sp1, elms, sp2) =>
          @@(elms map visit) map {
            case es => WeededAst.Expression.FVec(es, mkSL(sp1, sp2))
          }

        case ParsedAst.Expression.FSet(sp1, elms, sp2) =>
          @@(elms map visit) map {
            case es => WeededAst.Expression.FSet(es, mkSL(sp1, sp2))
          }

        case ParsedAst.Expression.FMap(sp1, elms, sp2) =>
          val elmsVal = elms map {
            case (key, value) => @@(visit(key), visit(value))
          }

          @@(elmsVal) map {
            case es => WeededAst.Expression.FMap(es, mkSL(sp1, sp2))
          }

        case ParsedAst.Expression.GetIndex(sp1, exp1, exp2, sp2) =>
          @@(visit(exp1), visit(exp2)) map {
            case (e1, e2) => WeededAst.Expression.GetIndex(e1, e2, mkSL(sp1, sp2))
          }

        case ParsedAst.Expression.PutIndex(sp1, exp1, exp2, exp3, sp2) =>
          @@(visit(exp1), visit(exp2), visit(exp3)) map {
            case (e1, e2, e3) => WeededAst.Expression.PutIndex(e1, e2, e3, mkSL(sp1, sp2))
          }

        case ParsedAst.Expression.Existential(sp1, paramsOpt, exp, sp2) =>
          /*
           * Checks for `IllegalExistential`.
           */
          visit(exp) flatMap {
            case e => paramsOpt match {
              case None => IllegalExistential(mkSL(sp1, sp2)).toFailure
              case Some(Nil) => IllegalExistential(mkSL(sp1, sp2)).toFailure
              case Some(params) =>
                /*
                 * Check for `DuplicateFormal`.
                 */
                checkDuplicateFormal(params) map {
                  case ps => WeededAst.Expression.Existential(ps, e, mkSL(sp1, sp2))
                }
            }
          }

        case ParsedAst.Expression.Universal(sp1, paramsOpt, exp, sp2) =>
          /*
           * Checks for `IllegalUniversal`.
           */
          visit(exp) flatMap {
            case e => paramsOpt match {
              case None => IllegalUniversal(mkSL(sp1, sp2)).toFailure
              case Some(Nil) => IllegalUniversal(mkSL(sp1, sp2)).toFailure
              case Some(params) =>
                /*
                 * Check for `DuplicateFormal`.
                 */
                checkDuplicateFormal(params) map {
                  case ps => WeededAst.Expression.Universal(ps, e, mkSL(sp1, sp2))
                }
            }
          }

        case ParsedAst.Expression.Ascribe(exp, tpe, sp2) =>
          visit(exp) map {
            case e => WeededAst.Expression.Ascribe(e, Types.weed(tpe), mkSL(leftMostSourcePosition(exp), sp2))
          }

        case ParsedAst.Expression.UserError(sp1, sp2) =>
          WeededAst.Expression.UserError(mkSL(sp1, sp2)).toSuccess

        case ParsedAst.Expression.Bot(sp1, sp2) =>
          val ident = Name.Ident(sp1, "⊥", sp2)
          val namespace = Name.NName(sp1, List.empty, sp2)
          val name = Name.QName(sp1, namespace, ident, sp2)
          val lambda = WeededAst.Expression.VarOrRef(name, mkSL(sp1, sp2))
          WeededAst.Expression.Apply(lambda, List(), mkSL(sp1, sp2)).toSuccess

        case ParsedAst.Expression.Top(sp1, sp2) =>
          val ident = Name.Ident(sp1, "⊤", sp2)
          val namespace = Name.NName(sp1, List.empty, sp2)
          val name = Name.QName(sp1, namespace, ident, sp2)
          val lambda = WeededAst.Expression.VarOrRef(name, mkSL(sp1, sp2))
          WeededAst.Expression.Apply(lambda, List(), mkSL(sp1, sp2)).toSuccess
      }
      visit(exp0)
    }
  }

  object Patterns {

    /**
      * Weeds the given pattern.
      */
    def weed(pat0: ParsedAst.Literal): Validation[WeededAst.Pattern, WeederError] = pat0 match {
      case ParsedAst.Literal.Unit(sp1, sp2) => WeededAst.Pattern.Unit(mkSL(sp1, sp2)).toSuccess
      case ParsedAst.Literal.True(sp1, sp2) => WeededAst.Pattern.True(mkSL(sp1, sp2)).toSuccess
      case ParsedAst.Literal.False(sp1, sp2) => WeededAst.Pattern.False(mkSL(sp1, sp2)).toSuccess
      case ParsedAst.Literal.Char(sp1, lit, sp2) => WeededAst.Pattern.Char(lit(0), mkSL(sp1, sp2)).toSuccess
      case ParsedAst.Literal.Float32(sp1, sign, before, after, sp2) =>
        toFloat32(sign, before, after, mkSL(sp1, sp2)) map {
          case lit => WeededAst.Pattern.Float32(lit, mkSL(sp1, sp2))
        }
      case ParsedAst.Literal.Float64(sp1, sign, before, after, sp2) =>
        toFloat64(sign, before, after, mkSL(sp1, sp2)) map {
          case lit => WeededAst.Pattern.Float64(lit, mkSL(sp1, sp2))
        }
      case ParsedAst.Literal.Int8(sp1, sign, digits, sp2) =>
        toInt8(sign, digits, mkSL(sp1, sp2)) map {
          case lit => WeededAst.Pattern.Int8(lit, mkSL(sp1, sp2))
        }
      case ParsedAst.Literal.Int16(sp1, sign, digits, sp2) =>
        toInt16(sign, digits, mkSL(sp1, sp2)) map {
          case lit => WeededAst.Pattern.Int16(lit, mkSL(sp1, sp2))
        }
      case ParsedAst.Literal.Int32(sp1, sign, digits, sp2) =>
        toInt32(sign, digits, mkSL(sp1, sp2)) map {
          case lit => WeededAst.Pattern.Int32(lit, mkSL(sp1, sp2))
        }
      case ParsedAst.Literal.Int64(sp1, sign, digits, sp2) =>
        toInt64(sign, digits, mkSL(sp1, sp2)) map {
          case lit => WeededAst.Pattern.Int64(lit, mkSL(sp1, sp2))
        }
      case ParsedAst.Literal.BigInt(sp1, sign, digits, sp2) =>
        toBigInt(sign, digits, mkSL(sp1, sp2)) map {
          case lit => WeededAst.Pattern.BigInt(lit, mkSL(sp1, sp2))
        }
      case ParsedAst.Literal.Str(sp1, lit, sp2) =>
        WeededAst.Pattern.Str(lit, mkSL(sp1, sp2)).toSuccess
    }

    /**
      * Compiles a parsed pattern into a weeded pattern.
      */
    def weed(pattern: ParsedAst.Pattern): Validation[WeededAst.Pattern, WeederError] = {
      /*
       *  Check for non-linear pattern, i.e. if a variable occurs multiple times.
       */
      val seen = mutable.Map.empty[String, Name.Ident]

      /*
       * Local visitor.
       */
      def visit(pattern: ParsedAst.Pattern): Validation[WeededAst.Pattern, WeederError] = pattern match {
        case ParsedAst.Pattern.Wild(sp1, sp2) => WeededAst.Pattern.Wild(mkSL(sp1, sp2)).toSuccess

        case ParsedAst.Pattern.Var(sp1, ident, sp2) => seen.get(ident.name) match {
          case None =>
            seen += (ident.name -> ident)
            WeededAst.Pattern.Var(ident, mkSL(sp1, sp2)).toSuccess
          case Some(otherIdent) =>
            NonLinearPattern(ident.name, otherIdent.loc, mkSL(sp1, sp2)).toFailure
        }

        case ParsedAst.Pattern.Lit(sp1, lit, sp2) => weed(lit)

        case ParsedAst.Pattern.Tag(sp1, enum, tag, o, sp2) =>
          /*
           * Introduce implicit unit, if needed.
           */
          o match {
            case None =>
              val loc = mkSL(sp1, sp2)
              val lit = WeededAst.Pattern.Unit(loc)
              WeededAst.Pattern.Tag(enum, tag, lit, loc).toSuccess
            case Some(pat) => visit(pat) map {
              case p => WeededAst.Pattern.Tag(enum, tag, p, mkSL(sp1, sp2))
            }
          }

        case ParsedAst.Pattern.Tuple(sp1, pats, sp2) =>
          /*
           * Rewrites empty tuples to Unit and eliminate single-element tuples.
           */
          @@(pats map visit) map {
            case Nil => WeededAst.Pattern.Unit(mkSL(sp1, sp2))
            case x :: Nil => x
            case xs => WeededAst.Pattern.Tuple(xs, mkSL(sp1, sp2))
          }

        case ParsedAst.Pattern.FNil(sp1, sp2) =>
          WeededAst.Pattern.FNil(mkSL(sp1, sp2)).toSuccess

        case ParsedAst.Pattern.FList(pat1, pat2, sp2) =>
          @@(weed(pat1), weed(pat2)) map {
            case (hd, tl) => WeededAst.Pattern.FList(hd, tl, mkSL(pat1.leftMostSourcePosition, sp2))
          }

        case ParsedAst.Pattern.FVec(sp1, elms, rest, sp2) =>
          val elmsVal = @@(elms.map(visit))
          val restVal = @@(rest.map(visit))

          @@(elmsVal, restVal) map {
            case (es, r) => WeededAst.Pattern.FVec(es, r, mkSL(sp1, sp2))
          }

        case ParsedAst.Pattern.FSet(sp1, elms, rest, sp2) =>
          val elmsVal = @@(elms.map(visit))
          val restVal = @@(rest.map(visit))

          @@(elmsVal, restVal) map {
            case (es, r) => WeededAst.Pattern.FSet(es, r, mkSL(sp1, sp2))
          }

        case ParsedAst.Pattern.FMap(sp1, elms, rest, sp2) =>
          val elmsVal = @@(elms.map {
            case (key, value) => @@(visit(key), visit(value))
          })
          val restVal = @@(rest.map(visit))

          @@(elmsVal, restVal) map {
            case (es, r) => WeededAst.Pattern.FMap(es, r, mkSL(sp1, sp2))
          }
      }

      visit(pattern)
    }
  }

  object Predicate {

    object Head {

      /**
        * Weeds the given head predicate.
        */
      def weed(past: ParsedAst.Predicate): Validation[WeededAst.Predicate.Head, WeederError] = past match {
        case ParsedAst.Predicate.True(sp1, sp2) => WeededAst.Predicate.Head.True(mkSL(sp1, sp2)).toSuccess
        case ParsedAst.Predicate.False(sp1, sp2) => WeededAst.Predicate.Head.False(mkSL(sp1, sp2)).toSuccess
        case ParsedAst.Predicate.Filter(sp1, qname, term, sp2) => IllegalHeadPredicate(mkSL(sp1, sp2)).toFailure
        case ParsedAst.Predicate.Table(sp1, qname, terms, sp2) =>
          @@(terms.toList.map(t => Expressions.weed(t))) flatMap {
            case ts =>
              if (qname.isUpperCase)
                WeededAst.Predicate.Head.Table(qname, ts, mkSL(sp1, sp2)).toSuccess
              else
                IllegalSyntax("A head predicate must be uppercase and refer to a relation or lattice.", mkSL(sp1, sp2)).toFailure
          }
        case ParsedAst.Predicate.Loop(sp1, ident, term, sp2) => IllegalHeadPredicate(mkSL(sp1, sp2)).toFailure
        case ParsedAst.Predicate.NotEqual(sp1, ident1, ident2, sp2) => IllegalHeadPredicate(mkSL(sp1, sp2)).toFailure
      }

    }

    object Body {

      /**
        * Weeds the given body predicate.
        */
      def weed(past: ParsedAst.Predicate): Validation[WeededAst.Predicate.Body, WeederError] = past match {
        case ParsedAst.Predicate.True(sp1, sp2) => IllegalSyntax("A true predicate is not allowed in the body of a rule.", mkSL(sp1, sp2)).toFailure
        case ParsedAst.Predicate.False(sp1, sp2) => IllegalSyntax("A false predicate is not allowed in the body of a rule.", mkSL(sp1, sp2)).toFailure
        case ParsedAst.Predicate.Filter(sp1, qname, terms, sp2) =>
          val loc = mkSL(sp1, sp2)
          @@(terms.map(t => Expressions.weed(exp0 = t, allowWildcards = true))) map {
            case ts => WeededAst.Predicate.Body.Filter(qname, ts, loc)
          }
        case ParsedAst.Predicate.Table(sp1, qname, terms, sp2) =>
          val loc = mkSL(sp1, sp2)
          @@(terms.map(t => Expressions.weed(exp0 = t, allowWildcards = true))) map {
            case ts => WeededAst.Predicate.Body.Table(qname, ts, loc)
          }
        case ParsedAst.Predicate.NotEqual(sp1, ident1, ident2, sp2) =>
          WeededAst.Predicate.Body.NotEqual(ident1, ident2, mkSL(sp1, sp2)).toSuccess
        case ParsedAst.Predicate.Loop(sp1, ident, term, sp2) => Expressions.weed(term) map {
          case t => WeededAst.Predicate.Body.Loop(ident, t, mkSL(sp1, sp2))
        }
      }
    }

  }

  object Annotations {
    /**
      * Weeds the given sequence of parsed annotation `xs`.
      */
    def weed(xs: Seq[ParsedAst.Annotation]): Validation[Ast.Annotations, WeederError] = {
      // collect seen annotations.
      val seen = mutable.Map.empty[String, ParsedAst.Annotation]

      // loop through each annotation.
      val result = xs.toList map {
        case x => seen.get(x.ident.name) match {
          case None =>
            seen += (x.ident.name -> x)
            Annotations.weed(x)
          case Some(otherAnn) =>
            DuplicateAnnotation(x.ident.name, mkSL(otherAnn.sp1, otherAnn.sp2), mkSL(x.sp1, x.sp2)).toFailure
        }
      }
      @@(result) map Ast.Annotations
    }

    /**
      * Weeds the given parsed annotation `past`.
      */
    def weed(past: ParsedAst.Annotation): Validation[Ast.Annotation, WeederError] = {
      /*
       * Check for `UndefinedAnnotation`.
       */
      val loc = mkSL(past.sp1, past.sp2)
      past.ident.name match {
        case "associative" => Ast.Annotation.Associative(loc).toSuccess
        case "commutative" => Ast.Annotation.Commutative(loc).toSuccess
        case "internal" => Ast.Annotation.Internal(loc).toSuccess
        case "monotone" => Ast.Annotation.Monotone(loc).toSuccess
        case "strict" => Ast.Annotation.Strict(loc).toSuccess
        case "unchecked" => Ast.Annotation.Unchecked(loc).toSuccess
        case "unsafe" => Ast.Annotation.Unsafe(loc).toSuccess
        case _ => UndefinedAnnotation(past.ident.name, loc).toFailure
      }
    }
  }

  object Types {

    /**
      * Weeds the given parsed type `tpe`.
      */
    def weed(tpe: ParsedAst.Type): WeededAst.Type = tpe match {
      case ParsedAst.Type.Unit(sp1, sp2) => WeededAst.Type.Unit(mkSL(sp1, sp2))
      case ParsedAst.Type.Var(sp1, ident, sp2) => WeededAst.Type.Var(ident, mkSL(sp1, sp2))
      case ParsedAst.Type.Ref(sp1, qname, sp2) => WeededAst.Type.Ref(qname, mkSL(sp1, sp2))
      case ParsedAst.Type.Tuple(sp1, elms, sp2) => WeededAst.Type.Tuple(elms.toList.map(weed), mkSL(sp1, sp2))
      case ParsedAst.Type.Arrow(sp1, tparams, tresult, sp2) => WeededAst.Type.Arrow(tparams.toList.map(weed), weed(tresult), mkSL(sp1, sp2))
      case ParsedAst.Type.Apply(sp1, base, tparams, sp2) => WeededAst.Type.Apply(weed(base), tparams.toList.map(weed), mkSL(sp1, sp2))
    }

  }

  /**
    * Attempts to parse the given float32 with `sign` digits `before` and `after` the comma.
    */
  def toFloat32(sign: Boolean, before: String, after: String, loc: SourceLocation): Validation[Float, WeederError] = try {
    val s = if (sign) s"-$before.$after" else s"$before.$after"
    s.toFloat.toSuccess
  } catch {
    case e: NumberFormatException => IllegalFloat(loc).toFailure
  }

  /**
    * Attempts to parse the given float64 with `sign` digits `before` and `after` the comma.
    */
  def toFloat64(sign: Boolean, before: String, after: String, loc: SourceLocation): Validation[Double, WeederError] = try {
    val s = if (sign) s"-$before.$after" else s"$before.$after"
    s.toDouble.toSuccess
  } catch {
    case e: NumberFormatException => IllegalFloat(loc).toFailure
  }

  /**
    * Attempts to parse the given int8 with `sign` and `digits`.
    */
  def toInt8(sign: Boolean, digits: String, loc: SourceLocation): Validation[Byte, WeederError] = try {
    val s = if (sign) "-" + digits else digits
    s.toByte.toSuccess
  } catch {
    case ex: NumberFormatException => IllegalInt(loc).toFailure
  }

  /**
    * Attempts to parse the given int16 with `sign` and `digits`.
    */
  def toInt16(sign: Boolean, digits: String, loc: SourceLocation): Validation[Short, WeederError] = try {
    val s = if (sign) "-" + digits else digits
    s.toShort.toSuccess
  } catch {
    case ex: NumberFormatException => IllegalInt(loc).toFailure
  }

  /**
    * Attempts to parse the given int32 with `sign` and `digits`.
    */
  def toInt32(sign: Boolean, digits: String, loc: SourceLocation): Validation[Int, WeederError] = try {
    val s = if (sign) "-" + digits else digits
    s.toInt.toSuccess
  } catch {
    case ex: NumberFormatException => IllegalInt(loc).toFailure
  }

  /**
    * Attempts to parse the given int64 with `sign` and `digits`.
    */
  def toInt64(sign: Boolean, digits: String, loc: SourceLocation): Validation[Long, WeederError] = try {
    val s = if (sign) "-" + digits else digits
    s.toLong.toSuccess
  } catch {
    case ex: NumberFormatException => IllegalInt(loc).toFailure
  }

  /**
    * Attempts to parse the given BigInt with `sign` and `digits`.
    */
  def toBigInt(sign: Boolean, digits: String, loc: SourceLocation): Validation[BigInteger, WeederError] = try {
    val s = if (sign) "-" + digits else digits
    new BigInteger(s).toSuccess
  } catch {
    case ex: NumberFormatException => IllegalInt(loc).toFailure
  }

  /**
    * Alias for SourceLocation.mk
    */
  private def mkSL(sp1: SourcePosition, sp2: SourcePosition): SourceLocation = SourceLocation.mk(sp1, sp2)

  /**
    * Returns the left most source position in the sub-tree of the expression `e`.
    */
  private def leftMostSourcePosition(e: ParsedAst.Expression): SourcePosition = e match {
    case ParsedAst.Expression.Wild(sp1, _) => sp1
    case ParsedAst.Expression.SName(sp1, _, _) => sp1
    case ParsedAst.Expression.QName(sp1, _, _) => sp1
    case ParsedAst.Expression.Lit(sp1, _, _) => sp1
    case ParsedAst.Expression.Apply(e1, _, _) => leftMostSourcePosition(e1)
    case ParsedAst.Expression.Infix(e1, _, _, _) => leftMostSourcePosition(e1)
    case ParsedAst.Expression.Lambda(sp1, _, _, _) => sp1
    case ParsedAst.Expression.Unary(sp1, _, _, _) => sp1
    case ParsedAst.Expression.Binary(e1, _, _, _) => leftMostSourcePosition(e1)
    case ParsedAst.Expression.ExtendedBinary(e1, _, _, _) => leftMostSourcePosition(e1)
    case ParsedAst.Expression.IfThenElse(sp1, _, _, _, _) => sp1
    case ParsedAst.Expression.LetMatch(sp1, _, _, _, _) => sp1
    case ParsedAst.Expression.Match(sp1, _, _, _) => sp1
    case ParsedAst.Expression.Switch(sp1, _, _) => sp1
    case ParsedAst.Expression.Tag(sp1, _, _, _, _) => sp1
    case ParsedAst.Expression.Tuple(sp1, _, _) => sp1
    case ParsedAst.Expression.FNil(sp1, _) => sp1
    case ParsedAst.Expression.FList(hd, _, _) => leftMostSourcePosition(hd)
    case ParsedAst.Expression.FVec(sp1, _, _) => sp1
    case ParsedAst.Expression.FSet(sp1, _, _) => sp1
    case ParsedAst.Expression.FMap(sp1, _, _) => sp1
    case ParsedAst.Expression.GetIndex(sp1, _, _, _) => sp1
    case ParsedAst.Expression.PutIndex(sp1, _, _, _, _) => sp1
    case ParsedAst.Expression.Existential(sp1, _, _, _) => sp1
    case ParsedAst.Expression.Universal(sp1, _, _, _) => sp1
    case ParsedAst.Expression.Ascribe(e1, _, _) => leftMostSourcePosition(e1)
    case ParsedAst.Expression.UserError(sp1, _) => sp1
    case ParsedAst.Expression.Bot(sp1, sp2) => sp1
    case ParsedAst.Expression.Top(sp1, sp2) => sp1
  }

  /**
    * Checks that no attributes are repeated.
    */
  private def checkDuplicateAttribute(attrs: Seq[ParsedAst.Attribute]): Validation[List[WeededAst.Attribute], WeederError] = {
    val seen = mutable.Map.empty[String, Name.Ident]
    @@(attrs.map {
      case attr@ParsedAst.Attribute(sp1, ident, tpe, sp2) => seen.get(ident.name) match {
        case None =>
          seen += (ident.name -> ident)
          WeededAst.Attribute(ident, Types.weed(tpe), mkSL(sp1, sp2)).toSuccess
        case Some(otherIdent) =>
          DuplicateAttribute(ident.name, otherIdent.loc, ident.loc).toFailure
      }
    })
  }

  /**
    * Checks that no formal parameters are repeated.
    */
  private def checkDuplicateFormal(params: Seq[ParsedAst.FormalParam]): Validation[List[WeededAst.FormalParam], WeederError] = {
    val seen = mutable.Map.empty[String, Name.Ident]
    @@(params.map {
      case formal@ParsedAst.FormalParam(sp1, ident, tpe, sp2) => seen.get(ident.name) match {
        case None =>
          seen += (ident.name -> ident)
          WeededAst.FormalParam(ident, Types.weed(tpe), mkSL(sp1, sp2)).toSuccess
        case Some(otherIdent) =>
          DuplicateFormal(ident.name, otherIdent.loc, ident.loc).toFailure
      }
    })
  }

  /**
    * Checks that no formal parameters are repeated.
    */
  private def checkDuplicateFormal2(params: Seq[Name.Ident]): Validation[List[Name.Ident], WeederError] = {
    val seen = mutable.Map.empty[String, Name.Ident]
    @@(params.map {
      case ident => seen.get(ident.name) match {
        case None =>
          seen += (ident.name -> ident)
          ident.toSuccess
        case Some(otherIdent) =>
          DuplicateFormal(ident.name, otherIdent.loc, ident.loc).toFailure
      }
    })
  }

}
