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

import ca.uwaterloo.flix.api.Flix
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
object Weeder extends Phase[ParsedAst.Program, WeededAst.Program] {

  /**
    * Weeds the whole program.
    */
  def run(program: ParsedAst.Program)(implicit flix: Flix): Validation[WeededAst.Program, WeederError] = {
    val b = System.nanoTime()
    @@(program.roots map weed) map {
      case roots =>
        val e = System.nanoTime() - b
        WeededAst.Program(roots, program.hooks, flix.getReachableRoots, program.time.copy(weeder = e))
    }
  }

  /**
    * Weeds the given abstract syntax tree.
    */
  def weed(root: ParsedAst.Root)(implicit flix: Flix): Validation[WeededAst.Root, WeederError] = {
    @@(@@(root.decls map Declarations.weed), Properties.weed(root)) map {
      case (decls1, decls2) => WeededAst.Root(decls1.flatten ++ decls2)
    }
  }

  object Declarations {

    /**
      * Compiles the given parsed declaration `past` to a list of weeded declarations.
      */
    def weed(decl: ParsedAst.Declaration)(implicit flix: Flix): Validation[List[WeededAst.Declaration], WeederError] = decl match {
      case ParsedAst.Declaration.Namespace(sp1, name, decls, sp2) =>
        @@(decls.map(weed)) map {
          case ds => List(WeededAst.Declaration.Namespace(name, ds.flatten, mkSL(sp1, sp2)))
        }

      case ParsedAst.Declaration.Definition(docOpt, ann, sp1, ident, tparams0, paramsOpt, tpe, exp, sp2) =>
        val loc = mkSL(ident.sp1, ident.sp2)
        val doc = docOpt.map(d => Ast.Documentation(d.text.mkString(" "), loc))
        val annVal = Annotations.weed(ann)
        val expVal = Expressions.weed(exp)
        val tparams = tparams0.toList.map(_.ident)

        /*
         * Check for `IllegalParameterList`.
         */
        paramsOpt match {
          case None => @@(annVal, expVal) map {
            case (as, e) =>
              val t = WeededAst.Type.Arrow(Nil, Types.weed(tpe), loc)
              List(WeededAst.Declaration.Definition(doc, as, ident, tparams, Nil, e, t, loc))
          }
          case Some(Nil) => IllegalParameterList(loc).toFailure
          case Some(params) =>
            /*
             * Check for `DuplicateFormal`.
             */
            val formalsVal = checkDuplicateFormal(params)
            @@(annVal, formalsVal, expVal) map {
              case (as, fs, e) =>
                val t = WeededAst.Type.Arrow(fs map (_.tpe), Types.weed(tpe), loc)
                List(WeededAst.Declaration.Definition(doc, as, ident, tparams, fs, e, t, loc))
            }
        }

      case ParsedAst.Declaration.Law(docOpt, sp1, ident, tparams, paramsOpt, tpe, exp, sp2) =>
        val loc = mkSL(sp1, sp2)
        val doc = docOpt.map(d => Ast.Documentation(d.text.mkString(" "), loc))

        /*
         * Check for `IllegalParameterList`.
         */
        Expressions.weed(exp) flatMap {
          case e => paramsOpt match {
            case None =>
              // Rewrite to Definition.
              val ann = Ast.Annotations(List(Ast.Annotation.Law(loc)))
              val t = WeededAst.Type.Arrow(Nil, Types.weed(tpe), loc)
              List(WeededAst.Declaration.Definition(doc, ann, ident, tparams.map(_.ident).toList, Nil, e, t, loc)).toSuccess
            case Some(Nil) => IllegalParameterList(mkSL(sp1, sp2)).toFailure
            case Some(params) =>
              /*
               * Check for `DuplicateFormal`.
               */
              checkDuplicateFormal(params) map {
                case fs =>
                  // Rewrite to Definition.
                  val ann = Ast.Annotations(List(Ast.Annotation.Law(loc)))
                  val t = WeededAst.Type.Arrow(fs map (_.tpe), Types.weed(tpe), loc)
                  List(WeededAst.Declaration.Definition(doc, ann, ident, tparams.map(_.ident).toList, fs, e, t, loc))
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
            val tagName = caze.ident.name
            macc.get(tagName) match {
              case None => (macc + (tagName -> WeededAst.Case(ident, caze.ident, Types.weed(caze.tpe)))).toSuccess
              case Some(otherTag) =>
                val loc1 = otherTag.tag.loc
                val loc2 = mkSL(caze.ident.sp1, caze.ident.sp2)
                DuplicateTag(ident.name, tagName, loc1, loc2).toFailure
            }
        } map {
          case m => List(WeededAst.Declaration.Enum(doc, ident, tparams, m, mkSL(sp1, sp2)))
        }

      case ParsedAst.Declaration.Type(docOpt, sp1, ident, caze, sp2) =>
        val doc = docOpt.map(d => Ast.Documentation(d.text.mkString(" "), mkSL(d.sp1, d.sp2)))
        /*
         * Rewrites a type alias to a singleton enum declaration.
         */
        val cases = Map(caze.ident.name -> WeededAst.Case(ident, caze.ident, Types.weed(caze.tpe)))
        List(WeededAst.Declaration.Enum(doc, ident, Nil, cases, mkSL(sp1, sp2))).toSuccess

      case ParsedAst.Declaration.Relation(docOpt, sp1, ident, attrs, sp2) =>
        val doc = docOpt.map(d => Ast.Documentation(d.text.mkString(" "), mkSL(d.sp1, d.sp2)))

        /*
         * Check for `EmptyRelation`
         */
        if (attrs.isEmpty)
          return EmptyRelation(ident.name, mkSL(sp1, sp2)).toFailure

        /*
         * Check for `DuplicateAttribute`.
         */
        checkDuplicateAttribute(attrs) map {
          case as => List(WeededAst.Table.Relation(doc, ident, as, mkSL(sp1, sp2)))
        }

      case ParsedAst.Declaration.Lattice(docOpt, sp1, ident, attrs, sp2) =>
        val doc = docOpt.map(d => Ast.Documentation(d.text.mkString(" "), mkSL(d.sp1, d.sp2)))

        /*
         * Check for `EmptyLattice`.
         */
        if (attrs.isEmpty)
          return EmptyLattice(ident.name, mkSL(sp1, sp2)).toFailure

        /*
         * Check for `DuplicateAttribute`.
         */
        checkDuplicateAttribute(attrs) map {
          case as =>
            // Split the attributes into keys and element.
            List(WeededAst.Table.Lattice(doc, ident, as.init, as.last, mkSL(sp1, sp2)))
        }

      case ParsedAst.Declaration.Constraint(sp1, head, body, sp2) =>
        val headVal = @@(head.map(Predicate.Head.weed))
        val bodyVal = @@(body.map(disj => @@(disj.map(Predicate.Body.weed))))

        @@(headVal, bodyVal) map {
          case (headConj, bs) =>
            // Duplicate the constraint for each predicate in the head conjunction.
            headConj flatMap {
              case h =>
                // Duplicate the constraint for each predicate in a body disjunction.
                val unfolded = bs.foldRight(List(Nil): List[List[WeededAst.Predicate.Body]]) {
                  case (xs, acc) => xs.map(p => acc.flatMap(rs => p :: rs))
                }
                unfolded map {
                  case b => WeededAst.Declaration.Constraint(h, b, mkSL(sp1, sp2))
                }
            }
        }

      case ParsedAst.Declaration.Index(sp1, qname, indexes, sp2) =>
        /*
         * Check for `EmptyIndex` and `IllegalIndex`.
         */
        val sl = mkSL(sp1, sp2)
        if (indexes.isEmpty)
          EmptyIndex(qname.ident.name, sl).toFailure
        else if (indexes.exists(_.isEmpty))
          IllegalIndex(sl).toFailure
        else
          List(WeededAst.Declaration.Index(qname, indexes.toList.map(_.toList), sl)).toSuccess

      case ParsedAst.Declaration.BoundedLattice(sp1, tpe, elms, sp2) =>
        val elmsVal = @@(elms.toList.map(e => Expressions.weed(e)))
        elmsVal flatMap {
          case List(bot, top, leq, lub, glb) => List(WeededAst.Declaration.BoundedLattice(Types.weed(tpe), bot, top, leq, lub, glb, mkSL(sp1, sp2))).toSuccess
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
    def weed(exp0: ParsedAst.Expression)(implicit flix: Flix): Validation[WeededAst.Expression, WeederError] = {
      /**
        * Inner visitor.
        *
        * @param e0     the expression.
        * @param unsafe `true` if we are inside an unsafe scope.
        */
      def visit(e0: ParsedAst.Expression, unsafe: Boolean): Validation[WeededAst.Expression, WeederError] = e0 match {
        case ParsedAst.Expression.Wild(sp1, sp2) => IllegalWildcard(mkSL(sp1, sp2)).toFailure

        case ParsedAst.Expression.SName(sp1, ident, sp2) =>
          val qname = Name.mkQName(ident)
          WeededAst.Expression.VarOrRef(qname, mkSL(sp1, sp2)).toSuccess

        case ParsedAst.Expression.QName(sp1, qname, sp2) =>
          WeededAst.Expression.VarOrRef(qname, mkSL(sp1, sp2)).toSuccess

        case ParsedAst.Expression.Lit(sp1, lit, sp2) => toExp(lit)

        case ParsedAst.Expression.Apply(lambda, args, sp2) =>
          val sp1 = leftMostSourcePosition(lambda)
          @@(visit(lambda, unsafe), @@(args.map(e => visit(e, unsafe)))) flatMap {
            case (e, as) => WeededAst.Expression.Apply(e, as, mkSL(sp1, sp2)).toSuccess
          }

        case ParsedAst.Expression.Infix(exp1, name, exp2, sp2) =>
          /*
           * Rewrites infix expressions to apply expressions.
           */
          @@(visit(exp1, unsafe), visit(exp2, unsafe)) map {
            case (e1, e2) =>
              val loc = mkSL(leftMostSourcePosition(exp1), sp2)
              val lambda = WeededAst.Expression.VarOrRef(name, loc)
              WeededAst.Expression.Apply(lambda, List(e1, e2), loc)
          }

        case ParsedAst.Expression.Postfix(exp, ident, exps, sp2) =>
          /*
           * Rewrites postfix expressions to apply expressions.
           */
          @@(visit(exp, unsafe), @@(exps.map(e => visit(e, unsafe)))) map {
            case (e, es) =>
              val sp1 = leftMostSourcePosition(exp)
              val loc = mkSL(sp1, sp2)
              val qname = Name.mkQName(ident)
              val lambda = WeededAst.Expression.VarOrRef(qname, loc)
              WeededAst.Expression.Apply(lambda, e :: es, loc)
          }

        case ParsedAst.Expression.Lambda(sp1, params, exp, sp2) =>
          /*
           * Check for `DuplicateFormal`.
           */
          checkDuplicateFormal2(params) flatMap {
            case ps =>
              visit(exp, unsafe) map {
                case e => WeededAst.Expression.Lambda(params.toList, e, mkSL(sp1, sp2))
              }
          }

        case ParsedAst.Expression.LambdaMatch(sp1, pat, exp, sp2) =>
          /*
           * Rewrites lambda pattern match expressions into a lambda expression with a nested pattern match.
           */
          @@(Patterns.weed(pat), Expressions.weed(exp)) map {
            case (p, e) =>
              val loc = mkSL(sp1, sp2)
              // The name of the lambda parameter.
              val ident = Name.Ident(sp1, "pat$0", sp2)
              val qname = Name.mkQName(ident)
              // Construct the body of the lambda expression.
              val varOrRef = WeededAst.Expression.VarOrRef(qname, loc)
              val rule = WeededAst.MatchRule(p, WeededAst.Expression.True(loc), e)
              val body = WeededAst.Expression.Match(varOrRef, List(rule), loc)
              WeededAst.Expression.Lambda(List(ident), body, loc)
          }

        case ParsedAst.Expression.Unary(sp1, op, exp, sp2) =>
          val loc = mkSL(sp1, sp2)
          visit(exp, unsafe) map {
            case e => op match {
              case "!" => WeededAst.Expression.Unary(UnaryOperator.LogicalNot, e, loc)
              case "+" => WeededAst.Expression.Unary(UnaryOperator.Plus, e, loc)
              case "-" => WeededAst.Expression.Unary(UnaryOperator.Minus, e, loc)
              case "~~~" => WeededAst.Expression.Unary(UnaryOperator.BitwiseNegate, e, loc)
              case _ => mkApply(op, List(e), sp1, sp2)
            }
          }

        case ParsedAst.Expression.Binary(exp1, op, exp2, sp2) =>
          val sp1 = leftMostSourcePosition(exp1)
          val loc = mkSL(sp1, sp2)
          @@(visit(exp1, unsafe), visit(exp2, unsafe)) map {
            case (e1, e2) => op match {
              case "+" => WeededAst.Expression.Binary(BinaryOperator.Plus, e1, e2, loc)
              case "-" => WeededAst.Expression.Binary(BinaryOperator.Minus, e1, e2, loc)
              case "*" => WeededAst.Expression.Binary(BinaryOperator.Times, e1, e2, loc)
              case "/" => WeededAst.Expression.Binary(BinaryOperator.Divide, e1, e2, loc)
              case "%" => WeededAst.Expression.Binary(BinaryOperator.Modulo, e1, e2, loc)
              case "**" => WeededAst.Expression.Binary(BinaryOperator.Exponentiate, e1, e2, loc)
              case "<" => WeededAst.Expression.Binary(BinaryOperator.Less, e1, e2, loc)
              case "<=" => WeededAst.Expression.Binary(BinaryOperator.LessEqual, e1, e2, loc)
              case ">" => WeededAst.Expression.Binary(BinaryOperator.Greater, e1, e2, loc)
              case ">=" => WeededAst.Expression.Binary(BinaryOperator.GreaterEqual, e1, e2, loc)
              case "==" => WeededAst.Expression.Binary(BinaryOperator.Equal, e1, e2, loc)
              case "!=" => WeededAst.Expression.Binary(BinaryOperator.NotEqual, e1, e2, loc)
              case "&&" => WeededAst.Expression.Binary(BinaryOperator.LogicalAnd, e1, e2, loc)
              case "||" => WeededAst.Expression.Binary(BinaryOperator.LogicalOr, e1, e2, loc)
              case "&&&" => WeededAst.Expression.Binary(BinaryOperator.BitwiseAnd, e1, e2, loc)
              case "|||" => WeededAst.Expression.Binary(BinaryOperator.BitwiseOr, e1, e2, loc)
              case "^^^" => WeededAst.Expression.Binary(BinaryOperator.BitwiseXor, e1, e2, loc)
              case "<<<" => WeededAst.Expression.Binary(BinaryOperator.BitwiseLeftShift, e1, e2, loc)
              case ">>>" => WeededAst.Expression.Binary(BinaryOperator.BitwiseRightShift, e1, e2, loc)
              case _ => mkApply(op, List(e1, e2), sp1, sp2)
            }
          }

        case ParsedAst.Expression.IfThenElse(sp1, exp1, exp2, exp3, sp2) =>
          @@(visit(exp1, unsafe), visit(exp2, unsafe), visit(exp3, unsafe)) map {
            case (e1, e2, e3) => WeededAst.Expression.IfThenElse(e1, e2, e3, mkSL(sp1, sp2))
          }

        case ParsedAst.Expression.LetMatch(sp1, pat, tpe, exp1, exp2, sp2) =>
          /*
           * Rewrites a let-match to a regular let-binding or a full-blown pattern match.
           */
          @@(Patterns.weed(pat), visit(exp1, unsafe), visit(exp2, unsafe)) map {
            case (WeededAst.Pattern.Var(ident, loc), value, body) =>
              // Let-binding.
              // Check if there is a type annotation for the value expression.
              tpe match {
                case None => WeededAst.Expression.Let(ident, value, body, mkSL(sp1, sp2))
                case Some(t) =>
                  val ascribed = WeededAst.Expression.Ascribe(value, Types.weed(t), value.loc)
                  WeededAst.Expression.Let(ident, ascribed, body, mkSL(sp1, sp2))
              }
            case (pattern, value, body) =>
              // Full-blown pattern match.
              val rule = WeededAst.MatchRule(pattern, WeededAst.Expression.True(mkSL(sp1, sp2)), body)
              // Check if there is a type annotation for the value expression.
              tpe match {
                case None => WeededAst.Expression.Match(value, List(rule), mkSL(sp1, sp2))
                case Some(t) =>
                  val ascribed = WeededAst.Expression.Ascribe(value, Types.weed(t), value.loc)
                  WeededAst.Expression.Match(ascribed, List(rule), mkSL(sp1, sp2))
              }
          }

        case ParsedAst.Expression.LetRec(sp1, ident, exp1, exp2, sp2) =>
          @@(visit(exp1, unsafe), visit(exp2, unsafe)) map {
            case (value, body) => WeededAst.Expression.LetRec(ident, value, body, mkSL(sp1, sp2))
          }

        case ParsedAst.Expression.Match(sp1, exp, rules, sp2) =>
          val rulesVal = rules map {
            case ParsedAst.MatchRule(pat, None, body) => @@(Patterns.weed(pat), visit(body, unsafe)) map {
              // Pattern match without guard.
              case (p, e) => WeededAst.MatchRule(p, WeededAst.Expression.True(mkSL(sp1, sp2)), e)
            }
            case ParsedAst.MatchRule(pat, Some(guard), body) => @@(Patterns.weed(pat), visit(guard, unsafe), visit(body, unsafe)) map {
              // Pattern match with guard.
              case (p, g, b) => WeededAst.MatchRule(p, g, b)
            }
          }
          @@(visit(exp, unsafe), @@(rulesVal)) map {
            case (e, rs) => WeededAst.Expression.Match(e, rs, mkSL(sp1, sp2))
          }

        case ParsedAst.Expression.Switch(sp1, rules, sp2) =>
          val rulesVal = rules map {
            case (cond, body) => @@(visit(cond, unsafe), visit(body, unsafe))
          }
          @@(rulesVal) map {
            case rs => WeededAst.Expression.Switch(rs, mkSL(sp1, sp2))
          }

        case ParsedAst.Expression.Tag(sp1, qname, o, sp2) =>
          /*
           * Introduce implicit unit, if needed.
           */
          val (enum, tag) = asTag(qname)
          o match {
            case None =>
              val loc = mkSL(sp1, sp2)
              val exp = WeededAst.Expression.Unit(loc)
              WeededAst.Expression.Tag(enum, tag, exp, loc).toSuccess
            case Some(exp) => visit(exp, unsafe) map {
              case e => WeededAst.Expression.Tag(enum, tag, e, mkSL(sp1, sp2))
            }
          }

        case ParsedAst.Expression.Tuple(sp1, elms, sp2) =>
          /*
           * Rewrites empty tuples to Unit and eliminate single-element tuples.
           */
          @@(elms.map(e => visit(e, unsafe))) map {
            case Nil =>
              val loc = mkSL(sp1, sp2)
              WeededAst.Expression.Unit(loc)
            case x :: Nil => x
            case xs => WeededAst.Expression.Tuple(xs, mkSL(sp1, sp2))
          }

        case ParsedAst.Expression.FNil(sp1, sp2) =>
          /*
           * Rewrites a `FNil` expression into a tag expression.
           */
          val tag = Name.Ident(sp1, "Nil", sp2)
          val exp = WeededAst.Expression.Unit(mkSL(sp1, sp2))
          WeededAst.Expression.Tag(None, tag, exp, mkSL(sp1, sp2)).toSuccess

        case ParsedAst.Expression.FCons(hd, sp1, sp2, tl) =>
          /*
           * Rewrites a `FCons` expression into a tag expression.
           */
          @@(visit(hd, unsafe), visit(tl, unsafe)) map {
            case (e1, e2) =>
              val tag = Name.Ident(sp1, "Cons", sp2)
              val exp = WeededAst.Expression.Tuple(List(e1, e2), mkSL(sp1, sp2))
              WeededAst.Expression.Tag(None, tag, exp, mkSL(sp1, sp2))
          }

        case ParsedAst.Expression.FAppend(fst, sp1, sp2, snd) =>
          /*
           * Rewrites a `FAppend` expression into a call to `List/append`.
           */
          @@(visit(fst, unsafe), visit(snd, unsafe)) map {
            case (e1, e2) =>
              // NB: We painstakingly construct the qualified name
              // to ensure that source locations are available.
              mkApply("List.append", List(e1, e2), sp1, sp2)
          }

        case ParsedAst.Expression.FSet(sp1, elms, sp2) =>
          /*
           * Rewrites a `FSet` expression into `Set/empty` and a `Set/insert` calls.
           */
          @@(elms.map(e => visit(e, unsafe))) map {
            case es =>
              val empty = mkApply("Set.empty", Nil, sp1, sp2)
              es.foldLeft(empty) {
                case (acc, elm) => mkApply("Set.insert", List(elm, acc), sp1, sp2)
              }
          }

        case ParsedAst.Expression.FMap(sp1, elms, sp2) =>
          /*
           * Rewrites a `FMap` expression into `Map/empty` and a `Map/insert` calls.
           */
          val elmsVal = elms map {
            case (key, value) => @@(visit(key, unsafe), visit(value, unsafe))
          }

          @@(elmsVal) map {
            case es =>
              val empty = mkApply("Map.empty", Nil, sp1, sp2)
              es.foldLeft(empty) {
                case (acc, (k, v)) => mkApply("Map.insert", List(k, v, acc), sp1, sp2)
              }
          }

        case ParsedAst.Expression.Existential(sp1, paramsOpt, exp, sp2) =>
          /*
           * Checks for `IllegalExistential`.
           */
          visit(exp, unsafe) flatMap {
            case body => paramsOpt match {
              case None => IllegalExistential(mkSL(sp1, sp2)).toFailure
              case Some(Nil) => IllegalExistential(mkSL(sp1, sp2)).toFailure
              case Some(params) =>
                /*
                 * Check for `DuplicateFormal`.
                 */
                checkDuplicateFormal(params) map {
                  case ps =>
                    /*
                     * Rewrites the multi-parameter existential to nested single-parameter existentials.
                     */
                    ps.foldRight(body) {
                      case (param, eacc) => WeededAst.Expression.Existential(param, eacc, mkSL(sp1, sp2))
                    }
                }
            }
          }

        case ParsedAst.Expression.Universal(sp1, paramsOpt, exp, sp2) =>
          /*
           * Checks for `IllegalUniversal`.
           */
          visit(exp, unsafe) flatMap {
            case body => paramsOpt match {
              case None => IllegalUniversal(mkSL(sp1, sp2)).toFailure
              case Some(Nil) => IllegalUniversal(mkSL(sp1, sp2)).toFailure
              case Some(params) =>
                /*
                 * Check for `DuplicateFormal`.
                 */
                checkDuplicateFormal(params) map {
                  case ps =>
                    /*
                     * Rewrites the multi-parameter universal to nested single-parameter universals.
                     */
                    ps.foldRight(body) {
                      case (param, eacc) => WeededAst.Expression.Universal(param, eacc, mkSL(sp1, sp2))
                    }
                }
            }
          }

        case ParsedAst.Expression.Ascribe(exp, tpe, sp2) =>
          visit(exp, unsafe) map {
            case e => WeededAst.Expression.Ascribe(e, Types.weed(tpe), mkSL(leftMostSourcePosition(exp), sp2))
          }

        case ParsedAst.Expression.Unsafe(sp1, exp, sp2) =>
          /*
           * Check if unsafe operations have been disabled.
           */
          if (flix.options.safe) {
            return WeederError.IllegalUnsafeExpressionInSafeMode(mkSL(sp1, sp2)).toFailure
          }
          visit(exp, unsafe = true)

        case ParsedAst.Expression.NativeConstructor(sp1, fqn, args, sp2) =>
          /*
           * Check for `IllegalUnsafeExpression`.
           */
          if (!unsafe) {
            return WeederError.IllegalUnsafeExpression(mkSL(sp1, sp2)).toFailure
          }

          val className = fqn.mkString(".")
          @@(args.map(e => weed(e))) flatMap {
            case es => WeededAst.Expression.NativeConstructor(className, es, mkSL(sp1, sp2)).toSuccess
          }

        case ParsedAst.Expression.NativeField(sp1, fqn, sp2) =>
          /*
           * Check for `IllegalUnsafeExpression`.
           */
          if (!unsafe) {
            return WeederError.IllegalUnsafeExpression(mkSL(sp1, sp2)).toFailure
          }

          /*
           * Check for `IllegalNativeFieldOrMethod`.
           */
          if (fqn.size == 1) {
            return WeederError.IllegalNativeFieldOrMethodName(mkSL(sp1, sp2)).toFailure
          }

          // Extract class and field name.
          val className = fqn.dropRight(1).mkString(".")
          val fieldName = fqn.last
          WeededAst.Expression.NativeField(className, fieldName, mkSL(sp1, sp2)).toSuccess

        case ParsedAst.Expression.NativeMethod(sp1, fqn, args, sp2) =>
          /*
           * Check for `IllegalUnsafeExpression`.
           */
          if (!unsafe) {
            return WeederError.IllegalUnsafeExpression(mkSL(sp1, sp2)).toFailure
          }

          /*
           * Check for `IllegalNativeFieldOrMethod`.
           */
          if (fqn.size == 1) {
            return WeederError.IllegalNativeFieldOrMethodName(mkSL(sp1, sp2)).toFailure
          }

          // Extract class and member name.
          val className = fqn.dropRight(1).mkString(".")
          val methodName = fqn.last
          @@(args.map(e => weed(e))) flatMap {
            case es => WeededAst.Expression.NativeMethod(className, methodName, es, mkSL(sp1, sp2)).toSuccess
          }

        case ParsedAst.Expression.UserError(sp1, sp2) =>
          WeededAst.Expression.UserError(mkSL(sp1, sp2)).toSuccess
      }

      visit(exp0, unsafe = false)
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

        case ParsedAst.Pattern.Tag(sp1, qname, o, sp2) =>
          /*
           * Introduce implicit unit, if needed.
           */
          val (enum, tag) = asTag(qname)
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
          /*
           * Rewrites a `FNil` pattern into a tag pattern.
           */
          val tag = Name.Ident(sp1, "Nil", sp2)
          val pat = WeededAst.Pattern.Unit(mkSL(sp1, sp2))
          WeededAst.Pattern.Tag(None, tag, pat, mkSL(sp1, sp2)).toSuccess

        case ParsedAst.Pattern.FCons(pat1, sp1, sp2, pat2) =>
          /*
           * Rewrites a `FCons` pattern into a tag pattern.
           */
          @@(weed(pat1), weed(pat2)) map {
            case (hd, tl) =>
              val tag = Name.Ident(sp1, "Cons", sp2)
              val pat = WeededAst.Pattern.Tuple(List(hd, tl), mkSL(sp1, sp2))
              WeededAst.Pattern.Tag(None, tag, pat, mkSL(sp1, sp2))
          }

        case ParsedAst.Pattern.FSet(sp1, elms, rest, sp2) => ??? // TODO

        case ParsedAst.Pattern.FMap(sp1, elms, rest, sp2) => ??? // TODO
      }

      visit(pattern)
    }
  }

  object Predicate {

    object Head {

      /**
        * Weeds the given head predicate.
        */
      def weed(past: ParsedAst.Predicate.Head)(implicit flix: Flix): Validation[WeededAst.Predicate.Head, WeederError] = past match {
        case ParsedAst.Predicate.Head.True(sp1, sp2) => WeededAst.Predicate.Head.True(mkSL(sp1, sp2)).toSuccess

        case ParsedAst.Predicate.Head.False(sp1, sp2) => WeededAst.Predicate.Head.False(mkSL(sp1, sp2)).toSuccess

        case ParsedAst.Predicate.Head.Positive(sp1, qname, terms, sp2) =>
          @@(terms.map(t => Expressions.weed(t))) map {
            case ts => WeededAst.Predicate.Head.Positive(qname, ts, mkSL(sp1, sp2))
          }

        case ParsedAst.Predicate.Head.Negative(sp1, qname, terms, sp2) =>
          @@(terms.map(t => Expressions.weed(t))) map {
            case ts => WeededAst.Predicate.Head.Negative(qname, ts, mkSL(sp1, sp2))
          }
      }

    }

    object Body {

      /**
        * Weeds the given body predicate.
        */
      def weed(past: ParsedAst.Predicate.Body)(implicit flix: Flix): Validation[WeededAst.Predicate.Body, WeederError] = past match {
        case ParsedAst.Predicate.Body.Positive(sp1, qname, terms, sp2) =>
          @@(terms.map(t => Patterns.weed(t))) map {
            case ts => WeededAst.Predicate.Body.Positive(qname, ts, mkSL(sp1, sp2))
          }

        case ParsedAst.Predicate.Body.Negative(sp1, qname, terms, sp2) =>
          val loc = mkSL(sp1, sp2)
          @@(terms.map(t => Patterns.weed(t))) map {
            case ts => WeededAst.Predicate.Body.Negative(qname, ts, loc)
          }

        case ParsedAst.Predicate.Body.Filter(sp1, qname, terms, sp2) =>
          @@(terms.map(t => Expressions.weed(t))) map {
            case ts => WeededAst.Predicate.Body.Filter(qname, ts, mkSL(sp1, sp2))
          }

        case ParsedAst.Predicate.Body.NotEqual(sp1, ident1, ident2, sp2) =>
          val qname = Name.mkQName("neq", sp1, sp2)
          val t1 = WeededAst.Expression.VarOrRef(Name.mkQName(ident1), mkSL(ident1.sp1, ident1.sp2))
          val t2 = WeededAst.Expression.VarOrRef(Name.mkQName(ident2), mkSL(ident2.sp1, ident2.sp2))
          WeededAst.Predicate.Body.Filter(qname, List(t1, t2), mkSL(sp1, sp2)).toSuccess

        case ParsedAst.Predicate.Body.Loop(sp1, pat, term, sp2) =>
          @@(Patterns.weed(pat), Expressions.weed(term)) map {
            case (p, t) => WeededAst.Predicate.Body.Loop(p, t, mkSL(sp1, sp2))
          }
      }
    }

  }


  object Annotations {
    /**
      * Weeds the given sequence of parsed annotation `xs`.
      */
    def weed(xs: Seq[ParsedAst.AnnotationOrProperty]): Validation[Ast.Annotations, WeederError] = {
      // collect seen annotations.
      val seen = mutable.Map.empty[String, ParsedAst.Annotation]

      // loop through each annotation.
      val result = xs.toList.collect {
        case x: ParsedAst.Annotation => seen.get(x.ident.name) match {
          case None =>
            seen += (x.ident.name -> x)
            weed(x)
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
        case "benchmark" => Ast.Annotation.Benchmark(loc).toSuccess
        case "internal" => Ast.Annotation.Internal(loc).toSuccess
        case "law" => Ast.Annotation.Law(loc).toSuccess
        case "test" => Ast.Annotation.Test(loc).toSuccess
        case "unchecked" => Ast.Annotation.Unchecked(loc).toSuccess
        case "unsafe" => Ast.Annotation.Unsafe(loc).toSuccess
        case name => WeederError.UndefinedAnnotation(name, loc).toFailure
      }
    }
  }

  object Properties {
    /**
      * Weeds all properties in the given AST `root`.
      */
    def weed(root: ParsedAst.Root)(implicit flix: Flix): Validation[List[WeededAst.Declaration], WeederError] = {

      /**
        * Processes a single declaration.
        */
      def visit(decl: ParsedAst.Declaration): Validation[List[WeededAst.Declaration], WeederError] = decl match {
        // Recurse through the namespace.
        case ParsedAst.Declaration.Namespace(sp1, name, decls, sp2) =>
          @@(decls.map(visit)) map {
            case ds => List(WeededAst.Declaration.Namespace(name, ds.flatten, mkSL(sp1, sp2)))
          }

        case ParsedAst.Declaration.Definition(_, meta, _, defn, _, _, _, _, _) =>
          // Instantiate properties based on the laws referenced by the definition.
          @@(meta.collect {
            case ParsedAst.Property(sp1, law, args, sp2) =>
              val loc = mkSL(sp1, sp2)

              // Weeds the arguments of the property.
              val argsVal = args match {
                case None => Nil.toSuccess
                case Some(es) => @@(es.map(e => Expressions.weed(e)))
              }

              argsVal map {
                case as =>
                  val lam = WeededAst.Expression.VarOrRef(law, loc)
                  val fun = WeededAst.Expression.VarOrRef(Name.QName(sp1, Name.RootNS, defn, sp2), loc)
                  val exp = WeededAst.Expression.Apply(lam, fun :: as, loc)
                  WeededAst.Declaration.Property(law, defn, exp, loc)
              }
          })
        case _ => Nil.toSuccess
      }

      @@(root.decls.map(visit)).map(_.flatten)
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
      case ParsedAst.Type.Infix(tpe1, base, tpe2, sp2) =>
        /*
         * Rewrites infix type applications to regular type applications.
         */
        WeededAst.Type.Apply(weed(base), List(weed(tpe1), weed(tpe2)), mkSL(leftMostSourcePosition(tpe1), sp2))

      case ParsedAst.Type.Apply(sp1, base, tparams, sp2) => WeededAst.Type.Apply(weed(base), tparams.toList.map(weed), mkSL(sp1, sp2))
    }

  }

  /**
    * Returns an apply expression for the given fully-qualified name `fqn` and the given arguments `args`.
    */
  def mkApply(fqn: String, args: List[WeededAst.Expression], sp1: SourcePosition, sp2: SourcePosition): WeededAst.Expression = {
    val lambda = WeededAst.Expression.VarOrRef(Name.mkQName(fqn, sp1, sp2), mkSL(sp1, sp2))
    WeededAst.Expression.Apply(lambda, args, mkSL(sp1, sp2))
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
    case ParsedAst.Expression.Postfix(e1, _, _, _) => leftMostSourcePosition(e1)
    case ParsedAst.Expression.Lambda(sp1, _, _, _) => sp1
    case ParsedAst.Expression.LambdaMatch(sp1, _, _, _) => sp1
    case ParsedAst.Expression.Unary(sp1, _, _, _) => sp1
    case ParsedAst.Expression.Binary(e1, _, _, _) => leftMostSourcePosition(e1)
    case ParsedAst.Expression.IfThenElse(sp1, _, _, _, _) => sp1
    case ParsedAst.Expression.LetMatch(sp1, _, _, _, _, _) => sp1
    case ParsedAst.Expression.LetRec(sp1, _, _, _, _) => sp1
    case ParsedAst.Expression.Match(sp1, _, _, _) => sp1
    case ParsedAst.Expression.Switch(sp1, _, _) => sp1
    case ParsedAst.Expression.Tag(sp1, _, _, _) => sp1
    case ParsedAst.Expression.Tuple(sp1, _, _) => sp1
    case ParsedAst.Expression.FNil(sp1, _) => sp1
    case ParsedAst.Expression.FCons(hd, _, _, _) => leftMostSourcePosition(hd)
    case ParsedAst.Expression.FAppend(fst, _, _, _) => leftMostSourcePosition(fst)
    case ParsedAst.Expression.FSet(sp1, _, _) => sp1
    case ParsedAst.Expression.FMap(sp1, _, _) => sp1
    case ParsedAst.Expression.Existential(sp1, _, _, _) => sp1
    case ParsedAst.Expression.Universal(sp1, _, _, _) => sp1
    case ParsedAst.Expression.Ascribe(e1, _, _) => leftMostSourcePosition(e1)
    case ParsedAst.Expression.Unsafe(sp1, _, _) => sp1
    case ParsedAst.Expression.NativeField(sp1, _, _) => sp1
    case ParsedAst.Expression.NativeMethod(sp1, _, _, _) => sp1
    case ParsedAst.Expression.NativeConstructor(sp1, _, _, _) => sp1
    case ParsedAst.Expression.UserError(sp1, _) => sp1
  }

  /**
    * Returns the left most source position in the sub-tree of the type `tpe`.
    */
  private def leftMostSourcePosition(tpe: ParsedAst.Type): SourcePosition = tpe match {
    case ParsedAst.Type.Unit(sp1, _) => sp1
    case ParsedAst.Type.Var(sp1, _, _) => sp1
    case ParsedAst.Type.Ref(sp1, _, _) => sp1
    case ParsedAst.Type.Tuple(sp1, _, _) => sp1
    case ParsedAst.Type.Arrow(sp1, _, _, _) => sp1
    case ParsedAst.Type.Infix(tpe1, _, _, _) => leftMostSourcePosition(tpe1)
    case ParsedAst.Type.Apply(sp1, _, _, _) => sp1
  }

  /**
    * Checks that no attributes are repeated.
    */
  private def checkDuplicateAttribute(attrs: Seq[ParsedAst.Attribute]): Validation[List[WeededAst.Attribute], WeederError] = {
    val seen = mutable.Map.empty[String, ParsedAst.Attribute]
    @@(attrs.map {
      case attr@ParsedAst.Attribute(sp1, ident, tpe, sp2) => seen.get(ident.name) match {
        case None =>
          seen += (ident.name -> attr)
          WeededAst.Attribute(ident, Types.weed(tpe), mkSL(sp1, sp2)).toSuccess
        case Some(otherAttr) =>
          val loc1 = mkSL(otherAttr.sp1, otherAttr.sp2)
          val loc2 = mkSL(attr.sp1, attr.sp2)
          DuplicateAttribute(ident.name, loc1, loc2).toFailure
      }
    })
  }

  /**
    * Checks that no formal parameters are repeated.
    */
  private def checkDuplicateFormal(params: Seq[ParsedAst.FormalParam]): Validation[List[WeededAst.FormalParam], WeederError] = {
    val seen = mutable.Map.empty[String, ParsedAst.FormalParam]
    @@(params.map {
      case param@ParsedAst.FormalParam(sp1, ident, tpe, sp2) => seen.get(ident.name) match {
        case None =>
          seen += (ident.name -> param)
          WeededAst.FormalParam(ident, Types.weed(tpe), mkSL(sp1, sp2)).toSuccess
        case Some(otherParam) =>
          val loc1 = mkSL(otherParam.sp1, otherParam.sp2)
          val loc2 = mkSL(param.sp1, param.sp2)
          DuplicateFormalParam(ident.name, loc1, loc2).toFailure
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
          DuplicateFormalParam(ident.name, otherIdent.loc, ident.loc).toFailure
      }
    })
  }

  /**
    * Re-interprets the given fully-qualified name `qname0` as an optionally fully-qualified type name followed by a tag name.
    *
    * For example,
    * -   the name `Foo` is re-interpreted as the tag name `Foo`.
    * -   the name `Foo.Bar` is re-interpreted as the type name `Foo` and the tag name `Bar`.
    * -   the name `Foo/Bar/Baz.Qux` is re-interpreted as the type name `Foo/Bar.Baz` and the tag name `Qux`.
    */
  private def asTag(qname0: Name.QName): (Option[Name.QName], Name.Ident) = {
    // The tag name is the last identifier in the qualified name.
    val tagName = qname0.ident
    // Check if there is a namespace.
    if (qname0.namespace.isRoot) {
      // No namespace, simply return the tag name.
      (None, tagName)
    } else {
      // Translates the name `Foo/Bar/Baz.Qux` into the name `Foo/Bar.Baz`.
      val nname = Name.NName(qname0.sp1, qname0.namespace.idents.init, qname0.sp2)
      val ident = qname0.namespace.idents.last
      val qname = Name.QName(qname0.sp1, nname, ident, qname0.sp2)
      (Some(qname), tagName)
    }
  }

}
