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
import ca.uwaterloo.flix.language.ast.Ast.Polarity
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.errors.WeederError
import ca.uwaterloo.flix.language.errors.WeederError._
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{CompilationMode, InternalCompilerException, Validation}

import scala.collection.immutable.Seq
import scala.collection.mutable

/**
  * The Weeder phase performs simple syntactic checks and rewritings.
  */
object Weeder extends Phase[ParsedAst.Program, WeededAst.Program] {

  /**
    * Weeds the whole program.
    */
  def run(program: ParsedAst.Program)(implicit flix: Flix): Validation[WeededAst.Program, WeederError] = flix.phase("Weeder") {
    val roots = @@(program.roots map weed)
    val named = @@(program.named.map {
      case (sym, exp) => Expressions.weed(exp).map(e => sym -> e)
    })

    @@(roots, named) map {
      case (rs, ne) =>
        WeededAst.Program(rs, ne.toMap, flix.getReachableRoots)
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

      case ParsedAst.Declaration.Def(doc0, ann, mods, sp1, ident, tparams0, fparams0, tpe, effOpt, exp0, sp2) =>
        val loc = mkSL(ident.sp1, ident.sp2)
        val doc = visitDoc(doc0)
        val annVal = Annotations.weed(ann)
        val modVal = visitModifiers(mods, legalModifiers = Set(Ast.Modifier.Inline, Ast.Modifier.Public))
        val expVal = Expressions.weed(exp0)
        val tparams = tparams0.toList.map(_.ident)
        val effVal = Effects.weed(effOpt)

        /*
          * Check for `DuplicateFormal`.
          */
        val formalsVal = Formals.weed(fparams0, typeRequired = true)
        mapN(annVal, modVal, formalsVal, expVal, effVal) {
          case (as, mod, fs, exp, eff) =>
            val e = mkCurried(fs.tail, exp, loc)
            val t = mkArrowType(fs, Types.weed(tpe), loc)
            List(WeededAst.Declaration.Def(doc, as, mod, ident, tparams, fs.head :: Nil, e, t, eff, loc))
        }

      case ParsedAst.Declaration.Eff(doc0, ann, mods, sp1, ident, tparams0, fparams0, tpe, effOpt, sp2) =>
        val loc = mkSL(ident.sp1, ident.sp2)
        val doc = visitDoc(doc0)
        val annVal = Annotations.weed(ann)
        val modVal = visitModifiers(mods, legalModifiers = Set(Ast.Modifier.Public))
        val tparams = tparams0.toList.map(_.ident)
        val effVal = Effects.weed(effOpt)

        /*
          * Check for `DuplicateFormal`.
          */
        val formalsVal = Formals.weed(fparams0, typeRequired = true)
        mapN(annVal, modVal, formalsVal, effVal) {
          case (as, mod, fs, eff) =>
            val t = mkArrowType(fs, Types.weed(tpe), loc)
            List(WeededAst.Declaration.Eff(doc, as, mod, ident, tparams, fs, t, eff, loc))
        }

      case ParsedAst.Declaration.Handler(doc0, ann, mods, sp1, ident, tparams0, fparams0, tpe, effOpt, exp0, sp2) =>
        val loc = mkSL(ident.sp1, ident.sp2)
        val doc = visitDoc(doc0)
        val annVal = Annotations.weed(ann)
        val modVal = visitModifiers(mods, legalModifiers = Set(Ast.Modifier.Public))
        val tparams = tparams0.toList.map(_.ident)
        val expVal = Expressions.weed(exp0)
        val effVal = Effects.weed(effOpt)

        /*
          * Check for `DuplicateFormal`.
          */
        val formalsVal = Formals.weed(fparams0, typeRequired = true)
        mapN(annVal, modVal, formalsVal, expVal, effVal) {
          case (as, mod, fs, exp, eff) =>
            val e = mkCurried(fs.tail, exp, loc)
            val t = mkArrowType(fs, Types.weed(tpe), loc)
            List(WeededAst.Declaration.Handler(doc, as, mod, ident, tparams, fs.head :: Nil, e, t, eff, loc))
        }

      case ParsedAst.Declaration.Law(doc0, sp1, ident, tparams0, fparams0, tpe, exp, sp2) =>
        val loc = mkSL(sp1, sp2)
        val doc = visitDoc(doc0)
        val mod = Ast.Modifiers(Ast.Modifier.Public :: Nil)

        /*
         * Check for `DuplicateFormal`.
         */
        for {
          fs <- Formals.weed(fparams0, typeRequired = true)
          e <- Expressions.weed(exp)
        } yield {
          // Rewrite to Definition.
          val ann = Ast.Annotations(List(Ast.Annotation.Law(loc)))
          val t = mkArrowType(fs, Types.weed(tpe), loc)
          List(WeededAst.Declaration.Def(doc, ann, mod, ident, tparams0.map(_.ident).toList, fs, e, t, Eff.Pure, loc))
        }

      case ParsedAst.Declaration.Enum(doc0, mods, sp1, ident, tparams0, cases, sp2) =>
        val doc = visitDoc(doc0)
        val modVal = visitModifiers(mods, legalModifiers = Set(Ast.Modifier.Public))
        val tparams = tparams0.toList.map(_.ident)

        modVal flatMap {
          case mod =>
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
              case m => List(WeededAst.Declaration.Enum(doc, mod, ident, tparams, m, mkSL(sp1, sp2)))
            }
        }

      case ParsedAst.Declaration.Type(doc0, mods, sp1, ident, caze, sp2) =>
        /*
         * Rewrites a type alias to a singleton enum declaration.
         */
        val doc = visitDoc(doc0)
        val modVal = visitModifiers(mods, legalModifiers = Set(Ast.Modifier.Public))

        modVal map {
          case mod =>
            val cases = Map(caze.ident.name -> WeededAst.Case(ident, caze.ident, Types.weed(caze.tpe)))
            List(WeededAst.Declaration.Enum(doc, mod, ident, Nil, cases, mkSL(sp1, sp2)))
        }

      case ParsedAst.Declaration.Relation(doc0, sp1, ident, attrs, sp2) =>
        val doc = visitDoc(doc0)

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

      case ParsedAst.Declaration.Lattice(doc0, sp1, ident, attrs, sp2) =>
        val doc = visitDoc(doc0)

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
          case List(bot, top, equ, leq, lub, glb) => List(WeededAst.Declaration.Lattice(Types.weed(tpe), bot, top, equ, leq, lub, glb, mkSL(sp1, sp2))).toSuccess
          case _ => IllegalLattice(mkSL(sp1, sp2)).toFailure
        }

      case ParsedAst.Declaration.Class(doc0, sp1, mod0, cc, decls, sp2) =>
        val modVal = visitModifiers(mod0, legalModifiers = Set(Ast.Modifier.Public))
        val ccVal = visitClassConstraint(cc)

        // Collect all signatures.
        val sigsVal = @@(decls.toList.collect {
          case sig: ParsedAst.Declaration.Sig => visitSig(sig)
        })

        // Collect all laws.
        // TODO
        val laws = Nil

        @@(modVal, ccVal, sigsVal) map {
          case (mod, (head, body), sigs) =>
            val doc = visitDoc(doc0)
            val loc = mkSL(sp1, sp2)
            List(
              WeededAst.Declaration.Class(doc, mod, head, body, sigs, laws, loc)
            )
        }

      case ParsedAst.Declaration.Impl(doc0, sp1, mod0, ic, defs0, sp2) =>
        val modVal = visitModifiers(mod0, legalModifiers = Set(Ast.Modifier.Public))
        val ccVal = visitImplConstraint(ic)

        // Collect all signatures.
        val defsVal = @@(defs0 map {
          case defn => Declarations.weed(defn)
        })

        @@(modVal, ccVal, defsVal) map {
          case (mod, (head, body), defs) =>
            // TODO: Slightly ugly due to lack of a visitDef.
            val ds = defs.flatten.asInstanceOf[List[WeededAst.Declaration.Def]]
            val doc = visitDoc(doc0)
            val loc = mkSL(sp1, sp2)
            List(
              WeededAst.Declaration.Impl(doc, mod, head, body, ds, loc)
            )
        }

      case ParsedAst.Declaration.Disallow(doc0, sp1, ic, sp2) =>
        visitDisallowConstraint(ic) map {
          case (body) =>
            val doc = visitDoc(doc0)
            val loc = mkSL(sp1, sp2)
            List(
              WeededAst.Declaration.Disallow(doc, body, loc)
            )
        }

      case ParsedAst.Declaration.Sig(doc0, ann, mods, sp1, ident, tparams0, fparams0, tpe, effOpt, sp2) =>
        throw InternalCompilerException(s"Unexpected declaration")
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
          WeededAst.Expression.VarOrDef(qname, mkSL(sp1, sp2)).toSuccess

        case ParsedAst.Expression.QName(sp1, qname, sp2) =>
          WeededAst.Expression.VarOrDef(qname, mkSL(sp1, sp2)).toSuccess

        case ParsedAst.Expression.Hole(sp1, name, sp2) =>
          val loc = mkSL(sp1, sp2)
          /*
           * Checks for `IllegalHole`.
           */
          if (flix.options.mode == CompilationMode.Release) {
            return IllegalHole(loc).toFailure
          }
          WeededAst.Expression.Hole(name, loc).toSuccess

        case ParsedAst.Expression.Lit(sp1, lit, sp2) => toExp(lit)

        case ParsedAst.Expression.Apply(lambda, args, sp2) =>
          val sp1 = leftMostSourcePosition(lambda)
          val loc = mkSL(sp1, sp2)
          @@(visit(lambda, unsafe), @@(args.map(e => visit(e, unsafe)))) map {
            case (e, as) =>
              val es = getApplyArgsCheckIfEmpty(as, sp1, sp2)
              mkApplyCurried(e, es, loc)
          }

        case ParsedAst.Expression.Infix(exp1, name, exp2, sp2) =>
          /*
           * Rewrites infix expressions to apply expressions.
           */
          @@(visit(exp1, unsafe), visit(exp2, unsafe)) map {
            case (e1, e2) =>
              val loc = mkSL(leftMostSourcePosition(exp1), sp2)
              val lambda = WeededAst.Expression.VarOrDef(name, loc)
              mkApplyCurried(lambda, List(e1, e2), loc)
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
              val lambda = WeededAst.Expression.VarOrDef(qname, loc)
              mkApplyCurried(lambda, e :: es, loc)
          }

        case ParsedAst.Expression.Lambda(sp1, fparams0, exp, sp2) =>
          val loc = mkSL(sp1, sp2)
          /*
           * Check for `DuplicateFormal`.
           */
          for {
            fs <- Formals.weed(fparams0, typeRequired = false)
            e <- visit(exp, unsafe)
          } yield mkCurried(fs, e, loc)

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
              val varOrRef = WeededAst.Expression.VarOrDef(qname, loc)
              val rule = WeededAst.MatchRule(p, WeededAst.Expression.True(loc), e)

              val fparam = WeededAst.FormalParam(ident, Ast.Modifiers.Empty, None, ident.loc)
              val body = WeededAst.Expression.Match(varOrRef, List(rule), loc)
              WeededAst.Expression.Lambda(fparam, body, loc)
          }

        case ParsedAst.Expression.Unary(sp1, op, exp, sp2) =>
          val loc = mkSL(sp1, sp2)
          visit(exp, unsafe) map {
            case e => op match {
              case "!" => WeededAst.Expression.Unary(UnaryOperator.LogicalNot, e, loc)
              case "+" => WeededAst.Expression.Unary(UnaryOperator.Plus, e, loc)
              case "-" => WeededAst.Expression.Unary(UnaryOperator.Minus, e, loc)
              case "~~~" => WeededAst.Expression.Unary(UnaryOperator.BitwiseNegate, e, loc)
              case _ => mkApplyFqn(op, List(e), sp1, sp2)
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
              case _ => mkApplyFqn(op, List(e1, e2), sp1, sp2)
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
                  val ascribed = WeededAst.Expression.Ascribe(value, Types.weed(t), Eff.Pure, value.loc)
                  WeededAst.Expression.Let(ident, ascribed, body, mkSL(sp1, sp2))
              }
            case (pattern, value, body) =>
              // Full-blown pattern match.
              val rule = WeededAst.MatchRule(pattern, WeededAst.Expression.True(mkSL(sp1, sp2)), body)
              // Check if there is a type annotation for the value expression.
              tpe match {
                case None => WeededAst.Expression.Match(value, List(rule), mkSL(sp1, sp2))
                case Some(t) =>
                  val ascribed = WeededAst.Expression.Ascribe(value, Types.weed(t), Eff.Pure, value.loc)
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

        case ParsedAst.Expression.Tag(sp1, qname, expOpt, sp2) =>
          val (enum, tag) = asTag(qname)

          expOpt match {
            case None =>
              // Case 1: The tag does not have an expression. Nothing more to be done.
              WeededAst.Expression.Tag(enum, tag, None, mkSL(sp1, sp2)).toSuccess
            case Some(exp) =>
              // Case 2: The tag has an expression. Perform weeding on it.
              visit(exp, unsafe) map {
                case e => WeededAst.Expression.Tag(enum, tag, Some(e), mkSL(sp1, sp2))
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

        case ParsedAst.Expression.ArrayLit(sp1, elms, sp2) =>
          @@(elms.map(e => visit(e, unsafe))) map {
            case es => WeededAst.Expression.ArrayLit(es, mkSL(sp1, sp2))
          }

        case ParsedAst.Expression.ArrayNew(sp1, elm, len, sp2) =>
          @@(visit(elm, unsafe), visit(len, unsafe)) map {
            case(e, ln) => WeededAst.Expression.ArrayNew(e, ln, mkSL(sp1, sp2))
          }

        case ParsedAst.Expression.ArrayLoad(base, index, sp2) =>
          val sp1 = leftMostSourcePosition(base)
          val loc = mkSL(sp1, sp2)

          @@(visit(base, unsafe), visit(index, unsafe)) map {
            case (b, i) => WeededAst.Expression.ArrayLoad(b, i, loc)
          }

        case ParsedAst.Expression.ArrayStore(base, indexes, elm, sp2) =>
          val sp1 = leftMostSourcePosition(base)
          val loc = mkSL(sp1, sp2)

          @@(visit(base, unsafe), @@(indexes.map(e => visit(e, unsafe))), visit(elm, unsafe)) map {
            case (b, es, e) =>
              val inner = es.init.foldLeft(b){
                case(acc, e) => WeededAst.Expression.ArrayLoad(acc, e, loc)
              }
              WeededAst.Expression.ArrayStore(inner, es.last, e, loc)
          }

        case ParsedAst.Expression.ArrayLength(sp1, base, sp2) =>
          visit(base, unsafe) map {
            case b => WeededAst.Expression.ArrayLength(b, mkSL(sp1, sp2))
          }

        case ParsedAst.Expression.ArraySlice(base, optStartIndex, optEndIndex, sp2) =>
          val sp1 = leftMostSourcePosition(base)
          val loc = mkSL(sp1, sp2)

          (optStartIndex, optEndIndex) match {
            case(None, None) =>
              visit(base, unsafe) map {
                case b => WeededAst.Expression.ArraySlice(b, WeededAst.Expression.Int32(0, loc), WeededAst.Expression.ArrayLength(b, loc), loc)
              }
            case(Some(startIndex), None) =>
              @@(visit(base, unsafe), visit(startIndex, unsafe)) map {
                case(b, i1) => WeededAst.Expression.ArraySlice(b, i1, WeededAst.Expression.ArrayLength(b, loc), loc)
              }
            case(None, Some(endIndex)) =>
              @@(visit(base, unsafe), visit(endIndex, unsafe)) map {
                case(b, i2) => WeededAst.Expression.ArraySlice(b, WeededAst.Expression.Int32(0, loc), i2, loc)
              }
            case(Some(startIndex), Some(endIndex)) =>
              @@(visit(base, unsafe), visit(startIndex, unsafe), visit(endIndex, unsafe)) map {
                case(b, i1, i2) => WeededAst.Expression.ArraySlice(b, i1, i2, loc)
              }
          }

        case ParsedAst.Expression.VectorLit(sp1, elms, sp2) =>
          @@(elms.map(e => visit(e, unsafe))) map{
            case es => WeededAst.Expression.VectorLit(es, mkSL(sp1, sp2))
          }

        case ParsedAst.Expression.VectorNew(sp1, elm, len, sp2) =>
          @@(visit(elm, unsafe), getVectorLength(len, sp1, sp2)) map {
            case (e, l) => WeededAst.Expression.VectorNew(e, l, mkSL(sp1, sp2))
            }

        case ParsedAst.Expression.VectorLoad(base, index, sp2) =>
          val sp1 = leftMostSourcePosition(base)
          val loc = mkSL(sp1, sp2)
          @@(visit(base, unsafe), getVectorLength(index, sp1, sp2)) map {
            case(b, l) => WeededAst.Expression.VectorLoad(b, l, loc)
          }

        case ParsedAst.Expression.VectorStore(base, indexes, elm, sp2) =>
          val sp1 = leftMostSourcePosition(base)
          val loc = mkSL(sp1, sp2)
          val indexesVal = checkIndexSequence(indexes, sp1, sp2)
          @@(visit(base, unsafe), seqM(indexesVal), visit(elm, unsafe)) map {
            case (b, is, e) =>
              val inner = is.init.foldLeft(b) {
                case (acc, e) => WeededAst.Expression.VectorLoad(acc, e, loc)
              }
              WeededAst.Expression.VectorStore(inner, is.last, e, loc)
          }

        case ParsedAst.Expression.VectorLength(sp1, base, sp2) =>
          visit(base, unsafe) map {
            case b => WeededAst.Expression.VectorLength(b, mkSL(sp1, sp2))
          }

        case ParsedAst.Expression.VectorSlice(base, optStartIndex, optEndIndex, sp2) =>
          val sp1 = leftMostSourcePosition(base)
          val loc = mkSL(sp1, sp2)
          (optStartIndex, optEndIndex) match {
            case (None, None) =>
              visit(base, unsafe) flatMap {
                case (b) => WeededAst.Expression.VectorSlice(b, 0, None, loc).toSuccess
              }
            case (None, Some(i)) =>
              @@(visit(base, unsafe), getVectorLength(i, sp1, sp2)) flatMap {
                case (b, l) => WeededAst.Expression.VectorSlice(b, 0, Some(l), loc).toSuccess
                case _ => WeederError.IllegalVectorLength(loc).toFailure
              }
            case (Some(i), None) =>
              @@(visit(base, unsafe), getVectorLength(i, sp1, sp2)) flatMap {
                case (b, l) => WeededAst.Expression.VectorSlice(b, l, None, loc).toSuccess
                case _ => WeederError.IllegalVectorLength(loc).toFailure
              }
            case (Some(i1), Some(i2)) =>
              @@(visit(base, unsafe), getVectorLength(i1, sp1, sp2), getVectorLength(i2, sp1, sp2)) flatMap {
                case (b, l1, l2) if l1 > l2 => WeederError.IllegalVectorIndex(loc).toFailure
                case (b, l1, l2) => WeededAst.Expression.VectorSlice(b, l1, Some(l2), loc).toSuccess
                case _ => WeederError.IllegalVectorLength(loc).toFailure
              }
          }

        case ParsedAst.Expression.FNil(sp1, sp2) =>
          /*
           * Rewrites a `FNil` expression into a tag expression.
           */
          val tag = Name.Ident(sp1, "Nil", sp2)
          val exp = WeededAst.Expression.Unit(mkSL(sp1, sp2))
          WeededAst.Expression.Tag(None, tag, Some(exp), mkSL(sp1, sp2)).toSuccess

        case ParsedAst.Expression.FCons(hd, sp1, sp2, tl) =>
          /*
           * Rewrites a `FCons` expression into a tag expression.
           */
          @@(visit(hd, unsafe), visit(tl, unsafe)) map {
            case (e1, e2) =>
              val tag = Name.Ident(sp1, "Cons", sp2)
              val exp = WeededAst.Expression.Tuple(List(e1, e2), mkSL(sp1, sp2))
              WeededAst.Expression.Tag(None, tag, Some(exp), mkSL(sp1, sp2))
          }

        case ParsedAst.Expression.FAppend(fst, sp1, sp2, snd) =>
          /*
           * Rewrites a `FAppend` expression into a call to `List/append`.
           */
          @@(visit(fst, unsafe), visit(snd, unsafe)) map {
            case (e1, e2) =>
              // NB: We painstakingly construct the qualified name
              // to ensure that source locations are available.
              mkApplyFqn("List.append", List(e1, e2), sp1, sp2)
          }

        case ParsedAst.Expression.FSet(sp1, elms, sp2) =>
          /*
           * Rewrites a `FSet` expression into `Set/empty` and a `Set/insert` calls.
           */
          @@(elms.map(e => visit(e, unsafe))) map {
            case es =>
              val empty = mkApplyFqn("Set.empty", List(WeededAst.Expression.Unit(mkSL(sp1, sp2))), sp1, sp2)
              es.foldLeft(empty) {
                case (acc, elm) => mkApplyFqn("Set.insert", List(elm, acc), sp1, sp2)
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
              val empty = mkApplyFqn("Map.empty", List(WeededAst.Expression.Unit(mkSL(sp1, sp2))), sp1, sp2)
              es.foldLeft(empty) {
                case (acc, (k, v)) => mkApplyFqn("Map.insert", List(k, v, acc), sp1, sp2)
              }
          }

        case ParsedAst.Expression.Ref(sp1, exp, sp2) =>
          for {
            e <- visit(exp, unsafe)
          } yield WeededAst.Expression.Ref(e, mkSL(sp1, sp2))

        case ParsedAst.Expression.Deref(sp1, exp, sp2) =>
          for {
            e <- visit(exp, unsafe)
          } yield WeededAst.Expression.Deref(e, mkSL(sp1, sp2))

        case ParsedAst.Expression.Assign(exp1, exp2, sp2) =>
          val sp1 = leftMostSourcePosition(exp1)
          for {
            e1 <- visit(exp1, unsafe)
            e2 <- visit(exp2, unsafe)
          } yield WeededAst.Expression.Assign(e1, e2, mkSL(sp1, sp2))

        case ParsedAst.Expression.HandleWith(sp1, exp, handlers, sp2) =>
          for {
            e <- visit(exp, unsafe)
            bs <- visitHandlers(handlers)
          } yield WeededAst.Expression.HandleWith(e, bs, mkSL(sp1, sp2))

        case ParsedAst.Expression.Existential(sp1, fparams, exp, sp2) =>
          /*
           * Checks for `IllegalExistential`.
           */
          if (fparams.isEmpty)
            return IllegalExistential(mkSL(sp1, sp2)).toFailure

          for {
            e <- visit(exp, unsafe)
            fs <- Formals.weed(fparams, typeRequired = true)
          } yield {
            /*
             * Rewrites the multi-parameter existential to nested single-parameter existentials.
             */
            fs.foldRight(e) {
              case (param, eacc) => WeededAst.Expression.Existential(param, eacc, mkSL(sp1, sp2))
            }
          }

        case ParsedAst.Expression.Universal(sp1, fparams, exp, sp2) =>
          /*
           * Checks for `IllegalUniversal`.
           */
          if (fparams.isEmpty)
            return IllegalUniversal(mkSL(sp1, sp2)).toFailure

          for {
            e <- visit(exp, unsafe)
            fs <- Formals.weed(fparams, typeRequired = true)
          } yield {
            /*
             * Rewrites the multi-parameter universal to nested single-parameter universals.
             */
            fs.foldRight(e) {
              case (param, eacc) => WeededAst.Expression.Universal(param, eacc, mkSL(sp1, sp2))
            }
          }

        case ParsedAst.Expression.Ascribe(exp, tpe, effOpt, sp2) =>
          for {
            e <- visit(exp, unsafe)
            eff <- Effects.weed(effOpt)
          } yield {
            WeededAst.Expression.Ascribe(e, Types.weed(tpe), eff, mkSL(leftMostSourcePosition(exp), sp2))
          }

        case ParsedAst.Expression.Cast(exp, tpe, effOpt, sp2) =>
          for {
            e <- visit(exp, unsafe)
            eff <- Effects.weed(effOpt)
          } yield {
            WeededAst.Expression.Cast(e, Types.weed(tpe), eff, mkSL(leftMostSourcePosition(exp), sp2))
          }

        case ParsedAst.Expression.Unsafe(sp1, exp, sp2) =>
          visit(exp, unsafe = true)

        case ParsedAst.Expression.TryCatch(sp1, exp, rules, sp2) =>
          /*
           * Check for `IllegalUnsafeExpression`.
           */
          if (!unsafe) {
            return WeederError.IllegalUnsafeExpression(mkSL(sp1, sp2)).toFailure
          }

          val expVal = visit(exp, unsafe)
          val rulesVal = rules map {
            case ParsedAst.CatchRule(ident, fqn, body) =>
              visit(body, unsafe) map {
                case b => WeededAst.CatchRule(ident, fqn.mkString("."), b)
              }
          }

          @@(expVal, seqM(rulesVal)) map {
            case (e, rs) => WeededAst.Expression.TryCatch(e, rs, mkSL(sp1, sp2))
          }


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
    private def getVectorLength(elm: ParsedAst.Literal, sp1: SourcePosition, sp2: SourcePosition) : Validation[Int, WeederError] = {
      elm match {
        case ParsedAst.Literal.Int32(sp1, sign, digits, sp2) => toInt32(sign, digits, mkSL(sp1, sp2)) flatMap {
          case l if l >= 0 => l.toSuccess
          case _ => WeederError.IllegalVectorLength(mkSL(sp1, sp2)).toFailure
        }
        case _ => throw InternalCompilerException(s"Expected literal.int32. Actual: ${elm}.")
      }
    }

    private def checkIndexSequence(elms: Seq[ParsedAst.Literal], sp1: SourcePosition, sp2: SourcePosition) : Seq[Validation[Int, WeederError]] = {
      elms map(e => getVectorLength(e, sp1, sp2))
      //(elms.map(e => visit(e, unsafe))) map{
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

        case ParsedAst.Predicate.Head.Atom(sp1, qname, terms, sp2) =>
          @@(terms.map(t => Expressions.weed(t))) map {
            case ts => WeededAst.Predicate.Head.Atom(qname, ts, mkSL(sp1, sp2))
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
            case ts => WeededAst.Predicate.Body.Atom(qname, Polarity.Positive, ts, mkSL(sp1, sp2))
          }

        case ParsedAst.Predicate.Body.Negative(sp1, qname, terms, sp2) =>
          val loc = mkSL(sp1, sp2)
          @@(terms.map(t => Patterns.weed(t))) map {
            case ts => WeededAst.Predicate.Body.Atom(qname, Polarity.Negative, ts, loc)
          }

        case ParsedAst.Predicate.Body.Filter(sp1, qname, terms, sp2) =>
          @@(terms.map(t => Expressions.weed(t))) map {
            case ts => WeededAst.Predicate.Body.Filter(qname, ts, mkSL(sp1, sp2))
          }

        case ParsedAst.Predicate.Body.NotEqual(sp1, ident1, ident2, sp2) =>
          val qname = Name.mkQName("neq", sp1, sp2)
          val t1 = WeededAst.Expression.VarOrDef(Name.mkQName(ident1), mkSL(ident1.sp1, ident1.sp2))
          val t2 = WeededAst.Expression.VarOrDef(Name.mkQName(ident2), mkSL(ident2.sp1, ident2.sp2))
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
      @@(result).map(as => Ast.Annotations(as))
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
        case "law" => Ast.Annotation.Law(loc).toSuccess
        case "test" => Ast.Annotation.Test(loc).toSuccess
        case "unchecked" => Ast.Annotation.Unchecked(loc).toSuccess
        case "unsafe" => Ast.Annotation.Unsafe(loc).toSuccess
        case name => WeederError.UndefinedAnnotation(name, loc).toFailure
      }
    }
  }

  /**
    * Weeds the given sequence of parsed modifiers `xs`.
    */
  def visitModifiers(xs: Seq[ParsedAst.Modifier], legalModifiers: Set[Ast.Modifier]): Validation[Ast.Modifiers, WeederError] = {
    val seen = mutable.Map.empty[String, ParsedAst.Modifier]
    val modifiersVal = xs map {
      modifier =>
        seen.get(modifier.name) match {
          case None =>
            seen += (modifier.name -> modifier)
            visitModifier(modifier, legalModifiers)
          case Some(other) =>
            val loc1 = mkSL(other.sp1, other.sp2)
            val loc2 = mkSL(modifier.sp1, modifier.sp2)
            WeederError.DuplicateModifier(modifier.name, loc1, loc2).toFailure
        }
    }

    // Sequence the results.
    for {
      ms <- seqM(modifiersVal)
    } yield {
      Ast.Modifiers(ms)
    }
  }

  /**
    * Weeds the given parsed modifier `m`.
    */
  def visitModifier(m: ParsedAst.Modifier, legalModifiers: Set[Ast.Modifier]): Validation[Ast.Modifier, WeederError] = {
    val modifier = m.name match {
      case "inline" => Ast.Modifier.Inline
      case "pub" => Ast.Modifier.Public
      case s => throw InternalCompilerException(s"Unknown modifier '$s' near ${mkSL(m.sp1, m.sp2).format}.")
    }

    //
    // Check for `IllegalModifier`.
    //
    if (legalModifiers contains modifier)
      modifier.toSuccess
    else
      IllegalModifier(mkSL(m.sp1, m.sp2)).toFailure
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

        case ParsedAst.Declaration.Def(_, meta, _, _, defn, _, _, _, _, _, _) =>
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
                  val lam = WeededAst.Expression.VarOrDef(law, loc)
                  val fun = WeededAst.Expression.VarOrDef(Name.QName(sp1, Name.RootNS, defn, sp2), loc)
                  val exp = mkApplyCurried(lam, fun :: as, loc)
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
      case ParsedAst.Type.Ambiguous(sp1, qname, sp2) => WeededAst.Type.Ambiguous(qname, mkSL(sp1, sp2))
      case ParsedAst.Type.Tuple(sp1, elms, sp2) => WeededAst.Type.Tuple(elms.toList.map(weed), mkSL(sp1, sp2))
      case ParsedAst.Type.Nat(sp1, len, sp2) => WeededAst.Type.Nat(CheckNaturalNumber(len, sp1, sp2), mkSL(sp1, sp2))
      case ParsedAst.Type.Native(sp1, fqn, sp2) => WeededAst.Type.Native(fqn.toList, mkSL(sp1, sp2))
      case ParsedAst.Type.Arrow(sp1, tparams, tresult, sp2) =>
        // Construct a curried arrow type.
        tparams.foldRight(weed(tresult)) {
          case (tparam, tacc) => WeededAst.Type.Arrow(List(weed(tparam)), tacc, mkSL(sp1, sp2))
        }

      case ParsedAst.Type.Infix(tpe1, base0, tpe2, sp2) =>
        /*
         * Rewrites infix type applications to regular type applications.
         */
        val loc = mkSL(leftMostSourcePosition(tpe1), sp2)
        // Construct the type: base[tpe1][tpe2]
        WeededAst.Type.Apply(WeededAst.Type.Apply(weed(base0), weed(tpe1), loc), weed(tpe2), loc)

      case ParsedAst.Type.Apply(t1, args, sp2) =>
        // Curry the type arguments.
        val sp1 = leftMostSourcePosition(t1)
        args.foldLeft(weed(t1)) {
          case (acc, t2) => WeededAst.Type.Apply(acc, weed(t2), mkSL(sp1, sp2))
        }
    }
  }

  object Effects {

    /**
      * Weeds the given parsed optional effect `effOpt`.
      */
    def weed(effOpt: Option[ParsedAst.Effect]): Validation[Eff, WeederError] = effOpt match {
      case None => Eff.Pure.toSuccess
      case Some(ParsedAst.Effect(xs)) =>
        /*
         * Check for the Any and Pure effects.
         */
        if (xs.length == 1) {
          if (xs.head.name == "Any") {
            return Eff.Box(EffectSet.Top).toSuccess
          }
          if (xs.head.name == "Pure") {
            return Eff.Box(EffectSet.Pure).toSuccess
          }
        }

        /*
         * Process each effect.
         */
        val effectsVal = traverse(xs) {
          case ident => ident.name match {
            case "IO" => Effect.IO.toSuccess
            case "File" => Effect.File.toSuccess
            case "Network" => Effect.Network.toSuccess
            case name => IllegalEffect(ident.loc).toFailure
          }
        }

        for {
          eff <- effectsVal
        } yield Eff.Box(EffectSet.MayMust(eff.toSet, eff.toSet))
    }

  }

  object Formals {

    /**
      * Weeds the given list of formal parameter `fparams`.
      *
      * Checks for [[IllegalFormalParameter]] and [[DuplicateFormalParam]].
      */
    def weed(fparams: Seq[ParsedAst.FormalParam], typeRequired: Boolean): Validation[List[WeededAst.FormalParam], WeederError] = {
      //
      // Special Case: Check if no formal parameters are present. If so, introduce a unit parameter.
      //
      if (fparams.isEmpty) {
        val sp1 = SourcePosition.Unknown
        val sp2 = SourcePosition.Unknown
        val loc = mkSL(sp1, sp2)
        val ident = Name.Ident(sp1, "_unit", sp2)
        val tpe = Some(WeededAst.Type.Unit(loc))
        return List(WeededAst.FormalParam(ident, Ast.Modifiers.Empty, tpe, loc)).toSuccess
      }

      val seen = mutable.Map.empty[String, ParsedAst.FormalParam]
      val results = fparams map {
        case param@ParsedAst.FormalParam(sp1, mods, ident, typeOpt, sp2) => seen.get(ident.name) match {
          case None =>
            seen += (ident.name -> param)
            visitModifiers(mods, legalModifiers = Set(Ast.Modifier.Inline)) flatMap {
              case mod =>
                if (typeRequired && typeOpt.isEmpty)
                  IllegalFormalParameter(ident.name, mkSL(sp1, sp2)).toFailure
                else
                  WeededAst.FormalParam(ident, mod, typeOpt.map(Types.weed), mkSL(sp1, sp2)).toSuccess
            }
          case Some(otherParam) =>
            val loc1 = mkSL(otherParam.sp1, otherParam.sp2)
            val loc2 = mkSL(param.sp1, param.sp2)
            DuplicateFormalParam(ident.name, loc1, loc2).toFailure
        }
      }

      // Sequence the results.
      seqM(results)
    }

  }

  /**
    * Weeds the given documentation.
    */
  private def visitDoc(doc0: ParsedAst.Doc): Ast.Doc = Ast.Doc(doc0.lines.toList, mkSL(doc0.sp1, doc0.sp2))

  /**
    * Weeds the given class constraint `cc`.
    */
  private def visitClassConstraint(cc: ParsedAst.ClassConstraint): Validation[(WeededAst.SimpleClass, List[WeededAst.SimpleClass]), WeederError] = cc match {
    case ParsedAst.ClassConstraint(head0, body0) =>
      val headVal = visitSimpleClass(head0)
      val bodyVal = @@(body0 map visitSimpleClass)
      @@(headVal, bodyVal)
  }

  /**
    * Weeds the given impl constraint `ic`.
    */
  private def visitImplConstraint(ic: ParsedAst.ImplConstraint): Validation[(WeededAst.ComplexClass, List[WeededAst.ComplexClass]), WeederError] = ic match {
    case ParsedAst.ImplConstraint(head0, body0) =>
      val headVal = visitComplexClass(head0)
      val bodyVal = @@(body0 map visitComplexClass)
      @@(headVal, bodyVal)
  }

  /**
    * Weeds the given integrity constraint `ic`.
    */
  private def visitDisallowConstraint(ic: ParsedAst.DisallowConstraint): Validation[List[WeededAst.ComplexClass], WeederError] = ic match {
    case ParsedAst.DisallowConstraint(body0) => @@(body0 map visitComplexClass)
  }

  /**
    * Weeds the given simple class atom `a`.
    */
  private def visitSimpleClass(a: ParsedAst.SimpleClass): Validation[WeededAst.SimpleClass, WeederError] = a match {
    case ParsedAst.SimpleClass(sp1, qname, targs, sp2) =>
      val loc = mkSL(sp1, sp2)
      WeededAst.SimpleClass(qname, targs.toList, loc).toSuccess
  }

  /**
    * Weeds the given complex class atom `a`.
    */
  private def visitComplexClass(a: ParsedAst.ComplexClass): Validation[WeededAst.ComplexClass, WeederError] = a match {
    case ParsedAst.ComplexClass.Positive(sp1, qname, targs, sp2) =>
      WeededAst.ComplexClass(qname, Polarity.Positive, (targs map Types.weed).toList, mkSL(sp1, sp2)).toSuccess

    case ParsedAst.ComplexClass.Negative(sp1, qname, targs, sp2) =>
      WeededAst.ComplexClass(qname, Polarity.Negative, (targs map Types.weed).toList, mkSL(sp1, sp2)).toSuccess
  }

  /**
    * Weeds the given signature `sig`.
    */
  private def visitSig(sig: ParsedAst.Declaration.Sig): Validation[WeededAst.Declaration.Sig, WeederError] = sig match {
    case ParsedAst.Declaration.Sig(doc0, ann0, mod0, sp1, ident, tparams0, fparams0, tpe, effOpt, sp2) =>
      val loc = mkSL(ident.sp1, ident.sp2)
      val doc = visitDoc(doc0)
      val annVal = Annotations.weed(ann0)
      // TODO: legalAnnotations
      val modVal = visitModifiers(mod0, legalModifiers = Set.empty)
      val tparams = tparams0.toList.map(_.ident)
      val effVal = Effects.weed(effOpt)

      /*
        * Check for `DuplicateFormal`.
        */
      val formalsVal = Formals.weed(fparams0, typeRequired = true)
      mapN(annVal, modVal, formalsVal, effVal) {
        case (ann, mod, fs, eff) =>
          val t = WeededAst.Type.Arrow(fs map (_.tpe.get), Types.weed(tpe), loc)
          WeededAst.Declaration.Sig(doc, ann, mod, ident, tparams, fs, t, eff, loc)
      }
  }

  /**
    * Weeds the given effect handler bindings `bs0`.
    */
  def visitHandlers(bs0: Seq[ParsedAst.HandlerBinding])(implicit flix: Flix): Validation[List[WeededAst.HandlerBinding], WeederError] = {
    seqM(bs0 map visitHandler)
  }

  /**
    * Weeds the given effect handler binding `b0`.
    */
  def visitHandler(b0: ParsedAst.HandlerBinding)(implicit flix: Flix): Validation[WeededAst.HandlerBinding, WeederError] = b0 match {
    case ParsedAst.HandlerBinding(qname, exp) =>
      for {
        e <- Expressions.weed(exp)
      } yield WeededAst.HandlerBinding(qname, e)
  }

  /**
    * Returns the arrow type constructed from the given formal parameters `fparams0` and return type `tpe0`.
    */
  private def mkArrowType(fparams0: List[WeededAst.FormalParam], tpe0: WeededAst.Type, loc: SourceLocation): WeededAst.Type = {
    // Construct a curried arrow type.
    fparams0.foldRight(tpe0) {
      case (fparam, tacc) => WeededAst.Type.Arrow(List(fparam.tpe.get), tacc, loc)
    }
  }

  /**
    * Returns a curried apply expression for the given `base` and `args` argument expressions.
    */
  private def mkApplyCurried(base: WeededAst.Expression, args: List[WeededAst.Expression], loc: SourceLocation): WeededAst.Expression = {
    args.foldLeft(base) {
      case (eacc, arg) => WeededAst.Expression.Apply(eacc, arg, loc)
    }
  }

  /**
    * Returns an apply expression for the given fully-qualified name `fqn` and the given arguments `args`.
    */
  private def mkApplyFqn(fqn: String, args: List[WeededAst.Expression], sp1: SourcePosition, sp2: SourcePosition): WeededAst.Expression = {
    val lambda = WeededAst.Expression.VarOrDef(Name.mkQName(fqn, sp1, sp2), mkSL(sp1, sp2))
    mkApplyCurried(lambda, args, mkSL(sp1, sp2))
  }

  /**
    * Returns a curried version of the given expression `e` for each formal parameter in `fparams0`.
    */
  private def mkCurried(fparams0: List[WeededAst.FormalParam], e: WeededAst.Expression, loc: SourceLocation): WeededAst.Expression = {
    fparams0.foldRight(e) {
      case (fparam, eacc) => WeededAst.Expression.Lambda(fparam, eacc, loc)
    }
  }

  /**
    * Returns the list of expressions `args0` unless the list is empty.
    *
    * If so, returns a list with a single unit expression.
    */
  private def getApplyArgsCheckIfEmpty(args0: List[WeededAst.Expression], sp1: SourcePosition, sp2: SourcePosition): List[WeededAst.Expression] = args0 match {
    case Nil => List(WeededAst.Expression.Unit(mkSL(sp1, sp2)))
    case as => as
  }

  /**
    * Attempts to parse the given float32 with `sign` digits `before` and `after` the comma.
    */
  private def toFloat32(sign: Boolean, before: String, after: String, loc: SourceLocation): Validation[Float, WeederError] = try {
    val s = if (sign) s"-$before.$after" else s"$before.$after"
    s.toFloat.toSuccess
  } catch {
    case e: NumberFormatException => IllegalFloat(loc).toFailure
  }

  /**
    * Attempts to parse the given float64 with `sign` digits `before` and `after` the comma.
    */
  private def toFloat64(sign: Boolean, before: String, after: String, loc: SourceLocation): Validation[Double, WeederError] = try {
    val s = if (sign) s"-$before.$after" else s"$before.$after"
    s.toDouble.toSuccess
  } catch {
    case e: NumberFormatException => IllegalFloat(loc).toFailure
  }

  /**
    * Attempts to parse the given int8 with `sign` and `digits`.
    */
  private def toInt8(sign: Boolean, digits: String, loc: SourceLocation): Validation[Byte, WeederError] = try {
    val s = if (sign) "-" + digits else digits
    s.toByte.toSuccess
  } catch {
    case ex: NumberFormatException => IllegalInt(loc).toFailure
  }

  /**
    * Attempts to parse the given int16 with `sign` and `digits`.
    */
  private def toInt16(sign: Boolean, digits: String, loc: SourceLocation): Validation[Short, WeederError] = try {
    val s = if (sign) "-" + digits else digits
    s.toShort.toSuccess
  } catch {
    case ex: NumberFormatException => IllegalInt(loc).toFailure
  }

  /**
    * Attempts to parse the given int32 with `sign` and `digits`.
    */
  private def toInt32(sign: Boolean, digits: String, loc: SourceLocation): Validation[Int, WeederError] = try {
    val s = if (sign) "-" + digits else digits
    s.toInt.toSuccess
  } catch {
    case ex: NumberFormatException => IllegalInt(loc).toFailure
  }

  /**
    * Attempts to parse the given int64 with `sign` and `digits`.
    */
  private def toInt64(sign: Boolean, digits: String, loc: SourceLocation): Validation[Long, WeederError] = try {
    val s = if (sign) "-" + digits else digits
    s.toLong.toSuccess
  } catch {
    case ex: NumberFormatException => IllegalInt(loc).toFailure
  }

  /**
    * Attempts to parse the given BigInt with `sign` and `digits`.
    */
  private def toBigInt(sign: Boolean, digits: String, loc: SourceLocation): Validation[BigInteger, WeederError] = try {
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
    case ParsedAst.Expression.Hole(sp1, _, _) => sp1
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
    case ParsedAst.Expression.ArrayLit(sp1, _, _) => sp1
    case ParsedAst.Expression.ArrayNew(sp1, _, _, _) => sp1
    case ParsedAst.Expression.ArrayLoad(base, _, _) => leftMostSourcePosition(base)
    case ParsedAst.Expression.ArrayStore(base, _, _, _) => leftMostSourcePosition(base)
    case ParsedAst.Expression.ArrayLength(sp1, _, _) => sp1
    case ParsedAst.Expression.ArraySlice(base, _, _, _) => leftMostSourcePosition(base)
    case ParsedAst.Expression.VectorLit(sp1, _,_) => sp1
    case ParsedAst.Expression.VectorNew(sp1,_,_,_) => sp1
    case ParsedAst.Expression.VectorLoad(base,_,_) => leftMostSourcePosition(base)
    case ParsedAst.Expression.VectorStore(base,_,_,_) => leftMostSourcePosition(base)
    case ParsedAst.Expression.VectorLength(sp1,_,_) => sp1
    case ParsedAst.Expression.VectorSlice(base,_,_,_) => leftMostSourcePosition(base)
    case ParsedAst.Expression.FNil(sp1, _) => sp1
    case ParsedAst.Expression.FCons(hd, _, _, _) => leftMostSourcePosition(hd)
    case ParsedAst.Expression.FAppend(fst, _, _, _) => leftMostSourcePosition(fst)
    case ParsedAst.Expression.FSet(sp1, _, _) => sp1
    case ParsedAst.Expression.FMap(sp1, _, _) => sp1
    case ParsedAst.Expression.Ref(sp1, _, _) => sp1
    case ParsedAst.Expression.Deref(sp1, _, _) => sp1
    case ParsedAst.Expression.Assign(e1, _, _) => leftMostSourcePosition(e1)
    case ParsedAst.Expression.HandleWith(sp1, _, _, _) => sp1
    case ParsedAst.Expression.Existential(sp1, _, _, _) => sp1
    case ParsedAst.Expression.Universal(sp1, _, _, _) => sp1
    case ParsedAst.Expression.Ascribe(e1, _, _, _) => leftMostSourcePosition(e1)
    case ParsedAst.Expression.Cast(e1, _, _, _) => leftMostSourcePosition(e1)
    case ParsedAst.Expression.Unsafe(sp1, _, _) => sp1
    case ParsedAst.Expression.TryCatch(sp1, _, _, _) => sp1
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
    case ParsedAst.Type.Ambiguous(sp1, _, _) => sp1
    case ParsedAst.Type.Tuple(sp1, _, _) => sp1
    case ParsedAst.Type.Nat(sp1, _, _) => sp1
    case ParsedAst.Type.Native(sp1, _, _) => sp1
    case ParsedAst.Type.Arrow(sp1, _, _, _) => sp1
    case ParsedAst.Type.Infix(tpe1, _, _, _) => leftMostSourcePosition(tpe1)
    case ParsedAst.Type.Apply(tpe1, _, _) => leftMostSourcePosition(tpe1)
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
    * Helper method for Succ type.
    * Checks to make sure Literal.Int32 is >= 0, and converts it to int.
    * Throws InternalCompilerException if check fails.
    * TODO make type handling for vertification.
    */
  private def CheckNaturalNumber(elm: ParsedAst.Literal.Int32, sp1: SourcePosition, sp2: SourcePosition) : Int = {
  toInt32(elm.sign, elm.lit, mkSL(sp1, sp2)) match {
       case Validation.Success(l) if l >= 0 => l
       // TODO Make Types.weed handle validation.
       case _ => throw InternalCompilerException("Vector length must be an integer of minimum 0.")
     }
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
