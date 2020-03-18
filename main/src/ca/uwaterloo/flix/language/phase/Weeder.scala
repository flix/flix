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
import java.lang.{Byte => JByte, Integer => JInt, Long => JLong, Short => JShort}

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Ast.Denotation
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.errors.WeederError
import ca.uwaterloo.flix.language.errors.WeederError._
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{CompilationMode, InternalCompilerException, Validation}

import scala.annotation.tailrec
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
    val roots = parTraverse(program.roots)(visitRoot)
    val named = traverse(program.named) {
      case (sym, exp) => visitExp(exp).map(e => sym -> e)
    }
    val constraints = visitAllConstraints(program.roots)

    mapN(roots, named, constraints) {
      case (rs, ne, cs) =>
        // Check if there are top-level constraints and we should introduce a main.
        if (cs.isEmpty)
          WeededAst.Program(rs, ne.toMap, flix.getReachableRoots)
        else
          WeededAst.Program(mkMain(cs) :: rs, ne.toMap, flix.getReachableRoots)
    }
  }

  /**
    * Weeds the given abstract syntax tree.
    */
  private def visitRoot(root: ParsedAst.Root)(implicit flix: Flix): Validation[WeededAst.Root, WeederError] = {
    val usesVal = traverse(root.uses)(visitUse)
    val declarationsVal = traverse(root.decls)(visitDecl)
    val propertiesVal = visitAllProperties(root)
    val loc = mkSL(root.sp1, root.sp2)

    mapN(usesVal, declarationsVal, propertiesVal) {
      case (uses, decls1, decls2) =>
        WeededAst.Root(uses.flatten, decls1.flatten ++ decls2, loc)
    }
  }

  /**
    * Compiles the given parsed declaration `past` to a list of weeded declarations.
    */
  private def visitDecl(decl: ParsedAst.Declaration)(implicit flix: Flix): Validation[List[WeededAst.Declaration], WeederError] = decl match {
    case ParsedAst.Declaration.Namespace(sp1, name, decls, sp2) =>
      traverse(decls)(visitDecl) map {
        case ds => List(WeededAst.Declaration.Namespace(name, ds.flatten, mkSL(sp1, sp2)))
      }

    case d: ParsedAst.Declaration.Def => visitDef(d)

    case d: ParsedAst.Declaration.Law => visitLaw(d)

    case d: ParsedAst.Declaration.Enum => visitEnum(d)

    case d: ParsedAst.Declaration.OpaqueType => visitOpaqueType(d)

    case d: ParsedAst.Declaration.TypeAlias => visitTypeAlias(d)

    case d: ParsedAst.Declaration.Relation => visitRelation(d)

    case d: ParsedAst.Declaration.Lattice => visitLattice(d)

    case d: ParsedAst.Declaration.Constraint => Nil.toSuccess

    case d: ParsedAst.Declaration.LatticeComponents => visitLatticeComponents(d)

    case d: ParsedAst.Declaration.Class => Nil.toSuccess

    case d: ParsedAst.Declaration.Impl => Nil.toSuccess

    case d: ParsedAst.Declaration.Disallow => Nil.toSuccess

    case ParsedAst.Declaration.Sig(doc0, ann, mods, sp1, ident, tparams0, fparams0, tpe, effOpt, sp2) =>
      throw InternalCompilerException(s"Unexpected declaration")
  }

  /**
    * Performs weeding on the given def declaration `d0`.
    */
  private def visitDef(d0: ParsedAst.Declaration.Def)(implicit flix: Flix): Validation[List[WeededAst.Declaration.Def], WeederError] = d0 match {
    case ParsedAst.Declaration.Def(doc0, ann, mods, sp1, ident, tparams0, fparams0, tpe, effOpt, exp0, sp2) =>
      val loc = mkSL(ident.sp1, ident.sp2)
      val doc = visitDoc(doc0)
      val annVal = visitAnnotationOrProperty(ann)
      val modVal = visitModifiers(mods, legalModifiers = Set(Ast.Modifier.Inline, Ast.Modifier.Public))
      val expVal = visitExp(exp0)
      val tparams = visitTypeParams(tparams0)
      val formalsVal = visitFormalParams(fparams0, typeRequired = true)
      val effVal = visitEff(effOpt)

      mapN(annVal, modVal, formalsVal, expVal, effVal) {
        case (as, mod, fs, exp, eff) =>
          val ts = fs.map(_.tpe.get)
          val e = mkCurried(fs.tail, exp, loc)
          val t = mkCurriedArrow(ts, eff, visitType(tpe), loc)
          List(WeededAst.Declaration.Def(doc, as, mod, ident, tparams, fs.head :: Nil, e, t, eff, loc))
      }
  }

  /**
    * Performs weeding on the given law declaration `d0`.
    */
  private def visitLaw(d0: ParsedAst.Declaration.Law)(implicit flix: Flix): Validation[List[WeededAst.Declaration.Def], WeederError] = d0 match {
    case ParsedAst.Declaration.Law(doc0, sp1, ident, tparams0, fparams0, tpe, exp0, sp2) =>
      val loc = mkSL(ident.sp1, ident.sp2)
      val doc = visitDoc(doc0)
      val expVal = visitExp(exp0)
      val tparams = visitTypeParams(tparams0)
      val formalsVal = visitFormalParams(fparams0, typeRequired = true)

      mapN(formalsVal, expVal) {
        case (fs, exp) =>
          val e = mkCurried(fs.tail, exp, loc)
          val ts = fs.map(_.tpe.get)
          val t = mkCurriedArrow(ts, WeededAst.Type.Pure(loc), visitType(tpe), loc)
          val ann = Ast.Annotations(List(Ast.Annotation.Law(loc)))
          val mod = Ast.Modifiers(Ast.Modifier.Public :: Nil)
          List(WeededAst.Declaration.Def(doc, ann, mod, ident, tparams, fs.head :: Nil, e, t, WeededAst.Type.Pure(loc), loc))
      }
  }

  /**
    * Performs weeding on the given enum declaration `d0`.
    */
  private def visitEnum(d0: ParsedAst.Declaration.Enum)(implicit flix: Flix): Validation[List[WeededAst.Declaration.Enum], WeederError] = d0 match {
    case ParsedAst.Declaration.Enum(doc0, mods, sp1, ident, tparams0, cases, sp2) =>
      val doc = visitDoc(doc0)
      val modVal = visitModifiers(mods, legalModifiers = Set(Ast.Modifier.Public))
      val tparams = visitTypeParams(tparams0)

      modVal flatMap {
        case mod =>
          /*
           * Check for `DuplicateTag`.
           */
          Validation.fold[ParsedAst.Case, Map[String, WeededAst.Case], WeederError](cases, Map.empty) {
            case (macc, caze: ParsedAst.Case) =>
              val tagName = caze.ident.name
              macc.get(tagName) match {
                case None => (macc + (tagName -> WeededAst.Case(ident, caze.ident, visitType(caze.tpe)))).toSuccess
                case Some(otherTag) =>
                  val loc1 = otherTag.tag.loc
                  val loc2 = mkSL(caze.ident.sp1, caze.ident.sp2)
                  DuplicateTag(ident.name, tagName, loc1, loc2).toFailure
              }
          } map {
            case m => List(WeededAst.Declaration.Enum(doc, mod, ident, tparams, m, mkSL(sp1, sp2)))
          }
      }
  }

  /**
    * Performs weeding on the given opaque type declaration `d0`.
    */
  private def visitOpaqueType(d0: ParsedAst.Declaration.OpaqueType)(implicit flix: Flix): Validation[List[WeededAst.Declaration.Enum], WeederError] = d0 match {
    case ParsedAst.Declaration.OpaqueType(doc0, mod0, sp1, ident, tparams0, tpe0, sp2) =>
      /*
       * Rewrites an opaque type to an enum declaration.
       */
      val doc = visitDoc(doc0)
      val modVal = visitModifiers(mod0, legalModifiers = Set(Ast.Modifier.Public))

      modVal map {
        case mod =>
          val tparams = visitTypeParams(tparams0)
          val cases = Map(ident.name -> WeededAst.Case(ident, ident, visitType(tpe0)))
          List(WeededAst.Declaration.Enum(doc, mod, ident, tparams, cases, mkSL(sp1, sp2)))
      }
  }

  /**
    * Performs weeding on the given type alias declaration `d0`.
    */
  private def visitTypeAlias(d0: ParsedAst.Declaration.TypeAlias)(implicit flix: Flix): Validation[List[WeededAst.Declaration.TypeAlias], WeederError] = d0 match {
    case ParsedAst.Declaration.TypeAlias(doc0, mod0, sp1, ident, tparams0, tpe0, sp2) =>
      val doc = visitDoc(doc0)
      val modVal = visitModifiers(mod0, legalModifiers = Set(Ast.Modifier.Public))

      modVal map {
        case mod =>
          val tparams = visitTypeParams(tparams0)
          val tpe = visitType(tpe0)
          List(WeededAst.Declaration.TypeAlias(doc, mod, ident, tparams, tpe, mkSL(sp1, sp2)))
      }
  }

  /**
    * Performs weeding on the given relation `r0`.
    */
  private def visitRelation(r0: ParsedAst.Declaration.Relation)(implicit flix: Flix): Validation[List[WeededAst.Declaration.Relation], WeederError] = r0 match {
    case ParsedAst.Declaration.Relation(doc0, mod0, sp1, ident, tparams0, attrs, sp2) =>
      val doc = visitDoc(doc0)
      val modVal = visitModifiers(mod0, legalModifiers = Set(Ast.Modifier.Public))
      val tparams = visitTypeParams(tparams0)

      /*
       * Check for `DuplicateAttribute`.
       */
      mapN(modVal, checkDuplicateAttribute(attrs)) {
        case (mod, as) =>
          List(WeededAst.Declaration.Relation(doc, mod, ident, tparams, as, mkSL(sp1, sp2)))
      }
  }

  /**
    * Performs weeding on the given lattice `r0`.
    */
  private def visitLattice(l0: ParsedAst.Declaration.Lattice)(implicit flix: Flix): Validation[List[WeededAst.Declaration.Lattice], WeederError] = l0 match {
    case ParsedAst.Declaration.Lattice(doc0, mod0, sp1, ident, tparams0, attr, sp2) =>
      val doc = visitDoc(doc0)
      val modVal = visitModifiers(mod0, legalModifiers = Set(Ast.Modifier.Public))
      val tparams = visitTypeParams(tparams0)

      /*
       * Check for `DuplicateAttribute`.
       */
      mapN(modVal, checkDuplicateAttribute(attr)) {
        case (mod, as) =>
          // Split the attributes into keys and element.
          List(WeededAst.Declaration.Lattice(doc, mod, ident, tparams, as, mkSL(sp1, sp2)))
      }
  }

  /**
    * Performs weeding on the given constraint `c0`.
    */
  private def visitConstraint(c0: ParsedAst.Declaration.Constraint)(implicit flix: Flix): Validation[WeededAst.Constraint, WeederError] = c0 match {
    case ParsedAst.Declaration.Constraint(sp1, head0, body0, sp2) =>
      val headVal = visitHeadPredicate(head0)
      val bodyVal = traverse(body0)(visitPredicateBody)

      mapN(headVal, bodyVal) {
        case (h, bs) => WeededAst.Constraint(h, bs, mkSL(sp1, sp2))
      }
  }

  /**
    * Performs weeding on the given lattice components `lc0`.
    */
  private def visitLatticeComponents(lc0: ParsedAst.Declaration.LatticeComponents)(implicit flix: Flix): Validation[List[WeededAst.Declaration.LatticeComponents], WeederError] = lc0 match {
    case ParsedAst.Declaration.LatticeComponents(sp1, tpe, elms, sp2) =>
      val elmsVal = traverse(elms)(e => visitExp(e))
      elmsVal flatMap {
        case List(bot, top, equ, leq, lub, glb) => List(WeededAst.Declaration.LatticeComponents(visitType(tpe), bot, top, equ, leq, lub, glb, mkSL(sp1, sp2))).toSuccess
        case _ => IllegalLattice(mkSL(sp1, sp2)).toFailure
      }
  }

  /**
    * Performs weeding on the given use `u0`.
    */
  private def visitUse(u0: ParsedAst.Use): Validation[List[WeededAst.Use], WeederError] = u0 match {
    case ParsedAst.Use.UseOne(sp1, nname, ident, sp2) =>
      if (ident.isLower)
        List(WeededAst.Use.UseDef(Name.QName(sp1, nname, ident, sp2), ident, mkSL(sp1, sp2))).toSuccess
      else
        List(WeededAst.Use.UseTyp(Name.QName(sp1, nname, ident, sp2), ident, mkSL(sp1, sp2))).toSuccess
    case ParsedAst.Use.UseMany(_, nname, names, _) =>
      val us = names.foldRight(Nil: List[WeededAst.Use]) {
        case (ParsedAst.Use.NameAndAlias(sp1, ident, aliasOpt, sp2), acc) =>
          val alias = aliasOpt.getOrElse(ident)
          if (ident.isLower)
            WeededAst.Use.UseDef(Name.QName(sp1, nname, ident, sp2), alias, mkSL(sp1, sp2)) :: acc
          else
            WeededAst.Use.UseTyp(Name.QName(sp1, nname, ident, sp2), alias, mkSL(sp1, sp2)) :: acc
      }
      us.toSuccess

    case ParsedAst.Use.UseOneTag(sp1, qname, tag, sp2) =>
      List(WeededAst.Use.UseTag(qname, tag, tag, mkSL(sp1, sp2))).toSuccess

    case ParsedAst.Use.UseManyTag(sp1, qname, tags, sp2) =>
      val us = tags.foldRight(Nil: List[WeededAst.Use]) {
        case (ParsedAst.Use.NameAndAlias(sp1, ident, aliasOpt, sp2), acc) =>
          val alias = aliasOpt.getOrElse(ident)
          WeededAst.Use.UseTag(qname, ident, alias, mkSL(sp1, sp2)) :: acc
      }
      us.toSuccess
  }

  /**
    * Weeds the given expression.
    */
  private def visitExp(exp0: ParsedAst.Expression)(implicit flix: Flix): Validation[WeededAst.Expression, WeederError] = exp0 match {
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

    case ParsedAst.Expression.Use(sp1, use, exp, sp2) =>
      mapN(visitUse(use), visitExp(exp)) {
        case (us, e) => WeededAst.Expression.Use(us, e, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.Lit(sp1, lit, sp2) => lit2exp(lit)

    case ParsedAst.Expression.Apply(lambda, args, sp2) =>
      val sp1 = leftMostSourcePosition(lambda)
      val loc = mkSL(sp1, sp2)
      mapN(visitExp(lambda), traverse(args)(e => visitExp(e))) {
        case (e, as) =>
          val es = getApplyArgsCheckIfEmpty(as, sp1, sp2)
          mkApplyCurried(e, es, loc)
      }

    case ParsedAst.Expression.Infix(exp1, name, exp2, sp2) =>
      /*
       * Rewrites infix expressions to apply expressions.
       */
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) =>
          val loc = mkSL(leftMostSourcePosition(exp1), sp2)
          val lambda = WeededAst.Expression.VarOrDef(name, loc)
          mkApplyCurried(lambda, List(e1, e2), loc)
      }

    case ParsedAst.Expression.Postfix(exp, ident, exps, sp2) =>
      /*
       * Rewrites postfix expressions to apply expressions.
       */
      mapN(visitExp(exp), traverse(exps)(e => visitExp(e))) {
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
        fs <- visitFormalParams(fparams0, typeRequired = false)
        e <- visitExp(exp)
      } yield mkCurried(fs, e, loc)

    case ParsedAst.Expression.LambdaMatch(sp1, pat, exp, sp2) =>
      /*
       * Rewrites lambda pattern match expressions into a lambda expression with a nested pattern match.
       */
      mapN(visitPattern(pat), visitExp(exp)) {
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
      visitExp(exp) map {
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
      mapN(visitExp(exp1), visitExp(exp2)) {
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
          case "<=>" => WeededAst.Expression.Binary(BinaryOperator.Spaceship, e1, e2, loc)
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
      mapN(visitExp(exp1), visitExp(exp2), visitExp(exp3)) {
        case (e1, e2, e3) => WeededAst.Expression.IfThenElse(e1, e2, e3, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.Statement(exp1, exp2, sp2) =>
      val sp1 = leftMostSourcePosition(exp1)
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => WeededAst.Expression.Stm(e1, e2, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.LetMatch(sp1, pat, tpe, exp1, exp2, sp2) =>
      //
      // Rewrites a let-match to a regular let-binding or a full-blown pattern match.
      //
      mapN(visitPattern(pat), visitExp(exp1), visitExp(exp2)) {
        case (WeededAst.Pattern.Var(ident, loc), value, body) =>
          // No pattern match.
          WeededAst.Expression.Let(ident, withAscription(value, tpe), body, mkSL(sp1, sp2))
        case (pat, value, body) =>
          // Full-blown pattern match.
          val rule = WeededAst.MatchRule(pat, WeededAst.Expression.True(mkSL(sp1, sp2)), body)
          WeededAst.Expression.Match(withAscription(value, tpe), List(rule), mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.LetMatchStar(sp1, pat, tpe, exp1, exp2, sp2) =>
      val loc = SourceLocation.mk(sp1, sp2)

      //
      // Rewrites a monadic let-match to a regular let-binding or a full-blown pattern match inside a flatMap.
      //
      // let* x = exp1; exp2     ==>   flatMap(x -> exp2)(exp1)
      //
      val qname = Name.mkQName(Name.Ident(SourcePosition.Unknown, "flatMap", SourcePosition.Unknown))
      val flatMap = WeededAst.Expression.VarOrDef(qname, loc)

      mapN(visitPattern(pat), visitExp(exp1), visitExp(exp2)) {
        case (WeededAst.Pattern.Var(ident, loc), value, body) =>
          // No pattern match.
          val fparam = WeededAst.FormalParam(ident, Ast.Modifiers.Empty, tpe.map(visitType), loc)
          val lambda = WeededAst.Expression.Lambda(fparam, body, loc)
          val inner = WeededAst.Expression.Apply(flatMap, lambda, loc)
          WeededAst.Expression.Apply(inner, value, loc)
        case (pat, value, body) =>
          // Full-blown pattern match.
          val lambdaIdent = Name.Ident(SourcePosition.Unknown, "pat$0", SourcePosition.Unknown)
          val lambdaVar = WeededAst.Expression.VarOrDef(Name.mkQName(lambdaIdent), loc)

          val rule = WeededAst.MatchRule(pat, WeededAst.Expression.True(mkSL(sp1, sp2)), body)
          val lambdaBody = WeededAst.Expression.Match(withAscription(lambdaVar, tpe), List(rule), mkSL(sp1, sp2))

          val fparam = WeededAst.FormalParam(lambdaIdent, Ast.Modifiers.Empty, tpe.map(visitType), loc)
          val lambda = WeededAst.Expression.Lambda(fparam, lambdaBody, loc)
          val inner = WeededAst.Expression.Apply(flatMap, lambda, loc)
          WeededAst.Expression.Apply(inner, value, loc)
      }

    case ParsedAst.Expression.LetRec(sp1, ident, exp1, exp2, sp2) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (value, body) => WeededAst.Expression.LetRec(ident, value, body, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.LetImport(sp1, impl, exp2, sp2) =>
      val loc = mkSL(sp1, sp2)

      //
      // Visit the inner expression exp2.
      //
      impl match {
        case ParsedAst.JvmOp.Constructor(fqn, sig, ident) =>
          //
          // Introduce a let-bound lambda: (args...) -> InvokeConstructor(args).
          //
          mapN(visitExp(exp2)) {
            case e2 =>
              // Compute the class name.
              val className = fqn.mkString(".")

              //
              // Case 1: No arguments.
              //
              if (sig.isEmpty) {
                val fparam = WeededAst.FormalParam(Name.Ident(sp1, "_", sp2), Ast.Modifiers.Empty, Some(WeededAst.Type.Unit(loc)), loc)
                val lambdaBody = WeededAst.Expression.InvokeConstructor(className, Nil, Nil, loc)
                val e1 = WeededAst.Expression.Lambda(fparam, lambdaBody, loc)
                return WeededAst.Expression.Let(ident, e1, e2, loc).toSuccess
              }

              // Compute the types of declared parameters.
              val ts = sig.map(visitType).toList

              // Introduce a formal parameter (of appropriate type) for each declared argument.
              val fs = ts.zipWithIndex.map {
                case (tpe, index) =>
                  val id = Name.Ident(sp1, "a" + index, sp2)
                  WeededAst.FormalParam(id, Ast.Modifiers.Empty, Some(tpe), loc)
              }

              // Compute the argument to the method call.
              val as = ts.zipWithIndex.map {
                case (tpe, index) =>
                  val ident = Name.Ident(sp1, "a" + index, sp2)
                  WeededAst.Expression.VarOrDef(Name.mkQName(ident), loc)
              }

              // Assemble the lambda expression.
              val lambdaBody = WeededAst.Expression.InvokeConstructor(className, as, ts, loc)
              val e1 = mkCurried(fs, lambdaBody, loc)
              WeededAst.Expression.Let(ident, e1, e2, loc)
          }

        case ParsedAst.JvmOp.Method(fqn, sig, identOpt) =>
          //
          // Introduce a let-bound lambda: (obj, args...) -> InvokeMethod(obj, args).
          //
          mapN(parseClassAndMember(fqn, loc), visitExp(exp2)) {
            case ((className, methodName), e2) =>
              // Compute the name of the let-bound variable.
              val ident = identOpt.getOrElse(Name.Ident(sp1, methodName, sp2))

              val receiverType = WeededAst.Type.Native(className, loc)

              // Compute the types of declared parameters.
              val ts = sig.map(visitType).toList

              // Introduce a formal parameter for the object argument.
              val objId = Name.Ident(sp1, "obj$", sp2)
              val objParam = WeededAst.FormalParam(objId, Ast.Modifiers.Empty, Some(receiverType), loc)
              val objExp = WeededAst.Expression.VarOrDef(Name.mkQName(objId), loc)

              // Introduce a formal parameter (of appropriate type) for each declared argument.
              val fs = objParam :: ts.zipWithIndex.map {
                case (tpe, index) =>
                  val ident = Name.Ident(sp1, "a" + index + "$", sp2)
                  WeededAst.FormalParam(ident, Ast.Modifiers.Empty, Some(tpe), loc)
              }

              // Compute the argument to the method call.
              val as = objExp :: ts.zipWithIndex.map {
                case (tpe, index) =>
                  val ident = Name.Ident(sp1, "a" + index + "$", sp2)
                  WeededAst.Expression.VarOrDef(Name.mkQName(ident), loc)
              }

              // Assemble the lambda expression.
              val lambdaBody = WeededAst.Expression.InvokeMethod(className, methodName, as.head, as.tail, ts, loc)
              val e1 = mkCurried(fs, lambdaBody, loc)
              WeededAst.Expression.Let(ident, e1, e2, loc)
          }

        case ParsedAst.JvmOp.StaticMethod(fqn, sig, identOpt) =>
          //
          // Introduce a let-bound lambda: (args...) -> InvokeStaticMethod(args).
          //
          mapN(parseClassAndMember(fqn, loc), visitExp(exp2)) {
            case ((className, methodName), e2) =>

              // Compute the name of the let-bound variable.
              val ident = identOpt.getOrElse(Name.Ident(sp1, methodName, sp2))

              //
              // Case 1: No arguments.
              //
              if (sig.isEmpty) {
                val fparam = WeededAst.FormalParam(Name.Ident(sp1, "_", sp2), Ast.Modifiers.Empty, Some(WeededAst.Type.Unit(loc)), loc)
                val lambdaBody = WeededAst.Expression.InvokeStaticMethod(className, methodName, Nil, Nil, loc)
                val e1 = WeededAst.Expression.Lambda(fparam, lambdaBody, loc)
                return WeededAst.Expression.Let(ident, e1, e2, loc).toSuccess
              }

              // Compute the types of declared parameters.
              val ts = sig.map(visitType).toList

              // Introduce a formal parameter (of appropriate type) for each declared argument.
              val fs = ts.zipWithIndex.map {
                case (tpe, index) =>
                  val id = Name.Ident(sp1, "a" + index + "$", sp2)
                  WeededAst.FormalParam(id, Ast.Modifiers.Empty, Some(tpe), loc)
              }

              // Compute the argument to the method call.
              val as = ts.zipWithIndex.map {
                case (tpe, index) =>
                  val ident = Name.Ident(sp1, "a" + index + "$", sp2)
                  WeededAst.Expression.VarOrDef(Name.mkQName(ident), loc)
              }

              // Assemble the lambda expression.
              val lambdaBody = WeededAst.Expression.InvokeStaticMethod(className, methodName, as, ts, loc)
              val e1 = mkCurried(fs, lambdaBody, loc)
              WeededAst.Expression.Let(ident, e1, e2, loc)
          }

        case ParsedAst.JvmOp.GetField(fqn, ident) =>
          //
          // Introduce a let-bound lambda: o -> GetField(o).
          //
          mapN(parseClassAndMember(fqn, loc), visitExp(exp2)) {
            case ((className, fieldName), e2) =>
              val objectId = Name.Ident(sp1, "o$", sp2)
              val objectExp = WeededAst.Expression.VarOrDef(Name.mkQName(objectId), loc)
              val objectParam = WeededAst.FormalParam(objectId, Ast.Modifiers.Empty, None, loc)
              val lambdaBody = WeededAst.Expression.GetField(className, fieldName, objectExp, loc)
              val e1 = WeededAst.Expression.Lambda(objectParam, lambdaBody, loc)
              WeededAst.Expression.Let(ident, e1, e2, loc)
          }

        case ParsedAst.JvmOp.PutField(fqn, ident) =>
          //
          // Introduce a let-bound lambda: (o, v) -> PutField(o, v)
          //
          mapN(parseClassAndMember(fqn, loc), visitExp(exp2)) {
            case ((className, fieldName), e2) =>
              val objectId = Name.Ident(sp1, "o$", sp2)
              val valueId = Name.Ident(sp1, "v$", sp2)
              val objectExp = WeededAst.Expression.VarOrDef(Name.mkQName(objectId), loc)
              val valueExp = WeededAst.Expression.VarOrDef(Name.mkQName(valueId), loc)
              val objectParam = WeededAst.FormalParam(objectId, Ast.Modifiers.Empty, None, loc)
              val valueParam = WeededAst.FormalParam(valueId, Ast.Modifiers.Empty, None, loc)
              val lambdaBody = WeededAst.Expression.PutField(className, fieldName, objectExp, valueExp, loc)
              val e1 = mkCurried(objectParam :: valueParam :: Nil, lambdaBody, loc)
              WeededAst.Expression.Let(ident, e1, e2, loc)
          }

        case ParsedAst.JvmOp.GetStaticField(fqn, ident) =>
          //
          // Introduce a let-bound lambda: _: Unit -> GetStaticField.
          //
          mapN(parseClassAndMember(fqn, loc), visitExp(exp2)) {
            case ((className, fieldName), e2) =>
              val unitId = Name.Ident(sp1, "_", sp2)
              val unitParam = WeededAst.FormalParam(unitId, Ast.Modifiers.Empty, Some(WeededAst.Type.Unit(loc)), loc)
              val lambdaBody = WeededAst.Expression.GetStaticField(className, fieldName, loc)
              val e1 = WeededAst.Expression.Lambda(unitParam, lambdaBody, loc)
              WeededAst.Expression.Let(ident, e1, e2, loc)
          }

        case ParsedAst.JvmOp.PutStaticField(fqn, ident) =>
          //
          // Introduce a let-bound lambda: x -> PutStaticField(x).
          //
          mapN(parseClassAndMember(fqn, loc), visitExp(exp2)) {
            case ((className, fieldName), e2) =>
              val valueId = Name.Ident(sp1, "v$", sp2)
              val valueExp = WeededAst.Expression.VarOrDef(Name.mkQName(valueId), loc)
              val valueParam = WeededAst.FormalParam(valueId, Ast.Modifiers.Empty, None, loc)
              val lambdaBody = WeededAst.Expression.PutStaticField(className, fieldName, valueExp, loc)
              val e1 = WeededAst.Expression.Lambda(valueParam, lambdaBody, loc)
              WeededAst.Expression.Let(ident, e1, e2, loc)
          }
      }

    case ParsedAst.Expression.Match(sp1, exp, rules, sp2) =>
      val rulesVal = traverse(rules) {
        case ParsedAst.MatchRule(pat, None, body) =>
          mapN(visitPattern(pat), visitExp(body)) {
            // Pattern match without guard.
            case (p, e) => WeededAst.MatchRule(p, WeededAst.Expression.True(mkSL(sp1, sp2)), e)
          }
        case ParsedAst.MatchRule(pat, Some(guard), body) => mapN(visitPattern(pat), visitExp(guard), visitExp(body)) {
          // Pattern match with guard.
          case (p, g, b) => WeededAst.MatchRule(p, g, b)
        }
      }
      mapN(visitExp(exp), rulesVal) {
        case (e, rs) => WeededAst.Expression.Match(e, rs, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.Tag(sp1, qname, expOpt, sp2) =>
      val (enum, tag) = asTag(qname)

      expOpt match {
        case None =>
          // Case 1: The tag does not have an expression. Nothing more to be done.
          WeededAst.Expression.Tag(enum, tag, None, mkSL(sp1, sp2)).toSuccess
        case Some(exp) =>
          // Case 2: The tag has an expression. Perform weeding on it.
          visitExp(exp) map {
            case e => WeededAst.Expression.Tag(enum, tag, Some(e), mkSL(sp1, sp2))
          }
      }

    case ParsedAst.Expression.Tuple(sp1, elms, sp2) =>
      /*
       * Rewrites empty tuples to Unit and eliminate single-element tuples.
       */
      traverse(elms)(e => visitExp(e)) map {
        case Nil =>
          val loc = mkSL(sp1, sp2)
          WeededAst.Expression.Unit(loc)
        case x :: Nil => x
        case xs => WeededAst.Expression.Tuple(xs, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.RecordLit(sp1, fields, sp2) =>
      val fieldsVal = traverse(fields) {
        case ParsedAst.RecordField(fsp1, label, exp, fsp2) =>
          mapN(visitExp(exp)) {
            case e => label -> e
          }
      }

      mapN(fieldsVal) {
        case fs =>
          // Rewrite into a sequence of nested record extensions.
          val zero = WeededAst.Expression.RecordEmpty(mkSL(sp1, sp2))
          fs.foldRight(zero: WeededAst.Expression) {
            case ((l, e), acc) => WeededAst.Expression.RecordExtend(l, e, acc, mkSL(sp1, sp2))
          }
      }

    case ParsedAst.Expression.RecordSelect(exp, label, sp2) =>
      val sp1 = leftMostSourcePosition(exp)
      mapN(visitExp(exp)) {
        case e =>
          // Special Case: Array Length
          if (label.name == "length")
            WeededAst.Expression.ArrayLength(e, mkSL(sp1, sp2))
          else
            WeededAst.Expression.RecordSelect(e, label, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.RecordSelectLambda(sp1, label, sp2) =>
      val loc = mkSL(sp1, sp2)
      val ident = Name.Ident(sp1, "_rec", sp2)
      val qname = Name.QName(sp1, Name.RootNS, ident, sp2)
      val fparam = WeededAst.FormalParam(ident, Ast.Modifiers.Empty, None, loc)
      val varExp = WeededAst.Expression.VarOrDef(qname, loc)
      val lambdaBody = WeededAst.Expression.RecordSelect(varExp, label, loc)
      WeededAst.Expression.Lambda(fparam, lambdaBody, loc).toSuccess

    case ParsedAst.Expression.RecordOperation(_, ops, rest, _) =>
      // We translate the sequence of record operations into a nested tree using a fold right.
      foldRight(ops)(visitExp(rest)) {
        case (ParsedAst.RecordOp.Extend(sp1, label, exp, sp2), acc) =>
          mapN(visitExp(exp)) {
            case e => WeededAst.Expression.RecordExtend(label, e, acc, mkSL(sp1, sp2))
          }

        case (ParsedAst.RecordOp.Restrict(sp1, label, sp2), acc) =>
          WeededAst.Expression.RecordRestrict(label, acc, mkSL(sp1, sp2)).toSuccess

        case (ParsedAst.RecordOp.Update(sp1, label, exp, sp2), acc) =>
          mapN(visitExp(exp)) {
            case e =>
              // An update is a restrict followed by an extension.
              val inner = WeededAst.Expression.RecordRestrict(label, acc, mkSL(sp1, sp2))
              WeededAst.Expression.RecordExtend(label, e, inner, mkSL(sp1, sp2))
          }
      }

    case ParsedAst.Expression.ArrayLit(sp1, elms, sp2) =>
      traverse(elms)(e => visitExp(e)) map {
        case es => WeededAst.Expression.ArrayLit(es, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.ArrayNew(sp1, elm, len, sp2) =>
      mapN(visitExp(elm), visitExp(len)) {
        case (e, ln) => WeededAst.Expression.ArrayNew(e, ln, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.ArrayLoad(base, index, sp2) =>
      val sp1 = leftMostSourcePosition(base)
      val loc = mkSL(sp1, sp2)

      mapN(visitExp(base), visitExp(index)) {
        case (b, i) => WeededAst.Expression.ArrayLoad(b, i, loc)
      }

    case ParsedAst.Expression.ArrayStore(base, indexes, elm, sp2) =>
      val sp1 = leftMostSourcePosition(base)
      val loc = mkSL(sp1, sp2)

      mapN(visitExp(base), traverse(indexes)(e => visitExp(e)), visitExp(elm)) {
        case (b, es, e) =>
          val inner = es.init.foldLeft(b) {
            case (acc, e) => WeededAst.Expression.ArrayLoad(acc, e, loc)
          }
          WeededAst.Expression.ArrayStore(inner, es.last, e, loc)
      }

    case ParsedAst.Expression.ArraySlice(base, optStartIndex, optEndIndex, sp2) =>
      val sp1 = leftMostSourcePosition(base)
      val loc = mkSL(sp1, sp2)

      (optStartIndex, optEndIndex) match {
        case (None, None) =>
          visitExp(base) map {
            case b => WeededAst.Expression.ArraySlice(b, WeededAst.Expression.Int32(0, loc), WeededAst.Expression.ArrayLength(b, loc), loc)
          }
        case (Some(startIndex), None) =>
          mapN(visitExp(base), visitExp(startIndex)) {
            case (b, i1) => WeededAst.Expression.ArraySlice(b, i1, WeededAst.Expression.ArrayLength(b, loc), loc)
          }
        case (None, Some(endIndex)) =>
          mapN(visitExp(base), visitExp(endIndex)) {
            case (b, i2) => WeededAst.Expression.ArraySlice(b, WeededAst.Expression.Int32(0, loc), i2, loc)
          }
        case (Some(startIndex), Some(endIndex)) =>
          mapN(visitExp(base), visitExp(startIndex), visitExp(endIndex)) {
            case (b, i1, i2) => WeededAst.Expression.ArraySlice(b, i1, i2, loc)
          }
      }

    case ParsedAst.Expression.VectorLit(sp1, elms, sp2) =>
      traverse(elms)(e => visitExp(e)) map {
        case es => WeededAst.Expression.VectorLit(es, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.VectorNew(sp1, elm, len, sp2) =>
      mapN(visitExp(elm), getVectorLength(len, sp1, sp2)) {
        case (e, l) => WeededAst.Expression.VectorNew(e, l, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.VectorLoad(base, index, sp2) =>
      val sp1 = leftMostSourcePosition(base)
      val loc = mkSL(sp1, sp2)
      mapN(visitExp(base), getVectorLength(index, sp1, sp2)) {
        case (b, l) => WeededAst.Expression.VectorLoad(b, l, loc)
      }

    case ParsedAst.Expression.VectorStore(base, indexes, elm, sp2) =>
      val sp1 = leftMostSourcePosition(base)
      val loc = mkSL(sp1, sp2)
      val indexesVal = checkIndexSequence(indexes, sp1, sp2)
      mapN(visitExp(base), indexesVal, visitExp(elm)) {
        case (b, is, e) =>
          val inner = is.init.foldLeft(b) {
            case (acc, e) => WeededAst.Expression.VectorLoad(acc, e, loc)
          }
          WeededAst.Expression.VectorStore(inner, is.last, e, loc)
      }

    case ParsedAst.Expression.VectorLength(sp1, base, sp2) =>
      visitExp(base) map {
        case b => WeededAst.Expression.VectorLength(b, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.VectorSlice(base, optStartIndex, optEndIndex, sp2) =>
      val sp1 = leftMostSourcePosition(base)
      val loc = mkSL(sp1, sp2)
      (optStartIndex, optEndIndex) match {
        case (None, None) =>
          visitExp(base) flatMap {
            case b => WeededAst.Expression.VectorSlice(b, 0, None, loc).toSuccess
          }
        case (None, Some(i)) =>
          mapN(visitExp(base), getVectorLength(i, sp1, sp2)) {
            case (b, l) => WeededAst.Expression.VectorSlice(b, 0, Some(l), loc)
          }
        case (Some(i), None) =>
          mapN(visitExp(base), getVectorLength(i, sp1, sp2)) {
            case (b, l) => WeededAst.Expression.VectorSlice(b, l, None, loc)
          }
        case (Some(i1), Some(i2)) =>
          flatMapN(visitExp(base), getVectorLength(i1, sp1, sp2), getVectorLength(i2, sp1, sp2)) {
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
      mapN(visitExp(hd), visitExp(tl)) {
        case (e1, e2) =>
          val tag = Name.Ident(sp1, "Cons", sp2)
          val exp = WeededAst.Expression.Tuple(List(e1, e2), mkSL(sp1, sp2))
          WeededAst.Expression.Tag(None, tag, Some(exp), mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.FAppend(fst, sp1, sp2, snd) =>
      /*
       * Rewrites a `FAppend` expression into a call to `List/append`.
       */
      mapN(visitExp(fst), visitExp(snd)) {
        case (e1, e2) =>
          // NB: We painstakingly construct the qualified name
          // to ensure that source locations are available.
          mkApplyFqn("List.append", List(e1, e2), sp1, sp2)
      }

    case ParsedAst.Expression.FSet(sp1, elms, sp2) =>
      /*
       * Rewrites a `FSet` expression into `Set/empty` and a `Set/insert` calls.
       */
      traverse(elms)(e => visitExp(e)) map {
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
      val elmsVal = traverse(elms) {
        case (key, value) => mapN(visitExp(key), visitExp(value)) {
          case (k, v) => (k, v)
        }
      }

      elmsVal map {
        case es =>
          val empty = mkApplyFqn("Map.empty", List(WeededAst.Expression.Unit(mkSL(sp1, sp2))), sp1, sp2)
          es.foldLeft(empty) {
            case (acc, (k, v)) => mkApplyFqn("Map.insert", List(k, v, acc), sp1, sp2)
          }
      }

    case ParsedAst.Expression.Interpolation(sp1, parts, sp2) =>
      val loc = mkSL(sp1, sp2)

      parts match {
        case Seq() =>
          // Case 1: Return the empty string if there are no interpolation parts.
          WeededAst.Expression.Str("", loc).toSuccess

        case Seq(ParsedAst.InterpolationPart.ExpPart(e)) =>
          // Case 2: Return the expression if there is only a single expression.
          visitExp(e)

        case Seq(ParsedAst.InterpolationPart.StrPart(s)) =>
          // Case 3: Return the string if there is only a single string.
          WeededAst.Expression.Str(s, loc).toSuccess

        case ps =>
          // Case 4: Construct a sequence of string append expressions.
          val init = WeededAst.Expression.Str("", loc)
          Validation.fold(ps, init: WeededAst.Expression) {
            case (acc, ParsedAst.InterpolationPart.ExpPart(e)) =>
              mapN(visitExp(e)) {
                case e2 =>
                  val op = BinaryOperator.Plus
                  WeededAst.Expression.Binary(op, acc, e2, loc)
              }

            case (acc, ParsedAst.InterpolationPart.StrPart(s)) =>
              val op = BinaryOperator.Plus
              val e1 = acc
              val e2 = WeededAst.Expression.Str(s, loc)
              WeededAst.Expression.Binary(op, e1, e2, loc).toSuccess
          }

      }

    case ParsedAst.Expression.Ref(sp1, exp, sp2) =>
      for {
        e <- visitExp(exp)
      } yield WeededAst.Expression.Ref(e, mkSL(sp1, sp2))

    case ParsedAst.Expression.Deref(sp1, exp, sp2) =>
      for {
        e <- visitExp(exp)
      } yield WeededAst.Expression.Deref(e, mkSL(sp1, sp2))

    case ParsedAst.Expression.Assign(exp1, exp2, sp2) =>
      val sp1 = leftMostSourcePosition(exp1)
      for {
        e1 <- visitExp(exp1)
        e2 <- visitExp(exp2)
      } yield WeededAst.Expression.Assign(e1, e2, mkSL(sp1, sp2))

    case ParsedAst.Expression.Existential(sp1, tparams, fparams, exp, sp2) =>
      /*
       * Checks for `IllegalExistential`.
       */
      if (fparams.isEmpty)
        return IllegalExistential(mkSL(sp1, sp2)).toFailure

      for {
        e <- visitExp(exp)
        fs <- visitFormalParams(fparams, typeRequired = true)
      } yield {
        /*
         * Rewrites the multi-parameter existential to nested single-parameter existentials.
         */
        fs.foldRight(e) {
          case (param, eacc) => WeededAst.Expression.Existential(/* TODO: Pass type params. */ WeededAst.TypeParams.Elided, param, eacc, mkSL(sp1, sp2))
        }
      }

    case ParsedAst.Expression.Universal(sp1, tparams, fparams, exp, sp2) =>
      /*
       * Checks for `IllegalUniversal`.
       */
      if (fparams.isEmpty)
        return IllegalUniversal(mkSL(sp1, sp2)).toFailure

      for {
        e <- visitExp(exp)
        fs <- visitFormalParams(fparams, typeRequired = true)
      } yield {
        /*
         * Rewrites the multi-parameter universal to nested single-parameter universals.
         */
        fs.foldRight(e) {
          case (param, eacc) => WeededAst.Expression.Universal(/* TODO: Pass type params. */ WeededAst.TypeParams.Elided, param, eacc, mkSL(sp1, sp2))
        }
      }

    case ParsedAst.Expression.Ascribe(exp, expectedType, expectedEff, sp2) =>
      val t = expectedType.map(visitType)
      val f = expectedEff.map(visitType)
      mapN(visitExp(exp)) {
        case e => WeededAst.Expression.Ascribe(e, t, f, mkSL(leftMostSourcePosition(exp), sp2))
      }

    case ParsedAst.Expression.Cast(exp, declaredType, declaredEff, sp2) =>
      val t = declaredType.map(visitType)
      val f = declaredEff.map(visitType)
      mapN(visitExp(exp)) {
        case e => WeededAst.Expression.Cast(e, t, f, mkSL(leftMostSourcePosition(exp), sp2))
      }

    case ParsedAst.Expression.TryCatch(sp1, exp, rules, sp2) =>
      val expVal = visitExp(exp)
      val rulesVal = traverse(rules) {
        case ParsedAst.CatchRule(ident, fqn, body) =>
          visitExp(body) map {
            case b => WeededAst.CatchRule(ident, fqn.mkString("."), b)
          }
      }

      mapN(expVal, rulesVal) {
        case (e, rs) => WeededAst.Expression.TryCatch(e, rs, mkSL(sp1, sp2))
      }

    // TODO SJ: Rewrite to Ascribe(newch, Channel[Int]), to remove the tpe (and get tvar like everything else)
    // TODO SJ: Also do not allow function types (Arrow) when rewriting
    case ParsedAst.Expression.NewChannel(sp1, tpe, exp, sp2) =>
      visitExp(exp) map {
        case e => WeededAst.Expression.NewChannel(e, visitType(tpe), mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.GetChannel(sp1, exp, sp2) =>
      visitExp(exp) map {
        case e => WeededAst.Expression.GetChannel(e, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.PutChannel(exp1, exp2, sp2) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => WeededAst.Expression.PutChannel(e1, e2, mkSL(leftMostSourcePosition(exp1), sp2))
      }

    case ParsedAst.Expression.SelectChannel(sp1, rules, default, sp2) =>
      val rulesVal = traverse(rules) {
        case ParsedAst.SelectChannelRule(ident, chan, body) => mapN(visitExp(chan), visitExp(body)) {
          case (c, b) => WeededAst.SelectChannelRule(ident, c, b)
        }
      }

      val defaultVal = default match {
        case Some(exp) => visitExp(exp) map {
          case e => Some(e)
        }
        case None => None.toSuccess
      }

      mapN(rulesVal, defaultVal) {
        case (rs, d) => WeededAst.Expression.SelectChannel(rs, d, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.ProcessSpawn(sp1, exp, sp2) =>
      visitExp(exp) map {
        case e => WeededAst.Expression.ProcessSpawn(e, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.ProcessPanic(sp1, msg, sp2) =>
      WeededAst.Expression.ProcessPanic(msg.lit, mkSL(sp1, sp2)).toSuccess

    case ParsedAst.Expression.FixpointConstraint(sp1, con, sp2) =>
      val loc = mkSL(sp1, sp2)

      mapN(visitConstraint(con)) {
        case c => WeededAst.Expression.FixpointConstraintSet(c :: Nil, loc)
      }

    case ParsedAst.Expression.FixpointConstraintSet(sp1, cs0, sp2) =>
      val loc = mkSL(sp1, sp2)

      traverse(cs0)(visitConstraint) map {
        case cs => WeededAst.Expression.FixpointConstraintSet(cs, loc)
      }

    case ParsedAst.Expression.FixpointCompose(exp1, exp2, sp2) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) =>
          val sp1 = leftMostSourcePosition(exp1)
          WeededAst.Expression.FixpointCompose(e1, e2, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.FixpointSolve(sp1, exp, sp2) =>
      visitExp(exp) map {
        case e => WeededAst.Expression.FixpointSolve(e, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.FixpointProject(sp1, qname, exp, sp2) =>
      val loc = mkSL(sp1, sp2)

      mapN(visitExp(exp)) {
        case e =>
          WeededAst.Expression.FixpointProject(qname, e, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.FixpointEntails(exp1, exp2, sp2) =>
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) =>
          val sp1 = leftMostSourcePosition(exp1)
          WeededAst.Expression.FixpointEntails(e1, e2, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.FixpointFold(sp1, qname, init, f, constraints, sp2) =>
      val loc = mkSL(sp1, sp2)

      mapN(visitExp(init), visitExp(f), visitExp(constraints)) {
        case (e1, e2, e3) =>
          WeededAst.Expression.FixpointFold(qname, e1, e2, e3, loc)
      }
  }


  /**
    * Translates the given literal to an expression.
    */
  private def lit2exp(lit0: ParsedAst.Literal)(implicit flix: Flix): Validation[WeededAst.Expression, WeederError] = lit0 match {
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

    case ParsedAst.Literal.Int8(sp1, sign, radix, digits, sp2) =>
      toInt8(sign, radix, digits, mkSL(sp1, sp2)) map {
        case lit => WeededAst.Expression.Int8(lit, mkSL(sp1, sp2))
      }

    case ParsedAst.Literal.Int16(sp1, sign, radix, digits, sp2) =>
      toInt16(sign, radix, digits, mkSL(sp1, sp2)) map {
        case lit => WeededAst.Expression.Int16(lit, mkSL(sp1, sp2))
      }

    case ParsedAst.Literal.Int32(sp1, sign, radix, digits, sp2) =>
      toInt32(sign, radix, digits, mkSL(sp1, sp2)) map {
        case lit => WeededAst.Expression.Int32(lit, mkSL(sp1, sp2))
      }

    case ParsedAst.Literal.Int64(sp1, sign, radix, digits, sp2) =>
      toInt64(sign, radix, digits, mkSL(sp1, sp2)) map {
        case lit => WeededAst.Expression.Int64(lit, mkSL(sp1, sp2))
      }

    case ParsedAst.Literal.BigInt(sp1, sign, radix, digits, sp2) =>
      toBigInt(sign, radix, digits, mkSL(sp1, sp2)) map {
        case lit => WeededAst.Expression.BigInt(lit, mkSL(sp1, sp2))
      }

    case ParsedAst.Literal.Str(sp1, lit, sp2) =>
      WeededAst.Expression.Str(lit, mkSL(sp1, sp2)).toSuccess
  }

  // TODO: Comment
  private def getVectorLength(elm: ParsedAst.Literal, sp1: SourcePosition, sp2: SourcePosition): Validation[Int, WeederError] = elm match {
    case ParsedAst.Literal.Int32(_, sign, radix, digits, _) => toInt32(sign, radix, digits, mkSL(sp1, sp2)) flatMap {
      case l if l >= 0 => l.toSuccess
      case _ => WeederError.IllegalVectorLength(mkSL(sp1, sp2)).toFailure
    }
    case _ => throw InternalCompilerException(s"Expected literal.int32. Actual: $elm.")
  }

  // TODO: Comment
  private def checkIndexSequence(elms: Seq[ParsedAst.Literal], sp1: SourcePosition, sp2: SourcePosition): Validation[List[Int], WeederError] = {
    traverse(elms)(e => getVectorLength(e, sp1, sp2))
  }

  /**
    * Weeds the given pattern.
    */
  private def visitLitPat(pat0: ParsedAst.Literal): Validation[WeededAst.Pattern, WeederError] = pat0 match {
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
    case ParsedAst.Literal.Int8(sp1, sign, radix, digits, sp2) =>
      toInt8(sign, radix, digits, mkSL(sp1, sp2)) map {
        case lit => WeededAst.Pattern.Int8(lit, mkSL(sp1, sp2))
      }
    case ParsedAst.Literal.Int16(sp1, sign, radix, digits, sp2) =>
      toInt16(sign, radix, digits, mkSL(sp1, sp2)) map {
        case lit => WeededAst.Pattern.Int16(lit, mkSL(sp1, sp2))
      }
    case ParsedAst.Literal.Int32(sp1, sign, radix, digits, sp2) =>
      toInt32(sign, radix, digits, mkSL(sp1, sp2)) map {
        case lit => WeededAst.Pattern.Int32(lit, mkSL(sp1, sp2))
      }
    case ParsedAst.Literal.Int64(sp1, sign, radix, digits, sp2) =>
      toInt64(sign, radix, digits, mkSL(sp1, sp2)) map {
        case lit => WeededAst.Pattern.Int64(lit, mkSL(sp1, sp2))
      }
    case ParsedAst.Literal.BigInt(sp1, sign, radix, digits, sp2) =>
      toBigInt(sign, radix, digits, mkSL(sp1, sp2)) map {
        case lit => WeededAst.Pattern.BigInt(lit, mkSL(sp1, sp2))
      }
    case ParsedAst.Literal.Str(sp1, lit, sp2) =>
      WeededAst.Pattern.Str(lit, mkSL(sp1, sp2)).toSuccess
  }

  /**
    * Compiles a parsed pattern into a weeded pattern.
    */
  private def visitPattern(pattern: ParsedAst.Pattern): Validation[WeededAst.Pattern, WeederError] = {
    /*
     *  Check for non-linear pattern, i.e. if a variable occurs multiple times.
     */
    val seen = mutable.Map.empty[String, Name.Ident]

    /*
     * Local visitor.
     */
    def visit(pattern: ParsedAst.Pattern): Validation[WeededAst.Pattern, WeederError] = pattern match {
      case ParsedAst.Pattern.Var(sp1, ident, sp2) =>
        // Check if the identifier is a wildcard.
        if (ident.name == "_") {
          WeededAst.Pattern.Wild(mkSL(sp1, sp2)).toSuccess
        } else {
          seen.get(ident.name) match {
            case None =>
              seen += (ident.name -> ident)
              WeededAst.Pattern.Var(ident, mkSL(sp1, sp2)).toSuccess
            case Some(otherIdent) =>
              NonLinearPattern(ident.name, otherIdent.loc, mkSL(sp1, sp2)).toFailure
          }
        }

      case ParsedAst.Pattern.Lit(sp1, lit, sp2) => visitLitPat(lit)

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
        traverse(pats)(visit) map {
          case Nil => WeededAst.Pattern.Unit(mkSL(sp1, sp2))
          case x :: Nil => x
          case xs => WeededAst.Pattern.Tuple(xs, mkSL(sp1, sp2))
        }

      case ParsedAst.Pattern.Array(sp1, pats, sp2) =>
        traverse(pats)(visit) map {
          case xs => WeededAst.Pattern.Array(xs, mkSL(sp1, sp2))
        }

      case ParsedAst.Pattern.ArrayTailSpread(sp1, pats, ident, sp2) =>
        val array = traverse(pats)(visit) map {
          case xs => WeededAst.Pattern.Array(xs, mkSL(sp1, sp2))
        }
        if (ident.name == "_") {
          WeededAst.Pattern.ArrayTailSpread(array.get.elms, None, mkSL(sp2, sp2)).toSuccess
        } else {
          seen.get(ident.name) match {
            case None =>
              seen += (ident.name -> ident)
              WeededAst.Pattern.ArrayTailSpread(array.get.elms, Some(ident), mkSL(sp1, sp2)).toSuccess
            case Some(otherIdent) =>
              NonLinearPattern(ident.name, otherIdent.loc, mkSL(sp1, sp2)).toFailure
          }
        }


      case ParsedAst.Pattern.ArrayHeadSpread(sp1, ident, pats, sp2) =>
        val array = traverse(pats)(visit) map {
          case xs => WeededAst.Pattern.Array(xs, mkSL(sp1, sp2))
        }
        if (ident.name == "_") {
          WeededAst.Pattern.ArrayHeadSpread(None, array.get.elms, mkSL(sp1, sp2)).toSuccess
        } else {
          seen.get(ident.name) match {
            case None =>
              seen += (ident.name -> ident)
              WeededAst.Pattern.ArrayHeadSpread(Some(ident), array.get.elms, mkSL(sp1, sp2)).toSuccess
            case Some(otherIdent) =>
              NonLinearPattern(ident.name, otherIdent.loc, mkSL(sp1, sp2)).toFailure
          }
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
        mapN(visitPattern(pat1), visitPattern(pat2)) {
          case (hd, tl) =>
            val tag = Name.Ident(sp1, "Cons", sp2)
            val pat = WeededAst.Pattern.Tuple(List(hd, tl), mkSL(sp1, sp2))
            WeededAst.Pattern.Tag(None, tag, pat, mkSL(sp1, sp2))
        }

    }

    visit(pattern)
  }

  /**
    * Weeds the given head predicate.
    */
  private def visitHeadPredicate(past: ParsedAst.Predicate.Head)(implicit flix: Flix): Validation[WeededAst.Predicate.Head, WeederError] = past match {
    case ParsedAst.Predicate.Head.Atom(sp1, qname, terms, None, sp2) =>
      // Case 1: the atom has a relational denotation (because of the absence of the optional lattice term).
      val loc = mkSL(sp1, sp2)
      mapN(traverse(terms)(visitExp)) {
        case ts => WeededAst.Predicate.Head.Atom(qname, Denotation.Relational, ts, loc)
      }

    case ParsedAst.Predicate.Head.Atom(sp1, qname, terms, Some(term), sp2) =>
      // Case 2: the atom has a latticenal denotation (because of the presence of the optional lattice term).
      val loc = mkSL(sp1, sp2)
      mapN(traverse(terms)(visitExp), visitExp(term)) {
        case (ts, t) => WeededAst.Predicate.Head.Atom(qname, Denotation.Latticenal, ts ::: t :: Nil, loc)
      }

    case ParsedAst.Predicate.Head.Union(sp1, exp, sp2) =>
      mapN(visitExp(exp)) {
        case e => WeededAst.Predicate.Head.Union(e, mkSL(sp1, sp2))
      }
  }

  /**
    * Weeds the given body predicate.
    */
  private def visitPredicateBody(b: ParsedAst.Predicate.Body)(implicit flix: Flix): Validation[WeededAst.Predicate.Body, WeederError] = b match {
    case ParsedAst.Predicate.Body.Atom(sp1, polarity, qname, terms, None, sp2) =>
      // Case 1: the atom has a relational denotation (because of the absence of the optional lattice term).
      val loc = mkSL(sp1, sp2)
      mapN(traverse(terms)(visitPattern)) {
        case ts =>
          WeededAst.Predicate.Body.Atom(qname, Denotation.Relational, polarity, ts, loc)
      }

    case ParsedAst.Predicate.Body.Atom(sp1, polarity, qname, terms, Some(term), sp2) =>
      // Case 2: the atom has a latticenal denotation (because of the presence of the optional lattice term).
      val loc = mkSL(sp1, sp2)
      mapN(traverse(terms)(visitPattern), visitPattern(term)) {
        case (ts, t) =>
          WeededAst.Predicate.Body.Atom(qname, Denotation.Latticenal, polarity, ts ::: t :: Nil, loc)
      }

    case ParsedAst.Predicate.Body.Guard(sp1, exp, sp2) =>
      mapN(visitExp(exp)) {
        case e => WeededAst.Predicate.Body.Guard(e, mkSL(sp1, sp2))
      }

    case ParsedAst.Predicate.Body.Filter(sp1, qname, terms, sp2) =>
      val loc = mkSL(sp1, sp2)
      traverse(terms)(visitExp) map {
        case ts =>
          // Check if the argument list is empty. If so, invoke the function with the Unit value.
          val as = if (ts.isEmpty) List(WeededAst.Expression.Unit(loc)) else ts
          val b = WeededAst.Expression.VarOrDef(qname, loc)
          val e = mkApplyCurried(b, as, loc)
          WeededAst.Predicate.Body.Guard(e, loc)
      }
  }

  /**
    * Weeds the given sequence of parsed annotation `xs`.
    */
  private def visitAnnotationOrProperty(xs: Seq[ParsedAst.AnnotationOrProperty]): Validation[Ast.Annotations, WeederError] = {
    // collect seen annotations.
    val seen = mutable.Map.empty[String, ParsedAst.Annotation]

    // loop through each annotation.
    val result = xs.toList.collect {
      case x: ParsedAst.Annotation => seen.get(x.ident.name) match {
        case None =>
          seen += (x.ident.name -> x)
          visitAnnotation(x)
        case Some(otherAnn) =>
          DuplicateAnnotation(x.ident.name, mkSL(otherAnn.sp1, otherAnn.sp2), mkSL(x.sp1, x.sp2)).toFailure
      }
    }

    sequence(result).map(as => Ast.Annotations(as))
  }

  /**
    * Weeds the given parsed annotation `past`.
    */
  private def visitAnnotation(past: ParsedAst.Annotation): Validation[Ast.Annotation, WeederError] = {
    /*
     * Check for `UndefinedAnnotation`.
     */
    val loc = mkSL(past.sp1, past.sp2)
    past.ident.name match {
      case "benchmark" => Ast.Annotation.Benchmark(loc).toSuccess
      case "law" => Ast.Annotation.Law(loc).toSuccess
      case "lint" => Ast.Annotation.Lint(loc).toSuccess
      case "test" => Ast.Annotation.Test(loc).toSuccess
      case "unchecked" => Ast.Annotation.Unchecked(loc).toSuccess
      case name => WeederError.UndefinedAnnotation(name, loc).toFailure
    }
  }

  /**
    * Weeds the given sequence of parsed modifiers `xs`.
    */
  private def visitModifiers(xs: Seq[ParsedAst.Modifier], legalModifiers: Set[Ast.Modifier]): Validation[Ast.Modifiers, WeederError] = {
    val seen = mutable.Map.empty[String, ParsedAst.Modifier]
    val modifiersVal = traverse(xs) {
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

    modifiersVal.map(ms => Ast.Modifiers(ms))
  }

  /**
    * Weeds the given parsed modifier `m`.
    */
  private def visitModifier(m: ParsedAst.Modifier, legalModifiers: Set[Ast.Modifier]): Validation[Ast.Modifier, WeederError] = {
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

  /**
    * Collects all constraints in the given AST `roots`.
    */
  private def visitAllConstraints(roots: List[ParsedAst.Root])(implicit flix: Flix): Validation[List[WeededAst.Constraint], WeederError] = {

    // TODO: What if a constraint occurs in another namespace????

    /**
      * Local root visitor.
      */
    def visitRoot(root: ParsedAst.Root): Validation[List[WeededAst.Constraint], WeederError] = traverse(root.decls)(visitDecl).map(_.flatten)

    /**
      * Local declaration visitor.
      */
    def visitDecl(d0: ParsedAst.Declaration): Validation[List[WeededAst.Constraint], WeederError] = d0 match {
      case ParsedAst.Declaration.Namespace(sp1, name, decls, sp2) => traverse(decls)(visitDecl).map(_.flatten)
      case d: ParsedAst.Declaration.Constraint => visitConstraint(d).map(x => x :: Nil)
      case _ => Nil.toSuccess
    }

    traverse(roots)(visitRoot).map(_.flatten)
  }

  /**
    * Collects all the properties in the given AST `root`.
    */
  private def visitAllProperties(root: ParsedAst.Root)(implicit flix: Flix): Validation[List[WeededAst.Declaration], WeederError] = {

    /**
      * Local declaration visitor.
      */
    def visit(decl: ParsedAst.Declaration): Validation[List[WeededAst.Declaration], WeederError] = decl match {
      // Recurse through the namespace.
      case ParsedAst.Declaration.Namespace(sp1, name, decls, sp2) =>
        traverse(decls)(visit) map {
          case ds => List(WeededAst.Declaration.Namespace(name, ds.flatten, mkSL(sp1, sp2)))
        }

      case ParsedAst.Declaration.Def(_, meta, _, _, defn, _, _, _, _, _, _) =>
        // Instantiate properties based on the laws referenced by the definition.
        sequence(meta.collect {
          case ParsedAst.Property(sp1, law, args, sp2) =>
            val loc = mkSL(sp1, sp2)

            // Weeds the arguments of the property.
            val argsVal = args match {
              case None => Nil.toSuccess
              case Some(es) => traverse(es)(e => visitExp(e))
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

    sequence(root.decls.map(visit)).map(_.flatten)
  }

  /**
    * Weeds the given parsed type `tpe`.
    */
  private def visitType(tpe: ParsedAst.Type): WeededAst.Type = tpe match {
    case ParsedAst.Type.Unit(sp1, sp2) => WeededAst.Type.Unit(mkSL(sp1, sp2))

    case ParsedAst.Type.Var(sp1, ident, sp2) => WeededAst.Type.Var(ident, mkSL(sp1, sp2))

    case ParsedAst.Type.Ambiguous(sp1, qname, sp2) => WeededAst.Type.Ambiguous(qname, mkSL(sp1, sp2))

    case ParsedAst.Type.Tuple(sp1, elms, sp2) => WeededAst.Type.Tuple(elms.toList.map(visitType), mkSL(sp1, sp2))

    case ParsedAst.Type.Record(sp1, fields, restOpt, sp2) =>
      val init = restOpt match {
        case None => WeededAst.Type.RecordEmpty(mkSL(sp1, sp2))
        case Some(base) => WeededAst.Type.Var(base, mkSL(sp1, sp2))
      }
      fields.foldRight(init: WeededAst.Type) {
        case (ParsedAst.RecordFieldType(ssp1, l, t, ssp2), acc) =>
          WeededAst.Type.RecordExtend(l, visitType(t), acc, mkSL(ssp1, ssp2))
      }

    case ParsedAst.Type.Schema(sp1, ps, restOpt, sp2) =>
      val zero = restOpt match {
        case None => WeededAst.Type.SchemaEmpty(mkSL(sp1, sp2))
        case Some(base) => WeededAst.Type.Var(base, mkSL(sp1, sp2))
      }
      val ts = ps.map(visitType).toList
      WeededAst.Type.Schema(ts, zero, mkSL(sp1, sp2))

    case ParsedAst.Type.Nat(sp1, len, sp2) => WeededAst.Type.Nat(checkNaturalNumber(len, sp1, sp2), mkSL(sp1, sp2))

    case ParsedAst.Type.Native(sp1, fqn, sp2) => WeededAst.Type.Native(fqn.mkString("."), mkSL(sp1, sp2))

    case ParsedAst.Type.UnaryImpureArrow(tpe1, tpe2, sp2) =>
      val loc = mkSL(leftMostSourcePosition(tpe1), sp2)
      val t1 = visitType(tpe1)
      val t2 = visitType(tpe2)
      val eff = WeededAst.Type.Impure(loc)
      mkArrow(t1, eff, t2, loc)

    case ParsedAst.Type.UnaryPolymorphicArrow(tpe1, tpe2, effOpt, sp2) =>
      val loc = mkSL(leftMostSourcePosition(tpe1), sp2)
      val t1 = visitType(tpe1)
      val t2 = visitType(tpe2)
      val eff = effOpt match {
        // NB: If there is no explicit effect then the arrow is pure.
        case None => WeededAst.Type.Pure(loc)
        case Some(f) => visitType(f)
      }
      mkArrow(t1, eff, t2, loc)

    case ParsedAst.Type.ImpureArrow(sp1, tparams, tresult, sp2) =>
      val loc = mkSL(sp1, sp2)
      val ts = tparams.map(visitType)
      val tr = visitType(tresult)
      val eff = WeededAst.Type.Impure(loc)
      mkCurriedArrow(ts, eff, tr, loc)

    case ParsedAst.Type.PolymorphicArrow(sp1, tparams, tresult, effOpt, sp2) =>
      val loc = mkSL(sp1, sp2)
      val ts = tparams.map(visitType)
      val tr = visitType(tresult)
      val eff = effOpt match {
        // NB: If there is no explicit effect then the arrow is pure.
        case None => WeededAst.Type.Pure(loc)
        case Some(f) => visitType(f)
      }
      mkCurriedArrow(ts, eff, tr, loc)

    case ParsedAst.Type.Apply(t1, args, sp2) =>
      // Curry the type arguments.
      val sp1 = leftMostSourcePosition(t1)
      args.foldLeft(visitType(t1)) {
        case (acc, t2) => WeededAst.Type.Apply(acc, visitType(t2), mkSL(sp1, sp2))
      }

    case ParsedAst.Type.Pure(sp1, sp2) =>
      WeededAst.Type.Pure(mkSL(sp1, sp2))

    case ParsedAst.Type.Impure(sp1, sp2) =>
      WeededAst.Type.Impure(mkSL(sp1, sp2))

    case ParsedAst.Type.And(eff1, eff2) =>
      val t1 = visitType(eff1)
      val t2 = visitType(eff2)
      WeededAst.Type.And(t1, t2, SourceLocation.Unknown)
  }

  /**
    * Returns an arrow type from `tpe1` to `tpe2` with effect `eff`.
    *
    * In other words, the type is of the form `tpe1 ->{eff} tpe2`
    */
  private def mkArrow(tpe1: WeededAst.Type, eff: WeededAst.Type, tpe2: WeededAst.Type, loc: SourceLocation): WeededAst.Type =
    WeededAst.Type.Arrow(List(tpe1), eff, tpe2, loc)

  /**
    * Returns a sequence of arrow types type from `tparams` to `tresult` where every arrow is pure except the last which has effect `eff`.
    *
    * In other words, the type is of the form `tpe1 ->> tpe2 ->> ... ->{eff} tresult`.
    */
  private def mkCurriedArrow(tparams: Seq[WeededAst.Type], eff: WeededAst.Type, tresult: WeededAst.Type, loc: SourceLocation): WeededAst.Type = {
    val base = mkArrow(tparams.last, eff, tresult, loc)
    tparams.init.foldRight(base)(mkArrow(_, WeededAst.Type.Pure(loc), _, loc))
  }

  /**
    * Weeds the given parsed optional effect `effOpt`.
    */
  private def visitEff(effOpt: Option[ParsedAst.Type])(implicit flix: Flix): Validation[WeededAst.Type, WeederError] = effOpt match {
    case None => WeededAst.Type.Pure(SourceLocation.Unknown).toSuccess
    case Some(tpe) => visitType(tpe).toSuccess
  }

  /**
    * Weeds the given list of formal parameter `fparams`.
    *
    * Checks for [[IllegalFormalParameter]] and [[DuplicateFormalParam]].
    */
  private def visitFormalParams(fparams: Seq[ParsedAst.FormalParam], typeRequired: Boolean): Validation[List[WeededAst.FormalParam], WeederError] = {
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

    traverse(fparams) {
      case param@ParsedAst.FormalParam(sp1, mods, ident, typeOpt, sp2) => seen.get(ident.name) match {
        case None =>
          if (!ident.name.startsWith("_")) {
            // Wildcards cannot be duplicate.
            seen += (ident.name -> param)
          }

          visitModifiers(mods, legalModifiers = Set(Ast.Modifier.Inline)) flatMap {
            case mod =>
              if (typeRequired && typeOpt.isEmpty)
                IllegalFormalParameter(ident.name, mkSL(sp1, sp2)).toFailure
              else
                WeededAst.FormalParam(ident, mod, typeOpt.map(visitType), mkSL(sp1, sp2)).toSuccess
          }
        case Some(otherParam) =>
          val loc1 = mkSL(otherParam.sp1, otherParam.sp2)
          val loc2 = mkSL(param.sp1, param.sp2)
          DuplicateFormalParam(ident.name, loc1, loc2).toFailure
      }
    }
  }

  /**
    * Weeds the given documentation.
    */
  private def visitDoc(doc0: ParsedAst.Doc): Ast.Doc = Ast.Doc(doc0.lines.toList, mkSL(doc0.sp1, doc0.sp2))

  /**
    * Weeds the given type parameters `tparams0`.
    */
  private def visitTypeParams(tparams0: ParsedAst.TypeParams): WeededAst.TypeParams = tparams0 match {
    case ParsedAst.TypeParams.Elided => WeededAst.TypeParams.Elided
    case ParsedAst.TypeParams.Explicit(bounds) => WeededAst.TypeParams.Explicit(bounds.map(_.ident))
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
    * Returns the given expression `exp0` optionally wrapped in a type ascription if `tpe0` is `Some`.
    */
  private def withAscription(exp0: WeededAst.Expression, tpe0: Option[ParsedAst.Type])(implicit flix: Flix): WeededAst.Expression = tpe0 match {
    case None => exp0
    case Some(t) => WeededAst.Expression.Ascribe(exp0, Some(visitType(t)), None, exp0.loc)
  }

  /**
    * Removes underscores from the given string of digits.
    */
  private def stripUnderscores(digits: String): String = {
    digits.filterNot(_ == '_')
  }

  /**
    * Attempts to parse the given float32 with `sign` digits `before` and `after` the comma.
    */
  private def toFloat32(sign: Boolean, before: String, after: String, loc: SourceLocation): Validation[Float, WeederError] = try {
    val s = if (sign) s"-$before.$after" else s"$before.$after"
    stripUnderscores(s).toFloat.toSuccess
  } catch {
    case e: NumberFormatException => IllegalFloat(loc).toFailure
  }

  /**
    * Attempts to parse the given float64 with `sign` digits `before` and `after` the comma.
    */
  private def toFloat64(sign: Boolean, before: String, after: String, loc: SourceLocation): Validation[Double, WeederError] = try {
    val s = if (sign) s"-$before.$after" else s"$before.$after"
    stripUnderscores(s).toDouble.toSuccess
  } catch {
    case e: NumberFormatException => IllegalFloat(loc).toFailure
  }

  /**
    * Attempts to parse the given int8 with `sign` and `digits`.
    */
  private def toInt8(sign: Boolean, radix: Int, digits: String, loc: SourceLocation): Validation[Byte, WeederError] = try {
    val s = if (sign) "-" + digits else digits
    JByte.parseByte(stripUnderscores(s), radix).toSuccess
  } catch {
    case ex: NumberFormatException => IllegalInt(loc).toFailure
  }

  /**
    * Attempts to parse the given int16 with `sign` and `digits`.
    */
  private def toInt16(sign: Boolean, radix: Int, digits: String, loc: SourceLocation): Validation[Short, WeederError] = try {
    val s = if (sign) "-" + digits else digits
    JShort.parseShort(stripUnderscores(s), radix).toSuccess
  } catch {
    case ex: NumberFormatException => IllegalInt(loc).toFailure
  }

  /**
    * Attempts to parse the given int32 with `sign` and `digits`.
    */
  private def toInt32(sign: Boolean, radix: Int, digits: String, loc: SourceLocation): Validation[Int, WeederError] = try {
    val s = if (sign) "-" + digits else digits
    JInt.parseInt(stripUnderscores(s), radix).toSuccess
  } catch {
    case ex: NumberFormatException => IllegalInt(loc).toFailure
  }

  /**
    * Attempts to parse the given int64 with `sign` and `digits`.
    */
  private def toInt64(sign: Boolean, radix: Int, digits: String, loc: SourceLocation): Validation[Long, WeederError] = try {
    val s = if (sign) "-" + digits else digits
    JLong.parseLong(stripUnderscores(s), radix).toSuccess
  } catch {
    case ex: NumberFormatException => IllegalInt(loc).toFailure
  }

  /**
    * Attempts to parse the given BigInt with `sign` and `digits`.
    */
  private def toBigInt(sign: Boolean, radix: Int, digits: String, loc: SourceLocation): Validation[BigInteger, WeederError] = try {
    val s = if (sign) "-" + digits else digits
    new BigInteger(stripUnderscores(s), radix).toSuccess
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
  @tailrec
  private def leftMostSourcePosition(e: ParsedAst.Expression): SourcePosition = e match {
    case ParsedAst.Expression.SName(sp1, _, _) => sp1
    case ParsedAst.Expression.QName(sp1, _, _) => sp1
    case ParsedAst.Expression.Hole(sp1, _, _) => sp1
    case ParsedAst.Expression.Use(sp1, _, _, _) => sp1
    case ParsedAst.Expression.Lit(sp1, _, _) => sp1
    case ParsedAst.Expression.Apply(e1, _, _) => leftMostSourcePosition(e1)
    case ParsedAst.Expression.Infix(e1, _, _, _) => leftMostSourcePosition(e1)
    case ParsedAst.Expression.Postfix(e1, _, _, _) => leftMostSourcePosition(e1)
    case ParsedAst.Expression.Lambda(sp1, _, _, _) => sp1
    case ParsedAst.Expression.LambdaMatch(sp1, _, _, _) => sp1
    case ParsedAst.Expression.Unary(sp1, _, _, _) => sp1
    case ParsedAst.Expression.Binary(e1, _, _, _) => leftMostSourcePosition(e1)
    case ParsedAst.Expression.IfThenElse(sp1, _, _, _, _) => sp1
    case ParsedAst.Expression.Statement(e1, _, _) => leftMostSourcePosition(e1)
    case ParsedAst.Expression.LetMatch(sp1, _, _, _, _, _) => sp1
    case ParsedAst.Expression.LetMatchStar(sp1, _, _, _, _, _) => sp1
    case ParsedAst.Expression.LetRec(sp1, _, _, _, _) => sp1
    case ParsedAst.Expression.LetImport(sp1, _, _, _) => sp1
    case ParsedAst.Expression.Match(sp1, _, _, _) => sp1
    case ParsedAst.Expression.Tag(sp1, _, _, _) => sp1
    case ParsedAst.Expression.Tuple(sp1, _, _) => sp1
    case ParsedAst.Expression.RecordLit(sp1, _, _) => sp1
    case ParsedAst.Expression.RecordSelect(base, _, _) => leftMostSourcePosition(base)
    case ParsedAst.Expression.RecordSelectLambda(sp1, _, _) => sp1
    case ParsedAst.Expression.RecordOperation(sp1, _, _, _) => sp1
    case ParsedAst.Expression.ArrayLit(sp1, _, _) => sp1
    case ParsedAst.Expression.ArrayNew(sp1, _, _, _) => sp1
    case ParsedAst.Expression.ArrayLoad(base, _, _) => leftMostSourcePosition(base)
    case ParsedAst.Expression.ArrayStore(base, _, _, _) => leftMostSourcePosition(base)
    case ParsedAst.Expression.ArraySlice(base, _, _, _) => leftMostSourcePosition(base)
    case ParsedAst.Expression.VectorLit(sp1, _, _) => sp1
    case ParsedAst.Expression.VectorNew(sp1, _, _, _) => sp1
    case ParsedAst.Expression.VectorLoad(base, _, _) => leftMostSourcePosition(base)
    case ParsedAst.Expression.VectorStore(base, _, _, _) => leftMostSourcePosition(base)
    case ParsedAst.Expression.VectorLength(sp1, _, _) => sp1
    case ParsedAst.Expression.VectorSlice(base, _, _, _) => leftMostSourcePosition(base)
    case ParsedAst.Expression.FNil(sp1, _) => sp1
    case ParsedAst.Expression.FCons(hd, _, _, _) => leftMostSourcePosition(hd)
    case ParsedAst.Expression.FAppend(fst, _, _, _) => leftMostSourcePosition(fst)
    case ParsedAst.Expression.FSet(sp1, _, _) => sp1
    case ParsedAst.Expression.FMap(sp1, _, _) => sp1
    case ParsedAst.Expression.Interpolation(sp1, _, _) => sp1
    case ParsedAst.Expression.Ref(sp1, _, _) => sp1
    case ParsedAst.Expression.Deref(sp1, _, _) => sp1
    case ParsedAst.Expression.Assign(e1, _, _) => leftMostSourcePosition(e1)
    case ParsedAst.Expression.Existential(sp1, _, _, _, _) => sp1
    case ParsedAst.Expression.Universal(sp1, _, _, _, _) => sp1
    case ParsedAst.Expression.Ascribe(e1, _, _, _) => leftMostSourcePosition(e1)
    case ParsedAst.Expression.Cast(e1, _, _, _) => leftMostSourcePosition(e1)
    case ParsedAst.Expression.TryCatch(sp1, _, _, _) => sp1
    case ParsedAst.Expression.NewChannel(sp1, _, _, _) => sp1
    case ParsedAst.Expression.GetChannel(sp1, _, _) => sp1
    case ParsedAst.Expression.PutChannel(e1, _, _) => leftMostSourcePosition(e1)
    case ParsedAst.Expression.SelectChannel(sp1, _, _, _) => sp1
    case ParsedAst.Expression.ProcessSpawn(sp1, _, _) => sp1
    case ParsedAst.Expression.ProcessPanic(sp1, _, _) => sp1
    case ParsedAst.Expression.FixpointConstraint(sp1, _, _) => sp1
    case ParsedAst.Expression.FixpointConstraintSet(sp1, _, _) => sp1
    case ParsedAst.Expression.FixpointCompose(e1, _, _) => leftMostSourcePosition(e1)
    case ParsedAst.Expression.FixpointSolve(sp1, _, _) => sp1
    case ParsedAst.Expression.FixpointProject(sp1, _, _, _) => sp1
    case ParsedAst.Expression.FixpointEntails(exp1, _, _) => leftMostSourcePosition(exp1)
    case ParsedAst.Expression.FixpointFold(sp1, _, _, _, _, _) => sp1
  }

  /**
    * Returns the left most source position in the sub-tree of the type `tpe`.
    */
  @tailrec
  private def leftMostSourcePosition(tpe: ParsedAst.Type): SourcePosition = tpe match {
    case ParsedAst.Type.Unit(sp1, _) => sp1
    case ParsedAst.Type.Var(sp1, _, _) => sp1
    case ParsedAst.Type.Ambiguous(sp1, _, _) => sp1
    case ParsedAst.Type.Tuple(sp1, _, _) => sp1
    case ParsedAst.Type.Record(sp1, _, _, _) => sp1
    case ParsedAst.Type.Schema(sp1, _, _, _) => sp1
    case ParsedAst.Type.Nat(sp1, _, _) => sp1
    case ParsedAst.Type.Native(sp1, _, _) => sp1
    case ParsedAst.Type.UnaryImpureArrow(tpe1, _, _) => leftMostSourcePosition(tpe1)
    case ParsedAst.Type.UnaryPolymorphicArrow(tpe1, _, _, _) => leftMostSourcePosition(tpe1)
    case ParsedAst.Type.ImpureArrow(sp1, _, _, _) => sp1
    case ParsedAst.Type.PolymorphicArrow(sp1, _, _, _, _) => sp1
    case ParsedAst.Type.Apply(tpe1, _, _) => leftMostSourcePosition(tpe1)
    case ParsedAst.Type.Pure(sp1, _) => sp1
    case ParsedAst.Type.Impure(sp1, _) => sp1
    case ParsedAst.Type.And(tpe1, _) => leftMostSourcePosition(tpe1)
  }

  /**
    * Checks that no attributes are repeated.
    */
  private def checkDuplicateAttribute(attrs: Seq[ParsedAst.Attribute]): Validation[List[WeededAst.Attribute], WeederError] = {
    val seen = mutable.Map.empty[String, ParsedAst.Attribute]
    traverse(attrs) {
      case attr@ParsedAst.Attribute(sp1, ident, tpe, sp2) => seen.get(ident.name) match {
        case None =>
          seen += (ident.name -> attr)
          WeededAst.Attribute(ident, visitType(tpe), mkSL(sp1, sp2)).toSuccess
        case Some(otherAttr) =>
          val loc1 = mkSL(otherAttr.sp1, otherAttr.sp2)
          val loc2 = mkSL(attr.sp1, attr.sp2)
          DuplicateAttribute(ident.name, loc1, loc2).toFailure
      }
    }
  }

  /**
    * Helper method for Succ type.
    * Checks to make sure Literal.Int32 is >= 0, and converts it to int.
    * Throws InternalCompilerException if check fails.
    * TODO make type handling for vertification.
    */
  private def checkNaturalNumber(elm: ParsedAst.Literal.Int32, sp1: SourcePosition, sp2: SourcePosition): Int = {
    toInt32(elm.sign, elm.radix, elm.lit, mkSL(sp1, sp2)) match {
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

  /**
    * Returns the class and member name constructed from the given fully-qualified name `fqn`.
    */
  private def parseClassAndMember(fqn: Seq[String], loc: SourceLocation): Validation[(String, String), WeederError] = {
    // Ensure that the fqn has at least two components.
    if (fqn.length == 1) {
      return WeederError.IllegalJvmFieldOrMethodName(loc).toFailure
    }

    // Compute the class and member name.
    val className = fqn.dropRight(1).mkString(".")
    val memberName = fqn.last

    (className, memberName).toSuccess
  }

  /**
    * Introduces a main declaration that wraps the given constraints in a solve expression.
    */
  private def mkMain(cs: List[WeededAst.Constraint])(implicit flix: Flix): WeededAst.Root = {
    // Source positions and source locations for the generated main.
    val sp1 = SourcePosition.Unknown
    val sp2 = SourcePosition.Unknown
    val loc = SourceLocation.Generated

    // Useful types.
    val StringType = WeededAst.Type.Ambiguous(Name.mkQName("String"), loc)

    // Documentation, annotations, and modifiers for the generated main.
    val doc = Ast.Doc(Nil, loc)
    val ann = Ast.Annotations.Empty
    val mod = Ast.Modifiers.Empty
    val ident = Name.Ident(sp1, "main", sp2)

    // Type and formal parameters for the generated main.
    val tparams = WeededAst.TypeParams.Explicit(Nil)
    val fparams = WeededAst.FormalParam(Name.Ident(sp1, "_unit", sp2), Ast.Modifiers.Empty, None, loc) :: Nil

    // Collect all the constraints into a single constraint set.
    val innerExp = WeededAst.Expression.FixpointConstraintSet(cs, loc)

    // The solve expression.
    val outerExp = WeededAst.Expression.FixpointSolve(innerExp, loc)
    val castedExp = WeededAst.Expression.Cast(outerExp, Some(WeededAst.Type.Native("java.lang.Object", loc)), None, loc)
    val toStringExp = WeededAst.Expression.InvokeMethod("java.lang.Object", "toString", castedExp, Nil, Nil, loc)
    val castedToStringExp = WeededAst.Expression.Cast(toStringExp, None, Some(WeededAst.Type.Pure(loc)), loc)

    // The type and effect of the generated main.
    val argType = WeededAst.Type.Ambiguous(Name.mkQName("Unit"), loc)
    val resultType = StringType
    val tpe = mkArrow(argType, WeededAst.Type.Pure(loc), resultType, loc)
    val eff = WeededAst.Type.Pure(loc)

    // Construct the declaration.
    val decl = WeededAst.Declaration.Def(doc, ann, mod, ident, tparams, fparams, castedToStringExp, tpe, eff, loc)

    // Construct an AST root that contains the main declaration.
    WeededAst.Root(Nil, List(decl), SourceLocation.Unknown)
  }

}
