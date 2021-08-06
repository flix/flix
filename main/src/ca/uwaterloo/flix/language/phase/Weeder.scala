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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Ast.Denotation
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.errors.WeederError
import ca.uwaterloo.flix.language.errors.WeederError._
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps, Validation}

import java.lang.{Byte => JByte, Integer => JInt, Long => JLong, Short => JShort}
import java.math.BigInteger
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
    val roots = Validation.sequence(ParOps.parMap(program.roots, visitRoot))

    mapN(roots) {
      case rs => WeededAst.Program(rs, flix.getReachableRoots)
    }
  }

  /**
    * Weeds the given abstract syntax tree.
    */
  private def visitRoot(root: ParsedAst.Root)(implicit flix: Flix): Validation[WeededAst.Root, WeederError] = {
    val usesVal = traverse(root.uses)(visitUse)
    val declarationsVal = traverse(root.decls)(visitDecl)
    val loc = mkSL(root.sp1, root.sp2)

    mapN(usesVal, declarationsVal) {
      case (uses, decls) =>
        WeededAst.Root(uses.flatten, decls.flatten, loc)
    }
  }

  /**
    * Compiles the given parsed declaration `past` to a list of weeded declarations.
    */
  private def visitDecl(decl: ParsedAst.Declaration)(implicit flix: Flix): Validation[List[WeededAst.Declaration], WeederError] = decl match {
    case ParsedAst.Declaration.Namespace(sp1, name, uses, decls, sp2) =>
      val usesVal = traverse(uses)(visitUse)
      val declarationsVal = traverse(decls)(visitDecl)
      mapN(usesVal, declarationsVal) {
        case (us, ds) => List(WeededAst.Declaration.Namespace(name, us.flatten, ds.flatten, mkSL(sp1, sp2)))
      }

    case d: ParsedAst.Declaration.Def => visitTopDef(d)

    case d: ParsedAst.Declaration.Law => visitLaw(d)

    case d: ParsedAst.Declaration.Enum => visitEnum(d)

    case d: ParsedAst.Declaration.OpaqueType => visitOpaqueType(d)

    case d: ParsedAst.Declaration.TypeAlias => visitTypeAlias(d)

    case d: ParsedAst.Declaration.Relation => visitRelation(d)

    case d: ParsedAst.Declaration.Lattice => visitLattice(d)

    case d: ParsedAst.Declaration.Class => visitClass(d)

    case d: ParsedAst.Declaration.Instance => visitInstance(d)

    case _: ParsedAst.Declaration.Sig => throw InternalCompilerException(s"Unexpected declaration")
  }

  /**
    * Performs weeding on the given class declaration `c0`.
    */
  private def visitClass(c0: ParsedAst.Declaration.Class)(implicit flix: Flix): Validation[List[WeededAst.Declaration.Class], WeederError] = c0 match {
    case ParsedAst.Declaration.Class(doc0, mods0, sp1, ident, tparam0, superClasses0, lawsAndSigs, sp2) =>
      val loc = mkSL(sp1, sp2)
      val doc = visitDoc(doc0)
      val laws0 = lawsAndSigs.collect { case law: ParsedAst.Declaration.Law => law }
      val sigs0 = lawsAndSigs.collect { case sig: ParsedAst.Declaration.Sig => sig }
      for {
        mods <- visitModifiers(mods0, legalModifiers = Set(Ast.Modifier.Public, Ast.Modifier.Sealed, Ast.Modifier.Lawless))
        sigs <- traverse(sigs0)(visitSig)
        laws <- traverse(laws0)(visitLaw)
        tparam = visitTypeParam(tparam0)
        superClasses <- traverse(superClasses0)(visitTypeConstraint)
      } yield List(WeededAst.Declaration.Class(doc, mods, ident, tparam, superClasses, sigs.flatten, laws.flatten, loc))
  }

  /**
    * Performs weeding on the given sig declaration `s0`.
    */
  private def visitSig(s0: ParsedAst.Declaration.Sig)(implicit flix: Flix): Validation[List[WeededAst.Declaration.Sig], WeederError] = s0 match {
    case ParsedAst.Declaration.Sig(doc0, ann, mods, sp1, ident, tparams0, fparams0, tpe0, effOpt, tconstrs0, exp0, sp2) =>
      val loc = mkSL(ident.sp1, ident.sp2)
      val doc = visitDoc(doc0)
      val annVal = visitAnnotations(ann)
      val modVal = visitModifiers(mods, legalModifiers = Set(Ast.Modifier.Inline, Ast.Modifier.Public))
      val tparamsVal = visitKindedTypeParams(tparams0)
      val formalsVal = visitFormalParams(fparams0, typeRequired = true)
      val effVal = visitEff(effOpt, loc)
      val expVal = Validation.traverse(exp0)(visitExp)

      for {
        res <- sequenceT(annVal, modVal, tparamsVal, formalsVal, effVal, expVal)
        (as, mod, tparams, fparams, eff, exp) = res
        _ <- requirePublic(mod, ident)
        ts = fparams.map(_.tpe.get)
        retTpe = visitType(tpe0)
        tpe = WeededAst.Type.Arrow(ts, eff, retTpe, loc)
        tconstrs <- traverse(tconstrs0)(visitTypeConstraint)
      } yield List(WeededAst.Declaration.Sig(doc, as, mod, ident, tparams, fparams, exp.headOption, tpe, retTpe, eff, tconstrs, loc))
  }

  /**
    * Performs weeding on the given instance declaration `i0`.
    */
  private def visitInstance(i0: ParsedAst.Declaration.Instance)(implicit flix: Flix): Validation[List[WeededAst.Declaration.Instance], WeederError] = i0 match {
    case ParsedAst.Declaration.Instance(doc0, mods0, sp1, clazz, tpe0, constrs0, defs0, sp2) =>
      val loc = mkSL(sp1, sp2)
      val doc = visitDoc(doc0)
      val tpe = visitType(tpe0)
      for {
        mods <- visitModifiers(mods0, legalModifiers = Set(Ast.Modifier.Public, Ast.Modifier.Unlawful))
        defs <- traverse(defs0)(visitInstanceDef)
        constrs <- traverse(constrs0)(visitTypeConstraint)
      } yield List(WeededAst.Declaration.Instance(doc, mods, clazz, tpe, constrs, defs.flatten, clazz.loc))

  }

  /**
    * Performs weeding on the given top-level def declaration `d0`.
    */
  private def visitTopDef(d0: ParsedAst.Declaration.Def)(implicit flix: Flix): Validation[List[WeededAst.Declaration.Def], WeederError] = {
    visitDef(d0, Set(Ast.Modifier.Public, Ast.Modifier.Inline), requiresPublic = false)
  }

  /**
    * Performs weeding on the given instance def declaration `d0`.
    */
  private def visitInstanceDef(d0: ParsedAst.Declaration.Def)(implicit flix: Flix): Validation[List[WeededAst.Declaration.Def], WeederError] = {
    visitDef(d0, Set(Ast.Modifier.Public, Ast.Modifier.Inline, Ast.Modifier.Override), requiresPublic = true)
  }

  /**
    * Performs weeding on the given def declaration `d0`.
    */
  private def visitDef(d0: ParsedAst.Declaration.Def, legalModifiers: Set[Ast.Modifier], requiresPublic: Boolean)(implicit flix: Flix): Validation[List[WeededAst.Declaration.Def], WeederError] = d0 match {
    case ParsedAst.Declaration.Def(doc0, ann, mods, sp1, ident, tparams0, fparams0, tpe0, effOpt, tconstrs0, exp0, sp2) =>
      val loc = mkSL(ident.sp1, ident.sp2)
      val doc = visitDoc(doc0)
      val annVal = visitAnnotations(ann)
      val modVal = visitModifiers(mods, legalModifiers)
      val expVal = visitExp(exp0)
      val tparamsVal = visitKindedTypeParams(tparams0)
      val formalsVal = visitFormalParams(fparams0, typeRequired = true)
      val effVal = visitEff(effOpt, loc)

      for {
        res <- sequenceT(annVal, modVal, tparamsVal, formalsVal, expVal, effVal)
        (as, mod, tparams, fparams, exp, eff) = res
        _ <- if (requiresPublic) requirePublic(mod, ident) else ().toSuccess // conditionally require a public modifier
        ts = fparams.map(_.tpe.get)
        retTpe = visitType(tpe0)
        tpe = WeededAst.Type.Arrow(ts, eff, retTpe, loc)
        tconstrs <- traverse(tconstrs0)(visitTypeConstraint)
      } yield List(WeededAst.Declaration.Def(doc, as, mod, ident, tparams, fparams, exp, tpe, retTpe, eff, tconstrs, loc))
  }

  /**
    * Performs weeding on the given law declaration `d0`.
    */
  private def visitLaw(d0: ParsedAst.Declaration.Law)(implicit flix: Flix): Validation[List[WeededAst.Declaration.Def], WeederError] = d0 match {
    case ParsedAst.Declaration.Law(doc0, ann0, mod0, sp1, ident, tparams0, fparams0, tconstrs0, exp0, sp2) =>
      val loc = mkSL(ident.sp1, ident.sp2)
      val doc = visitDoc(doc0)
      val annVal = visitAnnotations(ann0)
      val modVal = visitModifiers(mod0, legalModifiers = Set.empty)
      val expVal = visitExp(exp0)
      val tparamsVal = visitKindedTypeParams(tparams0)
      val formalsVal = visitFormalParams(fparams0, typeRequired = true)
      val tconstrsVal = Validation.traverse(tconstrs0)(visitTypeConstraint)

      mapN(annVal, modVal, tparamsVal, formalsVal, expVal, tconstrsVal) {
        case (ann, mod, tparams, fs, exp, tconstrs) =>
          val ts = fs.map(_.tpe.get)
          val retTpe = WeededAst.Type.Ambiguous(Name.mkQName("Bool"), loc)
          val tpe = WeededAst.Type.Arrow(ts, WeededAst.Type.True(loc), retTpe, loc)
          List(WeededAst.Declaration.Def(doc, ann, mod, ident, tparams, fs, exp, tpe, retTpe, WeededAst.Type.True(loc), tconstrs, loc))
      }
  }

  /**
    * Performs weeding on the given enum declaration `d0`.
    */
  private def visitEnum(d0: ParsedAst.Declaration.Enum)(implicit flix: Flix): Validation[List[WeededAst.Declaration.Enum], WeederError] = d0 match {
    case ParsedAst.Declaration.Enum(doc0, mods, sp1, ident, tparams0, cases, sp2) =>
      val doc = visitDoc(doc0)
      val modVal = visitModifiers(mods, legalModifiers = Set(Ast.Modifier.Public))
      val tparamsVal = visitTypeParams(tparams0)

      flatMapN(modVal, tparamsVal) {
        case (mod, tparams) =>
          /*
           * Check for `DuplicateTag`.
           */
          Validation.fold[ParsedAst.Case, Map[Name.Tag, WeededAst.Case], WeederError](cases, Map.empty) {
            case (macc, caze: ParsedAst.Case) =>
              val tagName = Name.mkTag(caze.ident)
              macc.get(tagName) match {
                case None => (macc + (tagName -> WeededAst.Case(ident, Name.mkTag(caze.ident), visitType(caze.tpe)))).toSuccess
                case Some(otherTag) =>
                  val enumName = ident.name
                  val loc1 = otherTag.tag.loc
                  val loc2 = mkSL(caze.ident.sp1, caze.ident.sp2)
                  Failure(LazyList(
                    // NB: We report an error at both source locations.
                    DuplicateTag(enumName, tagName, loc1, loc2),
                    DuplicateTag(enumName, tagName, loc2, loc1)
                  ))
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
      val tparamsVal = visitTypeParams(tparams0)

      mapN(modVal, tparamsVal) {
        case (mod, tparams) =>
          val cases = Map(Name.mkTag(ident) -> WeededAst.Case(ident, Name.mkTag(ident), visitType(tpe0)))
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
      val tparamsVal = visitTypeParams(tparams0)

      mapN(modVal, tparamsVal) {
        case (mod, tparams) =>
          val tpe = visitType(tpe0)
          List(WeededAst.Declaration.TypeAlias(doc, mod, ident, tparams, tpe, mkSL(sp1, sp2)))
      }
  }

  /**
    * Rewrites the given relation declaration `r0` to a type alias.
    */
  private def visitRelation(r0: ParsedAst.Declaration.Relation)(implicit flix: Flix): Validation[List[WeededAst.Declaration.TypeAlias], WeederError] = r0 match {
    case ParsedAst.Declaration.Relation(doc0, mod0, sp1, ident, tparams0, attr, sp2) =>
      val doc = visitDoc(doc0)
      val loc = mkSL(sp1, sp2)
      val modVal = visitModifiers(mod0, legalModifiers = Set(Ast.Modifier.Public))
      val tparamsVal = visitTypeParams(tparams0)

      //
      // Rewrite the relation declaration to a type alias.
      //
      mapN(modVal, tparamsVal) {
        case (mod, tparams) =>
          val termTypes = attr.map(a => visitType(a.tpe))
          val tpe = WeededAst.Type.Relation(termTypes.toList, loc)
          List(WeededAst.Declaration.TypeAlias(doc, mod, ident, tparams, tpe, loc))
      }
  }

  /**
    * Performs weeding on the given lattice `r0`.
    */
  private def visitLattice(l0: ParsedAst.Declaration.Lattice)(implicit flix: Flix): Validation[List[WeededAst.Declaration.TypeAlias], WeederError] = l0 match {
    case ParsedAst.Declaration.Lattice(doc0, mod0, sp1, ident, tparams0, attr, sp2) =>
      val doc = visitDoc(doc0)
      val loc = mkSL(sp1, sp2)
      val modVal = visitModifiers(mod0, legalModifiers = Set(Ast.Modifier.Public))
      val tparamsVal = visitTypeParams(tparams0)

      //
      // Rewrite the lattice declaration to a type alias.
      //
      mapN(modVal, tparamsVal) {
        case (mod, tparams) =>
          val termTypes = attr.map(a => visitType(a.tpe))
          val tpe = WeededAst.Type.Lattice(termTypes.toList, loc)
          List(WeededAst.Declaration.TypeAlias(doc, mod, ident, tparams, tpe, loc))
      }
  }

  /**
    * Performs weeding on the given use `u0`.
    */
  private def visitUse(u0: ParsedAst.Use): Validation[List[WeededAst.Use], WeederError] = u0 match {
    case ParsedAst.Use.UseOne(sp1, nname, ident, sp2) =>
      if (ident.isUpper)
        List(WeededAst.Use.UseTypeOrClass(Name.QName(sp1, nname, ident, sp2), ident, mkSL(sp1, sp2))).toSuccess
      else
        List(WeededAst.Use.UseDefOrSig(Name.QName(sp1, nname, ident, sp2), ident, mkSL(sp1, sp2))).toSuccess
    case ParsedAst.Use.UseMany(_, nname, names, _) =>
      val us = names.foldRight(Nil: List[WeededAst.Use]) {
        case (ParsedAst.Use.NameAndAlias(sp1, ident, aliasOpt, sp2), acc) =>
          val alias = aliasOpt.getOrElse(ident)
          if (ident.isUpper)
            WeededAst.Use.UseTypeOrClass(Name.QName(sp1, nname, ident, sp2), alias, mkSL(sp1, sp2)) :: acc
          else
            WeededAst.Use.UseDefOrSig(Name.QName(sp1, nname, ident, sp2), alias, mkSL(sp1, sp2)) :: acc
      }
      us.toSuccess

    case ParsedAst.Use.UseOneTag(sp1, qname, tag, sp2) =>
      List(WeededAst.Use.UseTag(qname, Name.mkTag(tag), tag, mkSL(sp1, sp2))).toSuccess

    case ParsedAst.Use.UseManyTag(sp1, qname, tags, sp2) =>
      val us = tags.foldRight(Nil: List[WeededAst.Use]) {
        case (ParsedAst.Use.NameAndAlias(sp1, ident, aliasOpt, sp2), acc) =>
          val alias = aliasOpt.getOrElse(ident)
          WeededAst.Use.UseTag(qname, Name.mkTag(ident), alias, mkSL(sp1, sp2)) :: acc
      }
      us.toSuccess

  }

  /**
    * Weeds the given expression.
    */
  private def visitExp(exp0: ParsedAst.Expression)(implicit flix: Flix): Validation[WeededAst.Expression, WeederError] = exp0 match {
    case ParsedAst.Expression.SName(sp1, ident, sp2) =>
      WeededAst.Expression.VarOrDefOrSig(ident, mkSL(sp1, sp2)).toSuccess

    case ParsedAst.Expression.QName(sp1, qname, sp2) =>
      WeededAst.Expression.DefOrSig(qname, mkSL(sp1, sp2)).toSuccess

    case ParsedAst.Expression.Hole(sp1, name, sp2) =>
      val loc = mkSL(sp1, sp2)
      WeededAst.Expression.Hole(name, loc).toSuccess

    case ParsedAst.Expression.Use(sp1, use, exp, sp2) =>
      mapN(visitUse(use), visitExp(exp)) {
        case (us, e) => WeededAst.Expression.Use(us, e, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.Lit(sp1, lit, sp2) => lit2exp(lit)

    case ParsedAst.Expression.Intrinsic(sp1, op, exps, sp2) =>
      val loc = mkSL(sp1, sp2)
      flatMapN(traverse(exps)(visitExp)) {
        case es => (op.name, es) match {
          case ("BOOL_NOT", e1 :: Nil) => WeededAst.Expression.Unary(SemanticOperator.BoolOp.Not, e1, loc).toSuccess
          case ("BOOL_AND", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BoolOp.And, e1, e2, loc).toSuccess
          case ("BOOL_OR", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BoolOp.Or, e1, e2, loc).toSuccess
          case ("BOOL_EQ", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BoolOp.Eq, e1, e2, loc).toSuccess
          case ("BOOL_NEQ", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BoolOp.Neq, e1, e2, loc).toSuccess

          case ("CHAR_EQ", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.CharOp.Eq, e1, e2, loc).toSuccess
          case ("CHAR_NEQ", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.CharOp.Neq, e1, e2, loc).toSuccess
          case ("CHAR_LT", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.CharOp.Lt, e1, e2, loc).toSuccess
          case ("CHAR_LE", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.CharOp.Le, e1, e2, loc).toSuccess
          case ("CHAR_GT", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.CharOp.Gt, e1, e2, loc).toSuccess
          case ("CHAR_GE", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.CharOp.Ge, e1, e2, loc).toSuccess

          case ("FLOAT32_NEG", e1 :: Nil) => WeededAst.Expression.Unary(SemanticOperator.Float32Op.Neg, e1, loc).toSuccess
          case ("FLOAT32_ADD", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Float32Op.Add, e1, e2, loc).toSuccess
          case ("FLOAT32_SUB", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Float32Op.Sub, e1, e2, loc).toSuccess
          case ("FLOAT32_MUL", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Float32Op.Mul, e1, e2, loc).toSuccess
          case ("FLOAT32_DIV", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Float32Op.Div, e1, e2, loc).toSuccess
          case ("FLOAT32_REM", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Float32Op.Rem, e1, e2, loc).toSuccess
          case ("FLOAT32_EXP", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Float32Op.Exp, e1, e2, loc).toSuccess
          case ("FLOAT32_EQ", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Float32Op.Eq, e1, e2, loc).toSuccess
          case ("FLOAT32_NEQ", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Float32Op.Neq, e1, e2, loc).toSuccess
          case ("FLOAT32_LT", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Float32Op.Lt, e1, e2, loc).toSuccess
          case ("FLOAT32_LE", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Float32Op.Le, e1, e2, loc).toSuccess
          case ("FLOAT32_GT", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Float32Op.Gt, e1, e2, loc).toSuccess
          case ("FLOAT32_GE", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Float32Op.Ge, e1, e2, loc).toSuccess

          case ("FLOAT64_NEG", e1 :: Nil) => WeededAst.Expression.Unary(SemanticOperator.Float64Op.Neg, e1, loc).toSuccess
          case ("FLOAT64_ADD", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Float64Op.Add, e1, e2, loc).toSuccess
          case ("FLOAT64_SUB", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Float64Op.Sub, e1, e2, loc).toSuccess
          case ("FLOAT64_MUL", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Float64Op.Mul, e1, e2, loc).toSuccess
          case ("FLOAT64_DIV", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Float64Op.Div, e1, e2, loc).toSuccess
          case ("FLOAT64_REM", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Float64Op.Rem, e1, e2, loc).toSuccess
          case ("FLOAT64_EXP", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Float64Op.Exp, e1, e2, loc).toSuccess
          case ("FLOAT64_EQ", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Float64Op.Eq, e1, e2, loc).toSuccess
          case ("FLOAT64_NEQ", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Float64Op.Neq, e1, e2, loc).toSuccess
          case ("FLOAT64_LT", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Float64Op.Lt, e1, e2, loc).toSuccess
          case ("FLOAT64_LE", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Float64Op.Le, e1, e2, loc).toSuccess
          case ("FLOAT64_GT", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Float64Op.Gt, e1, e2, loc).toSuccess
          case ("FLOAT64_GE", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Float64Op.Ge, e1, e2, loc).toSuccess

          case ("INT8_NEG", e1 :: Nil) => WeededAst.Expression.Unary(SemanticOperator.Int8Op.Neg, e1, loc).toSuccess
          case ("INT8_NOT", e1 :: Nil) => WeededAst.Expression.Unary(SemanticOperator.Int8Op.Not, e1, loc).toSuccess
          case ("INT8_ADD", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int8Op.Add, e1, e2, loc).toSuccess
          case ("INT8_SUB", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int8Op.Sub, e1, e2, loc).toSuccess
          case ("INT8_MUL", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int8Op.Mul, e1, e2, loc).toSuccess
          case ("INT8_DIV", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int8Op.Div, e1, e2, loc).toSuccess
          case ("INT8_REM", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int8Op.Rem, e1, e2, loc).toSuccess
          case ("INT8_EXP", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int8Op.Exp, e1, e2, loc).toSuccess
          case ("INT8_AND", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int8Op.And, e1, e2, loc).toSuccess
          case ("INT8_OR", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int8Op.Or, e1, e2, loc).toSuccess
          case ("INT8_XOR", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int8Op.Xor, e1, e2, loc).toSuccess
          case ("INT8_SHL", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int8Op.Shl, e1, e2, loc).toSuccess
          case ("INT8_SHR", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int8Op.Shr, e1, e2, loc).toSuccess
          case ("INT8_EQ", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int8Op.Eq, e1, e2, loc).toSuccess
          case ("INT8_NEQ", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int8Op.Neq, e1, e2, loc).toSuccess
          case ("INT8_LT", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int8Op.Lt, e1, e2, loc).toSuccess
          case ("INT8_LE", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int8Op.Le, e1, e2, loc).toSuccess
          case ("INT8_GT", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int8Op.Gt, e1, e2, loc).toSuccess
          case ("INT8_GE", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int8Op.Ge, e1, e2, loc).toSuccess

          case ("INT16_NEG", e1 :: Nil) => WeededAst.Expression.Unary(SemanticOperator.Int16Op.Neg, e1, loc).toSuccess
          case ("INT16_NOT", e1 :: Nil) => WeededAst.Expression.Unary(SemanticOperator.Int16Op.Not, e1, loc).toSuccess
          case ("INT16_ADD", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int16Op.Add, e1, e2, loc).toSuccess
          case ("INT16_SUB", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int16Op.Sub, e1, e2, loc).toSuccess
          case ("INT16_MUL", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int16Op.Mul, e1, e2, loc).toSuccess
          case ("INT16_DIV", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int16Op.Div, e1, e2, loc).toSuccess
          case ("INT16_REM", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int16Op.Rem, e1, e2, loc).toSuccess
          case ("INT16_EXP", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int16Op.Exp, e1, e2, loc).toSuccess
          case ("INT16_AND", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int16Op.And, e1, e2, loc).toSuccess
          case ("INT16_OR", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int16Op.Or, e1, e2, loc).toSuccess
          case ("INT16_XOR", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int16Op.Xor, e1, e2, loc).toSuccess
          case ("INT16_SHL", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int16Op.Shl, e1, e2, loc).toSuccess
          case ("INT16_SHR", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int16Op.Shr, e1, e2, loc).toSuccess
          case ("INT16_EQ", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int16Op.Eq, e1, e2, loc).toSuccess
          case ("INT16_NEQ", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int16Op.Neq, e1, e2, loc).toSuccess
          case ("INT16_LT", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int16Op.Lt, e1, e2, loc).toSuccess
          case ("INT16_LE", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int16Op.Le, e1, e2, loc).toSuccess
          case ("INT16_GT", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int16Op.Gt, e1, e2, loc).toSuccess
          case ("INT16_GE", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int16Op.Ge, e1, e2, loc).toSuccess

          case ("INT32_NEG", e1 :: Nil) => WeededAst.Expression.Unary(SemanticOperator.Int32Op.Neg, e1, loc).toSuccess
          case ("INT32_NOT", e1 :: Nil) => WeededAst.Expression.Unary(SemanticOperator.Int32Op.Not, e1, loc).toSuccess
          case ("INT32_ADD", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int32Op.Add, e1, e2, loc).toSuccess
          case ("INT32_SUB", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int32Op.Sub, e1, e2, loc).toSuccess
          case ("INT32_MUL", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int32Op.Mul, e1, e2, loc).toSuccess
          case ("INT32_DIV", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int32Op.Div, e1, e2, loc).toSuccess
          case ("INT32_REM", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int32Op.Rem, e1, e2, loc).toSuccess
          case ("INT32_EXP", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int32Op.Exp, e1, e2, loc).toSuccess
          case ("INT32_AND", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int32Op.And, e1, e2, loc).toSuccess
          case ("INT32_OR", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int32Op.Or, e1, e2, loc).toSuccess
          case ("INT32_XOR", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int32Op.Xor, e1, e2, loc).toSuccess
          case ("INT32_SHL", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int32Op.Shl, e1, e2, loc).toSuccess
          case ("INT32_SHR", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int32Op.Shr, e1, e2, loc).toSuccess
          case ("INT32_EQ", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int32Op.Eq, e1, e2, loc).toSuccess
          case ("INT32_NEQ", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int32Op.Neq, e1, e2, loc).toSuccess
          case ("INT32_LT", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int32Op.Lt, e1, e2, loc).toSuccess
          case ("INT32_LE", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int32Op.Le, e1, e2, loc).toSuccess
          case ("INT32_GT", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int32Op.Gt, e1, e2, loc).toSuccess
          case ("INT32_GE", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int32Op.Ge, e1, e2, loc).toSuccess

          case ("INT64_NEG", e1 :: Nil) => WeededAst.Expression.Unary(SemanticOperator.Int64Op.Neg, e1, loc).toSuccess
          case ("INT64_NOT", e1 :: Nil) => WeededAst.Expression.Unary(SemanticOperator.Int64Op.Not, e1, loc).toSuccess
          case ("INT64_ADD", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int64Op.Add, e1, e2, loc).toSuccess
          case ("INT64_SUB", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int64Op.Sub, e1, e2, loc).toSuccess
          case ("INT64_MUL", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int64Op.Mul, e1, e2, loc).toSuccess
          case ("INT64_DIV", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int64Op.Div, e1, e2, loc).toSuccess
          case ("INT64_REM", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int64Op.Rem, e1, e2, loc).toSuccess
          case ("INT64_EXP", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int64Op.Exp, e1, e2, loc).toSuccess
          case ("INT64_AND", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int64Op.And, e1, e2, loc).toSuccess
          case ("INT64_OR", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int64Op.Or, e1, e2, loc).toSuccess
          case ("INT64_XOR", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int64Op.Xor, e1, e2, loc).toSuccess
          case ("INT64_SHL", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int64Op.Shl, e1, e2, loc).toSuccess
          case ("INT64_SHR", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int64Op.Shr, e1, e2, loc).toSuccess
          case ("INT64_EQ", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int64Op.Eq, e1, e2, loc).toSuccess
          case ("INT64_NEQ", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int64Op.Neq, e1, e2, loc).toSuccess
          case ("INT64_LT", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int64Op.Lt, e1, e2, loc).toSuccess
          case ("INT64_LE", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int64Op.Le, e1, e2, loc).toSuccess
          case ("INT64_GT", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int64Op.Gt, e1, e2, loc).toSuccess
          case ("INT64_GE", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.Int64Op.Ge, e1, e2, loc).toSuccess

          case ("BIGINT_NEG", e1 :: Nil) => WeededAst.Expression.Unary(SemanticOperator.BigIntOp.Neg, e1, loc).toSuccess
          case ("BIGINT_NOT", e1 :: Nil) => WeededAst.Expression.Unary(SemanticOperator.BigIntOp.Not, e1, loc).toSuccess
          case ("BIGINT_ADD", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BigIntOp.Add, e1, e2, loc).toSuccess
          case ("BIGINT_SUB", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BigIntOp.Sub, e1, e2, loc).toSuccess
          case ("BIGINT_MUL", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BigIntOp.Mul, e1, e2, loc).toSuccess
          case ("BIGINT_DIV", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BigIntOp.Div, e1, e2, loc).toSuccess
          case ("BIGINT_REM", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BigIntOp.Rem, e1, e2, loc).toSuccess
          case ("BIGINT_EXP", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BigIntOp.Exp, e1, e2, loc).toSuccess
          case ("BIGINT_AND", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BigIntOp.And, e1, e2, loc).toSuccess
          case ("BIGINT_OR", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BigIntOp.Or, e1, e2, loc).toSuccess
          case ("BIGINT_XOR", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BigIntOp.Xor, e1, e2, loc).toSuccess
          case ("BIGINT_SHL", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BigIntOp.Shl, e1, e2, loc).toSuccess
          case ("BIGINT_SHR", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BigIntOp.Shr, e1, e2, loc).toSuccess
          case ("BIGINT_EQ", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BigIntOp.Eq, e1, e2, loc).toSuccess
          case ("BIGINT_NEQ", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BigIntOp.Neq, e1, e2, loc).toSuccess
          case ("BIGINT_LT", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BigIntOp.Lt, e1, e2, loc).toSuccess
          case ("BIGINT_LE", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BigIntOp.Le, e1, e2, loc).toSuccess
          case ("BIGINT_GT", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BigIntOp.Gt, e1, e2, loc).toSuccess
          case ("BIGINT_GE", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.BigIntOp.Ge, e1, e2, loc).toSuccess

          case ("STRING_EQ", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.StringOp.Eq, e1, e2, loc).toSuccess
          case ("STRING_NEQ", e1 :: e2 :: Nil) => WeededAst.Expression.Binary(SemanticOperator.StringOp.Neq, e1, e2, loc).toSuccess

          case _ => WeederError.IllegalIntrinsic(loc).toFailure
        }
      }

    case ParsedAst.Expression.Apply(lambda, args, sp2) =>
      val sp1 = leftMostSourcePosition(lambda)
      val loc = mkSL(sp1, sp2)
      mapN(visitExp(lambda), traverse(args)(e => visitExp(e))) {
        case (e, as) =>
          val es = getArguments(as, sp1, sp2)
          WeededAst.Expression.Apply(e, es, loc)
      }

    case ParsedAst.Expression.Infix(exp1, name, exp2, sp2) =>
      /*
       * Rewrites infix expressions to apply expressions.
       */
      mapN(visitExp(exp1), visitExp(name), visitExp(exp2)) {
        case (e1, lambda, e2) =>
          val loc = mkSL(leftMostSourcePosition(exp1), sp2)
          WeededAst.Expression.Apply(lambda, List(e1, e2), loc)
      }

    case ParsedAst.Expression.Postfix(exp, ident, exps, sp2) =>
      /*
       * Rewrites postfix expressions to apply expressions.
       */
      mapN(visitExp(exp), traverse(exps)(e => visitExp(e))) {
        case (e, es) =>
          val sp1 = leftMostSourcePosition(exp)
          val loc = mkSL(sp1, sp2)
          val lambda = WeededAst.Expression.VarOrDefOrSig(ident, loc)
          WeededAst.Expression.Apply(lambda, e :: es, loc)
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
          // Construct the body of the lambda expression.
          val varOrRef = WeededAst.Expression.VarOrDefOrSig(ident, loc)
          val rule = WeededAst.MatchRule(p, WeededAst.Expression.True(loc), e)

          val fparam = WeededAst.FormalParam(ident, Ast.Modifiers.Empty, None, ident.loc)
          val body = WeededAst.Expression.Match(varOrRef, List(rule), loc)
          WeededAst.Expression.Lambda(fparam, body, loc)
      }

    case ParsedAst.Expression.Unary(sp1, op, exp, sp2) =>
      val loc = mkSL(sp1, sp2)
      visitExp(exp) map {
        case e => op match {
          case "not" => WeededAst.Expression.Unary(SemanticOperator.BoolOp.Not, e, loc)
          case "+" => e
          case "-" => mkApplyFqn("Neg.neg", List(e), sp1, sp2)
          case "~~~" => mkApplyFqn("BitwiseNot.not", List(e), sp1, sp2)
          case _ => mkApplyFqn(op, List(e), sp1, sp2)
        }
      }

    case ParsedAst.Expression.Binary(exp1, op, exp2, sp2) =>
      val sp1 = leftMostSourcePosition(exp1)
      val loc = mkSL(sp1, sp2)
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => op match {
          case "+" => mkApplyFqn("Add.add", List(e1, e2), sp1, sp2)
          case "-" => mkApplyFqn("Sub.sub", List(e1, e2), sp1, sp2)
          case "*" => mkApplyFqn("Mul.mul", List(e1, e2), sp1, sp2)
          case "/" => mkApplyFqn("Div.div", List(e1, e2), sp1, sp2)
          case "%" => mkApplyFqn("Rem.rem", List(e1, e2), sp1, sp2)
          case "**" => mkApplyFqn("Exp.exp", List(e1, e2), sp1, sp2)
          case "<" => mkApplyFqn("Order.less", List(e1, e2), sp1, sp2)
          case "<=" => mkApplyFqn("Order.lessEqual", List(e1, e2), sp1, sp2)
          case ">" => mkApplyFqn("Order.greater", List(e1, e2), sp1, sp2)
          case ">=" => mkApplyFqn("Order.greaterEqual", List(e1, e2), sp1, sp2)
          case "==" => mkApplyFqn("Eq.eq", List(e1, e2), sp1, sp2)
          case "!=" => mkApplyFqn("Eq.neq", List(e1, e2), sp1, sp2)
          case "<=>" => mkApplyFqn("Order.compare", List(e1, e2), sp1, sp2)
          case "and" => WeededAst.Expression.Binary(SemanticOperator.BoolOp.And, e1, e2, loc)
          case "or" => WeededAst.Expression.Binary(SemanticOperator.BoolOp.Or, e1, e2, loc)
          case "&&&" => mkApplyFqn("BitwiseAnd.and", List(e1, e2), sp1, sp2)
          case "|||" => mkApplyFqn("BitwiseOr.or", List(e1, e2), sp1, sp2)
          case "^^^" => mkApplyFqn("BitwiseXor.xor", List(e1, e2), sp1, sp2)
          case "<<<" => mkApplyFqn("BitwiseShl.shl", List(e1, e2), sp1, sp2)
          case ">>>" => mkApplyFqn("BitwiseShr.shr", List(e1, e2), sp1, sp2)
          case _ => mkApplyIdent(op, List(e1, e2), sp1, sp2)
        }
      }

    case ParsedAst.Expression.IfThenElse(sp1, exp1, exp2, exp3, sp2) =>
      mapN(visitExp(exp1), visitExp(exp2), visitExp(exp3)) {
        case (e1, e2, e3) => WeededAst.Expression.IfThenElse(e1, e2, e3, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.Stm(exp1, exp2, sp2) =>
      val sp1 = leftMostSourcePosition(exp1)
      mapN(visitExp(exp1), visitExp(exp2)) {
        case (e1, e2) => WeededAst.Expression.Stm(e1, e2, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.LetMatch(sp1, mod0, pat, tpe, exp1, exp2, sp2) =>
      //
      // Rewrites a let-match to a regular let-binding or a full-blown pattern match.
      //
      flatMapN(visitPattern(pat), visitExp(exp1), visitExp(exp2)) {
        case (WeededAst.Pattern.Var(ident, loc), value, body) =>
          // No pattern match.
          mapN(visitModifiers(mod0, legalModifiers = Set(Ast.Modifier.Scoped))) {
            mod => WeededAst.Expression.Let(ident, mod, withAscription(value, tpe), body, mkSL(sp1, sp2))
          }
        case (pat, value, body) =>
          // Full-blown pattern match.
          mapN(visitModifiers(mod0, legalModifiers = Set.empty)) {
            mod =>
              val rule = WeededAst.MatchRule(pat, WeededAst.Expression.True(mkSL(sp1, sp2)), body)
              WeededAst.Expression.Match(withAscription(value, tpe), List(rule), mkSL(sp1, sp2))
          }
      }

    case ParsedAst.Expression.LetMatchStar(sp1, pat, tpe, exp1, exp2, sp2) =>
      val loc = SourceLocation.mk(sp1, sp2)

      //
      // Rewrites a monadic let-match to a regular let-binding or a full-blown pattern match inside a flatMap.
      //
      // let* x = exp1; exp2     ==>   flatMap(x -> exp2)(exp1)
      //
      val ident = Name.Ident(sp1, "flatMap", sp2)
      val flatMap = WeededAst.Expression.VarOrDefOrSig(ident, loc)

      mapN(visitPattern(pat), visitExp(exp1), visitExp(exp2)) {
        case (WeededAst.Pattern.Var(ident, loc), value, body) =>
          // No pattern match.
          val fparam = WeededAst.FormalParam(ident, Ast.Modifiers.Empty, tpe.map(visitType), loc)
          val lambda = WeededAst.Expression.Lambda(fparam, body, loc)
          val inner = WeededAst.Expression.Apply(flatMap, List(lambda), loc)
          WeededAst.Expression.Apply(inner, List(value), loc)
        case (pat, value, body) =>
          // Full-blown pattern match.
          val lambdaIdent = Name.Ident(sp1, "pat$0", sp2)
          val lambdaVar = WeededAst.Expression.VarOrDefOrSig(lambdaIdent, loc)

          val rule = WeededAst.MatchRule(pat, WeededAst.Expression.True(mkSL(sp1, sp2)), body)
          val lambdaBody = WeededAst.Expression.Match(withAscription(lambdaVar, tpe), List(rule), mkSL(sp1, sp2))

          val fparam = WeededAst.FormalParam(lambdaIdent, Ast.Modifiers.Empty, tpe.map(visitType), loc)
          val lambda = WeededAst.Expression.Lambda(fparam, lambdaBody, loc)
          val inner = WeededAst.Expression.Apply(flatMap, List(lambda), loc)
          WeededAst.Expression.Apply(inner, List(value), loc)
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
                return WeededAst.Expression.Let(ident, Ast.Modifiers.Empty, e1, e2, loc).toSuccess
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
                  WeededAst.Expression.VarOrDefOrSig(ident, loc)
              }

              // Assemble the lambda expression.
              val lambdaBody = WeededAst.Expression.InvokeConstructor(className, as, ts, loc)
              val e1 = mkCurried(fs, lambdaBody, loc)
              WeededAst.Expression.Let(ident, Ast.Modifiers.Empty, e1, e2, loc)
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
              val objExp = WeededAst.Expression.VarOrDefOrSig(objId, loc)

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
                  WeededAst.Expression.VarOrDefOrSig(ident, loc)
              }

              // Assemble the lambda expression.
              val lambdaBody = WeededAst.Expression.InvokeMethod(className, methodName, as.head, as.tail, ts, loc)
              val e1 = mkCurried(fs, lambdaBody, loc)
              WeededAst.Expression.Let(ident, Ast.Modifiers.Empty, e1, e2, loc)
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
                return WeededAst.Expression.Let(ident, Ast.Modifiers.Empty, e1, e2, loc).toSuccess
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
                  WeededAst.Expression.VarOrDefOrSig(ident, loc)
              }

              // Assemble the lambda expression.
              val lambdaBody = WeededAst.Expression.InvokeStaticMethod(className, methodName, as, ts, loc)
              val e1 = mkCurried(fs, lambdaBody, loc)
              WeededAst.Expression.Let(ident, Ast.Modifiers.Empty, e1, e2, loc)
          }

        case ParsedAst.JvmOp.GetField(fqn, ident) =>
          //
          // Introduce a let-bound lambda: o -> GetField(o).
          //
          mapN(parseClassAndMember(fqn, loc), visitExp(exp2)) {
            case ((className, fieldName), e2) =>
              val objectId = Name.Ident(sp1, "o$", sp2)
              val objectExp = WeededAst.Expression.VarOrDefOrSig(objectId, loc)
              val objectParam = WeededAst.FormalParam(objectId, Ast.Modifiers.Empty, None, loc)
              val lambdaBody = WeededAst.Expression.GetField(className, fieldName, objectExp, loc)
              val e1 = WeededAst.Expression.Lambda(objectParam, lambdaBody, loc)
              WeededAst.Expression.Let(ident, Ast.Modifiers.Empty, e1, e2, loc)
          }

        case ParsedAst.JvmOp.PutField(fqn, ident) =>
          //
          // Introduce a let-bound lambda: (o, v) -> PutField(o, v)
          //
          mapN(parseClassAndMember(fqn, loc), visitExp(exp2)) {
            case ((className, fieldName), e2) =>
              val objectId = Name.Ident(sp1, "o$", sp2)
              val valueId = Name.Ident(sp1, "v$", sp2)
              val objectExp = WeededAst.Expression.VarOrDefOrSig(objectId, loc)
              val valueExp = WeededAst.Expression.VarOrDefOrSig(valueId, loc)
              val objectParam = WeededAst.FormalParam(objectId, Ast.Modifiers.Empty, None, loc)
              val valueParam = WeededAst.FormalParam(valueId, Ast.Modifiers.Empty, None, loc)
              val lambdaBody = WeededAst.Expression.PutField(className, fieldName, objectExp, valueExp, loc)
              val e1 = mkCurried(objectParam :: valueParam :: Nil, lambdaBody, loc)
              WeededAst.Expression.Let(ident, Ast.Modifiers.Empty, e1, e2, loc)
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
              WeededAst.Expression.Let(ident, Ast.Modifiers.Empty, e1, e2, loc)
          }

        case ParsedAst.JvmOp.PutStaticField(fqn, ident) =>
          //
          // Introduce a let-bound lambda: x -> PutStaticField(x).
          //
          mapN(parseClassAndMember(fqn, loc), visitExp(exp2)) {
            case ((className, fieldName), e2) =>
              val valueId = Name.Ident(sp1, "v$", sp2)
              val valueExp = WeededAst.Expression.VarOrDefOrSig(valueId, loc)
              val valueParam = WeededAst.FormalParam(valueId, Ast.Modifiers.Empty, None, loc)
              val lambdaBody = WeededAst.Expression.PutStaticField(className, fieldName, valueExp, loc)
              val e1 = WeededAst.Expression.Lambda(valueParam, lambdaBody, loc)
              WeededAst.Expression.Let(ident, Ast.Modifiers.Empty, e1, e2, loc)
          }
      }

    case ParsedAst.Expression.LetRegion(sp1, ident, exp, sp2) =>
      mapN(visitExp(exp)) {
        case e => WeededAst.Expression.LetRegion(ident, e, mkSL(sp1, sp2))
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

    case ParsedAst.Expression.Choose(sp1, star, exps, rules, sp2) =>
      //
      // Check for mismatched arity of `exps` and `rules`.
      //
      val expectedArity = exps.length
      for (ParsedAst.ChoiceRule(sp1, pat, _, sp2) <- rules) {
        val actualArity = pat.length
        if (actualArity != expectedArity) {
          return WeederError.MismatchedArity(expectedArity, actualArity, mkSL(sp1, sp2)).toFailure
        }
      }

      val expsVal = traverse(exps)(visitExp)
      val rulesVal = traverse(rules) {
        case ParsedAst.ChoiceRule(_, pat, exp, _) =>
          val p = pat.map {
            case ParsedAst.ChoicePattern.Wild(sp1, sp2) => WeededAst.ChoicePattern.Wild(mkSL(sp1, sp2))
            case ParsedAst.ChoicePattern.Absent(sp1, sp2) => WeededAst.ChoicePattern.Absent(mkSL(sp1, sp2))
            case ParsedAst.ChoicePattern.Present(sp1, ident, sp2) => WeededAst.ChoicePattern.Present(ident, mkSL(sp1, sp2))
          }
          mapN(visitExp(exp)) {
            case e => WeededAst.ChoiceRule(p.toList, e)
          }
      }
      mapN(expsVal, rulesVal) {
        case (es, rs) => WeededAst.Expression.Choose(star, es, rs, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.Tag(sp1, qname, expOpt, sp2) =>
      val (enum, tag) = asTag(qname)

      expOpt match {
        case None =>
          // Case 1: The tag does not have an expression. Nothing more to be done.
          WeededAst.Expression.Tag(enum, Name.mkTag(tag), None, mkSL(sp1, sp2)).toSuccess
        case Some(exp) =>
          // Case 2: The tag has an expression. Perform weeding on it.
          mapN(visitExp(exp)) {
            case e => WeededAst.Expression.Tag(enum, Name.mkTag(tag), Some(e), mkSL(sp1, sp2))
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
        case ParsedAst.RecordField(fsp1, ident, exp, fsp2) =>
          flatMapN(visitExp(exp)) {
            case e =>
              if (ident.name == "length")
                WeederError.IllegalFieldName(ident.loc).toFailure
              else
                (ident -> e).toSuccess
          }
      }

      mapN(fieldsVal) {
        case fs =>
          // Rewrite into a sequence of nested record extensions.
          val zero = WeededAst.Expression.RecordEmpty(mkSL(sp1, sp2))
          fs.foldRight(zero: WeededAst.Expression) {
            case ((ident, e), acc) => WeededAst.Expression.RecordExtend(Name.mkField(ident), e, acc, mkSL(sp1, sp2))
          }
      }

    case ParsedAst.Expression.RecordSelect(exp, ident, sp2) =>
      val sp1 = leftMostSourcePosition(exp)
      mapN(visitExp(exp)) {
        case e =>
          // Special Case: Array Length
          if (ident.name == "length")
            WeededAst.Expression.ArrayLength(e, mkSL(sp1, sp2))
          else
            WeededAst.Expression.RecordSelect(e, Name.mkField(ident), mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.RecordSelectLambda(sp1, ident, sp2) =>
      val loc = mkSL(sp1, sp2)
      val ident = Name.Ident(sp1, "_rec", sp2)
      val fparam = WeededAst.FormalParam(ident, Ast.Modifiers.Empty, None, loc)
      val varExp = WeededAst.Expression.VarOrDefOrSig(ident, loc)
      val lambdaBody = WeededAst.Expression.RecordSelect(varExp, Name.mkField(ident), loc)
      WeededAst.Expression.Lambda(fparam, lambdaBody, loc).toSuccess

    case ParsedAst.Expression.RecordOperation(_, ops, rest, _) =>
      // We translate the sequence of record operations into a nested tree using a fold right.
      foldRight(ops)(visitExp(rest)) {
        case (ParsedAst.RecordOp.Extend(sp1, ident, exp, sp2), acc) =>
          flatMapN(visitExp(exp)) {
            case e =>
              if (ident.name == "length")
                WeederError.IllegalFieldName(ident.loc).toFailure
              else
                WeededAst.Expression.RecordExtend(Name.mkField(ident), e, acc, mkSL(sp1, sp2)).toSuccess
          }

        case (ParsedAst.RecordOp.Restrict(sp1, ident, sp2), acc) =>
          if (ident.name == "length")
            WeederError.IllegalFieldName(ident.loc).toFailure
          else
            WeededAst.Expression.RecordRestrict(Name.mkField(ident), acc, mkSL(sp1, sp2)).toSuccess

        case (ParsedAst.RecordOp.Update(sp1, ident, exp, sp2), acc) =>
          flatMapN(visitExp(exp)) {
            case e =>
              if (ident.name == "length")
                WeederError.IllegalFieldName(ident.loc).toFailure
              else {
                // An update is a restrict followed by an extension.
                val inner = WeededAst.Expression.RecordRestrict(Name.mkField(ident), acc, mkSL(sp1, sp2))
                WeededAst.Expression.RecordExtend(Name.mkField(ident), e, inner, mkSL(sp1, sp2)).toSuccess
              }
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

    case ParsedAst.Expression.FNil(sp1, sp2) =>
      /*
       * Rewrites a `FNil` expression into a tag expression.
       */
      val loc = mkSL(sp1, sp2)
      val tag = Name.Tag("Nil", loc)
      val exp = WeededAst.Expression.Unit(loc)
      WeededAst.Expression.Tag(None, tag, Some(exp), loc).toSuccess

    case ParsedAst.Expression.FCons(hd, sp1, sp2, tl) =>
      /*
       * Rewrites a `FCons` expression into a tag expression.
       */
      mapN(visitExp(hd), visitExp(tl)) {
        case (e1, e2) =>
          val loc = mkSL(sp1, sp2)
          val tag = Name.Tag("Cons", loc)
          val exp = WeededAst.Expression.Tuple(List(e1, e2), loc)
          WeededAst.Expression.Tag(None, tag, Some(exp), loc)
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

      /**
        * Returns an expression that concatenates the result of the expression `e1` with the expression `e2`.
        */
      def mkConcat(e1: WeededAst.Expression, e2: WeededAst.Expression, loc: SourceLocation): WeededAst.Expression = {
        val sop = SemanticOperator.StringOp.Concat
        WeededAst.Expression.Binary(sop, e1, e2, loc)
      }

      /**
        * Returns an expression that applies `toString` to the result of the given expression `e`.
        */
      def mkApplyToString(e: WeededAst.Expression, sp1: SourcePosition, sp2: SourcePosition): WeededAst.Expression = {
        val fqn = "ToString.toString"
        mkApplyFqn(fqn, List(e), sp1, sp2)
      }

      val loc = mkSL(sp1, sp2)

      parts match {
        case Seq(ParsedAst.InterpolationPart.StrPart(innerSp1, chars, innerSp2)) =>
          // Special case: We have a constant string. Check the contents and return it.
          weedCharSequence(chars) map {
            string => WeededAst.Expression.Str(string, mkSL(innerSp1, innerSp2))
          }

        case _ =>
          // General Case: Fold the interpolator parts together.
          val init = WeededAst.Expression.Str("", loc)
          Validation.fold(parts, init: WeededAst.Expression) {
            case (acc, ParsedAst.InterpolationPart.ExpPart(innerSp1, exp, innerSp2)) =>
              mapN(visitExp(exp)) {
                case e =>
                  val e2 = mkApplyToString(e, innerSp1, innerSp2)
                  mkConcat(acc, e2, mkSL(innerSp1, innerSp2))
              }
            case (acc, ParsedAst.InterpolationPart.StrPart(innerSp1, chars, innerSp2)) =>
              weedCharSequence(chars) map {
                string =>
                  val e2 = WeededAst.Expression.Str(string, mkSL(innerSp1, innerSp2))
                  mkConcat(acc, e2, loc)
              }
          }
      }

    case ParsedAst.Expression.Ref(sp1, exp, reg, sp2) => reg match {
      case None =>
        for {
          e <- visitExp(exp)
        } yield WeededAst.Expression.Ref(e, mkSL(sp1, sp2))

      case Some(exp2) =>
        for {
          e <- visitExp(exp)
          e2 <- visitExp(exp2)
        } yield WeededAst.Expression.RefWithRegion(e, e2, mkSL(sp1, sp2))
    }

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

    case ParsedAst.Expression.Spawn(sp1, exp, sp2) =>
      visitExp(exp) map {
        case e => WeededAst.Expression.Spawn(e, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.Lazy(sp1, exp, sp2) =>
      visitExp(exp) map {
        case e => WeededAst.Expression.Lazy(e, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.Force(sp1, exp, sp2) =>
      visitExp(exp) map {
        case e => WeededAst.Expression.Force(e, mkSL(sp1, sp2))
      }

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
          WeededAst.Expression.FixpointMerge(e1, e2, mkSL(sp1, sp2))
      }

    case ParsedAst.Expression.FixpointProjectInto(sp1, exps, idents, sp2) =>
      val loc = mkSL(sp1, sp2)

      ///
      /// Check for [[MismatchedArity]].
      ///
      if (exps.length != idents.length) {
        return WeederError.MismatchedArity(exps.length, idents.length, loc).toFailure
      }

      mapN(traverse(exps)(visitExp)) {
        case es =>
          val init = WeededAst.Expression.FixpointConstraintSet(Nil, loc)
          es.zip(idents.toList).foldRight(init: WeededAst.Expression) {
            case ((exp, ident), acc) =>
              val pred = Name.mkPred(ident)
              val innerExp = WeededAst.Expression.FixpointProjectIn(exp, pred, loc)
              WeededAst.Expression.FixpointMerge(innerExp, acc, loc)
          }
      }

    case ParsedAst.Expression.FixpointSolveWithProject(sp1, exps, optIdents, sp2) =>
      val loc = mkSL(sp1, sp2)
      mapN(traverse(exps)(visitExp)) {
        case es =>
          //
          // Performs the following rewrite:
          //
          // solve e1, e2, e3 project P1, P2, P3
          //
          // =>
          //
          // let $tmp = solve (merge e1, 2, e3);
          // merge (project P1 $tmp, project P2 $tmp, project P3 $tmp)

          // Introduce a $tmp variable that holds the minimal model of the merge of the exps.
          val freshVar = flix.genSym.freshId()
          val localVar = Name.Ident(sp1, s"tmp$freshVar", sp2)

          // Merge all the exps into one Datalog program value.
          val mergeExp = es.reduceRight[WeededAst.Expression] {
            case (e, acc) => WeededAst.Expression.FixpointMerge(e, acc, loc)
          }
          val modelExp = WeededAst.Expression.FixpointSolve(mergeExp, loc)

          // Any projections?
          val bodyExp = optIdents match {
            case None =>
              // Case 1: No projections: Simply return the minimal model.
              WeededAst.Expression.VarOrDefOrSig(localVar, loc)
            case Some(idents) =>
              // Case 2: A non-empty sequence of predicate symbols to project.

              // Construct a list of each projection.
              val projectExps = idents.map {
                case ident =>
                  val varExp = WeededAst.Expression.VarOrDefOrSig(localVar, loc)
                  WeededAst.Expression.FixpointFilter(Name.Pred(ident.name, loc), varExp, loc)
              }

              // Merge all of the projections into one result.
              projectExps.reduceRight[WeededAst.Expression] {
                case (e, acc) => WeededAst.Expression.FixpointMerge(e, acc, loc)
              }
          }

          // Bind the $tmp variable to the minimal model and combine it with the body expression.
          WeededAst.Expression.Let(localVar, Ast.Modifiers.Empty, modelExp, bodyExp, loc)
      }

    case ParsedAst.Expression.FixpointQueryWithSelect(sp1, exps0, selects0, from0, whereExp0, sp2) =>
      val loc = mkSL(sp1, sp2)
      val selects1 = selects0 match {
        case ParsedAst.SelectFragment(exps, None) => exps
        case ParsedAst.SelectFragment(exps, Some(exp)) => exps.toList ::: exp :: Nil
      }

      mapN(traverse(exps0)(visitExp), traverse(selects1)(visitExp), traverse(from0)(visitPredicateBody), traverse(whereExp0)(visitExp)) {
        case (exps, selects, from, where) =>
          //
          // Performs the following rewrite:
          //
          // query e1, e2, e3 select (x, y, z) from A(x, y), B(z) where x > 0
          //
          // =>
          //
          // facts $Result (solve (merge (merge e1, 2, e3) #{ #Result(x, y, z) :- A(x, y), B(y) if x > 0 } )

          // The fresh predicate name where to store the result of the query.
          val pred = Name.Pred("$Result", loc)

          // The head of the pseudo-rule.
          val den = selects0 match {
            case ParsedAst.SelectFragment(_, None) => Denotation.Relational
            case ParsedAst.SelectFragment(_, Some(_)) => Denotation.Latticenal
          }
          val head = WeededAst.Predicate.Head.Atom(pred, den, selects, loc)

          // The body of the pseudo-rule.
          val body = where match {
            case Nil => from
            case g :: Nil => WeededAst.Predicate.Body.Guard(g, loc) :: from
            case _ => throw InternalCompilerException("Impossible. The list must have 0 or 1 elements.")
          }

          // Construct the pseudo-query.
          val pseudoConstraint = WeededAst.Constraint(head, body, loc)

          // Construct a constraint set that contains the single pseudo constraint.
          val queryExp = WeededAst.Expression.FixpointConstraintSet(List(pseudoConstraint), loc)

          // Construct the merge of all the expressions.
          val dbExp = exps.reduceRight[WeededAst.Expression] {
            case (e, acc) => WeededAst.Expression.FixpointMerge(e, acc, loc)
          }

          // Extract the tuples of the result predicate.
          WeededAst.Expression.FixpointProjectOut(pred, queryExp, dbExp, loc)
      }

    case ParsedAst.Expression.MatchEff(sp1, exp1, exp2, exp3, sp2) =>
      mapN(visitExp(exp1), visitExp(exp2), visitExp(exp3)) {
        case (e1, e2, e3) => WeededAst.Expression.MatchEff(e1, e2, e3, mkSL(sp1, sp2))
      }

  }

  /**
    * Translates the hex code into the corresponding character.
    * Returns an error if the code is not hexadecimal.
    */
  private def translateHexCode(code: String, loc: SourceLocation): Validation[Char, WeederError] = {
    try {
      Integer.parseInt(code, 16).toChar.toSuccess
    } catch {
      case _: NumberFormatException => WeederError.MalformedUnicodeEscapeSequence(code, loc).toFailure
    }
  }

  /**
    * Performs weeding on the given sequence of CharCodes.
    */
  private def weedCharSequence(chars0: Seq[ParsedAst.CharCode]): Validation[String, WeederError] = {

    @tailrec
    def visit(chars: List[ParsedAst.CharCode], acc: List[Char]): Validation[String, WeederError] = {
      chars match {
        // Case 1: End of the sequence
        case Nil => acc.reverse.mkString.toSuccess
        // Case 2: Simple character literal
        case ParsedAst.CharCode.Literal(_, char, _) :: rest => visit(rest, char.head :: acc)
        // Case 3: Escape sequence
        case (esc@ParsedAst.CharCode.Escape(sp1, char, sp2)) :: rest => char match {
          // Cases 3.1: Standard escapes
          case "n" => visit(rest, '\n' :: acc)
          case "r" => visit(rest, '\r' :: acc)
          case "\\" => visit(rest, '\\' :: acc)
          case "\"" => visit(rest, '\"' :: acc)
          case "\'" => visit(rest, '\'' :: acc)
          case "t" => visit(rest, '\t' :: acc)

          // Case 3.2: Unicode escape
          case "u" => rest match {
            // Case 3.2.1: `\u` followed by 4 or more literals
            case ParsedAst.CharCode.Literal(sp1, d0, _) ::
              ParsedAst.CharCode.Literal(_, d1, _) ::
              ParsedAst.CharCode.Literal(_, d2, _) ::
              ParsedAst.CharCode.Literal(_, d3, sp2) ::
              rest2 =>
              val code = List(d0, d1, d2, d3).mkString
              // Doing a manual flatMap to keep the function tail-recursive
              translateHexCode(code, mkSL(sp1, sp2)) match {
                case Validation.Success(char) => visit(rest2, char :: acc)
                case Validation.Failure(errors) => Validation.Failure(errors)
              }
            // Case 3.2.2: `\u` followed by less than 4 literals
            case rest2 =>
              val code = rest2.takeWhile(_.isInstanceOf[ParsedAst.CharCode.Literal])
              val sp2 = code.lastOption.getOrElse(esc).sp2
              WeederError.MalformedUnicodeEscapeSequence(code.mkString, mkSL(esc.sp1, sp2)).toFailure
          }

          // Case 3.3: Invalid escape character
          case _ => WeederError.InvalidEscapeSequence(char.head, mkSL(sp1, sp2)).toFailure
        }
      }
    }

    visit(chars0.toList, Nil)
  }

  /**
    * Translates the given literal to an expression.
    */
  private def lit2exp(lit0: ParsedAst.Literal)(implicit flix: Flix): Validation[WeededAst.Expression, WeederError] = lit0 match {
    case ParsedAst.Literal.Unit(sp1, sp2) =>
      WeededAst.Expression.Unit(mkSL(sp1, sp2)).toSuccess

    case ParsedAst.Literal.Null(sp1, sp2) =>
      WeededAst.Expression.Null(mkSL(sp1, sp2)).toSuccess

    case ParsedAst.Literal.True(sp1, sp2) =>
      WeededAst.Expression.True(mkSL(sp1, sp2)).toSuccess

    case ParsedAst.Literal.False(sp1, sp2) =>
      WeededAst.Expression.False(mkSL(sp1, sp2)).toSuccess

    case ParsedAst.Literal.Char(sp1, chars, sp2) =>
      weedCharSequence(chars) flatMap {
        case string if string.lengthIs == 1 => WeededAst.Expression.Char(string.head, mkSL(sp1, sp2)).toSuccess
        case string => WeederError.NonSingleCharacter(string, mkSL(sp1, sp2)).toFailure
      }

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

    case ParsedAst.Literal.Str(sp1, chars, sp2) =>
      weedCharSequence(chars) map {
        string => WeededAst.Expression.Str(string, mkSL(sp1, sp2))
      }

    case ParsedAst.Literal.Default(sp1, sp2) =>
      WeededAst.Expression.Default(mkSL(sp1, sp2)).toSuccess
  }

  /**
    * Weeds the given pattern.
    */
  private def visitLitPat(pat0: ParsedAst.Literal): Validation[WeededAst.Pattern, WeederError] = pat0 match {
    case ParsedAst.Literal.Unit(sp1, sp2) => WeededAst.Pattern.Unit(mkSL(sp1, sp2)).toSuccess
    case ParsedAst.Literal.Null(sp1, sp2) => WeederError.IllegalNullPattern(mkSL(sp1, sp2)).toFailure
    case ParsedAst.Literal.True(sp1, sp2) => WeededAst.Pattern.True(mkSL(sp1, sp2)).toSuccess
    case ParsedAst.Literal.False(sp1, sp2) => WeededAst.Pattern.False(mkSL(sp1, sp2)).toSuccess
    case ParsedAst.Literal.Char(sp1, chars, sp2) =>
      weedCharSequence(chars) flatMap {
        case string if string.lengthIs == 1 => WeededAst.Pattern.Char(string.head, mkSL(sp1, sp2)).toSuccess
        case string => WeederError.NonSingleCharacter(string, mkSL(sp1, sp2)).toFailure
      }
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
    case ParsedAst.Literal.Str(sp1, chars, sp2) =>
      weedCharSequence(chars) map {
        string => WeededAst.Pattern.Str(string, mkSL(sp1, sp2))
      }
    case ParsedAst.Literal.Default(sp1, sp2) =>
      throw InternalCompilerException(s"Illegal default pattern near: ${mkSL(sp1, sp2).format}")
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
            WeededAst.Pattern.Tag(enum, Name.mkTag(tag), lit, loc).toSuccess
          case Some(pat) => visit(pat) map {
            case p => WeededAst.Pattern.Tag(enum, Name.mkTag(tag), p, mkSL(sp1, sp2))
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
        flatMapN(traverse(pats)(visit)) {
          case elms if ident.name == "_" => WeededAst.Pattern.ArrayTailSpread(elms, None, mkSL(sp2, sp2)).toSuccess
          case elms =>
            seen.get(ident.name) match {
              case None =>
                seen += (ident.name -> ident)
                WeededAst.Pattern.ArrayTailSpread(elms, Some(ident), mkSL(sp1, sp2)).toSuccess
              case Some(otherIdent) =>
                NonLinearPattern(ident.name, otherIdent.loc, mkSL(sp1, sp2)).toFailure
            }
        }

      case ParsedAst.Pattern.ArrayHeadSpread(sp1, ident, pats, sp2) =>
        flatMapN(traverse(pats)(visit)) {
          case elms if ident.name == "_" => WeededAst.Pattern.ArrayHeadSpread(None, elms, mkSL(sp1, sp2)).toSuccess
          case elms =>
            seen.get(ident.name) match {
              case None =>
                seen += (ident.name -> ident)
                WeededAst.Pattern.ArrayHeadSpread(Some(ident), elms, mkSL(sp1, sp2)).toSuccess
              case Some(otherIdent) =>
                NonLinearPattern(ident.name, otherIdent.loc, mkSL(sp1, sp2)).toFailure
            }
        }

      case ParsedAst.Pattern.FNil(sp1, sp2) =>
        /*
         * Rewrites a `FNil` pattern into a tag pattern.
         */
        val loc = mkSL(sp1, sp2)
        val tag = Name.Tag("Nil", loc)
        val pat = WeededAst.Pattern.Unit(loc)
        WeededAst.Pattern.Tag(None, tag, pat, loc).toSuccess

      case ParsedAst.Pattern.FCons(pat1, sp1, sp2, pat2) =>
        /*
         * Rewrites a `FCons` pattern into a tag pattern.
         */
        mapN(visitPattern(pat1), visitPattern(pat2)) {
          case (hd, tl) =>
            val loc = mkSL(sp1, sp2)
            val tag = Name.Tag("Cons", loc)
            val pat = WeededAst.Pattern.Tuple(List(hd, tl), loc)
            WeededAst.Pattern.Tag(None, tag, pat, loc)
        }

    }

    visit(pattern)
  }

  /**
    * Performs weeding on the given constraint `c0`.
    */
  private def visitConstraint(c0: ParsedAst.Constraint)(implicit flix: Flix): Validation[WeededAst.Constraint, WeederError] = c0 match {
    case ParsedAst.Constraint(sp1, head0, body0, sp2) =>
      val headVal = visitHeadPredicate(head0)
      val bodyVal = traverse(body0)(visitPredicateBody)

      mapN(headVal, bodyVal) {
        case (h, bs) => WeededAst.Constraint(h, bs, mkSL(sp1, sp2))
      }
  }

  /**
    * Weeds the given head predicate.
    */
  private def visitHeadPredicate(past: ParsedAst.Predicate.Head)(implicit flix: Flix): Validation[WeededAst.Predicate.Head, WeederError] = past match {
    case ParsedAst.Predicate.Head.Atom(sp1, ident, terms, None, sp2) =>
      // Case 1: the atom has a relational denotation (because of the absence of the optional lattice term).
      val loc = mkSL(sp1, sp2)
      mapN(traverse(terms)(visitExp)) {
        case ts => WeededAst.Predicate.Head.Atom(Name.mkPred(ident), Denotation.Relational, ts, loc)
      }

    case ParsedAst.Predicate.Head.Atom(sp1, ident, terms, Some(term), sp2) =>
      // Case 2: the atom has a latticenal denotation (because of the presence of the optional lattice term).
      val loc = mkSL(sp1, sp2)
      mapN(traverse(terms)(visitExp), visitExp(term)) {
        case (ts, t) => WeededAst.Predicate.Head.Atom(Name.Pred(ident.name, mkSL(ident.sp1, ident.sp2)), Denotation.Latticenal, ts ::: t :: Nil, loc)
      }
  }

  /**
    * Weeds the given body predicate.
    */
  private def visitPredicateBody(b: ParsedAst.Predicate.Body)(implicit flix: Flix): Validation[WeededAst.Predicate.Body, WeederError] = b match {
    case ParsedAst.Predicate.Body.Atom(sp1, polarity, ident, terms, None, sp2) =>
      // Case 1: the atom has a relational denotation (because of the absence of the optional lattice term).
      val loc = mkSL(sp1, sp2)
      mapN(traverse(terms)(visitPattern)) {
        case ts =>
          WeededAst.Predicate.Body.Atom(Name.mkPred(ident), Denotation.Relational, polarity, ts, loc)
      }

    case ParsedAst.Predicate.Body.Atom(sp1, polarity, ident, terms, Some(term), sp2) =>
      // Case 2: the atom has a latticenal denotation (because of the presence of the optional lattice term).
      val loc = mkSL(sp1, sp2)
      mapN(traverse(terms)(visitPattern), visitPattern(term)) {
        case (ts, t) =>
          WeededAst.Predicate.Body.Atom(Name.mkPred(ident), Denotation.Latticenal, polarity, ts ::: t :: Nil, loc)
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
          val b = WeededAst.Expression.DefOrSig(qname, loc)
          val e = WeededAst.Expression.Apply(b, as, loc)
          WeededAst.Predicate.Body.Guard(e, loc)
      }
  }

  /**
    * Weeds the given sequence of parsed annotation `xs`.
    */
  private def visitAnnotations(xs: Seq[ParsedAst.Annotation])(implicit flix: Flix): Validation[List[WeededAst.Annotation], WeederError] = {
    // collect seen annotations.
    val seen = mutable.Map.empty[String, ParsedAst.Annotation]

    // loop through each annotation.
    val result = xs.toList.collect {
      case x: ParsedAst.Annotation => seen.get(x.ident.name) match {
        case None =>
          seen += (x.ident.name -> x)
          visitAnnotation(x)
        case Some(otherAnn) =>
          val name = x.ident.name
          val loc1 = mkSL(otherAnn.sp1, otherAnn.sp2)
          val loc2 = mkSL(x.sp1, x.sp2)
          Failure(LazyList(
            // NB: We report an error at both source locations.
            DuplicateAnnotation(name, loc1, loc2),
            DuplicateAnnotation(name, loc2, loc1),
          ))
      }
    }

    sequence(result)
  }

  /**
    * Weeds the given parsed annotation `past`.
    */
  private def visitAnnotation(past: ParsedAst.Annotation)(implicit flix: Flix): Validation[WeededAst.Annotation, WeederError] = {
    val loc = mkSL(past.sp1, past.sp2)
    val argsVal = traverse(past.args.getOrElse(Nil))(visitExp)

    flatMapN(argsVal) {
      case args =>
        past.ident.name match {
          case "benchmark" => WeededAst.Annotation(Ast.Annotation.Benchmark(loc), args, loc).toSuccess
          case "deprecated" => WeededAst.Annotation(Ast.Annotation.Deprecated(loc), args, loc).toSuccess
          case "law" => WeededAst.Annotation(Ast.Annotation.Law(loc), args, loc).toSuccess
          case "lint" => WeededAst.Annotation(Ast.Annotation.Lint(loc), args, loc).toSuccess
          case "test" => WeededAst.Annotation(Ast.Annotation.Test(loc), args, loc).toSuccess
          case "unchecked" => WeededAst.Annotation(Ast.Annotation.Unchecked(loc), args, loc).toSuccess
          case "Time" => WeededAst.Annotation(Ast.Annotation.Time(loc), args, loc).toSuccess
          case "Space" => WeededAst.Annotation(Ast.Annotation.Space(loc), args, loc).toSuccess
          case name => WeederError.UndefinedAnnotation(name, loc).toFailure
        }
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
            val name = modifier.name
            val loc1 = mkSL(other.sp1, other.sp2)
            val loc2 = mkSL(modifier.sp1, modifier.sp2)
            Failure(LazyList(
              // NB: We report an error at both source locations.
              WeederError.DuplicateModifier(name, loc1, loc2),
              WeederError.DuplicateModifier(name, loc2, loc1)
            ))
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
      case "lawless" => Ast.Modifier.Lawless
      case "override" => Ast.Modifier.Override
      case "pub" => Ast.Modifier.Public
      case "sealed" => Ast.Modifier.Sealed
      case "scoped" => Ast.Modifier.Scoped
      case "unlawful" => Ast.Modifier.Unlawful
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
    * Returns an error if `public` is not among the modifiers in `mods`.
    */
  private def requirePublic(mods: Ast.Modifiers, ident: Name.Ident): Validation[Unit, WeederError] = {
    if (mods.isPublic) {
      ().toSuccess
    } else {
      WeederError.IllegalPrivateDeclaration(ident, ident.loc).toFailure
    }
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
      def buildRecord(base: WeededAst.Type): WeededAst.Type = {
        fields.foldRight(base) {
          case (ParsedAst.RecordFieldType(ssp1, ident, t, ssp2), acc) =>
            WeededAst.Type.RecordExtend(Name.mkField(ident), visitType(t), acc, mkSL(ssp1, ssp2))
        }
      }

      (fields, restOpt) match {
        // Case 1: `{| r}` Polymorphic record with no fields. `r` must be a `Record` variable.
        case (Nil, Some(ident)) => WeededAst.Type.RecordGeneric(WeededAst.Type.Var(ident, ident.loc), mkSL(sp1, sp2))
        // Case 2: `{x: Int}` Nonpolymorphic record. Base must be an empty record.
        case (_, None) => buildRecord(WeededAst.Type.RecordEmpty(mkSL(sp1, sp2)))
        // Case 3: `{x: Int | r}` Polymorphic record with field. `r` must be a `Record` variable.
        case (_, Some(base)) => buildRecord(WeededAst.Type.Var(base, base.loc))
      }

    case ParsedAst.Type.Schema(sp1, predicates, restOpt, sp2) =>
      def buildSchema(base: WeededAst.Type): WeededAst.Type = {
        predicates.foldRight(base) {
          case (ParsedAst.PredicateType.PredicateWithAlias(ssp1, qname, targs, ssp2), acc) =>
            val ts = targs match {
              case None => Nil
              case Some(xs) => xs.map(visitType).toList
            }
            WeededAst.Type.SchemaExtendByAlias(qname, ts, acc, mkSL(ssp1, ssp2))

          case (ParsedAst.PredicateType.RelPredicateWithTypes(ssp1, name, ts, ssp2), acc) =>
            WeededAst.Type.SchemaExtendByTypes(name, Ast.Denotation.Relational, ts.toList.map(visitType), acc, mkSL(ssp1, ssp2))

          case (ParsedAst.PredicateType.LatPredicateWithTypes(ssp1, name, ts, tpe, ssp2), acc) =>
            WeededAst.Type.SchemaExtendByTypes(name, Ast.Denotation.Latticenal, ts.toList.map(visitType) ::: visitType(tpe) :: Nil, acc, mkSL(ssp1, ssp2))
        }
      }

      (predicates, restOpt) match {
        // Case 1: `#{| r}` Polymorphic schema with no fields. `r` must be a `Schema` variable.
        case (Nil, Some(ident)) => WeededAst.Type.SchemaGeneric(WeededAst.Type.Var(restOpt.get, restOpt.get.loc), mkSL(sp1, sp2))
        // Case 2: `#{X(Int)}` Nonpolymorphic schema. Base must be an empty schema.
        case (_, None) => buildSchema(WeededAst.Type.SchemaEmpty(mkSL(sp1, sp2)))
        // Case 3: `#{X(Int) | r}` Polymorphic schema with field. `r` must be a `Schema` variable.
        case (_, Some(ident)) => buildSchema(WeededAst.Type.Var(ident, mkSL(sp1, sp2)))
      }

    case ParsedAst.Type.UnaryImpureArrow(tpe1, tpe2, sp2) =>
      val loc = mkSL(leftMostSourcePosition(tpe1), sp2)
      val t1 = visitType(tpe1)
      val t2 = visitType(tpe2)
      val eff = WeededAst.Type.False(loc)
      mkArrow(t1, eff, t2, loc)

    case ParsedAst.Type.UnaryPolymorphicArrow(tpe1, tpe2, effOpt, sp2) =>
      val loc = mkSL(leftMostSourcePosition(tpe1), sp2)
      val t1 = visitType(tpe1)
      val t2 = visitType(tpe2)
      val eff = effOpt match {
        // NB: If there is no explicit effect then the arrow is pure.
        case None => WeededAst.Type.True(loc)
        case Some(f) => visitType(f)
      }
      mkArrow(t1, eff, t2, loc)

    case ParsedAst.Type.ImpureArrow(sp1, tparams, tresult, sp2) =>
      val loc = mkSL(sp1, sp2)
      val ts = tparams.map(visitType)
      val tr = visitType(tresult)
      val eff = WeededAst.Type.False(loc)
      mkCurriedArrow(ts, eff, tr, loc)

    case ParsedAst.Type.PolymorphicArrow(sp1, tparams, tresult, effOpt, sp2) =>
      val loc = mkSL(sp1, sp2)
      val ts = tparams.map(visitType)
      val tr = visitType(tresult)
      val eff = effOpt match {
        // NB: If there is no explicit effect then the arrow is pure.
        case None => WeededAst.Type.True(loc)
        case Some(f) => visitType(f)
      }
      mkCurriedArrow(ts, eff, tr, loc)

    case ParsedAst.Type.Native(sp1, fqn, sp2) =>
      WeededAst.Type.Native(fqn.mkString("."), mkSL(sp1, sp2))

    case ParsedAst.Type.Apply(t1, args, sp2) =>
      // Curry the type arguments.
      val sp1 = leftMostSourcePosition(t1)
      args.foldLeft(visitType(t1)) {
        case (acc, t2) => WeededAst.Type.Apply(acc, visitType(t2), mkSL(sp1, sp2))
      }

    case ParsedAst.Type.True(sp1, sp2) =>
      WeededAst.Type.True(mkSL(sp1, sp2))

    case ParsedAst.Type.False(sp1, sp2) =>
      WeededAst.Type.False(mkSL(sp1, sp2))

    case ParsedAst.Type.Not(sp1, tpe, sp2) =>
      val t = visitType(tpe)
      WeededAst.Type.Not(t, mkSL(sp1, sp2))

    case ParsedAst.Type.And(tpe1, tpe2, sp2) =>
      val sp1 = leftMostSourcePosition(tpe1)
      val t1 = visitType(tpe1)
      val t2 = visitType(tpe2)
      WeededAst.Type.And(t1, t2, mkSL(sp1, sp2))

    case ParsedAst.Type.Or(tpe1, tpe2, sp2) =>
      val sp1 = leftMostSourcePosition(tpe1)
      val t1 = visitType(tpe1)
      val t2 = visitType(tpe2)
      WeededAst.Type.Or(t1, t2, mkSL(sp1, sp2))

    case ParsedAst.Type.Ascribe(tpe, kind, sp2) =>
      val sp1 = leftMostSourcePosition(tpe)
      val t = visitType(tpe)
      val k = visitKind(kind)
      WeededAst.Type.Ascribe(t, k, mkSL(sp1, sp2))
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
    tparams.init.foldRight(base)(mkArrow(_, WeededAst.Type.True(loc), _, loc))
  }

  /**
    * Weeds the given parsed optional effect `effOpt`.
    */
  private def visitEff(effOpt: Option[ParsedAst.Type], loc: SourceLocation)(implicit flix: Flix): Validation[WeededAst.Type, WeederError] = effOpt match {
    case None => WeededAst.Type.True(loc).toSuccess
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

          visitModifiers(mods, legalModifiers = Set(Ast.Modifier.Inline, Ast.Modifier.Scoped)) flatMap {
            case mod =>
              if (typeRequired && typeOpt.isEmpty)
                IllegalFormalParameter(ident.name, mkSL(sp1, sp2)).toFailure
              else
                WeededAst.FormalParam(ident, mod, typeOpt.map(visitType), mkSL(sp1, sp2)).toSuccess
          }
        case Some(otherParam) =>
          val name = ident.name
          val loc1 = mkSL(otherParam.sp1, otherParam.sp2)
          val loc2 = mkSL(param.sp1, param.sp2)
          Failure(LazyList(
            // NB: We report an error at both source locations.
            DuplicateFormalParam(name, loc1, loc2),
            DuplicateFormalParam(name, loc2, loc1)
          ))
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
  private def visitTypeParams(tparams0: ParsedAst.TypeParams): Validation[WeededAst.TypeParams, WeederError] = tparams0 match {
    case ParsedAst.TypeParams.Elided => WeededAst.TypeParams.Elided.toSuccess
    case ParsedAst.TypeParams.Explicit(tparams) =>
      val newTparams = tparams.map(visitTypeParam)
      val kindedTypeParams = newTparams.collect { case t: WeededAst.TypeParam.Kinded => t }
      val unkindedTypeParams = newTparams.collect { case t: WeededAst.TypeParam.Unkinded => t }
      (kindedTypeParams, unkindedTypeParams) match {
        // Case 1: only unkinded type parameters
        case (Nil, _ :: _) => WeededAst.TypeParams.Unkinded(unkindedTypeParams).toSuccess
        // Case 2: only kinded type parameters
        case (_ :: _, Nil) => WeededAst.TypeParams.Kinded(kindedTypeParams).toSuccess
        // Case 3: some unkinded and some kinded
        case (_ :: _, _ :: _) =>
          val loc = mkSL(tparams.head.sp1, tparams.last.sp2)
          WeederError.InconsistentTypeParameters(loc).toFailure
        // Case 4: no type parameters: should be prevented by parser
        case (Nil, Nil) => throw InternalCompilerException("Unexpected empty type parameters.")
      }
  }

  /**
    * Weeds the type params, requiring that they be explicitly kinded.
    */
  private def visitKindedTypeParams(tparams0: ParsedAst.TypeParams): Validation[WeededAst.KindedTypeParams, WeederError] = tparams0 match {
    case ParsedAst.TypeParams.Elided => WeededAst.TypeParams.Elided.toSuccess
    case ParsedAst.TypeParams.Explicit(tparams) =>
      val newTparams = tparams.map(visitTypeParam)
      val kindedTypeParams = newTparams.collect { case t: WeededAst.TypeParam.Kinded => t }
      val unkindedTypeParams = newTparams.collect { case t: WeededAst.TypeParam.Unkinded => t }
      (kindedTypeParams, unkindedTypeParams) match {
        // Case 1: some unkinded type params
        case (_, _ :: _) =>
          val loc = mkSL(tparams.head.sp1, tparams.last.sp2)
          WeederError.UnkindedTypeParameters(loc).toFailure
        // Case 2: only kinded type parameters
        case (_ :: _, Nil) => WeededAst.TypeParams.Kinded(kindedTypeParams).toSuccess
        // Case 3: no type parameters: should be prevented by parser
        case (Nil, Nil) => throw InternalCompilerException("Unexpected empty type parameters.")
      }
  }

  /**
    * Weeds the given type param `tparam`.
    */
  private def visitTypeParam(tparam0: ParsedAst.TypeParam): WeededAst.TypeParam = tparam0 match {
    case ParsedAst.TypeParam(sp1, ident, kind0, sp2) =>
      kind0.map(visitKind) match {
        case None => WeededAst.TypeParam.Unkinded(ident)
        case Some(kind) => WeededAst.TypeParam.Kinded(ident, kind)
      }
  }

  /**
    * Weeds the given kind `kind`.
    */
  private def visitKind(kind: ParsedAst.Kind): Kind = kind match {
    case ParsedAst.Kind.Star(sp1, sp2) => Kind.Star
    case ParsedAst.Kind.Bool(sp1, sp2) => Kind.Bool
    case ParsedAst.Kind.Record(sp1, sp2) => Kind.Record
    case ParsedAst.Kind.Schema(sp1, sp2) => Kind.Schema
    case ParsedAst.Kind.Arrow(k1, k2, sp2) => Kind.Arrow(visitKind(k1), visitKind(k2))
  }

  /**
    * Weeds the given type constraint `tconstr`.
    */
  private def visitTypeConstraint(tconstr: ParsedAst.TypeConstraint): Validation[WeededAst.TypeConstraint, WeederError] = tconstr match {
    case ParsedAst.TypeConstraint(sp1, clazz, tparam0, sp2) =>
      val tpe = visitType(tparam0)
      if (isAllVars(tpe)) {
        WeededAst.TypeConstraint(clazz, tpe, mkSL(sp1, sp2)).toSuccess
      } else {
        WeederError.IllegalTypeConstraintParameter(mkSL(sp1, sp2)).toFailure
      }
  }

  // MATT docs
  private def isAllVars(tpe: WeededAst.Type): Boolean = tpe match {
    case _: WeededAst.Type.Var => true
    case WeededAst.Type.Apply(tpe1, tpe2, _) => isAllVars(tpe1) && isAllVars(tpe2)
    case _ => false
  }

  /**
    * Returns an apply expression for the given fully-qualified name `fqn` and the given arguments `args`.
    */
  private def mkApplyFqn(fqn: String, args: List[WeededAst.Expression], sp1: SourcePosition, sp2: SourcePosition): WeededAst.Expression = {
    val lambda = WeededAst.Expression.DefOrSig(Name.mkQName(fqn, sp1, sp2), mkSL(sp1, sp2))
    WeededAst.Expression.Apply(lambda, args, mkSL(sp1, sp2))
  }

  /**
    * Returns an apply expression for the given ident `ident` and the given arguments `args`.
    */
  private def mkApplyIdent(ident: String, args: List[WeededAst.Expression], sp1: SourcePosition, sp2: SourcePosition): WeededAst.Expression = {
    val lambda = WeededAst.Expression.VarOrDefOrSig(Name.Ident(sp1, ident, sp2), mkSL(sp1, sp2))
    WeededAst.Expression.Apply(lambda, args, mkSL(sp1, sp2))

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
  private def getArguments(args0: List[WeededAst.Expression], sp1: SourcePosition, sp2: SourcePosition): List[WeededAst.Expression] = args0 match {
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
    case ParsedAst.Expression.Intrinsic(sp1, _, _, _) => sp1
    case ParsedAst.Expression.Apply(e1, _, _) => leftMostSourcePosition(e1)
    case ParsedAst.Expression.Infix(e1, _, _, _) => leftMostSourcePosition(e1)
    case ParsedAst.Expression.Postfix(e1, _, _, _) => leftMostSourcePosition(e1)
    case ParsedAst.Expression.Lambda(sp1, _, _, _) => sp1
    case ParsedAst.Expression.LambdaMatch(sp1, _, _, _) => sp1
    case ParsedAst.Expression.Unary(sp1, _, _, _) => sp1
    case ParsedAst.Expression.Binary(e1, _, _, _) => leftMostSourcePosition(e1)
    case ParsedAst.Expression.IfThenElse(sp1, _, _, _, _) => sp1
    case ParsedAst.Expression.Stm(e1, _, _) => leftMostSourcePosition(e1)
    case ParsedAst.Expression.LetMatch(sp1, _, _, _, _, _, _) => sp1
    case ParsedAst.Expression.LetMatchStar(sp1, _, _, _, _, _) => sp1
    case ParsedAst.Expression.LetImport(sp1, _, _, _) => sp1
    case ParsedAst.Expression.LetRegion(sp1, _, _, _) => sp1
    case ParsedAst.Expression.Match(sp1, _, _, _) => sp1
    case ParsedAst.Expression.Choose(sp1, _, _, _, _) => sp1
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
    case ParsedAst.Expression.FNil(sp1, _) => sp1
    case ParsedAst.Expression.FCons(hd, _, _, _) => leftMostSourcePosition(hd)
    case ParsedAst.Expression.FAppend(fst, _, _, _) => leftMostSourcePosition(fst)
    case ParsedAst.Expression.FSet(sp1, _, _) => sp1
    case ParsedAst.Expression.FMap(sp1, _, _) => sp1
    case ParsedAst.Expression.Interpolation(sp1, _, _) => sp1
    case ParsedAst.Expression.Ref(sp1, _, _, _) => sp1
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
    case ParsedAst.Expression.Spawn(sp1, _, _) => sp1
    case ParsedAst.Expression.Lazy(sp1, _, _) => sp1
    case ParsedAst.Expression.Force(sp1, _, _) => sp1
    case ParsedAst.Expression.FixpointConstraint(sp1, _, _) => sp1
    case ParsedAst.Expression.FixpointConstraintSet(sp1, _, _) => sp1
    case ParsedAst.Expression.FixpointCompose(e1, _, _) => leftMostSourcePosition(e1)
    case ParsedAst.Expression.FixpointProjectInto(sp1, _, _, _) => sp1
    case ParsedAst.Expression.FixpointSolveWithProject(sp1, _, _, _) => sp1
    case ParsedAst.Expression.FixpointQueryWithSelect(sp1, _, _, _, _, _) => sp1
    case ParsedAst.Expression.MatchEff(sp1, _, _, _, _) => sp1
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
    case ParsedAst.Type.UnaryImpureArrow(tpe1, _, _) => leftMostSourcePosition(tpe1)
    case ParsedAst.Type.UnaryPolymorphicArrow(tpe1, _, _, _) => leftMostSourcePosition(tpe1)
    case ParsedAst.Type.ImpureArrow(sp1, _, _, _) => sp1
    case ParsedAst.Type.PolymorphicArrow(sp1, _, _, _, _) => sp1
    case ParsedAst.Type.Native(sp1, _, _) => sp1
    case ParsedAst.Type.Apply(tpe1, _, _) => leftMostSourcePosition(tpe1)
    case ParsedAst.Type.True(sp1, _) => sp1
    case ParsedAst.Type.False(sp1, _) => sp1
    case ParsedAst.Type.Not(sp1, _, _) => sp1
    case ParsedAst.Type.And(tpe1, _, _) => leftMostSourcePosition(tpe1)
    case ParsedAst.Type.Or(tpe1, _, _) => leftMostSourcePosition(tpe1)
    case ParsedAst.Type.Ascribe(tpe, _, _) => leftMostSourcePosition(tpe)
  }

  /**
    * Returns the left most source position in the sub-tree of the kind `kind`.
    */
  @tailrec
  private def leftMostSourcePosition(kind: ParsedAst.Kind): SourcePosition = kind match {
    case ParsedAst.Kind.Star(sp1, _) => sp1
    case ParsedAst.Kind.Bool(sp1, _) => sp1
    case ParsedAst.Kind.Record(sp1, _) => sp1
    case ParsedAst.Kind.Schema(sp1, _) => sp1
    case ParsedAst.Kind.Arrow(k1, _, _) => leftMostSourcePosition(k1)
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

}
