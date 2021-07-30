/*
 * Copyright 2021 Matthew Lutze
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
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.errors.KindError
import ca.uwaterloo.flix.language.phase.unification.KindInferMonad.seqM
import ca.uwaterloo.flix.language.phase.unification.KindUnification.unifyKindM
import ca.uwaterloo.flix.language.phase.unification.{KindInferMonad, KindSubstitution}
import ca.uwaterloo.flix.util.Validation.{ToFailure, ToSuccess, flatMapN, mapN, traverse}
import ca.uwaterloo.flix.util.{InternalCompilerException, Result, Validation}

// MATT docs
object Kinder extends Phase[ResolvedAst.Root, KindedAst.Root] {

  /**
    * Runs the p
    */
  override def run(root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Root, CompilationError] = flix.phase("Kinder") {
    val enumsVal = Validation.traverse(root.enums) {
      case (sym, enum) => visitEnum2(enum, root).map((sym, _))
    }

    val classesVal = Validation.traverse(root.classes) {
      case (sym, clazz) => visitClass(clazz, root).map((sym, _))
    }

    val defsVal = Validation.traverse(root.defs) {
      case (sym, defn) => visitDef2(defn, root).map((sym, _))
    }

    val instancesVal = Validation.traverse(root.instances) {
      case (sym, insts0) => traverse(insts0)(visitInstance(_, root)).map((sym, _))
    }

    mapN(enumsVal, classesVal, defsVal, instancesVal) {
      case (enums, classes, defs, instances) =>
        KindedAst.Root(classes.toMap, instances.toMap, defs.toMap, enums.toMap, root.reachable, root.sources)
    }

  }

  private def visitEnum2(enum: ResolvedAst.Enum, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Enum, CompilationError] = enum match {
    case ResolvedAst.Enum(doc, mod, sym, tparams0, cases0, tpeDeprecated0, sc0, loc) =>
      val kinds = getKindsFromTparamsDefaultStar(tparams0)

      val tparamsVal = Validation.traverse(tparams0.tparams)(ascribeTparam(_, kinds))
      val casesVal = Validation.traverse(cases0) {
        case (tag, case0) => mapN(ascribeCase(case0, kinds, root)) {
          caze => (tag, caze)
        }
      }
      val tpeDeprecatedVal = ascribeType(tpeDeprecated0, KindMatch.subKindOf(KindMatch.Template.Star), kinds, root)
      val scVal = ascribeScheme(sc0, kinds, root)

      mapN(tparamsVal, casesVal, tpeDeprecatedVal, scVal) {
        case (tparams, cases, tpeDeprecated, sc) => KindedAst.Enum(doc, mod, sym, tparams, cases.toMap, tpeDeprecated, sc, loc)
      }
  }

  private def ascribeTparam(tparam: ResolvedAst.TypeParam, kinds: Map[UnkindedType.Var, Kind]): Validation[KindedAst.TypeParam, KindError] = tparam match {
    // MATT don't really need monad here because this should never fail
    case ResolvedAst.TypeParam.Kinded(name, tpe0, _, loc) =>
      mapN(ascribeTypeVar(tpe0, KindMatch.wild, kinds)) {
        tpe => KindedAst.TypeParam(name, tpe, loc)
      }
    case ResolvedAst.TypeParam.Unkinded(name, tpe0, loc) =>
      mapN(ascribeTypeVar(tpe0, KindMatch.wild, kinds)) {
        tpe => KindedAst.TypeParam(name, tpe, loc)
      }
  }
  private def ascribeCase(caze0: ResolvedAst.Case, kinds: Map[UnkindedType.Var, Kind], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Case, KindError] = caze0 match {
    case ResolvedAst.Case(enum, tag, tpeDeprecated0, sc0) =>
      for {
        tpeDeprecated <- ascribeType(tpeDeprecated0, KindMatch.subKindOf(KindMatch.Template.Star), kinds, root)
        sc <- ascribeScheme(sc0, kinds, root)
      } yield KindedAst.Case(enum, tag, tpeDeprecated, sc)
  }

  private def ascribeTypeVar(tvar: UnkindedType.Var, kindMatch: KindMatch, kinds: Map[UnkindedType.Var, Kind]): Validation[Type.Var, KindError] = tvar match {
    case tvar@UnkindedType.Var(id, _, text) =>
      kinds.get(tvar) match {
        // MATT I don't know if we should be here
        // Case 1: we don't know about this kind, just ascribe it with what the context expects
        case None => tvar.ascribedWith(kindMatch.kind.toKind2).toSuccess
        // Case 2: we know about this kind, make sure it's behaving as we expect
        case Some(actualKind) =>
          if (KindMatch.matches(actualKind, kindMatch)) {
            Type.Var(id, actualKind, text = text).toSuccess
          } else {
            val expectedKind = KindMatch.Template.toKind(kindMatch.kind)
            KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = actualKind, SourceLocation.Unknown).toFailure
            // MATT get real source loc
          }
      }

  }

  private def ascribeFreeTypeVar(tvar: UnkindedType.Var, kindMatch: KindMatch): Type.Var = tvar match {
    case UnkindedType.Var(id, _, text) =>
      val kind = KindMatch.Template.toKind(kindMatch.kind)
      Type.Var(id, kind, text = text)
  }

  private def ascribeType(tpe0: UnkindedType, expectedKind: KindMatch, kinds: Map[UnkindedType.Var, Kind], root: ResolvedAst.Root)(implicit flix: Flix): Validation[Type, KindError] = tpe0 match {
    case tvar: UnkindedType.Var => ascribeTypeVar(tvar, expectedKind, kinds)
    case UnkindedType.Cst(cst, loc) =>
      val tycon = reassembleTypeConstructor(cst, root)
      val kind = tycon.kind
      if (KindMatch.matches(kind, expectedKind)) {
        Type.Cst(tycon, loc).toSuccess
      } else {
        KindError.UnexpectedKind(expectedKind = KindMatch.Template.toKind(expectedKind.kind), actualKind = kind, loc).toFailure
      }
    case UnkindedType.Apply(t10, t20) =>
      for {
        t2 <- ascribeType(t20, KindMatch.wild, kinds, root)
        k1 = KindMatch.subKindOf(KindMatch.Template.Arrow(KindMatch.Template.fromKind(t2.kind), expectedKind.kind))
        t1 <- ascribeType(t10, k1, kinds, root)
      } yield Type.Apply(t1, t2)
    case UnkindedType.Lambda(t10, t20) =>
      expectedKind match {
        case KindMatch(_, KindMatch.Template.Arrow(expK1, expK2)) =>
          val t1 = ascribeFreeTypeVar(t10, KindMatch.subKindOf(expK1))
          val newKinds = kinds + (t10 -> t1.kind)
          for {
            t2 <- ascribeType(t20, KindMatch.subKindOf(expK2), newKinds, root)
          } yield Type.Lambda(t1, t2)
        case _ => ??? // MATT KindError (maybe we can accept Wild here?)
      }
  }

  private def ascribeScheme(sc: ResolvedAst.Scheme, kinds: Map[UnkindedType.Var, Kind], root: ResolvedAst.Root)(implicit flix: Flix): Validation[Scheme, KindError] = sc match {
    case ResolvedAst.Scheme(quantifiers0, constraints0, base0) =>
      for {
        quantifiers <- Validation.traverse(quantifiers0)(ascribeTypeVar(_, KindMatch.wild, kinds))
        constraints <- Validation.traverse(constraints0)(ascribeTypeConstraint(_, kinds, root))
        base <- ascribeType(base0, KindMatch.subKindOf(KindMatch.Template.Star), kinds, root)
      } yield Scheme(quantifiers, constraints, base)
  }

  private def ascribeTypeConstraint(tconstr: ResolvedAst.TypeConstraint, kinds: Map[UnkindedType.Var, Kind], root: ResolvedAst.Root)(implicit flix: Flix): Validation[Ast.TypeConstraint, KindError] = tconstr match {
    case ResolvedAst.TypeConstraint(clazz, tpe0, loc) =>
      val classKind = getClassKind(root.classes(clazz))
      mapN(ascribeType(tpe0, KindMatch.subKindOf(KindMatch.Template.fromKind(classKind)), kinds, root)) {
        tpe => Ast.TypeConstraint(clazz, tpe, loc)
      }
  }

  // MATT docs
  private def visitEnum(enum: ResolvedAst.Enum, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Enum, CompilationError] = enum match {
    case ResolvedAst.Enum(doc, mod, sym, tparams0, cases0, tpeDeprecated, sc0, loc) =>
      val initialSubst = getSubstFromTparamsDefaultStar(tparams0)

      val inference = for {
        _ <- inferScheme(sc0, root)
        _ <- inferType(tpeDeprecated, root)
        _ <- seqM(cases0.values.map(inferCase(_, root)).toList)
      } yield ()

      val KindInferMonad(run) = inference

      run(initialSubst) match {
        case Result.Ok((subst, _)) =>
          val tparams = reassembleTparams(tparams0, subst, root)
          val cases = cases0.map {
            case (tag, caze) => (tag, reassembleCase(caze, subst, root))
          }
          val tpe = reassembleType(tpeDeprecated, subst, root)
          val sc = reassembleScheme(sc0, subst, root)
          KindedAst.Enum(doc, mod, sym, tparams, cases, tpe, sc, loc).toSuccess
        case Result.Err(e) => Validation.Failure(LazyList(e))
      }
  }


  // MATT docs
  private def visitClass(clazz: ResolvedAst.Class, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Class, CompilationError] = clazz match {
    case ResolvedAst.Class(doc, mod, sym, tparam0, superClasses0, sigs0, laws0, loc) =>
      val initialSubst = getSubstFromTparamDefaultStar(tparam0)

      val inference = for {
        _ <- seqM(superClasses0.map(inferTconstr(_, root)))
      } yield ()

      val KindInferMonad(run) = inference

      val sigsVal = traverse(sigs0) {
        case (sigSym, sig0) => visitSig(sig0, inference, root).map(sig => sigSym -> sig)
      }
      val lawsVal = traverse(laws0)(visitDef(_, inference, root))

      run(initialSubst) match {
        case Result.Ok((subst, _)) =>
          val tparam = reassembleTparam(tparam0, subst, root)
          val superClasses = superClasses0.map(reassembleTconstr(_, subst, root))
          mapN(sigsVal, lawsVal) {
            case (sigs, laws) => KindedAst.Class(doc, mod, sym, tparam, superClasses, sigs.toMap, laws, loc)
          }
        case Result.Err(e) => Validation.Failure(LazyList(e))
      }

  }

  // MATT docs
  private def visitInstance(inst: ResolvedAst.Instance, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Instance, CompilationError] = inst match {
    case ResolvedAst.Instance(doc, mod, sym, tpe0, tconstrs0, defs0, ns, loc) =>
      val clazz = root.classes(sym)
      val expectedKind = getClassKind(clazz)

      val inference = for {
        kind <- inferType(tpe0, root)
        _ <- unifyKindM(kind, expectedKind, loc) // MATT better loc
        _ <- seqM(tconstrs0.map(inferTconstr(_, root)))
      } yield ()

      val KindInferMonad(run) = inference

      val defsVal = traverse(defs0)(visitDef(_, inference, root))

      // MATT instances should have internally explicit tparams
      // MATT so we don't run this on empty
      run(KindSubstitution.empty) match {
        case Result.Ok((subst, _)) =>
          val tpe = reassembleType(tpe0, subst, root)
          val tconstrs = tconstrs0.map(reassembleTconstr(_, subst, root))
          mapN(defsVal) {
            defs => KindedAst.Instance(doc, mod, sym, tpe, tconstrs, defs, ns, loc)
          }
        case Result.Err(e) => Validation.Failure(LazyList(e))
      }
  }

  // MATT docs
  def getEnumKind(enum: ResolvedAst.Enum)(implicit flix: Flix): Kind = enum match {
    case ResolvedAst.Enum(_, _, _, tparams, _, _, _, _) =>
      val subst = getSubstFromTparamsDefaultStar(tparams)
      tparams.tparams.foldRight(Kind.Star: Kind) { // MATT is foldRight right?
        case (tparam, acc) => subst(tparam.tpe.kvar) ->: acc
      }
  }

  // MATT docs
  def getClassKind(clazz: ResolvedAst.Class): Kind = clazz.tparam match {
    case ResolvedAst.TypeParam.Kinded(_, _, kind, _) => kind
    case _: ResolvedAst.TypeParam.Unkinded => Kind.Star
  }

  // MATT docs
  def reassembleTypeConstructor(tycon: UnkindedType.Constructor, root: ResolvedAst.Root)(implicit flix: Flix): TypeConstructor = tycon match {
    case UnkindedType.Constructor.Unit => TypeConstructor.Unit
    case UnkindedType.Constructor.Null => TypeConstructor.Null
    case UnkindedType.Constructor.Bool => TypeConstructor.Bool
    case UnkindedType.Constructor.Char => TypeConstructor.Char
    case UnkindedType.Constructor.Float32 => TypeConstructor.Float32
    case UnkindedType.Constructor.Float64 => TypeConstructor.Float64
    case UnkindedType.Constructor.Int8 => TypeConstructor.Int8
    case UnkindedType.Constructor.Int16 => TypeConstructor.Int16
    case UnkindedType.Constructor.Int32 => TypeConstructor.Int32
    case UnkindedType.Constructor.Int64 => TypeConstructor.Int64
    case UnkindedType.Constructor.BigInt => TypeConstructor.BigInt
    case UnkindedType.Constructor.Str => TypeConstructor.Str
    case UnkindedType.Constructor.Arrow(arity) => TypeConstructor.Arrow(arity)
    case UnkindedType.Constructor.RecordEmpty => TypeConstructor.RecordEmpty
    case UnkindedType.Constructor.RecordExtend(field) => TypeConstructor.RecordExtend(field)
    case UnkindedType.Constructor.SchemaEmpty => TypeConstructor.SchemaEmpty
    case UnkindedType.Constructor.SchemaExtend(pred) => TypeConstructor.SchemaExtend(pred)
    case UnkindedType.Constructor.Array => TypeConstructor.Array
    case UnkindedType.Constructor.Channel => TypeConstructor.Channel
    case UnkindedType.Constructor.Lazy => TypeConstructor.Lazy
    case UnkindedType.Constructor.Tag(sym, tag) => TypeConstructor.Tag(sym, tag)
    case UnkindedType.Constructor.Enum(sym) =>
      // Lookup the enum kind
      val kind = getEnumKind(root.enums(sym)) // MATT any reason to expect a bad lookup here?
      TypeConstructor.Enum(sym, kind)
    case UnkindedType.Constructor.Native(clazz) => TypeConstructor.Native(clazz)
    case UnkindedType.Constructor.ScopedRef => TypeConstructor.ScopedRef
    case UnkindedType.Constructor.Tuple(l) => TypeConstructor.Tuple(l)
    case UnkindedType.Constructor.Relation => TypeConstructor.Relation
    case UnkindedType.Constructor.Lattice => TypeConstructor.Lattice
    case UnkindedType.Constructor.True => TypeConstructor.True
    case UnkindedType.Constructor.False => TypeConstructor.False
    case UnkindedType.Constructor.Not => TypeConstructor.Not
    case UnkindedType.Constructor.And => TypeConstructor.And
    case UnkindedType.Constructor.Or => TypeConstructor.Or
    case UnkindedType.Constructor.Region => TypeConstructor.Region
  }

  private def visitSig(sig0: ResolvedAst.Sig, inf0: KindInferMonad[Unit], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Sig, KindError] = sig0 match {
    case ResolvedAst.Sig(sym, spec0, expOpt0) =>
      val inference = for {
        _ <- inf0
        _ <- inferSpec(spec0, root)
      } yield ()


      flatMapN(visitSpec(spec0, inference, root)) {
        spec =>
          val KindInferMonad(run) = inference
          run(KindSubstitution.empty) match {
            case Result.Ok((subst, _)) =>
              val expOpt = expOpt0.map(reassembleExpression(_, subst, root))
              KindedAst.Sig(sym, spec, expOpt).toSuccess
            case Result.Err(e) =>
              Validation.Failure(LazyList(e))
          }
      }
  }

  private def visitDef(def0: ResolvedAst.Def, inf0: KindInferMonad[Unit], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Def, KindError] = def0 match {
    case ResolvedAst.Def(sym, spec0, exp0) =>
      val inference = for {
        _ <- inf0
        _ <- inferSpec(spec0, root)
      } yield ()

      flatMapN(visitSpec(spec0, inference, root)) {
        spec =>
          val KindInferMonad(run) = inference
          run(KindSubstitution.empty) match {
            case Result.Ok((subst, _)) =>
              val exp = reassembleExpression(exp0, subst, root)
              KindedAst.Def(sym, spec, exp).toSuccess
            case Result.Err(e) =>
              Validation.Failure(LazyList(e))
          }
      }
  }

  private def visitDef2(def0: ResolvedAst.Def, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Def, KindError] = def0 match {
    case ResolvedAst.Def(sym, spec0, exp0) =>
      val kinds = getKindsFromSpec(spec0, root)
      val specVal = ascribeSpec(spec0, kinds, root)
      val expVal = ascribeExpression(exp0, kinds, root)
      mapN(specVal, expVal) {
        case (spec, exp) => KindedAst.Def(sym, spec, exp)
      }
  }

  private def ascribeSpec(spec0: ResolvedAst.Spec, kinds: Map[UnkindedType.Var, Kind], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Spec, KindError] = spec0 match {
    case ResolvedAst.Spec(doc, ann0, mod, tparams0, fparams0, sc0, tpe0, eff0, loc) =>
      val annVal = Validation.traverse(ann0)(ascribeAnnotation(_, kinds, root))
      val tparamsVal = Validation.traverse(tparams0.tparams)(ascribeTparam(_, kinds))
      val fparamsVal = Validation.traverse(fparams0)(ascribeFormalParam(_, kinds, root))
      val scVal = ascribeScheme(sc0, kinds, root)
      val tpeVal = ascribeType(tpe0, KindMatch.subKindOf(KindMatch.Template.Star), kinds, root)
      val effVal = ascribeType(eff0, KindMatch.subKindOf(KindMatch.Template.Bool), kinds, root)
      mapN(annVal, tparamsVal, fparamsVal, scVal, tpeVal, effVal) {
        case (ann, tparams, fparams, sc, tpe, eff) => KindedAst.Spec(doc, ann, mod, tparams, fparams, sc, tpe, eff, loc)
      }
  }

  private def ascribeFormalParam(fparam0: ResolvedAst.FormalParam, kinds: Map[UnkindedType.Var, Kind], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.FormalParam, KindError] = fparam0 match {
    case ResolvedAst.FormalParam(sym, mod, tpe0, loc) =>
      mapN(ascribeType(tpe0, KindMatch.subKindOf(KindMatch.Template.Star), kinds, root)) {
        tpe => KindedAst.FormalParam(sym, mod, tpe, loc)
      }
  }

  private def ascribeAnnotation(ann: ResolvedAst.Annotation, kinds: Map[UnkindedType.Var, Kind], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Annotation, KindError] = ann match {
    case ResolvedAst.Annotation(name, exps0, loc) =>
      mapN(Validation.traverse(exps0)(ascribeExpression(_, kinds, root))) {
        exps => KindedAst.Annotation(name, exps, loc)
      }
  }

  // MATT remove the tvars newly introduced in Resolver and introduce them here
  private def ascribeExpression(exp00: ResolvedAst.Expression, kinds: Map[UnkindedType.Var, Kind], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Expression, KindError] = exp00 match {
    case ResolvedAst.Expression.Wild(tpe, loc) => KindedAst.Expression.Wild(tpe.ascribedWith(Kind.Star), loc).toSuccess
    case ResolvedAst.Expression.Var(sym, tpe0, loc) =>
      mapN(ascribeType(tpe0, KindMatch.subKindOf(KindMatch.Template.Star), kinds, root)) {
        tpe => KindedAst.Expression.Var(sym, tpe, loc)
      }
    case ResolvedAst.Expression.Def(sym, tpe, loc) => KindedAst.Expression.Def(sym, tpe.ascribedWith(Kind.Star), loc).toSuccess
    case ResolvedAst.Expression.Sig(sym, tpe, loc) => KindedAst.Expression.Sig(sym, tpe.ascribedWith(Kind.Star), loc).toSuccess
    case ResolvedAst.Expression.Hole(sym, tpe, eff, loc) => KindedAst.Expression.Hole(sym, tpe.ascribedWith(Kind.Star), eff.ascribedWith(Kind.Bool), loc).toSuccess
    case ResolvedAst.Expression.Unit(loc) => KindedAst.Expression.Unit(loc).toSuccess
    case ResolvedAst.Expression.Null(loc) => KindedAst.Expression.Null(loc).toSuccess
    case ResolvedAst.Expression.True(loc) => KindedAst.Expression.True(loc).toSuccess
    case ResolvedAst.Expression.False(loc) => KindedAst.Expression.False(loc).toSuccess
    case ResolvedAst.Expression.Char(lit, loc) => KindedAst.Expression.Char(lit, loc).toSuccess
    case ResolvedAst.Expression.Float32(lit, loc) => KindedAst.Expression.Float32(lit, loc).toSuccess
    case ResolvedAst.Expression.Float64(lit, loc) => KindedAst.Expression.Float64(lit, loc).toSuccess
    case ResolvedAst.Expression.Int8(lit, loc) => KindedAst.Expression.Int8(lit, loc).toSuccess
    case ResolvedAst.Expression.Int16(lit, loc) => KindedAst.Expression.Int16(lit, loc).toSuccess
    case ResolvedAst.Expression.Int32(lit, loc) => KindedAst.Expression.Int32(lit, loc).toSuccess
    case ResolvedAst.Expression.Int64(lit, loc) => KindedAst.Expression.Int64(lit, loc).toSuccess
    case ResolvedAst.Expression.BigInt(lit, loc) => KindedAst.Expression.BigInt(lit, loc).toSuccess
    case ResolvedAst.Expression.Str(lit, loc) => KindedAst.Expression.Str(lit, loc).toSuccess
    case ResolvedAst.Expression.Default(tpe, loc) => KindedAst.Expression.Default(tpe.ascribedWith(Kind.Star), loc).toSuccess
    case ResolvedAst.Expression.Apply(exp0, exps0, tpe, eff, loc) =>
      for {
        exp <- ascribeExpression(exp0, kinds, root)
        exps <- Validation.traverse(exps0)(ascribeExpression(_, kinds, root))
      } yield KindedAst.Expression.Apply(exp, exps, tpe.ascribedWith(Kind.Star), eff.ascribedWith(Kind.Bool), loc)
    case ResolvedAst.Expression.Lambda(fparam0, exp0, tpe, loc) =>
      for {
        fparam <- ascribeFormalParam(fparam0, kinds, root)
        exp <- ascribeExpression(exp0, kinds, root)
      } yield KindedAst.Expression.Lambda(fparam, exp, tpe.ascribedWith(Kind.Star), loc)
    case ResolvedAst.Expression.Unary(sop, exp0, tpe, loc) =>
      for {
        exp <- ascribeExpression(exp0, kinds, root)
      } yield KindedAst.Expression.Unary(sop, exp, tpe.ascribedWith(Kind.Star), loc)
    case ResolvedAst.Expression.Binary(sop, exp10, exp20, tpe, loc) =>
      for {
        exp1 <- ascribeExpression(exp10, kinds, root)
        exp2 <- ascribeExpression(exp20, kinds, root)
      } yield KindedAst.Expression.Binary(sop, exp1, exp2, tpe.ascribedWith(Kind.Star), loc)
    case ResolvedAst.Expression.IfThenElse(exp10, exp20, exp30, loc) =>
      for {
        exp1 <- ascribeExpression(exp10, kinds, root)
        exp2 <- ascribeExpression(exp20, kinds, root)
        exp3 <- ascribeExpression(exp30, kinds, root)
      } yield KindedAst.Expression.IfThenElse(exp1, exp2, exp3, loc)
    case ResolvedAst.Expression.Stm(exp10, exp20, loc) =>
      for {
        exp1 <- ascribeExpression(exp10, kinds, root)
        exp2 <- ascribeExpression(exp20, kinds, root)
      } yield KindedAst.Expression.Stm(exp1, exp2, loc)
    case ResolvedAst.Expression.Let(sym, mod, exp10, exp20, loc) =>
      for {
        exp1 <- ascribeExpression(exp10, kinds, root)
        exp2 <- ascribeExpression(exp20, kinds, root)
      } yield KindedAst.Expression.Let(sym, mod, exp1, exp2, loc)
    case ResolvedAst.Expression.LetRegion(sym, exp0, evar, loc) =>
      for {
        exp <- ascribeExpression(exp0, kinds, root)
      } yield KindedAst.Expression.LetRegion(sym, exp, evar.ascribedWith(Kind.Bool), loc)
    case ResolvedAst.Expression.Match(exp0, rules0, loc) =>
      for {
        exp <- ascribeExpression(exp0, kinds, root)
        rules <- traverse(rules0)(ascribeMatchRule(_, kinds, root))
      } yield KindedAst.Expression.Match(exp, rules, loc)
    case ResolvedAst.Expression.Choose(star, exps0, rules0, tpe, loc) =>
      for {
        exps <- Validation.traverse(exps0)(ascribeExpression(_, kinds, root))
        rules <- Validation.traverse(rules0)(ascribeChoiceRule(_, kinds, root))
      } yield KindedAst.Expression.Choose(star, exps, rules, tpe.ascribedWith(Kind.Star), loc)
    case ResolvedAst.Expression.Tag(sym, tag, exp0, tpe, loc) =>
      for {
        exp <- ascribeExpression(exp0, kinds, root)
      } yield KindedAst.Expression.Tag(sym, tag, exp, tpe.ascribedWith(Kind.Star), loc)
    case ResolvedAst.Expression.Tuple(elms0, loc) =>
      for {
        elms <- Validation.traverse(elms0)(ascribeExpression(_, kinds, root))
      } yield KindedAst.Expression.Tuple(elms, loc)
    case ResolvedAst.Expression.RecordEmpty(tpe, loc) => KindedAst.Expression.RecordEmpty(tpe.ascribedWith(Kind.Record), loc).toSuccess
    case ResolvedAst.Expression.RecordSelect(exp0, field, tpe, loc) =>
      for {
        exp <- ascribeExpression(exp0, kinds, root)
      } yield KindedAst.Expression.RecordSelect(exp, field, tpe.ascribedWith(Kind.Star), loc)
    case ResolvedAst.Expression.RecordExtend(field, value0, rest0, tpe, loc) =>
      // Ideally, if `rest` is not of record kind, we should throw a kind error.
      // But because we have subkinding, we can't do this in the Kinder.
      // Consider: { +name = 5 | id({})
      // This is OK, but would be seen as a kind error since id is `(a: *) -> (a: *)`, so `id({}) :: *`
      // This KindError will be caught later in the Typer
      for {
        value <- ascribeExpression(value0, kinds, root)
        rest <- ascribeExpression(rest0, kinds, root)
      } yield KindedAst.Expression.RecordExtend(field, value, rest, tpe.ascribedWith(Kind.Record), loc)
    case ResolvedAst.Expression.RecordRestrict(field, rest0, tpe, loc) =>
      for {
        rest <- ascribeExpression(rest0, kinds, root)
      } yield KindedAst.Expression.RecordRestrict(field, rest, tpe.ascribedWith(Kind.Record), loc)
    case ResolvedAst.Expression.ArrayLit(elms0, tpe, loc) =>
      for {
        elms <- Validation.traverse(elms0)(ascribeExpression(_, kinds, root))
      } yield KindedAst.Expression.ArrayLit(elms, tpe.ascribedWith(Kind.Star), loc)
    case ResolvedAst.Expression.ArrayNew(elm0, len0, tpe, loc) =>
      for {
        elm <- ascribeExpression(elm0, kinds, root)
        len <- ascribeExpression(len0, kinds, root)
      } yield KindedAst.Expression.ArrayNew(elm, len, tpe.ascribedWith(Kind.Star), loc)
    case ResolvedAst.Expression.ArrayLoad(base0, index0, tpe, loc) =>
      for {
        base <- ascribeExpression(base0, kinds, root)
        index <- ascribeExpression(index0, kinds, root)
      } yield KindedAst.Expression.ArrayLoad(base, index, tpe.ascribedWith(Kind.Star), loc)
    case ResolvedAst.Expression.ArrayStore(base0, index0, elm0, loc) =>
      for {
        base <- ascribeExpression(base0, kinds, root)
        index <- ascribeExpression(index0, kinds, root)
        elm <- ascribeExpression(elm0, kinds, root)
      } yield KindedAst.Expression.ArrayStore(base, index, elm, loc)
    case ResolvedAst.Expression.ArrayLength(base0, loc) =>
      for {
        base <- ascribeExpression(base0, kinds, root)
      } yield KindedAst.Expression.ArrayLength(base, loc)
    case ResolvedAst.Expression.ArraySlice(base0, beginIndex0, endIndex0, loc) =>
      for {
        base <- ascribeExpression(base0, kinds, root)
        beginIndex <- ascribeExpression(beginIndex0, kinds, root)
        endIndex <- ascribeExpression(endIndex0, kinds, root)
      } yield KindedAst.Expression.ArraySlice(base, beginIndex, endIndex, loc)
    case ResolvedAst.Expression.Ref(exp0, tvar, loc) =>
      for {
        exp <- ascribeExpression(exp0, kinds, root)
      } yield KindedAst.Expression.Ref(exp, tvar.ascribedWith(Kind.Star), loc)
    case ResolvedAst.Expression.RefWithRegion(exp10, exp20, tpe, evar, loc) =>
      for {
        exp1 <- ascribeExpression(exp10, kinds, root)
        exp2 <- ascribeExpression(exp20, kinds, root)
      } yield KindedAst.Expression.RefWithRegion(exp1, exp2, tpe.ascribedWith(Kind.Star), evar.ascribedWith(Kind.Bool), loc)
    case ResolvedAst.Expression.Deref(exp0, tvar, evar, loc) =>
      for {
        exp <- ascribeExpression(exp0, kinds, root)
      } yield KindedAst.Expression.Deref(exp, tvar.ascribedWith(Kind.Star), evar.ascribedWith(Kind.Bool), loc)
    case ResolvedAst.Expression.Assign(exp10, exp20, evar, loc) =>
      for {
        exp1 <- ascribeExpression(exp10, kinds, root)
        exp2 <- ascribeExpression(exp20, kinds, root)
      } yield KindedAst.Expression.Assign(exp1, exp2, evar.ascribedWith(Kind.Bool), loc)
    case ResolvedAst.Expression.Existential(fparam0, exp0, loc) =>
      // add the formal param kinds to the environment
      val fparamKinds = inferFparam2(fparam0, root)
      val kinds1 = kinds ++ fparamKinds
      for {
        fparam <- ascribeFormalParam(fparam0, kinds1, root)
        exp <- ascribeExpression(exp0, kinds1, root)
      } yield KindedAst.Expression.Existential(fparam, exp, loc)
    case ResolvedAst.Expression.Universal(fparam0, exp0, loc) =>
      // add the formal param kinds to the environment
      val fparamKinds = inferFparam2(fparam0, root)
      val kinds1 = kinds ++ fparamKinds
      for {
        fparam <- ascribeFormalParam(fparam0, kinds1, root)
        exp <- ascribeExpression(exp0, kinds1, root)
      } yield KindedAst.Expression.Universal(fparam, exp, loc)
    case ResolvedAst.Expression.Ascribe(exp0, expectedType0, expectedEff0, tpe, loc) =>
      for {
        exp <- ascribeExpression(exp0, kinds, root)
        expectedType <- Validation.traverse(expectedType0)(ascribeType(_, KindMatch.subKindOf(KindMatch.Template.Star), kinds, root))
        expectedEff <- Validation.traverse(expectedEff0)(ascribeType(_, KindMatch.subKindOf(KindMatch.Template.Bool), kinds, root))
      } yield KindedAst.Expression.Ascribe(exp, expectedType.headOption, expectedEff.headOption, tpe.ascribedWith(Kind.Star), loc)
    case ResolvedAst.Expression.Cast(exp0, declaredType0, declaredEff0, tpe, loc) =>
      for {
        exp <- ascribeExpression(exp0, kinds, root)
        declaredType <- Validation.traverse(declaredType0)(ascribeType(_, KindMatch.subKindOf(KindMatch.Template.Star), kinds, root))
        declaredEff <- Validation.traverse(declaredEff0)(ascribeType(_, KindMatch.subKindOf(KindMatch.Template.Bool), kinds, root))
      } yield KindedAst.Expression.Cast(exp, declaredType.headOption, declaredEff.headOption, tpe.ascribedWith(Kind.Star), loc)
    case ResolvedAst.Expression.TryCatch(exp0, rules0, loc) =>
      for {
        exp <- ascribeExpression(exp0, kinds, root)
        rules <- Validation.traverse(rules0)(ascribeCatchRule(_, kinds, root))
      } yield KindedAst.Expression.TryCatch(exp, rules, loc)
    case ResolvedAst.Expression.InvokeConstructor(constructor, args0, loc) =>
      for {
        args <- Validation.traverse(args0)(ascribeExpression(_, kinds, root))
      } yield KindedAst.Expression.InvokeConstructor(constructor, args, loc)
    case ResolvedAst.Expression.InvokeMethod(method, exp0, args0, loc) =>
      for {
        exp <- ascribeExpression(exp0, kinds, root)
        args <- Validation.traverse(args0)(ascribeExpression(_, kinds, root))
      } yield KindedAst.Expression.InvokeMethod(method, exp, args, loc)
    case ResolvedAst.Expression.InvokeStaticMethod(method, args0, loc) =>
      for {
        args <- Validation.traverse(args0)(ascribeExpression(_, kinds, root))
      } yield KindedAst.Expression.InvokeStaticMethod(method, args, loc)
    case ResolvedAst.Expression.GetField(field, exp0, loc) =>
      for {
        exp <- ascribeExpression(exp0, kinds, root)
      } yield KindedAst.Expression.GetField(field, exp, loc)
    case ResolvedAst.Expression.PutField(field, exp10, exp20, loc) =>
      for {
        exp1 <- ascribeExpression(exp10, kinds, root)
        exp2 <- ascribeExpression(exp20, kinds, root)
      } yield KindedAst.Expression.PutField(field, exp1, exp2, loc)
    case ResolvedAst.Expression.GetStaticField(field, loc) => KindedAst.Expression.GetStaticField(field, loc).toSuccess
    case ResolvedAst.Expression.PutStaticField(field, exp0, loc) =>
      for {
        exp <- ascribeExpression(exp0, kinds, root)
      } yield KindedAst.Expression.PutStaticField(field, exp, loc)
    case ResolvedAst.Expression.NewChannel(exp0, tpe0, loc) =>
      for {
        exp <- ascribeExpression(exp0, kinds, root)
        tpe <- ascribeType(tpe0, KindMatch.subKindOf(KindMatch.Template.Star), kinds, root)
      } yield KindedAst.Expression.NewChannel(exp, tpe, loc)
    case ResolvedAst.Expression.GetChannel(exp0, tpe, loc) =>
      for {
        exp <- ascribeExpression(exp0, kinds, root)
      } yield KindedAst.Expression.GetChannel(exp, tpe.ascribedWith(Kind.Star), loc)
    case ResolvedAst.Expression.PutChannel(exp10, exp20, tpe, loc) =>
      for {
        exp1 <- ascribeExpression(exp10, kinds, root)
        exp2 <- ascribeExpression(exp20, kinds, root)
      } yield KindedAst.Expression.PutChannel(exp1, exp2, tpe.ascribedWith(Kind.Star), loc)
    case ResolvedAst.Expression.SelectChannel(rules0, default0, tpe, loc) =>
      for {
        rules <- Validation.traverse(rules0)(ascribeSelectChannelRule(_, kinds, root))
        default <- Validation.traverse(default0)(ascribeExpression(_, kinds, root))
      } yield KindedAst.Expression.SelectChannel(rules, default.headOption, tpe.ascribedWith(Kind.Star), loc)
    case ResolvedAst.Expression.Spawn(exp0, loc) =>
      for {
        exp <- ascribeExpression(exp0, kinds, root)
      } yield KindedAst.Expression.Spawn(exp, loc)
    case ResolvedAst.Expression.Lazy(exp0, loc) =>
      for {
        exp <- ascribeExpression(exp0, kinds, root)
      } yield KindedAst.Expression.Lazy(exp, loc)
    case ResolvedAst.Expression.Force(exp0, tpe, loc) =>
      for {
        exp <- ascribeExpression(exp0, kinds, root)
      } yield KindedAst.Expression.Force(exp, tpe.ascribedWith(Kind.Star), loc)
    case ResolvedAst.Expression.FixpointConstraintSet(cs0, tpe, loc) =>
      for {
        cs <- Validation.traverse(cs0)(ascribeConstraint(_, kinds, root))
      } yield KindedAst.Expression.FixpointConstraintSet(cs, tpe.ascribedWith(Kind.Schema), loc)
    case ResolvedAst.Expression.FixpointMerge(exp10, exp20, loc) =>
      for {
        exp1 <- ascribeExpression(exp10, kinds, root)
        exp2 <- ascribeExpression(exp20, kinds, root)
      } yield KindedAst.Expression.FixpointMerge(exp1, exp2, loc)
    case ResolvedAst.Expression.FixpointSolve(exp0, loc) =>
      for {
        exp <- ascribeExpression(exp0, kinds, root)
      } yield KindedAst.Expression.FixpointSolve(exp, loc)
    case ResolvedAst.Expression.FixpointFilter(pred, exp0, tpe, loc) =>
      for {
        exp <- ascribeExpression(exp0, kinds, root)
      } yield KindedAst.Expression.FixpointFilter(pred, exp, tpe.ascribedWith(Kind.Star), loc) // MATT right?
    case ResolvedAst.Expression.FixpointProjectIn(exp0, pred, tpe, loc) =>
      for {
        exp <- ascribeExpression(exp0, kinds, root)
      } yield KindedAst.Expression.FixpointProjectIn(exp, pred, tpe.ascribedWith(Kind.Star), loc) // MATT right?
    case ResolvedAst.Expression.FixpointProjectOut(pred, exp10, exp20, tpe, loc) =>
      for {
        exp1 <- ascribeExpression(exp10, kinds, root)
        exp2 <- ascribeExpression(exp20, kinds, root)
      } yield KindedAst.Expression.FixpointProjectOut(pred, exp1, exp2, tpe.ascribedWith(Kind.Star), loc) // MATT right?
    case ResolvedAst.Expression.MatchEff(exp10, exp20, exp30, loc) =>
      for {
        exp1 <- ascribeExpression(exp10, kinds, root)
        exp2 <- ascribeExpression(exp20, kinds, root)
        exp3 <- ascribeExpression(exp30, kinds, root)
      } yield KindedAst.Expression.MatchEff(exp1, exp2, exp3, loc)

  }

  private def ascribeMatchRule(rule0: ResolvedAst.MatchRule, kinds: Map[UnkindedType.Var, Kind], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.MatchRule, KindError] = rule0 match {
    case ResolvedAst.MatchRule(pat0, guard0, exp0) =>
      for {
        pat <- ascribePattern(pat0, kinds, root)
        guard <- ascribeExpression(guard0, kinds, root)
        exp <- ascribeExpression(exp0, kinds, root)
      } yield KindedAst.MatchRule(pat, guard, exp)
  }

  private def ascribeChoiceRule(rule0: ResolvedAst.ChoiceRule, kinds: Map[UnkindedType.Var, Kind], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.ChoiceRule, KindError] = rule0 match {
    case ResolvedAst.ChoiceRule(pat0, exp0) =>
      for {
        pat <- Validation.traverse(pat0)(ascribeChoicePattern(_, kinds, root))
        exp <- ascribeExpression(exp0, kinds, root)
      } yield KindedAst.ChoiceRule(pat, exp)
  }

  private def ascribeCatchRule(rule0: ResolvedAst.CatchRule, kinds: Map[UnkindedType.Var, Kind], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.CatchRule, KindError] = rule0 match {
    case ResolvedAst.CatchRule(sym, clazz, exp0) =>
      for {
        exp <- ascribeExpression(exp0, kinds, root)
      } yield KindedAst.CatchRule(sym, clazz, exp)
  }

  private def ascribeSelectChannelRule(rule0: ResolvedAst.SelectChannelRule, kinds: Map[UnkindedType.Var, Kind], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.SelectChannelRule, KindError] = rule0 match {
    case ResolvedAst.SelectChannelRule(sym, chan0, exp0) =>
      for {
        chan <- ascribeExpression(chan0, kinds, root)
        exp <- ascribeExpression(exp0, kinds, root)
      } yield KindedAst.SelectChannelRule(sym, chan, exp)
  }

  // MATT monad may be redundant here
  private def ascribePattern(pat0: ResolvedAst.Pattern, kinds: Map[UnkindedType.Var, Kind], root: ResolvedAst.Root): Validation[KindedAst.Pattern, KindError] = pat0 match {
    case ResolvedAst.Pattern.Wild(tvar, loc) => KindedAst.Pattern.Wild(tvar.ascribedWith(Kind.Star), loc).toSuccess
    case ResolvedAst.Pattern.Var(sym, tvar, loc) => KindedAst.Pattern.Var(sym, tvar.ascribedWith(Kind.Star), loc).toSuccess
    case ResolvedAst.Pattern.Unit(loc) => KindedAst.Pattern.Unit(loc).toSuccess
    case ResolvedAst.Pattern.True(loc) => KindedAst.Pattern.True(loc).toSuccess
    case ResolvedAst.Pattern.False(loc) => KindedAst.Pattern.False(loc).toSuccess
    case ResolvedAst.Pattern.Char(lit, loc) => KindedAst.Pattern.Char(lit, loc).toSuccess
    case ResolvedAst.Pattern.Float32(lit, loc) => KindedAst.Pattern.Float32(lit, loc).toSuccess
    case ResolvedAst.Pattern.Float64(lit, loc) => KindedAst.Pattern.Float64(lit, loc).toSuccess
    case ResolvedAst.Pattern.Int8(lit, loc) => KindedAst.Pattern.Int8(lit, loc).toSuccess
    case ResolvedAst.Pattern.Int16(lit, loc) => KindedAst.Pattern.Int16(lit, loc).toSuccess
    case ResolvedAst.Pattern.Int32(lit, loc) => KindedAst.Pattern.Int32(lit, loc).toSuccess
    case ResolvedAst.Pattern.Int64(lit, loc) => KindedAst.Pattern.Int64(lit, loc).toSuccess
    case ResolvedAst.Pattern.BigInt(lit, loc) => KindedAst.Pattern.BigInt(lit, loc).toSuccess
    case ResolvedAst.Pattern.Str(lit, loc) => KindedAst.Pattern.Str(lit, loc).toSuccess
    case ResolvedAst.Pattern.Tag(sym, tag, pat0, tvar, loc) =>
      for {
        pat <- ascribePattern(pat0, kinds, root)
      } yield KindedAst.Pattern.Tag(sym, tag, pat, tvar.ascribedWith(Kind.Star), loc)
    case ResolvedAst.Pattern.Tuple(elms0, loc) =>
      for {
        elms <- Validation.traverse(elms0)(ascribePattern(_, kinds, root))
      } yield KindedAst.Pattern.Tuple(elms, loc)
    case ResolvedAst.Pattern.Array(elms0, tvar, loc) =>
      for {
        elms <- Validation.traverse(elms0)(ascribePattern(_, kinds, root))
      } yield KindedAst.Pattern.Array(elms, tvar.ascribedWith(Kind.Star), loc)
    case ResolvedAst.Pattern.ArrayTailSpread(elms0, sym, tvar, loc) =>
      for {
        elms <- Validation.traverse(elms0)(ascribePattern(_, kinds, root))
      } yield KindedAst.Pattern.ArrayTailSpread(elms, sym, tvar.ascribedWith(Kind.Star), loc)
    case ResolvedAst.Pattern.ArrayHeadSpread(sym, elms0, tvar, loc) =>
      for {
        elms <- Validation.traverse(elms0)(ascribePattern(_, kinds, root))
      } yield KindedAst.Pattern.ArrayHeadSpread(sym, elms, tvar.ascribedWith(Kind.Star), loc)
  }


  // MATT validation unneeded here
  private def ascribeChoicePattern(pat0: ResolvedAst.ChoicePattern, kinds: Map[UnkindedType.Var, Kind], root: ResolvedAst.Root): Validation[KindedAst.ChoicePattern, KindError] = pat0 match {
    case ResolvedAst.ChoicePattern.Wild(loc) => KindedAst.ChoicePattern.Wild(loc).toSuccess
    case ResolvedAst.ChoicePattern.Absent(loc) => KindedAst.ChoicePattern.Absent(loc).toSuccess
    case ResolvedAst.ChoicePattern.Present(sym, tvar, loc) => KindedAst.ChoicePattern.Present(sym, tvar.ascribedWith(Kind.Star), loc).toSuccess
  }

  private def ascribeConstraint(constraint0: ResolvedAst.Constraint, kinds: Map[UnkindedType.Var, Kind], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Constraint, KindError] = constraint0 match {
    case ResolvedAst.Constraint(cparams0, head0, body0, loc) =>
      for {
        cparams <- Validation.traverse(cparams0)(ascribeCparam(_, kinds, root))
        head <- ascribeHeadPredicate(head0, kinds, root)
        body <- Validation.traverse(body0)(ascribeBodyPredicate(_, kinds, root))
      } yield KindedAst.Constraint(cparams, head, body, loc)
  }

  // MATT validation unneeded here
  // MATT other args unneeded here
  private def ascribeCparam(cparam0: ResolvedAst.ConstraintParam, kinds: Map[UnkindedType.Var, Kind], root: ResolvedAst.Root): Validation[KindedAst.ConstraintParam, KindError] = cparam0 match {
    case ResolvedAst.ConstraintParam.HeadParam(sym, tpe, loc) => KindedAst.ConstraintParam.HeadParam(sym, tpe.ascribedWith(Kind.Star), loc).toSuccess
    case ResolvedAst.ConstraintParam.RuleParam(sym, tpe, loc) => KindedAst.ConstraintParam.RuleParam(sym, tpe.ascribedWith(Kind.Star), loc).toSuccess
  }

  private def ascribeHeadPredicate(pred: ResolvedAst.Predicate.Head, kinds: Map[UnkindedType.Var, Kind], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Predicate.Head, KindError] = pred match {
    case ResolvedAst.Predicate.Head.Atom(pred, den, terms0, tvar, loc) =>
      for {
        terms <- Validation.traverse(terms0)(ascribeExpression(_, kinds, root))
      } yield KindedAst.Predicate.Head.Atom(pred, den, terms, tvar.ascribedWith(Kind.Star), loc)
  }

  private def ascribeBodyPredicate(pred: ResolvedAst.Predicate.Body, kinds: Map[UnkindedType.Var, Kind], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Predicate.Body, KindError] = pred match {
    case ResolvedAst.Predicate.Body.Atom(pred, den, polarity, terms0, tvar, loc) =>
      for {
        terms <- Validation.traverse(terms0)(ascribePattern(_, kinds, root))
      } yield KindedAst.Predicate.Body.Atom(pred, den, polarity, terms, tvar.ascribedWith(Kind.Star), loc)
    case ResolvedAst.Predicate.Body.Guard(exp0, loc) =>
      for {
        exp <- ascribeExpression(exp0, kinds, root)
      } yield KindedAst.Predicate.Body.Guard(exp, loc)
  }

  private def inferSpec2(spec0: ResolvedAst.Spec, root: ResolvedAst.Root)(implicit flix: Flix): Map[UnkindedType.Var, Kind] = spec0 match {
    case ResolvedAst.Spec(_, _, _, _, fparams, sc, _, eff, _) =>
      fparams.foldLeft(Map.empty[UnkindedType.Var, Kind]) {
        case (acc, fparam) => acc ++ inferFparam2(fparam, root)
      } ++
        inferScheme2(sc, root) ++
        inferType(eff, KindMatch.subKindOf(KindMatch.Template.Bool), root) // MATT merge smarter

  }

  private def inferFparam2(fparam0: ResolvedAst.FormalParam, root: ResolvedAst.Root)(implicit flix: Flix): Map[UnkindedType.Var, Kind] = fparam0 match {
    case ResolvedAst.FormalParam(_, _, tpe0, _) => inferType(tpe0, KindMatch.subKindOf(KindMatch.Template.Star), root)
  }

  private def inferScheme2(sc0: ResolvedAst.Scheme, root: ResolvedAst.Root)(implicit flix: Flix): Map[UnkindedType.Var, Kind] = sc0 match {
    case ResolvedAst.Scheme(_, constraints, base) =>
      val baseKinds = inferType(base, KindMatch.subKindOf(KindMatch.Template.Star), root)
      constraints.foldLeft(baseKinds) {
        case (acc, tconstr) => acc ++ inferTconstr2(tconstr, root) // MATT merge smarter
      }
  }

  private def inferTconstr2(tconstr: ResolvedAst.TypeConstraint, root: ResolvedAst.Root)(implicit flix: Flix): Map[UnkindedType.Var, Kind] = tconstr match {
    case ResolvedAst.TypeConstraint(clazz, tpe, loc) =>
      val kind = getClassKind(root.classes(clazz))
      inferType(tpe, KindMatch.subKindOf(KindMatch.Template.fromKind(kind)), root)
  }

  private def inferType(tpe: UnkindedType, expectedType: KindMatch, root: ResolvedAst.Root)(implicit flix: Flix): Map[UnkindedType.Var, Kind] = tpe.baseType match {
    // Case 1: the type constructor is a variable: all args are * and the constructor is * -> * -> * ... -> expectedType
    case tvar: UnkindedType.Var =>
      val kind = tpe.typeArguments.foldLeft(expectedType.kind.toKind2) {
        case (acc, _) => Kind.Star ->: acc
      }

      tpe.typeArguments.foldLeft(Map(tvar -> kind)) {
        case (acc, targ) => acc ++ inferType(targ, KindMatch.subKindOf(KindMatch.Template.Star), root)
      }
    case UnkindedType.Cst(cst, loc) =>
      val tyconKind = getTyconKind(cst, root)
      val args = Kind.args(tyconKind)

      tpe.typeArguments.zip(args).foldLeft(Map.empty[UnkindedType.Var, Kind]) {
        case (acc, (targ, kind)) => acc ++ inferType(targ, KindMatch.subKindOf(KindMatch.Template.fromKind(kind)), root)
      }
    case UnkindedType.Apply(_, _) => throw InternalCompilerException("Unexpected type application.")
    case UnkindedType.Lambda(t1, t2) => ??? // MATT I'll cross this bridge when i get to it
  }

  private def visitSpec(spec0: ResolvedAst.Spec, inference: KindInferMonad[Unit], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Spec, KindError] = spec0 match {
    case ResolvedAst.Spec(doc, ann0, mod, tparams0, fparams0, sc0, tpe0, eff0, loc) =>
      val KindInferMonad(run) = inference
      val initialSubst = getSubstFromTparams(tparams0)
      run(initialSubst) match {
        case Result.Ok((subst, _)) =>
          val ann = ann0.map(reassembleAnnotation(_, subst, root))
          val tparams = reassembleTparams(tparams0, subst, root)
          val fparams = fparams0.map(reassembleFparam(_, subst, root))
          val sc = reassembleScheme(sc0, subst, root)
          val tpe = reassembleType(tpe0, subst, root)
          val eff = reassembleType(eff0, subst, root)
          KindedAst.Spec(doc, ann, mod, tparams, fparams, sc, tpe, eff, loc).toSuccess

        case Result.Err(e) => Validation.Failure(LazyList(e))
      }
  }

  private def getSubstFromTparams(tparams0: ResolvedAst.TypeParams)(implicit flix: Flix): KindSubstitution = tparams0 match {
    case ResolvedAst.TypeParams.Kinded(tparams) =>
      tparams.foldLeft(KindSubstitution.empty) {
        case (acc, ResolvedAst.TypeParam.Kinded(_, tpe, kind, _)) =>
          acc ++ KindSubstitution.singleton(tpe.kvar, kind)
      }
    case _: ResolvedAst.TypeParams.Unkinded => KindSubstitution.empty
  }

  private def getKindsFromTparamsDefaultStar(tparams0: ResolvedAst.TypeParams)(implicit flix: Flix): Map[UnkindedType.Var, Kind] = tparams0 match {
    case tparams: ResolvedAst.TypeParams.Kinded =>
      getKindsFromKindedTparams(tparams)
    case tparams: ResolvedAst.TypeParams.Unkinded =>
      getStarKindsForTparams(tparams)
  }

  private def getKindsFromSpec(spec0: ResolvedAst.Spec, root: ResolvedAst.Root)(implicit flix: Flix): Map[UnkindedType.Var, Kind] = spec0 match {
    case ResolvedAst.Spec(_, _, _, tparams0, _, _, _, _, _) =>
      tparams0 match {
        case tparams: ResolvedAst.TypeParams.Kinded => getKindsFromKindedTparams(tparams)
        case _: ResolvedAst.TypeParams.Unkinded => inferSpec2(spec0, root)
      }
  }

  private def getKindsFromKindedTparams(tparams0: ResolvedAst.TypeParams.Kinded)(implicit flix: Flix): Map[UnkindedType.Var, Kind] = tparams0 match {
    case ResolvedAst.TypeParams.Kinded(tparams) =>
      tparams.foldLeft(Map.empty[UnkindedType.Var, Kind]) {
        case (acc, ResolvedAst.TypeParam.Kinded(_, tpe, kind, _)) =>
          acc + (tpe -> kind)
      }
  }

  private def getStarKindsForTparams(tparams0: ResolvedAst.TypeParams.Unkinded)(implicit flix: Flix): Map[UnkindedType.Var, Kind] = tparams0 match {
    case ResolvedAst.TypeParams.Unkinded(tparams) =>
      tparams.foldLeft(Map.empty[UnkindedType.Var, Kind]) {
        case (acc, ResolvedAst.TypeParam.Unkinded(_, tpe, _)) =>
          acc + (tpe -> Kind.Star)
      }
  }

  private def getSubstFromTparamsDefaultStar(tparams0: ResolvedAst.TypeParams)(implicit flix: Flix): KindSubstitution = tparams0 match {
    case ResolvedAst.TypeParams.Kinded(tparams) =>
      tparams.foldLeft(KindSubstitution.empty) {
        case (acc, ResolvedAst.TypeParam.Kinded(_, tpe, kind, _)) =>
          acc ++ KindSubstitution.singleton(tpe.kvar, kind)
      }
    case ResolvedAst.TypeParams.Unkinded(tparams) =>
      tparams.foldLeft(KindSubstitution.empty) {
        case (acc, ResolvedAst.TypeParam.Unkinded(_, tpe, _)) =>
          acc ++ KindSubstitution.singleton(tpe.kvar, Kind.Star)
      }
  }

  private def getSubstFromTparamDefaultStar(tparam0: ResolvedAst.TypeParam)(implicit flix: Flix): KindSubstitution = tparam0 match {
    case ResolvedAst.TypeParam.Kinded(_, tpe, kind, _) =>
      KindSubstitution.singleton(tpe.kvar, kind)
    case ResolvedAst.TypeParam.Unkinded(_, tpe, _) =>
      KindSubstitution.singleton(tpe.kvar, Kind.Star)
  }

  private def inferCase(case0: ResolvedAst.Case, root: ResolvedAst.Root)(implicit flix: Flix): KindInferMonad[Unit] = case0 match {
    case ResolvedAst.Case(enum, tag, tpeDeprecated, sc) =>
      val loc = SourceLocation.Unknown // MATT
      for {
        kind <- inferType(tpeDeprecated, root)
        _ <- unifyKindM(kind, Kind.Star, loc)
        _ <- inferScheme(sc, root)
      } yield ()
  }

  // MATT docs
  private def inferSpec(spec0: ResolvedAst.Spec, root: ResolvedAst.Root)(implicit flix: Flix): KindInferMonad[Unit] = spec0 match {
    case ResolvedAst.Spec(_, _, _, _, fparams, sc, _, eff, _) =>
      val loc = SourceLocation.Unknown // MATT
      for {
        _ <- seqM(fparams.map(inferFparam(_, root)))
        _ <- inferScheme(sc, root)
        effKind <- inferType(eff, root)
        _ <- unifyKindM(effKind, Kind.Bool, loc)
      } yield ()
  }

  // MATT docs
  private def inferScheme(scheme: ResolvedAst.Scheme, root: ResolvedAst.Root)(implicit flix: Flix): KindInferMonad[Unit] = scheme match {
    case ResolvedAst.Scheme(quantifiers, constraints, base) =>
      val loc = SourceLocation.Unknown // MATT
      for {
        _ <- seqM(quantifiers.map(inferType(_, root)))
        _ <- seqM(constraints.map(inferTconstr(_, root)))
        kind <- inferType(base, root)
        _ <- unifyKindM(kind, Kind.Star, loc)
      } yield ()
  }

  private def inferTconstr(tconstr: ResolvedAst.TypeConstraint, root: ResolvedAst.Root)(implicit flix: Flix): KindInferMonad[Unit] = tconstr match {
    case ResolvedAst.TypeConstraint(clazz, tpe, loc) =>
      val classKind = getClassKind(root.classes(clazz))
      for {
        kind <- inferType(tpe, root)
        _ <- unifyKindM(classKind, kind, loc)
      } yield ()
  }

  // MATT docs
  private def inferFparam(fparam: ResolvedAst.FormalParam, root: ResolvedAst.Root)(implicit flix: Flix): KindInferMonad[Unit] = fparam match {
    case ResolvedAst.FormalParam(_, _, tpe, loc) =>
      for {
        kind <- inferType(tpe, root)
        _ <- unifyKindM(kind, Kind.Star, loc)
      } yield ()
  }

  // MATT docs
  private def inferType(tpe00: UnkindedType, root: ResolvedAst.Root)(implicit flix: Flix): KindInferMonad[Kind] = {
    val loc = SourceLocation.Unknown // MATT

    def visitType(tpe0: UnkindedType): KindInferMonad[Kind] = tpe0 match {
      case UnkindedType.Cst(cst, _) => KindInferMonad.point(getTyconKind(cst, root))
      case UnkindedType.Apply(t1, t2) =>
        val resultKind = Kind.freshVar()
        for {
          tyconKind <- visitType(t1)
          argKind <- visitType(t2)
          _ <- unifyKindM(tyconKind, argKind ->: resultKind, loc)
        } yield resultKind
      case UnkindedType.Lambda(t1, t2) =>
        for {
          argKind <- visitType(t1)
          bodyKind <- visitType(t2)
        } yield argKind ->: bodyKind // MATT do I need to introduce a result kind var here?
      case UnkindedType.Var(_, kind, _) => KindInferMonad.point(kind)
    }

    visitType(tpe00)
  }

  private def getTyconKind(tycon: UnkindedType.Constructor, root: ResolvedAst.Root)(implicit flix: Flix): Kind = tycon match {
    case UnkindedType.Constructor.Unit => Kind.Star
    case UnkindedType.Constructor.Null => Kind.Star
    case UnkindedType.Constructor.Bool => Kind.Star
    case UnkindedType.Constructor.Char => Kind.Star
    case UnkindedType.Constructor.Float32 => Kind.Star
    case UnkindedType.Constructor.Float64 => Kind.Star
    case UnkindedType.Constructor.Int8 => Kind.Star
    case UnkindedType.Constructor.Int16 => Kind.Star
    case UnkindedType.Constructor.Int32 => Kind.Star
    case UnkindedType.Constructor.Int64 => Kind.Star
    case UnkindedType.Constructor.BigInt => Kind.Star
    case UnkindedType.Constructor.Str => Kind.Star
    case UnkindedType.Constructor.Arrow(arity) => Kind.Bool ->: Kind.mkArrow(arity)
    case UnkindedType.Constructor.RecordEmpty => Kind.Record
    case UnkindedType.Constructor.RecordExtend(field) => Kind.Star ->: Kind.Record ->: Kind.Record
    case UnkindedType.Constructor.SchemaEmpty => Kind.Schema
    case UnkindedType.Constructor.SchemaExtend(pred) => Kind.Star ->: Kind.Schema ->: Kind.Schema
    case UnkindedType.Constructor.Array => Kind.Star ->: Kind.Star
    case UnkindedType.Constructor.Channel => Kind.Star ->: Kind.Star
    case UnkindedType.Constructor.Lazy => Kind.Star ->: Kind.Star
    case UnkindedType.Constructor.Tag(sym, tag) => Kind.Star ->: Kind.Star ->: Kind.Star
    case UnkindedType.Constructor.Enum(sym) => getEnumKind(root.enums(sym))
    case UnkindedType.Constructor.Native(clazz) => Kind.Star
    case UnkindedType.Constructor.ScopedRef => Kind.Star ->: Kind.Bool ->: Kind.Star
    case UnkindedType.Constructor.Tuple(l) => Kind.mkArrow(l)
    case UnkindedType.Constructor.Relation => Kind.Star ->: Kind.Star
    case UnkindedType.Constructor.Lattice => Kind.Star ->: Kind.Star
    case UnkindedType.Constructor.True => Kind.Bool
    case UnkindedType.Constructor.False => Kind.Bool
    case UnkindedType.Constructor.Not => Kind.Bool ->: Kind.Bool
    case UnkindedType.Constructor.And => Kind.Bool ->: Kind.Bool ->: Kind.Bool
    case UnkindedType.Constructor.Or => Kind.Bool ->: Kind.Bool ->: Kind.Bool
    case UnkindedType.Constructor.Region => Kind.Bool ->: Kind.Star
  }

  private def reassembleAnnotation(ann0: ResolvedAst.Annotation, subst: KindSubstitution, root: ResolvedAst.Root)(implicit flix: Flix): KindedAst.Annotation = ann0 match {
    case ResolvedAst.Annotation(name, exps0, loc) =>
      val exps = exps0.map(reassembleExpression(_, subst, root))
      KindedAst.Annotation(name, exps, loc)
  }

  private def reassembleExpression(exp000: ResolvedAst.Expression, subst: KindSubstitution, root: ResolvedAst.Root)(implicit flix: Flix): KindedAst.Expression = {
    def visit(exp00: ResolvedAst.Expression): KindedAst.Expression = exp00 match {
      case ResolvedAst.Expression.Wild(tpe, loc) => KindedAst.Expression.Wild(reassembleTypeVar(tpe, subst, root), loc)
      case ResolvedAst.Expression.Var(sym, tpe, loc) => KindedAst.Expression.Var(sym, reassembleType(tpe, subst, root), loc)
      case ResolvedAst.Expression.Def(sym, tpe, loc) => KindedAst.Expression.Def(sym, reassembleTypeVar(tpe, subst, root), loc)
      case ResolvedAst.Expression.Sig(sym, tpe, loc) => KindedAst.Expression.Sig(sym, reassembleTypeVar(tpe, subst, root), loc)
      case ResolvedAst.Expression.Hole(sym, tpe0, eff0, loc) =>
        val tpe = reassembleTypeVar(tpe0, subst, root)
        val eff = reassembleTypeVar(eff0, subst, root)
        KindedAst.Expression.Hole(sym, tpe, eff, loc)
      case ResolvedAst.Expression.Unit(loc) => KindedAst.Expression.Unit(loc)
      case ResolvedAst.Expression.Null(loc) => KindedAst.Expression.Null(loc)
      case ResolvedAst.Expression.True(loc) => KindedAst.Expression.True(loc)
      case ResolvedAst.Expression.False(loc) => KindedAst.Expression.False(loc)
      case ResolvedAst.Expression.Char(lit, loc) => KindedAst.Expression.Char(lit, loc)
      case ResolvedAst.Expression.Float32(lit, loc) => KindedAst.Expression.Float32(lit, loc)
      case ResolvedAst.Expression.Float64(lit, loc) => KindedAst.Expression.Float64(lit, loc)
      case ResolvedAst.Expression.Int8(lit, loc) => KindedAst.Expression.Int8(lit, loc)
      case ResolvedAst.Expression.Int16(lit, loc) => KindedAst.Expression.Int16(lit, loc)
      case ResolvedAst.Expression.Int32(lit, loc) => KindedAst.Expression.Int32(lit, loc)
      case ResolvedAst.Expression.Int64(lit, loc) => KindedAst.Expression.Int64(lit, loc)
      case ResolvedAst.Expression.BigInt(lit, loc) => KindedAst.Expression.BigInt(lit, loc)
      case ResolvedAst.Expression.Str(lit, loc) => KindedAst.Expression.Str(lit, loc)
      case ResolvedAst.Expression.Default(tpe0, loc) =>
        val tpe = reassembleTypeVar(tpe0, subst, root)
        KindedAst.Expression.Default(tpe, loc)
      case ResolvedAst.Expression.Apply(exp0, exps0, tpe0, eff0, loc) =>
        val exp = visit(exp0)
        val exps = exps0.map(visit)
        val tpe = reassembleTypeVar(tpe0, subst, root)
        val eff = reassembleTypeVar(eff0, subst, root)
        KindedAst.Expression.Apply(exp, exps, tpe, eff, loc)
      case ResolvedAst.Expression.Lambda(fparam0, exp0, tpe0, loc) =>
        val fparam = reassembleFparam(fparam0, subst, root)
        val exp = visit(exp0)
        val tpe = reassembleTypeVar(tpe0, subst, root)
        KindedAst.Expression.Lambda(fparam, exp, tpe, loc)
      case ResolvedAst.Expression.Unary(sop, exp0, tpe0, loc) =>
        val exp = visit(exp0)
        val tpe = reassembleTypeVar(tpe0, subst, root)
        KindedAst.Expression.Unary(sop, exp, tpe, loc)
      case ResolvedAst.Expression.Binary(sop, exp10, exp20, tpe0, loc) =>
        val exp1 = visit(exp10)
        val exp2 = visit(exp20)
        val tpe = reassembleTypeVar(tpe0, subst, root)
        KindedAst.Expression.Binary(sop, exp1, exp2, tpe, loc)
      case ResolvedAst.Expression.IfThenElse(exp10, exp20, exp30, loc) =>
        val exp1 = visit(exp10)
        val exp2 = visit(exp20)
        val exp3 = visit(exp30)
        KindedAst.Expression.IfThenElse(exp1, exp2, exp3, loc)
      case ResolvedAst.Expression.Stm(exp10, exp20, loc) =>
        val exp1 = visit(exp10)
        val exp2 = visit(exp20)
        KindedAst.Expression.Stm(exp1, exp2, loc)
      case ResolvedAst.Expression.Let(sym, mod, exp10, exp20, loc) =>
        val exp1 = visit(exp10)
        val exp2 = visit(exp20)
        KindedAst.Expression.Let(sym, mod, exp1, exp2, loc)
      case ResolvedAst.Expression.LetRegion(sym, exp0, evar0, loc) =>
        val exp = visit(exp0)
        val evar = reassembleTypeVar(evar0, subst, root)
        KindedAst.Expression.LetRegion(sym, exp, evar, loc)
      case ResolvedAst.Expression.Match(exp0, rules0, loc) =>
        val exp = visit(exp0)
        val rules = rules0.map(visitMatchRule)
        KindedAst.Expression.Match(exp, rules, loc)
      case ResolvedAst.Expression.Choose(star, exps0, rules0, tpe0, loc) =>
        val exps = exps0.map(visit)
        val rules = rules0.map(visitChoiceRule)
        val tpe = reassembleTypeVar(tpe0, subst, root)
        KindedAst.Expression.Choose(star, exps, rules, tpe, loc)
      case ResolvedAst.Expression.Tag(sym, tag, exp0, tpe0, loc) =>
        val exp = visit(exp0)
        val tpe = reassembleTypeVar(tpe0, subst, root)
        KindedAst.Expression.Tag(sym, tag, exp, tpe, loc)
      case ResolvedAst.Expression.Tuple(elms0, loc) =>
        val elms = elms0.map(visit)
        KindedAst.Expression.Tuple(elms, loc)
      case ResolvedAst.Expression.RecordEmpty(tpe0, loc) =>
        val tpe = reassembleTypeVar(tpe0, subst, root)
        KindedAst.Expression.RecordEmpty(tpe, loc)
      case ResolvedAst.Expression.RecordSelect(exp0, field, tpe0, loc) =>
        val exp = visit(exp0)
        val tpe = reassembleTypeVar(tpe0, subst, root)
        KindedAst.Expression.RecordSelect(exp, field, tpe, loc)
      case ResolvedAst.Expression.RecordExtend(field, value0, rest0, tpe0, loc) =>
        val value = visit(value0)
        val rest = visit(rest0)
        val tpe = reassembleTypeVar(tpe0, subst, root)
        KindedAst.Expression.RecordExtend(field, value, rest, tpe, loc)
      case ResolvedAst.Expression.RecordRestrict(field, rest0, tpe0, loc) =>
        val rest = visit(rest0)
        val tpe = reassembleTypeVar(tpe0, subst, root)
        KindedAst.Expression.RecordRestrict(field, rest, tpe, loc)
      case ResolvedAst.Expression.ArrayLit(elms0, tpe0, loc) =>
        val elms = elms0.map(visit)
        val tpe = reassembleTypeVar(tpe0, subst, root)
        KindedAst.Expression.ArrayLit(elms, tpe, loc)
      case ResolvedAst.Expression.ArrayNew(elm0, len0, tpe0, loc) =>
        val elm = visit(elm0)
        val len = visit(len0)
        val tpe = reassembleTypeVar(tpe0, subst, root)
        KindedAst.Expression.ArrayNew(elm, len, tpe, loc)
      case ResolvedAst.Expression.ArrayLoad(base0, index0, tpe0, loc) =>
        val base = visit(base0)
        val index = visit(index0)
        val tpe = reassembleTypeVar(tpe0, subst, root)
        KindedAst.Expression.ArrayLoad(base, index, tpe, loc)
      case ResolvedAst.Expression.ArrayStore(base0, index0, elm0, loc) =>
        val base = visit(base0)
        val index = visit(index0)
        val elm = visit(elm0)
        KindedAst.Expression.ArrayStore(base, index, elm, loc)
      case ResolvedAst.Expression.ArrayLength(base0, loc) =>
        val base = visit(base0)
        KindedAst.Expression.ArrayLength(base, loc)
      case ResolvedAst.Expression.ArraySlice(base0, beginIndex0, endIndex0, loc) =>
        val base = visit(base0)
        val beginIndex = visit(beginIndex0)
        val endIndex = visit(endIndex0)
        KindedAst.Expression.ArraySlice(base, beginIndex, endIndex, loc)
      case ResolvedAst.Expression.Ref(exp0, tvar0, loc) =>
        val exp = visit(exp0)
        val tvar = reassembleTypeVar(tvar0, subst, root)
        KindedAst.Expression.Ref(exp, tvar, loc)
      case ResolvedAst.Expression.RefWithRegion(exp10, exp20, tpe0, evar0, loc)  =>
        val exp1 = visit(exp10)
        val exp2 = visit(exp20)
        val tvar = reassembleTypeVar(tpe0, subst, root)
        val evar = reassembleTypeVar(evar0, subst, root)
        KindedAst.Expression.RefWithRegion(exp1, exp2, tvar, evar, loc)
      case ResolvedAst.Expression.Deref(exp0, tvar0, evar0, loc) =>
        val exp = visit(exp0)
        val tvar = reassembleTypeVar(tvar0, subst, root)
        val evar = reassembleTypeVar(evar0, subst, root)
        KindedAst.Expression.Deref(exp, tvar, evar, loc)
      case ResolvedAst.Expression.Assign(exp10, exp20, evar0, loc) =>
        val exp1 = visit(exp10)
        val exp2 = visit(exp20)
        val evar = reassembleTypeVar(evar0, subst, root)
        KindedAst.Expression.Assign(exp1, exp2, evar, loc)
      case ResolvedAst.Expression.Existential(fparam0, exp0, loc) =>
        val fparam = reassembleFparam(fparam0, subst, root)
        val exp = visit(exp0)
        KindedAst.Expression.Existential(fparam, exp, loc)
      case ResolvedAst.Expression.Universal(fparam0, exp0, loc) =>
        val fparam = reassembleFparam(fparam0, subst, root)
        val exp = visit(exp0)
        KindedAst.Expression.Universal(fparam, exp, loc)
      case ResolvedAst.Expression.Ascribe(exp0, expectedType0, expectedEff0, tpe0, loc) =>
        val exp = visit(exp0)
        val expectedType = expectedType0.map(reassembleType(_, subst, root))
        val expectedEff = expectedEff0.map(reassembleType(_, subst, root))
        val tpe = reassembleTypeVar(tpe0, subst, root)
        KindedAst.Expression.Ascribe(exp, expectedType, expectedEff, tpe, loc)
      case ResolvedAst.Expression.Cast(exp0, declaredType0, declaredEff0, tpe0, loc) =>
        val exp = visit(exp0)
        val declaredType = declaredType0.map(reassembleType(_, subst, root))
        val declaredEff = declaredEff0.map(reassembleType(_, subst, root))
        val tpe = reassembleTypeVar(tpe0, subst, root)
        KindedAst.Expression.Cast(exp, declaredType, declaredEff, tpe, loc)
      case ResolvedAst.Expression.TryCatch(exp0, rules0, loc) =>
        val exp = visit(exp0)
        val rules = rules0.map(visitCatchRule)
        KindedAst.Expression.TryCatch(exp, rules, loc)
      case ResolvedAst.Expression.InvokeConstructor(constructor, args0, loc) =>
        val args = args0.map(visit)
        KindedAst.Expression.InvokeConstructor(constructor, args, loc)
      case ResolvedAst.Expression.InvokeMethod(method, exp0, args0, loc) =>
        val exp = visit(exp0)
        val args = args0.map(visit)
        KindedAst.Expression.InvokeMethod(method, exp, args, loc)
      case ResolvedAst.Expression.InvokeStaticMethod(method, args0, loc) =>
        val args = args0.map(visit)
        KindedAst.Expression.InvokeStaticMethod(method, args, loc)
      case ResolvedAst.Expression.GetField(field, exp0, loc) =>
        val exp = visit(exp0)
        KindedAst.Expression.GetField(field, exp, loc)
      case ResolvedAst.Expression.PutField(field, exp10, exp20, loc) =>
        val exp1 = visit(exp10)
        val exp2 = visit(exp20)
        KindedAst.Expression.PutField(field, exp1, exp2, loc)
      case ResolvedAst.Expression.GetStaticField(field, loc) =>
        KindedAst.Expression.GetStaticField(field, loc)
      case ResolvedAst.Expression.PutStaticField(field, exp0, loc) =>
        val exp = visit(exp0)
        KindedAst.Expression.PutStaticField(field, exp, loc)
      case ResolvedAst.Expression.NewChannel(exp0, tpe0, loc) =>
        val exp = visit(exp0)
        val tpe = reassembleType(tpe0, subst, root)
        KindedAst.Expression.NewChannel(exp, tpe, loc)
      case ResolvedAst.Expression.GetChannel(exp0, tpe0, loc) =>
        val exp = visit(exp0)
        val tpe = reassembleTypeVar(tpe0, subst, root)
        KindedAst.Expression.GetChannel(exp, tpe, loc)
      case ResolvedAst.Expression.PutChannel(exp10, exp20, tpe0, loc) =>
        val exp1 = visit(exp10)
        val exp2 = visit(exp20)
        val tpe = reassembleTypeVar(tpe0, subst, root)
        KindedAst.Expression.PutChannel(exp1, exp2, tpe, loc)
      case ResolvedAst.Expression.SelectChannel(rules0, default0, tpe0, loc) =>
        val rules = rules0.map(visitSelectChannelRule)
        val default = default0.map(visit)
        val tpe = reassembleTypeVar(tpe0, subst, root)
        KindedAst.Expression.SelectChannel(rules, default, tpe, loc)
      case ResolvedAst.Expression.Spawn(exp0, loc) =>
        val exp = visit(exp0)
        KindedAst.Expression.Spawn(exp, loc)
      case ResolvedAst.Expression.Lazy(exp0, loc) =>
        val exp = visit(exp0)
        KindedAst.Expression.Lazy(exp, loc)
      case ResolvedAst.Expression.Force(exp0, tpe0, loc) =>
        val exp = visit(exp0)
        val tpe = reassembleTypeVar(tpe0, subst, root)
        KindedAst.Expression.Force(exp, tpe, loc)
      case ResolvedAst.Expression.FixpointConstraintSet(cs0, tpe0, loc) =>
        val cs = cs0.map(visitConstraint)
        val tpe = reassembleTypeVar(tpe0, subst, root)
        KindedAst.Expression.FixpointConstraintSet(cs, tpe, loc)
      case ResolvedAst.Expression.FixpointMerge(exp10, exp20, loc) =>
        val exp1 = visit(exp10)
        val exp2 = visit(exp20)
        KindedAst.Expression.FixpointMerge(exp1, exp2, loc)
      case ResolvedAst.Expression.FixpointSolve(exp0, loc) =>
        val exp = visit(exp0)
        KindedAst.Expression.FixpointSolve(exp, loc)
      case ResolvedAst.Expression.FixpointFilter(pred, exp0, tpe0, loc) =>
        val exp = visit(exp0)
        val tpe = reassembleTypeVar(tpe0, subst, root)
        KindedAst.Expression.FixpointFilter(pred, exp, tpe, loc)
      case ResolvedAst.Expression.FixpointProjectIn(exp0, pred, tpe0, loc) =>
        val exp = visit(exp0)
        val tpe = reassembleTypeVar(tpe0, subst, root)
        KindedAst.Expression.FixpointProjectIn(exp, pred, tpe, loc)
      case ResolvedAst.Expression.FixpointProjectOut(pred, exp10, exp20, tpe0, loc) =>
        val exp1 = visit(exp10)
        val exp2 = visit(exp20)
        val tpe = reassembleTypeVar(tpe0, subst, root)
        KindedAst.Expression.FixpointProjectOut(pred, exp1, exp2, tpe, loc)
      case ResolvedAst.Expression.MatchEff(exp10, exp20, exp30, loc) =>
        val exp1 = visit(exp10)
        val exp2 = visit(exp20)
        val exp3 = visit(exp30)
        KindedAst.Expression.MatchEff(exp1, exp2, exp3, loc)
    }

    def visitMatchRule(rule: ResolvedAst.MatchRule): KindedAst.MatchRule = rule match {
      case ResolvedAst.MatchRule(pat0, guard0, exp0) =>
        val pat = visitPattern(pat0)
        val guard = visit(guard0)
        val exp = visit(exp0)
        KindedAst.MatchRule(pat, guard, exp)
    }

    def visitChoiceRule(rule: ResolvedAst.ChoiceRule): KindedAst.ChoiceRule = rule match {
      case ResolvedAst.ChoiceRule(pat0, exp0) =>
        val pat = pat0.map(visitChoicePattern)
        val exp = visit(exp0)
        KindedAst.ChoiceRule(pat, exp)
    }

    def visitCatchRule(rule: ResolvedAst.CatchRule): KindedAst.CatchRule = rule match {
      case ResolvedAst.CatchRule(sym, clazz, exp0) =>
        val exp = visit(exp0)
        KindedAst.CatchRule(sym, clazz, exp)
    }

    def visitSelectChannelRule(rule: ResolvedAst.SelectChannelRule): KindedAst.SelectChannelRule = rule match {
      case ResolvedAst.SelectChannelRule(sym, chan0, exp0) =>
        val chan = visit(chan0)
        val exp = visit(exp0)
        KindedAst.SelectChannelRule(sym, chan, exp)
    }

    def visitPattern(pattern: ResolvedAst.Pattern): KindedAst.Pattern = pattern match {
      case ResolvedAst.Pattern.Wild(tvar0, loc) =>
        val tvar = reassembleTypeVar(tvar0, subst, root)
        KindedAst.Pattern.Wild(tvar, loc)
      case ResolvedAst.Pattern.Var(sym, tvar0, loc) =>
        val tvar = reassembleTypeVar(tvar0, subst, root)
        KindedAst.Pattern.Var(sym, tvar, loc)
      case ResolvedAst.Pattern.Unit(loc) => KindedAst.Pattern.Unit(loc)
      case ResolvedAst.Pattern.True(loc) => KindedAst.Pattern.True(loc)
      case ResolvedAst.Pattern.False(loc) => KindedAst.Pattern.False(loc)
      case ResolvedAst.Pattern.Char(lit, loc) => KindedAst.Pattern.Char(lit, loc)
      case ResolvedAst.Pattern.Float32(lit, loc) => KindedAst.Pattern.Float32(lit, loc)
      case ResolvedAst.Pattern.Float64(lit, loc) => KindedAst.Pattern.Float64(lit, loc)
      case ResolvedAst.Pattern.Int8(lit, loc) => KindedAst.Pattern.Int8(lit, loc)
      case ResolvedAst.Pattern.Int16(lit, loc) => KindedAst.Pattern.Int16(lit, loc)
      case ResolvedAst.Pattern.Int32(lit, loc) => KindedAst.Pattern.Int32(lit, loc)
      case ResolvedAst.Pattern.Int64(lit, loc) => KindedAst.Pattern.Int64(lit, loc)
      case ResolvedAst.Pattern.BigInt(lit, loc) => KindedAst.Pattern.BigInt(lit, loc)
      case ResolvedAst.Pattern.Str(lit, loc) => KindedAst.Pattern.Str(lit, loc)
      case ResolvedAst.Pattern.Tag(sym, tag, pat0, tvar0, loc) =>
        val pat = visitPattern(pat0)
        val tvar = reassembleTypeVar(tvar0, subst, root)
        KindedAst.Pattern.Tag(sym, tag, pat, tvar, loc)
      case ResolvedAst.Pattern.Tuple(elms0, loc) =>
        val elms = elms0.map(visitPattern)
        KindedAst.Pattern.Tuple(elms, loc)
      case ResolvedAst.Pattern.Array(elms0, tvar0, loc) =>
        val elms = elms0.map(visitPattern)
        val tvar = reassembleTypeVar(tvar0, subst, root)
        KindedAst.Pattern.Array(elms, tvar, loc)
      case ResolvedAst.Pattern.ArrayTailSpread(elms0, sym, tvar0, loc) =>
        val elms = elms0.map(visitPattern)
        val tvar = reassembleTypeVar(tvar0, subst, root)
        KindedAst.Pattern.ArrayTailSpread(elms, sym, tvar, loc)
      case ResolvedAst.Pattern.ArrayHeadSpread(sym, elms0, tvar0, loc) =>
        val elms = elms0.map(visitPattern)
        val tvar = reassembleTypeVar(tvar0, subst, root)
        KindedAst.Pattern.ArrayHeadSpread(sym, elms, tvar, loc)
    }

    def visitChoicePattern(pattern: ResolvedAst.ChoicePattern): KindedAst.ChoicePattern = pattern match {
      case ResolvedAst.ChoicePattern.Wild(loc) => KindedAst.ChoicePattern.Wild(loc)
      case ResolvedAst.ChoicePattern.Absent(loc) => KindedAst.ChoicePattern.Absent(loc)
      case ResolvedAst.ChoicePattern.Present(sym, tvar0, loc) =>
        val tvar = reassembleTypeVar(tvar0, subst, root)
        KindedAst.ChoicePattern.Present(sym, tvar, loc)
    }

    def visitConstraint(constraint: ResolvedAst.Constraint): KindedAst.Constraint = constraint match {
      case ResolvedAst.Constraint(cparams0, head0, body0, loc) =>
        val cparams = cparams0.map(visitCparam)
        val head = visitHead(head0)
        val body = body0.map(visitBody)
        KindedAst.Constraint(cparams, head, body, loc)
    }

    def visitCparam(cparam: ResolvedAst.ConstraintParam): KindedAst.ConstraintParam = cparam match {
      case ResolvedAst.ConstraintParam.HeadParam(sym, tpe0, loc) =>
        val tpe = reassembleTypeVar(tpe0, subst, root)
        KindedAst.ConstraintParam.HeadParam(sym, tpe, loc)
      case ResolvedAst.ConstraintParam.RuleParam(sym, tpe0, loc) =>
        val tpe = reassembleTypeVar(tpe0, subst, root)
        KindedAst.ConstraintParam.RuleParam(sym, tpe, loc)
    }

    def visitHead(head: ResolvedAst.Predicate.Head): KindedAst.Predicate.Head = head match {
      case ResolvedAst.Predicate.Head.Atom(pred, den, terms0, tvar0, loc) =>
        val terms = terms0.map(visit)
        val tvar = reassembleTypeVar(tvar0, subst, root)
        KindedAst.Predicate.Head.Atom(pred, den, terms, tvar, loc)
    }

    def visitBody(body: ResolvedAst.Predicate.Body): KindedAst.Predicate.Body = body match {
      case ResolvedAst.Predicate.Body.Atom(pred, den, polarity, terms0, tvar0, loc) =>
        val terms = terms0.map(visitPattern)
        val tvar = reassembleTypeVar(tvar0, subst, root)
        KindedAst.Predicate.Body.Atom(pred, den, polarity, terms, tvar, loc)
      case ResolvedAst.Predicate.Body.Guard(exp0, loc) =>
        val exp = visit(exp0)
        KindedAst.Predicate.Body.Guard(exp, loc)
    }

    visit(exp000)
  }

  private def reassembleType(tpe0: UnkindedType, subst: KindSubstitution, root: ResolvedAst.Root)(implicit flix: Flix): Type = {

    def visit(tpe: UnkindedType): Type = {
      tpe match {
        case UnkindedType.Cst(cst, loc) => Type.Cst(reassembleTypeConstructor(cst, root), loc)
        case UnkindedType.Apply(t1, t2) => Type.Apply(visit(t1), visit(t2))
        case UnkindedType.Lambda(t1, t2) => Type.Lambda(visit(t1).asInstanceOf[Type.Var], visit(t2)) // MATT how to avoid cast
        case tvar: UnkindedType.Var => reassembleTypeVar(tvar, subst, root)
      }
    }

    visit(tpe0)
  }

  private def reassembleTypeVar(tvar0: UnkindedType.Var, subst: KindSubstitution, root: ResolvedAst.Root): Type.Var = tvar0 match {
    case UnkindedType.Var(id, kvar, text) =>
      val kind = subst(kvar) // MATT need to check for not found?
      Type.Var(id, kind, text = text)
  }

  private def reassembleTparams(tparams: ResolvedAst.TypeParams, subst: KindSubstitution, root: ResolvedAst.Root): List[KindedAst.TypeParam] = tparams match {
    case ResolvedAst.TypeParams.Kinded(tparams) => tparams.map(reassembleTparam(_, subst, root))
    case ResolvedAst.TypeParams.Unkinded(tparams) => tparams.map(reassembleTparam(_, subst, root))
  }

  private def reassembleTparam(tparam: ResolvedAst.TypeParam, subst: KindSubstitution, root: ResolvedAst.Root): KindedAst.TypeParam = tparam match {
    case ResolvedAst.TypeParam.Kinded(name, tvar0, _, loc) =>
      // MATT kinds should match here (can add assert for checking)
      val tvar = reassembleTypeVar(tvar0, subst, root)
      KindedAst.TypeParam(name, tvar, loc)
    case ResolvedAst.TypeParam.Unkinded(name, tvar0, loc) =>
      // MATT kinds should match here (can add assert for checking)
      val tvar = reassembleTypeVar(tvar0, subst, root)
      KindedAst.TypeParam(name, tvar, loc)
    // MATT copy paste
  }

  private def reassembleFparam(fparam0: ResolvedAst.FormalParam, subst: KindSubstitution, root: ResolvedAst.Root)(implicit flix: Flix): KindedAst.FormalParam = fparam0 match {
    case ResolvedAst.FormalParam(sym, mod, tpe0, loc) =>
      val tpe = reassembleType(tpe0, subst, root)
      KindedAst.FormalParam(sym, mod, tpe, loc)
  }

  private def reassembleScheme(scheme: ResolvedAst.Scheme, subst: KindSubstitution, root: ResolvedAst.Root)(implicit flix: Flix): Scheme = scheme match {
    case ResolvedAst.Scheme(quantifiers0, constraints0, base0) =>
      val quantifiers = quantifiers0.map(reassembleTypeVar(_, subst, root))
      val constraints = constraints0.map(reassembleTconstr(_, subst, root))
      val base = reassembleType(base0, subst, root)
      Scheme(quantifiers, constraints, base)
  }

  private def reassembleTconstr(tconstr: ResolvedAst.TypeConstraint, subst: KindSubstitution, root: ResolvedAst.Root)(implicit flix: Flix): Ast.TypeConstraint = tconstr match {
    case ResolvedAst.TypeConstraint(clazz, tpe0, loc) =>
      val tpe = reassembleType(tpe0, subst, root)
      Ast.TypeConstraint(clazz, tpe, loc)
  }

  private def reassembleCase(case0: ResolvedAst.Case, subst: KindSubstitution, root: ResolvedAst.Root)(implicit flix: Flix): KindedAst.Case = case0 match {
    case ResolvedAst.Case(enum, tag, tpeDeprecated, sc0) =>
      val tpe = reassembleType(tpeDeprecated, subst, root)
      val sc = reassembleScheme(sc0, subst, root)
      KindedAst.Case(enum, tag, tpe, sc)
  }

  private case class KindMatch(assoc: KindMatch.Association, kind: KindMatch.Template)

  private object KindMatch {

    // association doesn't matter
    def wild: KindMatch = KindMatch(Association.SubKind, Template.Wild)

    def subKindOf(template: Template): KindMatch = KindMatch(Association.SubKind, template)

    def superKindOf(template: Template): KindMatch = KindMatch(Association.SuperKind, template)

    sealed trait Association

    object Association {
      case object SubKind extends Association

      case object SuperKind extends Association

      def invert(assoc: Association): Association = assoc match {
        case SubKind => SuperKind
        case SuperKind => SubKind
      }
    }

    sealed trait Template {
      def toKind2: Kind = Template.toKind(this)
    }

    object Template {

      case object Wild extends Template

      case object Star extends Template

      case object Bool extends Template

      case object Record extends Template

      case object Schema extends Template

      case class Arrow(k1: Template, k2: Template) extends Template

      def fromKind(k: Kind): KindMatch.Template = k match {
        case Kind.Var(-1) => Wild // MATT hack
        case Kind.Var(id) => throw InternalCompilerException("unexpected kvar")
        case Kind.Star => Star
        case Kind.Bool => Bool
        case Kind.Record => Record
        case Kind.Schema => Schema
        case Kind.Arrow(k1, k2) => Arrow(fromKind(k1), fromKind(k2))
      }

      def toKind(k: KindMatch.Template): Kind = k match {
        case Star => Kind.Star
        case Bool => Kind.Bool
        case Record => Kind.Record
        case Schema => Kind.Schema
        case Arrow(k1, k2) => Kind.Arrow(toKind(k1), toKind(k2))
        case Wild => Kind.Var(-1) // MATT hack
      }
    }

    def matches(k1: Kind, k2: KindMatch): Boolean = (k1, k2) match {
      case (_, KindMatch(_, Template.Wild)) => true
      case (Kind.Star, KindMatch(_, Template.Star)) => true
      case (Kind.Bool, KindMatch(_, Template.Bool)) => true
      case (Kind.Record, KindMatch(_, Template.Record)) => true
      case (Kind.Schema, KindMatch(_, Template.Schema)) => true

      case (Kind.Record, KindMatch(Association.SubKind, Template.Star)) => true
      case (Kind.Schema, KindMatch(Association.SubKind, Template.Star)) => true

      case (Kind.Star, KindMatch(Association.SuperKind, Template.Record)) => true
      case (Kind.Star, KindMatch(Association.SuperKind, Template.Schema)) => true

      case (Kind.Arrow(k11, k12), KindMatch(assoc, Template.Arrow(k21, k22))) =>
        matches(k11, KindMatch(Association.invert(assoc), k21)) &&
          matches(k12, KindMatch(assoc, k22))

      case _ => false
    }

  }
}
