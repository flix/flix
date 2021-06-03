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
import ca.uwaterloo.flix.language.ast.ResolvedAst.Predicate.{Body, Head}
import ca.uwaterloo.flix.language.ast.ResolvedAst.{ConstraintParam, Pattern, TypeParam, TypeParams}
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.errors.KindError
import ca.uwaterloo.flix.language.phase.unification.KindInferMonad.seqM
import ca.uwaterloo.flix.language.phase.unification.KindUnification.unifyKindM
import ca.uwaterloo.flix.language.phase.unification.{KindInferMonad, KindSubstitution}
import ca.uwaterloo.flix.util.Validation.{ToSuccess, flatMapN, mapN, traverse}
import ca.uwaterloo.flix.util.{Result, Validation}

// MATT docs
object Kinder extends Phase[ResolvedAst.Root, KindedAst.Root] {

  /**
    * Runs the p
    */
  override def run(root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Root, CompilationError] = {
    val enumsVal = Validation.traverse(root.enums) {
      case (sym, enum) => visitEnum(enum, root).map((sym, _))
    }

    val classesVal = Validation.traverse(root.classes) {
      case (sym, clazz) => visitClass(clazz, root).map((sym, _))
    }

    val defsVal = Validation.traverse(root.defs) {
      case (sym, defn) => visitDef(defn, KindInferMonad.point(()), root).map((sym, _))
    }

    val instancesVal = Validation.traverse(root.instances) {
      case (sym, insts0) => traverse(insts0)(visitInstance(_, root)).map((sym, _))
    }

    mapN(enumsVal, classesVal, defsVal, instancesVal) {
      case (enums, classes, defs, instances) =>
        KindedAst.Root(classes.toMap, instances.toMap, defs.toMap, enums.toMap, root.reachable, root.sources)
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
    case TypeParam.Kinded(_, _, kind, _) => kind
    case _: TypeParam.Unkinded => Kind.Star
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
    case UnkindedType.Constructor.Ref => TypeConstructor.Ref
    case UnkindedType.Constructor.Tuple(l) => TypeConstructor.Tuple(l)
    case UnkindedType.Constructor.Relation => TypeConstructor.Relation
    case UnkindedType.Constructor.Lattice => TypeConstructor.Lattice
    case UnkindedType.Constructor.True => TypeConstructor.True
    case UnkindedType.Constructor.False => TypeConstructor.False
    case UnkindedType.Constructor.Not => TypeConstructor.Not
    case UnkindedType.Constructor.And => TypeConstructor.And
    case UnkindedType.Constructor.Or => TypeConstructor.Or
  }

  private def visitSig(sig0: ResolvedAst.Sig, inf0: KindInferMonad[Unit], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Sig, KindError] = sig0 match {
    case ResolvedAst.Sig(sym, spec0, expOpt0) =>
      val inference = for {
        _ <- inf0
        _ <- inferSpec(spec0, root)
        _ <- expOpt0.map(inferExp(_, root)).getOrElse(KindInferMonad.point(()))
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
        _ <- inferExp(exp0, root)
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

  private def visitSpec(spec0: ResolvedAst.Spec, inference: KindInferMonad[Unit], root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Spec, KindError] = spec0 match {
    case ResolvedAst.Spec(doc, ann0, mod, tparams0, fparams0, sc0, eff0, loc) =>
      val KindInferMonad(run) = inference
      val initialSubst = getSubstFromTparams(tparams0)
      run(initialSubst) match {
        case Result.Ok((subst, _)) =>
          val ann = ann0.map(reassembleAnnotation(_, subst, root))
          val tparams = reassembleTparams(tparams0, subst, root)
          val fparams = fparams0.map(reassembleFparam(_, subst, root))
          val sc = reassembleScheme(sc0, subst, root)
          val eff = reassembleType(eff0, subst, root)
          KindedAst.Spec(doc, ann, mod, tparams, fparams, sc, eff, loc).toSuccess

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
    case ResolvedAst.Spec(_, _, _, _, fparams, sc, eff, _) =>
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

  private def inferExp(exp00: ResolvedAst.Expression, root: ResolvedAst.Root)(implicit flix: Flix): KindInferMonad[Unit] = {

    def visit(exp0: ResolvedAst.Expression): KindInferMonad[Unit] = exp0 match {
      case ResolvedAst.Expression.Wild(tpe, loc) =>
        for {
          _ <- unifyKindM(tpe.kvar, Kind.Star, loc)
        } yield ()
      case ResolvedAst.Expression.Var(sym, tpe, loc) =>
        for {
          kind <- inferType(tpe, root)
          _ <- unifyKindM(kind, Kind.Star, loc)
        } yield ()
      case ResolvedAst.Expression.Def(sym, tpe, loc) =>
        for {
          _ <- unifyKindM(tpe.kvar, Kind.Star, loc)
        } yield ()
      case ResolvedAst.Expression.Sig(sym, tpe, loc) =>
        for {
          _ <- unifyKindM(tpe.kvar, Kind.Star, loc)
        } yield ()
      case ResolvedAst.Expression.Hole(sym, tpe, eff, loc) =>
        for {
          _ <- unifyKindM(tpe.kvar, Kind.Star, loc)
          _ <- unifyKindM(eff.kvar, Kind.Bool, loc)
        } yield ()
      case ResolvedAst.Expression.Unit(loc) => KindInferMonad.point(())
      case ResolvedAst.Expression.Null(loc) => KindInferMonad.point(())
      case ResolvedAst.Expression.True(loc) => KindInferMonad.point(())
      case ResolvedAst.Expression.False(loc) => KindInferMonad.point(())
      case ResolvedAst.Expression.Char(lit, loc) => KindInferMonad.point(())
      case ResolvedAst.Expression.Float32(lit, loc) => KindInferMonad.point(())
      case ResolvedAst.Expression.Float64(lit, loc) => KindInferMonad.point(())
      case ResolvedAst.Expression.Int8(lit, loc) => KindInferMonad.point(())
      case ResolvedAst.Expression.Int16(lit, loc) => KindInferMonad.point(())
      case ResolvedAst.Expression.Int32(lit, loc) => KindInferMonad.point(())
      case ResolvedAst.Expression.Int64(lit, loc) => KindInferMonad.point(())
      case ResolvedAst.Expression.BigInt(lit, loc) => KindInferMonad.point(())
      case ResolvedAst.Expression.Str(lit, loc) => KindInferMonad.point(())
      case ResolvedAst.Expression.Default(tpe, loc) => KindInferMonad.point(())
      case ResolvedAst.Expression.Apply(exp, exps, tpe, eff, loc) =>
        for {
          _ <- visit(exp)
          _ <- seqM(exps.map(visit))
          _ <- unifyKindM(tpe.kvar, Kind.Star, loc)
          _ <- unifyKindM(eff.kvar, Kind.Bool, loc)
        } yield ()
      case ResolvedAst.Expression.Lambda(fparam, exp, tpe, loc) =>
        for {
          _ <- inferFparam(fparam, root)
          _ <- visit(exp)
          _ <- unifyKindM(tpe.kvar, Kind.Star, loc)
        } yield ()
      case ResolvedAst.Expression.Unary(sop, exp, tpe, loc) =>
        for {
          _ <- visit(exp)
          _ <- unifyKindM(tpe.kvar, Kind.Star, loc)
        } yield ()
      case ResolvedAst.Expression.Binary(sop, exp1, exp2, tpe, loc) =>
        for {
          _ <- visit(exp1)
          _ <- visit(exp2)
          _ <- unifyKindM(tpe.kvar, Kind.Star, loc)
        } yield ()
      case ResolvedAst.Expression.IfThenElse(exp1, exp2, exp3, loc) =>
        for {
          _ <- visit(exp1)
          _ <- visit(exp2)
          _ <- visit(exp3)
        } yield ()
      case ResolvedAst.Expression.Stm(exp1, exp2, loc) =>
        for {
          _ <- visit(exp1)
          _ <- visit(exp2)
        } yield ()
      case ResolvedAst.Expression.Let(sym, exp1, exp2, loc) =>
        for {
          _ <- visit(exp1)
          _ <- visit(exp2)
        } yield ()
      case ResolvedAst.Expression.Match(exp, rules, loc) =>
        for {
          _ <- visit(exp)
          _ <- seqM(rules.map(visitMatchRule))
        } yield ()
      case ResolvedAst.Expression.Choose(star, exps, rules, tpe, loc) =>
        for {
          _ <- seqM(exps.map(visit))
          _ <- seqM(rules.map(visitChoiceRule))
          _ <- unifyKindM(tpe.kvar, Kind.Star, loc)
        } yield ()
      case ResolvedAst.Expression.Tag(sym, tag, exp, tpe, loc) =>
        for {
          _ <- visit(exp)
          _ <- unifyKindM(tpe.kvar, Kind.Star, loc)
        } yield ()
      case ResolvedAst.Expression.Tuple(elms, loc) =>
        for {
          _ <- seqM(elms.map(visit))
        } yield ()
      case ResolvedAst.Expression.RecordEmpty(tpe, loc) =>
        for {
          _ <- unifyKindM(tpe.kvar, Kind.Record, loc)
        } yield ()
      case ResolvedAst.Expression.RecordSelect(exp, field, tpe, loc) =>
        for {
          _ <- visit(exp)
          _ <- unifyKindM(tpe.kvar, Kind.Star, loc)
        } yield ()
      case ResolvedAst.Expression.RecordExtend(field, value, rest, tpe, loc) =>
        for {
          _ <- visit(value)
          _ <- visit(rest)
          _ <- unifyKindM(tpe.kvar, Kind.Record, loc)
        } yield ()
      case ResolvedAst.Expression.RecordRestrict(field, rest, tpe, loc) =>
        for {
          _ <- visit(rest)
          _ <- unifyKindM(tpe.kvar, Kind.Record, loc)
        } yield ()
      case ResolvedAst.Expression.ArrayLit(elms, tpe, loc) =>
        for {
          _ <- seqM(elms.map(visit))
          _ <- unifyKindM(tpe.kvar, Kind.Star, loc)
        } yield ()
      case ResolvedAst.Expression.ArrayNew(elm, len, tpe, loc) =>
        for {
          _ <- visit(elm)
          _ <- visit(len)
          _ <- unifyKindM(tpe.kvar, Kind.Star, loc)
        } yield ()
      case ResolvedAst.Expression.ArrayLoad(base, index, tpe, loc) =>
        for {
          _ <- visit(base)
          _ <- visit(index)
          _ <- unifyKindM(tpe.kvar, Kind.Star, loc)
        } yield ()
      case ResolvedAst.Expression.ArrayStore(base, index, elm, loc) =>
        for {
          _ <- visit(base)
          _ <- visit(index)
          _ <- visit(elm)
        } yield ()
      case ResolvedAst.Expression.ArrayLength(base, loc) =>
        for {
          _ <- visit(base)
        } yield ()
      case ResolvedAst.Expression.ArraySlice(base, beginIndex, endIndex, loc) =>
        for {
          _ <- visit(base)
          _ <- visit(beginIndex)
          _ <- visit(endIndex)
        } yield ()
      case ResolvedAst.Expression.Ref(exp, loc) =>
        for {
          _ <- visit(exp)
        } yield ()
      case ResolvedAst.Expression.Deref(exp, tpe, loc) =>
        for {
          _ <- visit(exp)
          _ <- unifyKindM(tpe.kvar, Kind.Star, loc)
        } yield ()
      case ResolvedAst.Expression.Assign(exp1, exp2, loc) =>
        for {
          _ <- visit(exp1)
          _ <- visit(exp2)
        } yield ()
      case ResolvedAst.Expression.Existential(fparam, exp, loc) =>
        for {
          _ <- inferFparam(fparam, root)
          _ <- visit(exp)
        } yield ()
      case ResolvedAst.Expression.Universal(fparam, exp, loc) =>
        for {
          _ <- inferFparam(fparam, root)
          _ <- visit(exp)
        } yield ()
      case ResolvedAst.Expression.Ascribe(exp, expectedType, expectedEff, tpe, loc) =>
        for {
          _ <- visit(exp)
          expectedTypeKind <- expectedType.map(inferType(_, root)).getOrElse(KindInferMonad.point(Kind.Star))
          expectedEffKind <- expectedEff.map(inferType(_, root)).getOrElse(KindInferMonad.point(Kind.Bool))
          _ <- unifyKindM(expectedTypeKind, Kind.Star, loc)
          _ <- unifyKindM(expectedEffKind, Kind.Bool, loc)
          _ <- unifyKindM(tpe.kvar, Kind.Star, loc)
        } yield ()
      case ResolvedAst.Expression.Cast(exp, declaredType, declaredEff, tpe, loc) =>
        for {
          _ <- visit(exp)
          declaredTypeKind <- declaredType.map(inferType(_, root)).getOrElse(KindInferMonad.point(Kind.Star))
          declaredEffKind <- declaredEff.map(inferType(_, root)).getOrElse(KindInferMonad.point(Kind.Bool))
          _ <- unifyKindM(declaredTypeKind, Kind.Star, loc)
          _ <- unifyKindM(declaredEffKind, Kind.Bool, loc)
          _ <- unifyKindM(tpe.kvar, Kind.Star, loc)
        } yield ()
      case ResolvedAst.Expression.TryCatch(exp, rules, loc) =>
        for {
          _ <- visit(exp)
          _ <- seqM(rules.map(visitCatchRule))
        } yield ()
      case ResolvedAst.Expression.InvokeConstructor(constructor, args, loc) =>
        for {
          _ <- seqM(args.map(visit))
        } yield ()
      case ResolvedAst.Expression.InvokeMethod(method, exp, args, loc) =>
        for {
          _ <- visit(exp)
          _ <- seqM(args.map(visit))
        } yield ()
      case ResolvedAst.Expression.InvokeStaticMethod(method, args, loc) =>
        for {
          _ <- seqM(args.map(visit))
        } yield ()
      case ResolvedAst.Expression.GetField(field, exp, loc) =>
        for {
          _ <- visit(exp)
        } yield ()
      case ResolvedAst.Expression.PutField(field, exp1, exp2, loc) =>
        for {
          _ <- visit(exp1)
          _ <- visit(exp2)
        } yield ()
      case ResolvedAst.Expression.GetStaticField(field, loc) => KindInferMonad.point(())
      case ResolvedAst.Expression.PutStaticField(field, exp, loc) =>
        for {
          _ <- visit(exp)
        } yield ()
      case ResolvedAst.Expression.NewChannel(exp, tpe, loc) =>
        for {
          _ <- visit(exp)
          kind <- inferType(tpe, root)
          _ <- unifyKindM(kind, Kind.Star, loc)
        } yield ()
      case ResolvedAst.Expression.GetChannel(exp, tpe, loc) =>
        for {
          _ <- visit(exp)
          _ <- unifyKindM(tpe.kvar, Kind.Star, loc)
        } yield ()
      case ResolvedAst.Expression.PutChannel(exp1, exp2, tpe, loc) =>
        for {
          _ <- visit(exp1)
          _ <- visit(exp2)
          _ <- unifyKindM(tpe.kvar, Kind.Star, loc)
        } yield ()
      case ResolvedAst.Expression.SelectChannel(rules, default, tpe, loc) =>
        for {
          _ <- seqM(rules.map(visitSelectChannelRule))
          _ <- default.map(visit).getOrElse(KindInferMonad.point(())) // MATT change seqM to take iterable
          _ <- unifyKindM(tpe.kvar, Kind.Star, loc)
        } yield ()
      case ResolvedAst.Expression.Spawn(exp, loc) =>
        for {
          _ <- visit(exp)
        } yield ()
      case ResolvedAst.Expression.Lazy(exp, loc) =>
        for {
          _ <- visit(exp)
        } yield ()
      case ResolvedAst.Expression.Force(exp, tpe, loc) =>
        for {
          _ <- visit(exp)
          _ <- unifyKindM(tpe.kvar, Kind.Star, loc)
        } yield ()
      case ResolvedAst.Expression.FixpointConstraintSet(cs, tpe, loc) =>
        for {
          _ <- seqM(cs.map(visitConstraint))
          _ <- unifyKindM(tpe.kvar, Kind.Schema, loc)
        } yield ()
      case ResolvedAst.Expression.FixpointMerge(exp1, exp2, loc) =>
        for {
          _ <- visit(exp1)
          _ <- visit(exp2)
        } yield ()
      case ResolvedAst.Expression.FixpointSolve(exp, loc) =>
        for {
          _ <- visit(exp)
        } yield ()
      case ResolvedAst.Expression.FixpointFilter(pred, exp, tpe, loc) =>
        for {
          _ <- visit(exp)
          _ <- unifyKindM(tpe.kvar, Kind.Schema, loc) // MATT right?
        } yield ()
      case ResolvedAst.Expression.FixpointProjectIn(exp, pred, tpe, loc) =>
        for {
          _ <- visit(exp)
          _ <- unifyKindM(tpe.kvar, Kind.Schema, loc) // MATT right?
        } yield ()
      case ResolvedAst.Expression.FixpointProjectOut(pred, exp1, exp2, tpe, loc) =>
        for {
          _ <- visit(exp1)
          _ <- visit(exp2)
          _ <- unifyKindM(tpe.kvar, Kind.Star, loc) // MATT right?
        } yield ()
    }

    def visitMatchRule(rule: ResolvedAst.MatchRule): KindInferMonad[Unit] = rule match {
      case ResolvedAst.MatchRule(pat, guard, exp) =>
        for {
          _ <- visitPattern(pat)
          _ <- visit(guard)
          _ <- visit(exp)
        } yield ()
    }

    def visitChoiceRule(rule: ResolvedAst.ChoiceRule): KindInferMonad[Unit] = rule match {
      case ResolvedAst.ChoiceRule(pat, exp) =>
        for {
          _ <- seqM(pat.map(visitChoicePattern))
          _ <- visit(exp)
        } yield ()
    }

    def visitCatchRule(rule: ResolvedAst.CatchRule): KindInferMonad[Unit] = rule match {
      case ResolvedAst.CatchRule(sym, clazz, exp) =>
        for {
          _ <- visit(exp)
        } yield ()
    }

    def visitSelectChannelRule(rule: ResolvedAst.SelectChannelRule): KindInferMonad[Unit] = rule match {
      case ResolvedAst.SelectChannelRule(sym, chan, exp) =>
        for {
          _ <- visit(chan)
          _ <- visit(exp)
        } yield ()
    }

    def visitPattern(pattern: ResolvedAst.Pattern): KindInferMonad[Unit] = pattern match {
      case Pattern.Wild(tvar, loc) =>
        for {
          _ <- unifyKindM(tvar.kvar, Kind.Star, loc)
        } yield ()
      case Pattern.Var(sym, tvar, loc) =>
        for {
          _ <- unifyKindM(tvar.kvar, Kind.Star, loc)
        } yield ()
      case Pattern.Unit(loc) => KindInferMonad.point(())
      case Pattern.True(loc) => KindInferMonad.point(())
      case Pattern.False(loc) => KindInferMonad.point(())
      case Pattern.Char(lit, loc) => KindInferMonad.point(())
      case Pattern.Float32(lit, loc) => KindInferMonad.point(())
      case Pattern.Float64(lit, loc) => KindInferMonad.point(())
      case Pattern.Int8(lit, loc) => KindInferMonad.point(())
      case Pattern.Int16(lit, loc) => KindInferMonad.point(())
      case Pattern.Int32(lit, loc) => KindInferMonad.point(())
      case Pattern.Int64(lit, loc) => KindInferMonad.point(())
      case Pattern.BigInt(lit, loc) => KindInferMonad.point(())
      case Pattern.Str(lit, loc) => KindInferMonad.point(())
      case Pattern.Tag(sym, tag, pat, tvar, loc) =>
        for {
          _ <- visitPattern(pat)
          _ <- unifyKindM(tvar.kvar, Kind.Star, loc)
        } yield ()
      case Pattern.Tuple(elms, loc) =>
        for {
          _ <- seqM(elms.map(visitPattern))
        } yield ()
      case Pattern.Array(elms, tvar, loc) =>
        for {
          _ <- seqM(elms.map(visitPattern))
          _ <- unifyKindM(tvar.kvar, Kind.Star, loc)
        } yield ()
      case Pattern.ArrayTailSpread(elms, sym, tvar, loc) =>
        for {
          _ <- seqM(elms.map(visitPattern))
          _ <- unifyKindM(tvar.kvar, Kind.Star, loc)
        } yield ()
      case Pattern.ArrayHeadSpread(sym, elms, tvar, loc) =>
        for {
          _ <- seqM(elms.map(visitPattern))
          _ <- unifyKindM(tvar.kvar, Kind.Star, loc)
        } yield ()
    }

    def visitChoicePattern(pattern: ResolvedAst.ChoicePattern): KindInferMonad[Unit] = pattern match {
      case ResolvedAst.ChoicePattern.Wild(loc) => KindInferMonad.point(())
      case ResolvedAst.ChoicePattern.Absent(loc) => KindInferMonad.point(())
      case ResolvedAst.ChoicePattern.Present(sym, tvar, loc) =>
        for {
          _ <- unifyKindM(tvar.kvar, Kind.Star, loc)
        } yield ()
    }

    def visitConstraint(constraint: ResolvedAst.Constraint): KindInferMonad[Unit] = constraint match {
      case ResolvedAst.Constraint(cparams, head, body, loc) =>
        for {
          _ <- seqM(cparams.map(visitCparam))
          _ <- visitHead(head)
          _ <- seqM(body.map(visitBody))
        } yield ()
    }

    def visitCparam(cparam: ResolvedAst.ConstraintParam): KindInferMonad[Unit] = cparam match {
      case ConstraintParam.HeadParam(sym, tpe, loc) =>
        for {
          _ <- unifyKindM(tpe.kvar, Kind.Star, loc)
        } yield ()
      case ConstraintParam.RuleParam(sym, tpe, loc) =>
        for {
          _ <- unifyKindM(tpe.kvar, Kind.Star, loc)
        } yield ()
    }

    def visitHead(head: ResolvedAst.Predicate.Head): KindInferMonad[Unit] = head match {
      case Head.Atom(pred, den, terms, tvar, loc) =>
        for {
          _ <- seqM(terms.map(visit))
          _ <- unifyKindM(tvar.kvar, Kind.Star, loc)
        } yield ()
    }

    def visitBody(body: ResolvedAst.Predicate.Body): KindInferMonad[Unit] = body match {
      case Body.Atom(pred, den, polarity, terms, tvar, loc) =>
        for {
          _ <- seqM(terms.map(visitPattern))
          _ <- unifyKindM(tvar.kvar, Kind.Star, loc)
        } yield ()
      case Body.Guard(exp, loc) =>
        for {
          _ <- visit(exp)
        } yield ()
    }

    visit(exp00)
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
    case UnkindedType.Constructor.Ref => Kind.Star ->: Kind.Star
    case UnkindedType.Constructor.Tuple(l) => Kind.mkArrow(l)
    case UnkindedType.Constructor.Relation => Kind.Star ->: Kind.Star
    case UnkindedType.Constructor.Lattice => Kind.Star ->: Kind.Star
    case UnkindedType.Constructor.True => Kind.Bool
    case UnkindedType.Constructor.False => Kind.Bool
    case UnkindedType.Constructor.Not => Kind.Bool ->: Kind.Bool
    case UnkindedType.Constructor.And => Kind.Bool ->: Kind.Bool ->: Kind.Bool
    case UnkindedType.Constructor.Or => Kind.Bool ->: Kind.Bool ->: Kind.Bool
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
      case ResolvedAst.Expression.Let(sym, exp10, exp20, loc) =>
        val exp1 = visit(exp10)
        val exp2 = visit(exp20)
        KindedAst.Expression.Let(sym, exp1, exp2, loc)
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
      case ResolvedAst.Expression.Ref(exp0, loc) =>
        val exp = visit(exp0)
        KindedAst.Expression.Ref(exp, loc)
      case ResolvedAst.Expression.Deref(exp0, tpe0, loc) =>
        val exp = visit(exp0)
        val tpe = reassembleTypeVar(tpe0, subst, root)
        KindedAst.Expression.Deref(exp, tpe, loc)
      case ResolvedAst.Expression.Assign(exp10, exp20, loc) =>
        val exp1 = visit(exp10)
        val exp2 = visit(exp20)
        KindedAst.Expression.Assign(exp1, exp2, loc)
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
    case TypeParams.Kinded(tparams) => tparams.map(reassembleTparam(_, subst, root))
    case TypeParams.Unkinded(tparams) => tparams.map(reassembleTparam(_, subst, root))
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

}
