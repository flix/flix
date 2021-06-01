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
import ca.uwaterloo.flix.language.ast.ResolvedAst.{Expression, TypeParam, TypeParams}
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.errors.KindError
import ca.uwaterloo.flix.language.phase.unification.KindInferMonad.seqM
import ca.uwaterloo.flix.language.phase.unification.KindUnification.unifyKindM
import ca.uwaterloo.flix.language.phase.unification.{KindInferMonad, KindSubstitution, KindUnification}
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
      case (sym, defn) => visitDef(defn, Map.empty, root).map((sym, _))
    }

    val instancesVal = Validation.traverse(root.instances) {
      case (sym, insts0) => traverse(insts0)(visitInstance(_, root)).map((sym, _))
    }

    mapN(enumsVal, classesVal, defsVal, instancesVal) {
      case (enums, classes, defs, instances) =>
        // MATT just hack around properties for now
        KindedAst.Root(classes.toMap, instances.toMap, defs.toMap, enums.toMap, Nil, root.reachable, root.sources)
    }

  }

  // MATT docs
  private def visitEnum(enum: ResolvedAst.Enum, root: ResolvedAst.Root): Validation[KindedAst.Enum, CompilationError] = enum match {
    case ResolvedAst.Enum(doc, mod, sym, tparams0, cases0, tpeDeprecated, sc0, loc) =>
      val (tparams, ascriptions) = visitTparams(tparams0)
      val casesVal = traverse(cases0) {
        case (_, ResolvedAst.Case(enum, tag, tpeDeprecated, sc)) =>
          val schemeVal = ascribeScheme(sc, ascriptions, root)
          val tpeVal = ascribeType(tpeDeprecated, KindMatch.Star, ascriptions, root)
          mapN(schemeVal, tpeVal) {
            case (scheme, tpe) => (tag, KindedAst.Case(enum, tag, tpe, scheme))
          }
      }

      val schemeVal = ascribeScheme(sc0, ascriptions, root)
      val tpeVal = ascribeType(tpeDeprecated, KindMatch.Star, ascriptions, root)
      mapN(casesVal, schemeVal, tpeVal) {
        case (cases, scheme, tpe) =>
          KindedAst.Enum(doc, mod, sym, tparams, cases.toMap, tpe, scheme, loc)
      }
  }


  // MATT docs
  private def visitClass(clazz: ResolvedAst.Class, root: ResolvedAst.Root): Validation[KindedAst.Class, CompilationError] = clazz match {
    case ResolvedAst.Class(doc, mod, sym, tparam0, superClasses0, sigs0, laws0, loc) =>
      val (tparam, ascriptions) = visitTparam(tparam0)
      val superClassesVal = traverse(superClasses0)(ascribeTconstr(_, ascriptions, root))
      val sigsVal = traverse(sigs0) {
        case (sigSym, sig0) => visitSig(sig0, ascriptions, root).map(sig => sigSym -> sig)
      }
      val lawsVal = traverse(laws0)(visitDef(_, ascriptions, root))

      mapN(superClassesVal, sigsVal, lawsVal) {
        case (superClasses, sigs, laws) => KindedAst.Class(doc, mod, sym, tparam, superClasses, sigs.toMap, laws, loc)
      }
  }

  // MATT docs
  private def visitInstance(inst: ResolvedAst.Instance, root: ResolvedAst.Root): Validation[KindedAst.Instance, CompilationError] = inst match {
    case ResolvedAst.Instance(doc, mod, sym, tpe, tconstrs0, defs0, ns, loc) =>
      val clazz = root.classes(sym)
      val kind = getClassKind(clazz)
      val expectedKind = KindMatch.fromKind(kind)

      inferKinds(tpe, expectedKind, root) flatMap {
        case (tpe, ascriptions) =>
          val tconstrsVal = traverse(tconstrs0)(ascribeTconstr(_, ascriptions, root))
          val defsVal = traverse(defs0)(visitDef(_, ascriptions, root))
          mapN(tconstrsVal, defsVal) {
            case (tconstrs, defs) => KindedAst.Instance(doc, mod, sym, tpe, tconstrs, defs, ns, loc)
          }
      }
  }

  // MATT docs
  private def visitTparams(tparams0: ResolvedAst.TypeParams): (List[KindedAst.TypeParam], Map[Int, Kind]) = tparams0 match {
    // Case 1: Kinded tparams: use their kinds
    case ResolvedAst.TypeParams.Kinded(tparams) =>
      val ascriptions = tparams.foldLeft(Map.empty[Int, Kind]) {
        case (acc, ResolvedAst.TypeParam.Kinded(_, tpe, kind, _)) => acc + (tpe.id -> kind)
      }
      val ktparams = tparams.map {
        case ResolvedAst.TypeParam.Kinded(name, tpe, kind, loc) => KindedAst.TypeParam(name, ascribeTvar(tpe, kind), loc)
      }
      (ktparams, ascriptions)
    // Case 2: Unkinded tparams: default to Star kind
    case ResolvedAst.TypeParams.Unkinded(tparams) =>
      val ascriptions = tparams.foldLeft(Map.empty[Int, Kind]) {
        case (acc, tparam) => acc + (tparam.tpe.id -> Kind.Star)
      }
      val ktparams = tparams.map {
        case ResolvedAst.TypeParam.Unkinded(name, tpe, loc) => KindedAst.TypeParam(name, ascribeTvar(tpe, Kind.Star), loc)
      }
      (ktparams, ascriptions)
  }

  // MATT docs
  private def visitDef(defn0: ResolvedAst.Def, ascriptions0: Map[Int, Kind], root: ResolvedAst.Root): Validation[KindedAst.Def, KindError] = defn0 match {
    case ResolvedAst.Def(sym, spec0, exp0) =>
      for {
        res <- visitSpec(spec0, ascriptions0, root)
        (spec, ascriptions) = res
        exp <- visitExp(exp0, ascriptions, root)
      } yield KindedAst.Def(sym, spec, exp)
  }

  // MATT docs
  def getEnumKind(enum: ResolvedAst.Enum): Kind = enum match {
    case ResolvedAst.Enum(_, _, _, tparams, _, _, _, _) =>
      val ascriptions = getAscriptions(tparams)
      tparams.tparams.foldRight(Kind.Star: Kind) { // MATT is foldRight right?
        case (tparam, acc) => ascriptions(tparam.tpe.id) ->: acc
      }
    // MATT use types to enforce explicit/implicit kinding invariant
  }

  // MATT docs
  def getClassKind(clazz: ResolvedAst.Class): Kind = clazz.tparam match {
    case TypeParam.Kinded(_, _, kind, _) => kind
    case _: TypeParam.Unkinded => Kind.Star
  }

  // MATT docs
  def reassembleTypeConstructor(tycon: UnkindedType.Constructor, root: ResolvedAst.Root): TypeConstructor = tycon match {
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

  // MATT docs
  def getAscriptions(tparams0: ResolvedAst.TypeParams): Map[Int, Kind] = tparams0 match {
      // Case 1: Kinded tparams: use their kinds
    case ResolvedAst.TypeParams.Kinded(tparams) => tparams.foldLeft(Map.empty[Int, Kind]) {
      case (acc, ResolvedAst.TypeParam.Kinded(_, tpe, kind, _)) => acc + (tpe.id -> kind)
    }
      // Case 2: Unkinded tparams: default to Star kind
    case ResolvedAst.TypeParams.Unkinded(tparams) =>
      tparams.foldLeft(Map.empty[Int, Kind]) {
        case (acc, tparam) => acc + (tparam.tpe.id -> Kind.Star)
      }
  }

  private def visitSig(sig0: ResolvedAst.Sig, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Sig, KindError] = sig0 match {
    case ResolvedAst.Sig(sym, spec0, expOpt0) =>
      val inference = for {
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
      case ResolvedAst.Expression.Lambda(fparam, exp, tpe, loc) => ??? // MATT have to add fparam to scope ?
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
      case ResolvedAst.Expression.Match(exp, rules, loc) => ???// MATT
      case ResolvedAst.Expression.Choose(star, exps, rules, tpe, loc) => ???// MATT
      case ResolvedAst.Expression.Tag(sym, tag, exp, tpe, loc) => ???// MATT
      case ResolvedAst.Expression.Tuple(elms, loc) => ???// MATT
      case ResolvedAst.Expression.RecordEmpty(tpe, loc) => ???// MATT
      case ResolvedAst.Expression.RecordSelect(exp, field, tpe, loc) => ???// MATT
      case ResolvedAst.Expression.RecordExtend(field, value, rest, tpe, loc) => ???// MATT
      case ResolvedAst.Expression.RecordRestrict(field, rest, tpe, loc) => ???// MATT
      case ResolvedAst.Expression.ArrayLit(elms, tpe, loc) => ???// MATT
      case ResolvedAst.Expression.ArrayNew(elm, len, tpe, loc) => ???// MATT
      case ResolvedAst.Expression.ArrayLoad(base, index, tpe, loc) => ???// MATT
      case ResolvedAst.Expression.ArrayStore(base, index, elm, loc) => ???// MATT
      case ResolvedAst.Expression.ArrayLength(base, loc) => ???// MATT
      case ResolvedAst.Expression.ArraySlice(base, beginIndex, endIndex, loc) => ???// MATT
      case ResolvedAst.Expression.Ref(exp, loc) => ???// MATT
      case ResolvedAst.Expression.Deref(exp, tpe, loc) => ???// MATT
      case ResolvedAst.Expression.Assign(exp1, exp2, loc) => ???// MATT
      case ResolvedAst.Expression.Existential(fparam, exp, loc) => ???// MATT
      case ResolvedAst.Expression.Universal(fparam, exp, loc) => ???// MATT
      case ResolvedAst.Expression.Ascribe(exp, expectedType, expectedEff, tpe, loc) => ???// MATT
      case ResolvedAst.Expression.Cast(exp, declaredType, declaredEff, tpe, loc) => ???// MATT
      case ResolvedAst.Expression.TryCatch(exp, rules, loc) => ???// MATT
      case ResolvedAst.Expression.InvokeConstructor(constructor, args, loc) => ???// MATT
      case ResolvedAst.Expression.InvokeMethod(method, exp, args, loc) => ???// MATT
      case ResolvedAst.Expression.InvokeStaticMethod(method, args, loc) => ???// MATT
      case ResolvedAst.Expression.GetField(field, exp, loc) => ???// MATT
      case ResolvedAst.Expression.PutField(field, exp1, exp2, loc) => ???// MATT
      case ResolvedAst.Expression.GetStaticField(field, loc) => ???// MATT
      case ResolvedAst.Expression.PutStaticField(field, exp, loc) => ???// MATT
      case ResolvedAst.Expression.NewChannel(exp, tpe, loc) => ???// MATT
      case ResolvedAst.Expression.GetChannel(exp, tpe, loc) => ???// MATT
      case ResolvedAst.Expression.PutChannel(exp1, exp2, tpe, loc) => ???// MATT
      case ResolvedAst.Expression.SelectChannel(rules, default, tpe, loc) => ???// MATT
      case ResolvedAst.Expression.Spawn(exp, loc) => ???// MATT
      case ResolvedAst.Expression.Lazy(exp, loc) => ???// MATT
      case ResolvedAst.Expression.Force(exp, tpe, loc) => ???// MATT
      case ResolvedAst.Expression.FixpointConstraintSet(cs, tpe, loc) => ???// MATT
      case ResolvedAst.Expression.FixpointMerge(exp1, exp2, loc) => ???// MATT
      case ResolvedAst.Expression.FixpointSolve(exp, loc) => ???// MATT
      case ResolvedAst.Expression.FixpointFilter(pred, exp, tpe, loc) => ???// MATT
      case ResolvedAst.Expression.FixpointProjectIn(exp, pred, tpe, loc) => ???// MATT
      case ResolvedAst.Expression.FixpointProjectOut(pred, exp1, exp2, tpe, loc) => ???// MATT
    }
  }

  private def getTyconKind(tycon: UnkindedType.Constructor, root: ResolvedAst.Root): Kind = tycon match {
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
    case UnkindedType.Constructor.Lazy => Kind.Star ->: Kind.Star ->: Kind.Star
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

  private def reassembleAnnotation(ann0: ResolvedAst.Annotation, subst: KindSubstitution, root: ResolvedAst.Root): KindedAst.Annotation = ann0 match {
    case ResolvedAst.Annotation(name, exps0, loc) =>
      val exps = exps0.map(reassembleExpression(_, subst, root))
      KindedAst.Annotation(name, exps, loc)
  }

  private def reassembleExpression(exp00: ResolvedAst.Expression, subst: KindSubstitution, root: ResolvedAst.Root): KindedAst.Expression = {
    def visit(exp0: ResolvedAst.Expression): KindedAst.Expression = exp0 match {
      case Expression.Wild(tpe, loc) => Expression.Wild(reassembleType(tpe, subst, root), loc)
      case Expression.Var(sym, tpe, loc) => ???
      case Expression.Def(sym, tpe, loc) => ???
      case Expression.Sig(sym, tpe, loc) => ???
      case Expression.Hole(sym, tpe, eff, loc) => ???
      case Expression.Unit(loc) => ???
      case Expression.Null(loc) => ???
      case Expression.True(loc) => ???
      case Expression.False(loc) => ???
      case Expression.Char(lit, loc) => ???
      case Expression.Float32(lit, loc) => ???
      case Expression.Float64(lit, loc) => ???
      case Expression.Int8(lit, loc) => ???
      case Expression.Int16(lit, loc) => ???
      case Expression.Int32(lit, loc) => ???
      case Expression.Int64(lit, loc) => ???
      case Expression.BigInt(lit, loc) => ???
      case Expression.Str(lit, loc) => ???
      case Expression.Default(tpe, loc) => ???
      case Expression.Apply(exp, exps, tpe, eff, loc) => ???
      case Expression.Lambda(fparam, exp, tpe, loc) => ???
      case Expression.Unary(sop, exp, tpe, loc) => ???
      case Expression.Binary(sop, exp1, exp2, tpe, loc) => ???
      case Expression.IfThenElse(exp1, exp2, exp3, loc) => ???
      case Expression.Stm(exp1, exp2, loc) => ???
      case Expression.Let(sym, exp1, exp2, loc) => ???
      case Expression.Match(exp, rules, loc) => ???
      case Expression.Choose(star, exps, rules, tpe, loc) => ???
      case Expression.Tag(sym, tag, exp, tpe, loc) => ???
      case Expression.Tuple(elms, loc) => ???
      case Expression.RecordEmpty(tpe, loc) => ???
      case Expression.RecordSelect(exp, field, tpe, loc) => ???
      case Expression.RecordExtend(field, value, rest, tpe, loc) => ???
      case Expression.RecordRestrict(field, rest, tpe, loc) => ???
      case Expression.ArrayLit(elms, tpe, loc) => ???
      case Expression.ArrayNew(elm, len, tpe, loc) => ???
      case Expression.ArrayLoad(base, index, tpe, loc) => ???
      case Expression.ArrayStore(base, index, elm, loc) => ???
      case Expression.ArrayLength(base, loc) => ???
      case Expression.ArraySlice(base, beginIndex, endIndex, loc) => ???
      case Expression.Ref(exp, loc) => ???
      case Expression.Deref(exp, tpe, loc) => ???
      case Expression.Assign(exp1, exp2, loc) => ???
      case Expression.Existential(fparam, exp, loc) => ???
      case Expression.Universal(fparam, exp, loc) => ???
      case Expression.Ascribe(exp, expectedType, expectedEff, tpe, loc) => ???
      case Expression.Cast(exp, declaredType, declaredEff, tpe, loc) => ???
      case Expression.TryCatch(exp, rules, loc) => ???
      case Expression.InvokeConstructor(constructor, args, loc) => ???
      case Expression.InvokeMethod(method, exp, args, loc) => ???
      case Expression.InvokeStaticMethod(method, args, loc) => ???
      case Expression.GetField(field, exp, loc) => ???
      case Expression.PutField(field, exp1, exp2, loc) => ???
      case Expression.GetStaticField(field, loc) => ???
      case Expression.PutStaticField(field, exp, loc) => ???
      case Expression.NewChannel(exp, tpe, loc) => ???
      case Expression.GetChannel(exp, tpe, loc) => ???
      case Expression.PutChannel(exp1, exp2, tpe, loc) => ???
      case Expression.SelectChannel(rules, default, tpe, loc) => ???
      case Expression.Spawn(exp, loc) => ???
      case Expression.Lazy(exp, loc) => ???
      case Expression.Force(exp, tpe, loc) => ???
      case Expression.FixpointConstraintSet(cs, tpe, loc) => ???
      case Expression.FixpointMerge(exp1, exp2, loc) => ???
      case Expression.FixpointSolve(exp, loc) => ???
      case Expression.FixpointFilter(pred, exp, tpe, loc) => ???
      case Expression.FixpointProjectIn(exp, pred, tpe, loc) => ???
      case Expression.FixpointProjectOut(pred, exp1, exp2, tpe, loc) => ???
    }
  }

  private def reassembleType(tpe0: UnkindedType, subst: KindSubstitution, root: ResolvedAst.Root): Type = {

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

  private def reassembleTypeVar(tvar0: UnkindedType.Var, subst: KindSubstitution, root: ResolvedAst.Root): Type.Var = tvar match {
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

  private def reassembleFparam(fparam0: ResolvedAst.FormalParam, subst: KindSubstitution, root: ResolvedAst.Root): KindedAst.FormalParam = fparam0 match {
    case ResolvedAst.FormalParam(sym, mod, tpe0, loc) =>
      val tpe = reassembleType(tpe0, subst, root)
      KindedAst.FormalParam(sym, mod, tpe, loc)
  }

  private def reassembleScheme(scheme: ResolvedAst.Scheme, subst: KindSubstitution, root: ResolvedAst.Root): Scheme = {
    case ResolvedAst.Scheme(quantifiers0, constraints0, base0) =>
      val quantifiers = quantifiers0.map(reassembleTypeVar(_, subst, root))
      val constraints = constraints0.map(reassembleTconstr(_, subst, root))
      val base = reassembleType(base0, subst, root)
      Scheme(quantifiers, constraints, base)
  }

  private def reassembleTconstr(tconstr: ResolvedAst.TypeConstraint, subst: KindSubstitution, root: ResolvedAst.Root): Ast.TypeConstraint = tconstr match {
    case ResolvedAst.TypeConstraint(clazz, tpe0, loc) =>
      val tpe = reassembleType(tpe0, subst, root)
      Ast.TypeConstraint(clazz, tpe, loc)
  }

}
