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
import ca.uwaterloo.flix.util.Validation.{ToFailure, ToSuccess, flatMapN, fold, mapN, sequenceT, traverse}
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}

import scala.annotation.tailrec

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
  private def ascribeAnnotation(ann: ResolvedAst.Annotation, ascriptions: Map[Int, Kind], root: ResolvedAst.Root): Validation[KindedAst.Annotation, KindError] = ann match {
    case ResolvedAst.Annotation(name, exps, loc) =>
      traverse(exps)(visitExp(_, ascriptions, root)) map {
        KindedAst.Annotation(name, _, loc)
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
  private def ascribeTvar(tvar: UnkindedType.Var, kind: Kind): Type.Var = tvar match {
    case UnkindedType.Var(id, text) => Type.Var(id, kind, text = text)
  }

  // MATT docs
  private def visitTparam(tparam0: ResolvedAst.TypeParam): (KindedAst.TypeParam, Map[Int, Kind]) = tparam0 match {
      // Case 1: explicit kind: use it
    case TypeParam.Kinded(name, tpe, kind, loc) =>
      (KindedAst.TypeParam(name, ascribeTvar(tpe, kind), loc), Map(tpe.id -> kind))
      // Case 2: no explicit kind: assume Star
    case TypeParam.Unkinded(name, tpe, loc) =>
      (KindedAst.TypeParam(name, ascribeTvar(tpe, Kind.Star), loc), Map(tpe.id -> Kind.Star))
  }

  // MATT docs
  private def visitSig(sig0: ResolvedAst.Sig, ascriptions0: Map[Int, Kind], root: ResolvedAst.Root): Validation[KindedAst.Sig, KindError] = sig0 match {
    case ResolvedAst.Sig(sym, spec0, exp0) =>
      for {
        res <- visitSpec(spec0, ascriptions0, root)
        (spec, ascriptions) = res
        exp <- traverse(exp0)(visitExp(_, ascriptions, root))
      } yield KindedAst.Sig(sym, spec, exp.headOption)
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
  // MATT include ascriptions?
  private def visitExp(exp0: ResolvedAst.Expression, ascriptions: Map[Int, Kind], root: ResolvedAst.Root): Validation[KindedAst.Expression, KindError] = exp0 match {
    case ResolvedAst.Expression.Wild(tpe, loc) => KindedAst.Expression.Wild(tpe.ascribedWith(Kind.Star), loc).toSuccess
    case ResolvedAst.Expression.Var(sym, tpe0, loc) => ascribeType(tpe0, KindMatch.Star, ascriptions, root).map {
      tpe => KindedAst.Expression.Var(sym, tpe, loc)
    }
    case ResolvedAst.Expression.Def(sym, tpe, loc) => KindedAst.Expression.Def(sym, tpe.ascribedWith(Kind.Star), loc).toSuccess
    case ResolvedAst.Expression.Sig(sym, tpe, loc) => KindedAst.Expression.Sig(sym, tpe.ascribedWith(Kind.Star), loc).toSuccess
    case ResolvedAst.Expression.Hole(sym, tpe0, eff0, loc) =>
      val tpe = tpe0.ascribedWith(Kind.Star)
      val eff = eff0.ascribedWith(Kind.Bool)
      KindedAst.Expression.Hole(sym, tpe, eff, loc).toSuccess

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
    case ResolvedAst.Expression.Apply(exp0, exps0, tpe0, eff0, loc) =>
      val expVal = visitExp(exp0, ascriptions, root)
      val expsVal = traverse(exps0)(visitExp(_, ascriptions, root))
      val tpe = tpe0.ascribedWith(Kind.Star)
      val eff = eff0.ascribedWith(Kind.Bool)
      mapN(expVal, expsVal) {
        case (exp, exps) => KindedAst.Expression.Apply(exp, exps, tpe, eff, loc)
      }
    case ResolvedAst.Expression.Lambda(fparam, exp, tpe, loc) =>
    case ResolvedAst.Expression.Unary(sop, exp, tpe, loc) =>
    case ResolvedAst.Expression.Binary(sop, exp1, exp2, tpe, loc) =>
    case ResolvedAst.Expression.IfThenElse(exp1, exp2, exp3, loc) =>
    case ResolvedAst.Expression.Stm(exp1, exp2, loc) =>
    case ResolvedAst.Expression.Let(sym, exp1, exp2, loc) =>
    case ResolvedAst.Expression.Match(exp, rules, loc) =>
    case ResolvedAst.Expression.Choose(star, exps, rules, tpe, loc) =>
    case ResolvedAst.Expression.Tag(sym, tag, exp, tpe, loc) =>
    case ResolvedAst.Expression.Tuple(elms, loc) =>
    case ResolvedAst.Expression.RecordEmpty(tpe, loc) =>
    case ResolvedAst.Expression.RecordSelect(exp, field, tpe, loc) =>
    case ResolvedAst.Expression.RecordExtend(field, value, rest, tpe, loc) =>
    case ResolvedAst.Expression.RecordRestrict(field, rest, tpe, loc) =>
    case ResolvedAst.Expression.ArrayLit(elms, tpe, loc) =>
    case ResolvedAst.Expression.ArrayNew(elm, len, tpe, loc) =>
    case ResolvedAst.Expression.ArrayLoad(base, index, tpe, loc) =>
    case ResolvedAst.Expression.ArrayStore(base, index, elm, loc) =>
    case ResolvedAst.Expression.ArrayLength(base, loc) =>
    case ResolvedAst.Expression.ArraySlice(base, beginIndex, endIndex, loc) =>
    case ResolvedAst.Expression.Ref(exp, loc) =>
    case ResolvedAst.Expression.Deref(exp, tpe, loc) =>
    case ResolvedAst.Expression.Assign(exp1, exp2, loc) =>
    case ResolvedAst.Expression.Existential(fparam, exp, loc) =>
    case ResolvedAst.Expression.Universal(fparam, exp, loc) =>
    case ResolvedAst.Expression.Ascribe(exp, expectedUnkindedType, expectedEff, tpe, loc) =>
    case ResolvedAst.Expression.Cast(exp, declaredUnkindedType, declaredEff, tpe, loc) =>
    case ResolvedAst.Expression.TryCatch(exp, rules, loc) =>
    case ResolvedAst.Expression.InvokeConstructor(constructor, args, loc) =>
    case ResolvedAst.Expression.InvokeMethod(method, exp, args, loc) =>
    case ResolvedAst.Expression.InvokeStaticMethod(method, args, loc) =>
    case ResolvedAst.Expression.GetField(field, exp, loc) =>
    case ResolvedAst.Expression.PutField(field, exp1, exp2, loc) =>
    case ResolvedAst.Expression.GetStaticField(field, loc) =>
    case ResolvedAst.Expression.PutStaticField(field, exp, loc) =>
    case ResolvedAst.Expression.NewChannel(exp, tpe, loc) =>
    case ResolvedAst.Expression.GetChannel(exp, tpe, loc) =>
    case ResolvedAst.Expression.PutChannel(exp1, exp2, tpe, loc) =>
    case ResolvedAst.Expression.SelectChannel(rules, default, tpe, loc) =>
    case ResolvedAst.Expression.Spawn(exp, loc) =>
    case ResolvedAst.Expression.Lazy(exp, loc) =>
    case ResolvedAst.Expression.Force(exp, tpe, loc) =>
    case ResolvedAst.Expression.FixpointConstraintSet(cs, tpe, loc) =>
    case ResolvedAst.Expression.FixpointMerge(exp1, exp2, loc) =>
    case ResolvedAst.Expression.FixpointSolve(exp, loc) =>
    case ResolvedAst.Expression.FixpointFilter(pred, exp, tpe, loc) =>
    case ResolvedAst.Expression.FixpointProjectIn(exp, pred, tpe, loc) =>
    case ResolvedAst.Expression.FixpointProjectOut(pred, exp1, exp2, tpe, loc) =>
  }

  // MATT only useful for instances b/c of complexity assumptions
  private def inferKinds(tpe: UnkindedType, expected: KindMatch, root: ResolvedAst.Root): Validation[(Type, Map[Int, Kind]), KindError] = tpe match {
    case tvar@UnkindedType.Var(id, text) =>
      val k = KindMatch.toKind(expected)
      (ascribeTvar(tvar, k), Map(id -> k)).toSuccess

    case UnkindedType.Cst(tc0, loc) =>
      val tc = visitTypeConstructor(tc0, root)
      checkKindsMatch(expected, tc.kind) map {
        _ => (Type.Cst(tc, loc), Map.empty)
      }

    case UnkindedType.Lambda(tvar, tpe) =>
      throw InternalCompilerException("TODO") // MATT can't do without kind vars?

    case _: UnkindedType.Apply =>
      // MATT inline docs
      val (base, args) = baseAndArgs(tpe)
      for {
        res <- inferKinds(base, KindMatch.Wild, root) // wild is ok here because base is surely not a var
        (baseTpe, baseMap) = res
        expectedArgs = argKinds(baseTpe.kind).map(KindMatch.fromKind)
        res <- traverse(args.zip(expectedArgs)) { case (arg, expectedArg) => inferKinds(arg, expectedArg, root) }
        (argTpes, argMaps) = res.unzip
        kind = applyKind(baseTpe.kind, argTpes.map(_.kind))
        _ <- checkKindsMatch(expected, kind)
        kindMap = argMaps.fold(baseMap)(_ ++ _) // MATT need to check for conflicts?
        tpe <- mkApply(baseTpe, argTpes)
      } yield (tpe, kindMap)
  }

  // MATT docs
  def applyKind(base: Kind, args: List[Kind]): Kind = {
    def apply1(base: Kind, arg: Kind): Kind = base match {
      case Kind.Arrow(k1, k2) if arg <:: k1 => k2
      case _ => throw InternalCompilerException("illegal kind application") // MATT actually do monad stuff
    }
    args.foldLeft(base)(apply1)
  }

  // MATT docs
  def mkApply(base: Type, args: List[Type]): Validation[Type, KindError] = {
    Type.mkApply(base, args).toSuccess // MATT actually check kinds
  }

  def baseAndArgs(tpe: UnkindedType): (UnkindedType, List[UnkindedType]) = {
    @tailrec
    def visit(tpe: UnkindedType, args: List[UnkindedType]): (UnkindedType, List[UnkindedType]) = tpe match {
      case UnkindedType.Apply(tpe1, tpe2) => visit(tpe1, tpe2 :: args)
      case _ => (tpe, args)
    }

    visit(tpe, Nil)
  }

  def argKinds(k: Kind): List[Kind] = {
    k match {
      case Kind.Arrow(k1, k2) => k1 :: argKinds(k2)
      case _ => Nil
    }
  }

  // MATT docs
  private def ascribeTconstr(tconstr: ResolvedAst.TypeConstraint, ascriptions: Map[Int, Kind], root: ResolvedAst.Root): Validation[Ast.TypeConstraint, KindError] = tconstr match {
    case ResolvedAst.TypeConstraint(sym, tpe0, loc) =>
      val clazz = root.classes(sym)
      val kind = getClassKind(clazz)
      val expectedKind = KindMatch.fromKind(kind)

      ascribeType(tpe0, expectedKind, ascriptions, root) map {
        tpe => Ast.TypeConstraint(sym, tpe, loc)
      }

  }

  // MATT docs
  private def ascribeType(tpe: UnkindedType, expected: KindMatch, ascriptions: Map[Int, Kind], root: ResolvedAst.Root): Validation[Type, KindError] = {

    def visit(tpe: UnkindedType, expected: KindMatch): Validation[Type, KindError] = tpe match {
      case UnkindedType.Var(id, text) =>
        val actual = ascriptions(id)
        checkKindsMatch(expected, actual) map {
          _ => Type.Var(id, actual, text = text)
        }
      case UnkindedType.Cst(tc0, loc) =>
        val tc = visitTypeConstructor(tc0, root)
        checkKindsMatch(expected, tc.kind) map {
          _ => Type.Cst(tc, loc)
        }
      case UnkindedType.Apply(tpe01, tpe02) =>
        for {
          tpe2 <- visit(tpe02, KindMatch.Wild)
          tpe1 <- visit(tpe01, KindMatch.Arrow(KindMatch.fromKind(tpe2.kind), expected))
          tpe = Type.Apply(tpe1, tpe2)
          _ <- checkKindsMatch(expected, tpe.kind)
        } yield tpe

      case UnkindedType.Lambda(tvar0, body0) =>
        for {
          tvar <- visit(tvar0, KindMatch.Wild)
          body <- visit(body0, KindMatch.Wild)
          tpe = Type.Lambda(tvar.asInstanceOf[Type.Var], body) // MATT avoid cast if possible
          _ <- checkKindsMatch(expected, tpe.kind)
        } yield tpe
    }

    visit(tpe, expected)
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
  def visitTypeConstructor(tycon: UnkindedType.Constructor, root: ResolvedAst.Root): TypeConstructor = tycon match {
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

  // MATT docs
  def getAscription(tparam0: ResolvedAst.TypeParam): (Int, Kind) = tparam0 match {
      // Case 1: the kind is explicit: use it
    case ResolvedAst.TypeParam.Kinded(_, tpe, kind, _) => tpe.id -> kind
      // Case 2: the kind is not explicit: assume Star
    case ResolvedAst.TypeParam.Unkinded(_, tpe, _) => tpe.id -> Kind.Star
  }

  def visitSpec(spec: ResolvedAst.Spec, ascriptions0: Map[Int, Kind], root: ResolvedAst.Root): Validation[(KindedAst.Spec, Map[Int, Kind]), KindError] = spec match {
    case ResolvedAst.Spec(doc, ann0, mod, tparams0, fparams0, sc0, eff0, loc) => tparams0 match {
      // Case 1: explicitly kinded tparams: just use the explicit kinds
      case _: TypeParams.Kinded =>
        val (tparams, tparamAscriptions) = visitTparams(tparams0)

        for {
          ascriptions <- mergeAscriptions(ascriptions0, tparamAscriptions)

          res <- sequenceT(
            ascribeScheme(sc0, ascriptions, root),
            ascribeType(eff0, KindMatch.Bool, ascriptions, root),
            traverse(fparams0)(ascribeFparam(_, ascriptions, root)),
            traverse(ann0)(ascribeAnnotation(_, tparamAscriptions, root))
          )
          (scheme, eff, fparams, ann) = res
        } yield (KindedAst.Spec(doc, ann, mod, tparams, fparams, scheme, eff, loc), tparamAscriptions)

      // Case 2: no kind annotations: need to infer them
      case utparams: TypeParams.Unkinded =>

        // Start by getting all the kind ascriptions possible
        // from fparams, the type scheme, and the effect

        val fparamAscriptionsVal = fold(fparams0, Map.empty[Int, Kind]) {
          case (acc, fparam) => inferKinds(fparam.tpe, KindMatch.Star, root) flatMap {
            case (_tpe, map) => mergeAscriptions(acc, map)
          }
        }

        val schemeAscriptionsVal = getSchemeAscriptions(sc0, root)

        val effAscriptionsVal = inferKinds(eff0, KindMatch.Bool, root) map {
          case (_, ascriptions) => ascriptions
        }

        // MATT more docs everywhere
        for {
          res <- sequenceT(schemeAscriptionsVal, effAscriptionsVal, fparamAscriptionsVal)
          (schemeAscriptions, effAscriptions, fparamAscriptions) = res
          ascriptions <- mergeAscriptions(List(ascriptions0, schemeAscriptions, effAscriptions, fparamAscriptions))

          // MATT looks like the only difference is in getting the ascriptions
          // MATT so we can probably merge these two
          res <- sequenceT(
            ascribeScheme(sc0, ascriptions, root),
            ascribeType(eff0, KindMatch.Bool, ascriptions, root),
            traverse(fparams0)(ascribeFparam(_, ascriptions, root)),
            traverse(ann0)(ascribeAnnotation(_, ascriptions, root))
          )
          (scheme, eff, fparams, ann) = res

          tparams = ascribeTparams(utparams, ascriptions)
        } yield (KindedAst.Spec(doc, ann, mod, tparams, fparams, scheme, eff, loc), ascriptions)
    }
  }

  // MATT docs
  private def ascribeTparams(tparams0: ResolvedAst.TypeParams.Unkinded, ascriptions: Map[Int, Kind]): List[KindedAst.TypeParam] = tparams0 match {
    case ResolvedAst.TypeParams.Unkinded(tparams) => tparams.map {
      case ResolvedAst.TypeParam.Unkinded(name, tpe, loc) => KindedAst.TypeParam(name, ascribeTvar(tpe, ascriptions(tpe.id)), loc)
    }
  }

  // MATT docs
  private def ascribeFparam(fparam: ResolvedAst.FormalParam, ascriptions: Map[Int, Kind], root: ResolvedAst.Root): Validation[KindedAst.FormalParam, KindError] = fparam match {
    case ResolvedAst.FormalParam(sym, mod, tpe0, loc) =>
      ascribeType(tpe0, KindMatch.Star, ascriptions, root) map {
        tpe => KindedAst.FormalParam(sym, mod, tpe, loc)
      }
  }

  // MATT docs
  private def getSchemeAscriptions(sc: ResolvedAst.Scheme, root: ResolvedAst.Root): Validation[Map[Int, Kind], KindError] = sc match {
    case ResolvedAst.Scheme(_, constraints0, base0) =>
      // MATT need to get from quantifiers, but what kinds?
      val tconstrAscriptionsVal = Validation.fold(constraints0, Map.empty[Int, Kind]) {
        case (acc, ResolvedAst.TypeConstraint(classSym, tpe, _)) =>
          val clazz = root.classes(classSym)
          val kind = getClassKind(clazz)
          val id = tpe.asInstanceOf[UnkindedType.Var].id // MATT valid cast?

          mergeAscriptions(acc, Map(id -> kind))
      }


      val baseAscriptionsVal = inferKinds(base0, KindMatch.Star, root)
      // MATT make dedicated getAscriptions instead of reusing inferKinds (?)

      flatMapN(tconstrAscriptionsVal, baseAscriptionsVal) {
        case (tconstrAscriptions, (_inferredBase, baseAscriptions)) =>
          mergeAscriptions(tconstrAscriptions, baseAscriptions)
      }
  }

  // MATT docs
  private def mergeAscriptions(ascriptions1: Map[Int, Kind], ascriptions2: Map[Int, Kind]): Validation[Map[Int, Kind], KindError] = {
    // ascription in one or the other: keep it
    Validation.fold(ascriptions1, ascriptions2) {
      // Case 1: ascription in both: ensure that one is a subkind of the other and use the subkind
      case (acc, (id, kind)) if ascriptions2.contains(id) => mergeKinds(kind, ascriptions2(id)).map(subkind => acc + (id -> subkind))
      // Case 2: ascription just in first, we can safely add it
      case (acc, (id, kind)) => (acc + (id -> kind)).toSuccess
    }
  }

  // MATT docs
  private def mergeAscriptions(ascriptions: List[Map[Int, Kind]]): Validation[Map[Int, Kind], KindError] = {
    Validation.fold(ascriptions, Map.empty[Int, Kind])(mergeAscriptions)
  }

  // MATT docs
  private def mergeKinds(k1: Kind, k2: Kind): Validation[Kind, KindError] = {
    if (k1 <:: k2) {
      k1.toSuccess
    } else if (k2 <:: k2) {
      k2.toSuccess
    } else {
      KindError.MismatchedKinds(k1, k2, SourceLocation.Unknown).toFailure // MATT real location
    }
  }
  // MATT docs
  private def ascribeScheme(scheme: ResolvedAst.Scheme, ascriptions: Map[Int, Kind], root: ResolvedAst.Root): Validation[Scheme, KindError] = scheme match {
    case ResolvedAst.Scheme(quantifiers0, constraints0, base0) =>
      val baseVal = ascribeType(base0, KindMatch.Star, ascriptions, root)
      // MATT use better types to avoid cast
      val quantifiersVal = traverse(quantifiers0)(ascribeType(_, KindMatch.Wild, ascriptions, root).map(_.asInstanceOf[Type.Var])) // MATT avoid cast ?
      val constraintsVal = traverse(constraints0)(ascribeTconstr(_, ascriptions, root))
      mapN(quantifiersVal, constraintsVal, baseVal) {
        case (quantifiers, constraints, base) => Scheme(quantifiers, constraints, base)
      }
  }

  private def checkKindsMatch(k1: KindMatch, k2: Kind): Validation[Unit, KindError] = {
    if (KindMatch.matches(k1, k2)) {
      ().toSuccess
    } else {
      KindError.MismatchedKinds(KindMatch.toKind(k1), k2, SourceLocation.Unknown).toFailure // MATT real location
      // MATT don't do toKind (?)
    }

  }

  private sealed trait KindMatch

  private object KindMatch {
    case object Wild extends KindMatch

    case class Arrow(k1: KindMatch, k2: KindMatch) extends KindMatch

    case object Bool extends KindMatch

    case object Star extends KindMatch

    case object Record extends KindMatch

    case object Schema extends KindMatch

    def fromKind(k: Kind): KindMatch = {
      k match {
        case Kind.Var(id) => ???
        case Kind.Star => Star
        case Kind.Bool => Bool
        case Kind.Record => Record
        case Kind.Schema => Schema
        case Kind.Arrow(k1, k2) => Arrow(fromKind(k1), fromKind(k2))
      }
    }

    def matches(k1: KindMatch, k2: Kind): Boolean = (k1, k2) match {
      case (Wild, _) => true
      case (Star, Kind.Star) => true
      case (Bool, Kind.Bool) => true
      case (Record, Kind.Record) => true
      case (Schema, Kind.Schema) => true
      case (Arrow(k11, k12),  Kind.Arrow(k21, k22)) => matches(k11, k21) && matches(k12, k22)

      case (Star, Kind.Record) => true
      case (Star, Kind.Schema) => true

      case _ => false
    }

    def toKind(k: KindMatch): Kind = k match {
      case Wild => throw InternalCompilerException("impossible maybe?") // MATT
      case Star => Kind.Star
      case Bool => Kind.Bool
      case Record => Kind.Record
      case Schema => Kind.Schema
      case Arrow(k1, k2) => toKind(k1) ->: toKind(k2)
    }
  }
}
