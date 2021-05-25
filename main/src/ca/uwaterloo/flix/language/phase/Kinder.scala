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
import ca.uwaterloo.flix.language.ast.ResolvedAst.{TypeParam, TypeParams}
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.errors.KindError
import ca.uwaterloo.flix.language.phase.unification.KindInferMonad
import ca.uwaterloo.flix.language.phase.unification.KindUnification.unifyKindM
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
  private def visitExp(exp000: ResolvedAst.Expression, ascriptions: Map[Int, Kind], root: ResolvedAst.Root): Validation[KindedAst.Expression, KindError] = {

    def visit(exp00: ResolvedAst.Expression): Validation[KindedAst.Expression, KindError] = exp00 match {
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
        val expVal = visit(exp0)
        val expsVal = traverse(exps0)(visit)
        val tpe = tpe0.ascribedWith(Kind.Star)
        val eff = eff0.ascribedWith(Kind.Bool)
        mapN(expVal, expsVal) {
          case (exp, exps) => KindedAst.Expression.Apply(exp, exps, tpe, eff, loc)
        }
      case ResolvedAst.Expression.Lambda(fparam0, exp0, tpe0, loc) =>
        val fparamVal = ascribeFparam(fparam0, ascriptions, root)
        val expVal = visit(exp0)
        val tpe = tpe0.ascribedWith(Kind.Star)
        mapN(fparamVal, expVal) {
          case (fparam, exp) => KindedAst.Expression.Lambda(fparam, exp, tpe, loc)
        }
      case ResolvedAst.Expression.Unary(sop, exp0, tpe0, loc) =>
        val expVal = visit(exp0)
        val tpe = tpe0.ascribedWith(Kind.Star)
        expVal.map {
          exp => KindedAst.Expression.Unary(sop, exp, tpe, loc)
        }
      case ResolvedAst.Expression.Binary(sop, exp10, exp20, tpe0, loc) =>
        val exp1Val = visit(exp10)
        val exp2Val = visit(exp20)
        val tpe = tpe0.ascribedWith(Kind.Star)
        mapN(exp1Val, exp2Val) {
          case (exp1, exp2) => KindedAst.Expression.Binary(sop, exp1, exp2, tpe, loc)
        }
      case ResolvedAst.Expression.IfThenElse(exp10, exp20, exp30, loc) =>
        val exp1Val = visit(exp10)
        val exp2Val = visit(exp20)
        val exp3Val = visit(exp30)
        mapN(exp1Val, exp2Val, exp3Val) {
          case (exp1, exp2, exp3) => KindedAst.Expression.IfThenElse(exp1, exp2, exp3, loc)
        }
      case ResolvedAst.Expression.Stm(exp10, exp20, loc) =>
        val exp1Val = visit(exp10)
        val exp2Val = visit(exp20)
        mapN(exp1Val, exp2Val) {
          case (exp1, exp2) => KindedAst.Expression.Stm(exp1, exp2, loc)
        }
      case ResolvedAst.Expression.Let(sym, exp10, exp20, loc) =>
        val exp1Val = visit(exp10)
        val exp2Val = visit(exp20)
        mapN(exp1Val, exp2Val) {
          case (exp1, exp2) => KindedAst.Expression.Let(sym, exp1, exp2, loc)
        }
      case ResolvedAst.Expression.Match(exp0, rules, loc) =>
        val expVal = visit(exp0)
        val rulesVal = traverse(rules)(visitMatchRule(_, ascriptions, root))
        mapN(expVal, rulesVal) {
          case (exp, rules) => KindedAst.Expression.Match(exp, rules, loc)
        }
      case ResolvedAst.Expression.Choose(star, exps0, rules0, tpe0, loc) =>
        val expsVal = traverse(exps0)(visit)
        val rulesVal = traverse(rules0)(visitChoiceRule(_, ascriptions, root))
        val tpe = tpe0.ascribedWith(Kind.Star)
        mapN(expsVal, rulesVal) {
          case (exps, rules) => KindedAst.Expression.Choose(star, exps, rules, tpe, loc)
        }
      case ResolvedAst.Expression.Tag(sym, tag, exp0, tpe0, loc) =>
        val expVal = visit(exp0)
        val tpe = tpe0.ascribedWith(Kind.Star)
        mapN(expVal) {
          exp => KindedAst.Expression.Tag(sym, tag, exp, tpe, loc)
        }
      case ResolvedAst.Expression.Tuple(elms0, loc) =>
        val elmsVal = traverse(elms0)(visit)
        mapN(elmsVal) {
          elms => KindedAst.Expression.Tuple(elms, loc)
        }
      case ResolvedAst.Expression.RecordEmpty(tpe, loc) => KindedAst.Expression.RecordEmpty(tpe.ascribedWith(Kind.Record), loc).toSuccess
      case ResolvedAst.Expression.RecordSelect(exp0, field, tpe0, loc) =>
        val expVal = visit(exp0)
        val tpe = tpe0.ascribedWith(Kind.Star)
        mapN(expVal) {
          exp => KindedAst.Expression.RecordSelect(exp, field, tpe, loc)
        }
      case ResolvedAst.Expression.RecordExtend(field, value0, rest0, tpe0, loc) =>
        val valueVal = visit(value0)
        val restVal = visit(rest0)
        val tpe = tpe0.ascribedWith(Kind.Record)
        mapN(valueVal, restVal) {
          case (value, rest) => KindedAst.Expression.RecordExtend(field, value, rest, tpe, loc)
        }
      case ResolvedAst.Expression.RecordRestrict(field, rest0, tpe0, loc) =>
        val restVal = visit(rest0)
        val tpe = tpe0.ascribedWith(Kind.Record)
        mapN(restVal) {
          rest => KindedAst.Expression.RecordRestrict(field, rest, tpe, loc)
        }
      case ResolvedAst.Expression.ArrayLit(elms0, tpe0, loc) =>
        val elmsVal = traverse(elms0)(visit)
        val tpe = tpe0.ascribedWith(Kind.Star)
        mapN(elmsVal) {
          elms => KindedAst.Expression.ArrayLit(elms, tpe, loc)
        }
      case ResolvedAst.Expression.ArrayNew(elm0, len0, tpe0, loc) =>
        val elmVal = visit(elm0)
        val lenVal = visit(len0)
        val tpe = tpe0.ascribedWith(Kind.Star)
        mapN(elmVal, lenVal) {
          case (elms, len) => KindedAst.Expression.ArrayNew(elms, len, tpe, loc)
        }
      case ResolvedAst.Expression.ArrayLoad(base0, index0, tpe0, loc) =>
        val baseVal = visit(base0)
        val indexVal = visit(index0)
        val tpe = tpe0.ascribedWith(Kind.Star)
        mapN(baseVal, indexVal) {
          case (base, index) => KindedAst.Expression.ArrayLoad(base, index, tpe, loc)
        }
      case ResolvedAst.Expression.ArrayStore(base0, index0, elm0, loc) =>
        val baseVal = visit(base0)
        val indexVal = visit(index0)
        val elmVal = visit(elm0)
        mapN(baseVal, indexVal, elmVal) {
          case (base, index, elm) => KindedAst.Expression.ArrayStore(base, index, elm, loc)
        }
      case ResolvedAst.Expression.ArrayLength(base0, loc) =>
        val baseVal = visit(base0)
        mapN(baseVal) {
          base => KindedAst.Expression.ArrayLength(base, loc)
        }
      case ResolvedAst.Expression.ArraySlice(base0, beginIndex0, endIndex0, loc) =>
        val baseVal = visit(base0)
        val beginIndexVal = visit(beginIndex0)
        val endIndexVal = visit(endIndex0)
        mapN(baseVal, beginIndexVal, endIndexVal) {
          case (base, beginIndex, endIndex) => KindedAst.Expression.ArraySlice(base, beginIndex, endIndex, loc)
        }
      case ResolvedAst.Expression.Ref(exp0, loc) =>
        val expVal = visit(exp0)
        mapN(expVal) {
          exp => KindedAst.Expression.Ref(exp, loc)
        }
      case ResolvedAst.Expression.Deref(exp0, tpe0, loc) =>
        val expVal = visit(exp0)
        val tpe = tpe0.ascribedWith(Kind.Star)
        mapN(expVal) {
          exp => KindedAst.Expression.Deref(exp, tpe, loc)
        }
      case ResolvedAst.Expression.Assign(exp10, exp20, loc) =>
        val exp1Val = visit(exp10)
        val exp2Val = visit(exp20)
        mapN(exp1Val, exp2Val) {
          case (exp1, exp2) => KindedAst.Expression.Assign(exp1, exp2, loc)
        }
      case ResolvedAst.Expression.Existential(fparam0, exp0, loc) =>
        // MATT need to add fparam to ascriptions?
        val fParamVal = ascribeFparam(fparam0, ascriptions, root)
        val expVal = visit(exp0)
        mapN(fParamVal, expVal) {
          case (fparam, exp) => KindedAst.Expression.Existential(fparam, exp, loc)
        }
      case ResolvedAst.Expression.Universal(fparam0, exp0, loc) =>
        // MATT need to add fparam to ascriptions?
        val fParamVal = ascribeFparam(fparam0, ascriptions, root)
        val expVal = visit(exp0)
        mapN(fParamVal, expVal) {
          case (fparam, exp) => KindedAst.Expression.Universal(fparam, exp, loc)
        }
      case ResolvedAst.Expression.Ascribe(exp0, expectedType0, expectedEff0, tpe0, loc) =>
        val expVal = visit(exp0)
        val expectedTypeVal = traverse(expectedType0)(ascribeType(_, KindMatch.Star, ascriptions, root))
        val expectedEffVal = traverse(expectedEff0)(ascribeType(_, KindMatch.Bool, ascriptions, root))
        val tpe = tpe0.ascribedWith(Kind.Star)
        mapN(expVal, expectedTypeVal, expectedEffVal) {
          case (exp, expectedType, expectedEff) => KindedAst.Expression.Ascribe(exp, expectedType.headOption, expectedEff.headOption, tpe, loc)
        }
      case ResolvedAst.Expression.Cast(exp0, declaredType0, declaredEff0, tpe0, loc) =>
        val expVal = visit(exp0)
        val declaredTypeVal = traverse(declaredType0)(ascribeType(_, KindMatch.Star, ascriptions, root))
        val declaredEffVal = traverse(declaredEff0)(ascribeType(_, KindMatch.Bool, ascriptions, root))
        val tpe = tpe0.ascribedWith(Kind.Star)
        mapN(expVal, declaredTypeVal, declaredEffVal) {
          case (exp, declaredType, declaredEff) => KindedAst.Expression.Cast(exp, declaredType.headOption, declaredEff.headOption, tpe, loc)
        }
      case ResolvedAst.Expression.TryCatch(exp0, rules0, loc) =>
        val expVal = visit(exp0)
        val rulesVal = traverse(rules0)(visitCatchRule(_, ascriptions, root))
        mapN(expVal, rulesVal) {
          case (exp, rules) => KindedAst.Expression.TryCatch(exp, rules, loc)
        }
      case ResolvedAst.Expression.InvokeConstructor(constructor, args0, loc) =>
        val argsVal = traverse(args0)(visit)
        mapN(argsVal) {
          args => KindedAst.Expression.InvokeConstructor(constructor, args, loc)
        }
      case ResolvedAst.Expression.InvokeMethod(method, exp0, args0, loc) =>
        val expVal = visit(exp0)
        val argsVal = traverse(args0)(visit)
        mapN(expVal, argsVal) {
          case (exp, args) => KindedAst.Expression.InvokeMethod(method, exp, args, loc)
        }
      case ResolvedAst.Expression.InvokeStaticMethod(method, args0, loc) =>
        val argsVal = traverse(args0)(visit)
        mapN(argsVal) {
          args => KindedAst.Expression.InvokeStaticMethod(method, args, loc)
        }
      case ResolvedAst.Expression.GetField(field, exp0, loc) =>
        val expVal = visit(exp0)
        mapN(expVal) {
          exp => KindedAst.Expression.GetField(field, exp, loc)
        }
      case ResolvedAst.Expression.PutField(field, exp10, exp20, loc) =>
        val exp1Val = visit(exp10)
        val exp2Val = visit(exp20)
        mapN(exp1Val, exp2Val) {
          case (exp1, exp2) => KindedAst.Expression.PutField(field, exp1, exp2, loc)
        }
      case ResolvedAst.Expression.GetStaticField(field, loc) => KindedAst.Expression.GetStaticField(field, loc).toSuccess
      case ResolvedAst.Expression.PutStaticField(field, exp0, loc) =>
        val expVal = visit(exp0)
        mapN(expVal) {
          exp => KindedAst.Expression.PutStaticField(field, exp, loc)
        }
      case ResolvedAst.Expression.NewChannel(exp0, tpe0, loc) =>
        val expVal = visit(exp0)
        val tpeVal = ascribeType(tpe0, KindMatch.Star, ascriptions, root) // MATT can fail because type and not tvar
        mapN(expVal, tpeVal) {
          case (exp, tpe) => KindedAst.Expression.NewChannel(exp, tpe, loc)
        }
      case ResolvedAst.Expression.GetChannel(exp0, tpe0, loc) =>
        val expVal = visit(exp0)
        val tpe = tpe0.ascribedWith(Kind.Star)
        mapN(expVal) {
          exp => KindedAst.Expression.GetChannel(exp, tpe, loc)
        }
      case ResolvedAst.Expression.PutChannel(exp10, exp20, tpe0, loc) =>
        val exp1Val = visit(exp10)
        val exp2Val = visit(exp20)
        val tpe = tpe0.ascribedWith(Kind.Star)
        mapN(exp1Val, exp2Val) {
          case (exp1, exp2) => KindedAst.Expression.PutChannel(exp1, exp2, tpe, loc)
        }
      case ResolvedAst.Expression.SelectChannel(rules0, default0, tpe0, loc) =>
        val rulesVal = traverse(rules0)(visitSelectChannelRule(_, ascriptions, root))
        val defaultVal = traverse(default0)(visit)
        val tpe = tpe0.ascribedWith(Kind.Star)
        mapN(rulesVal, defaultVal) {
          case (rules, default) => KindedAst.Expression.SelectChannel(rules, default.headOption, tpe, loc)
        }
      case ResolvedAst.Expression.Spawn(exp0, loc) =>
        val expVal = visit(exp0)
        mapN(expVal) {
          exp => KindedAst.Expression.Spawn(exp, loc)
        }
      case ResolvedAst.Expression.Lazy(exp0, loc) =>
        val expVal = visit(exp0)
        mapN(expVal) {
          exp => KindedAst.Expression.Lazy(exp, loc)
        }
      case ResolvedAst.Expression.Force(exp0, tpe0, loc) =>
        val expVal = visit(exp0)
        val tpe = tpe0.ascribedWith(Kind.Star)
        mapN(expVal) {
          exp => KindedAst.Expression.Force(exp, tpe, loc)
        }
      case ResolvedAst.Expression.FixpointConstraintSet(cs0, tpe0, loc) =>
        val csVal = traverse(cs0)(visitConstraint(_, ascriptions, root))
        val tpe = tpe0.ascribedWith(Kind.Schema)
        mapN(csVal) {
          cs => KindedAst.Expression.FixpointConstraintSet(cs, tpe, loc)
        }
      case ResolvedAst.Expression.FixpointMerge(exp10, exp20, loc) =>
        val exp1Val = visit(exp10)
        val exp2Val = visit(exp20)
        mapN(exp1Val, exp2Val) {
          case (exp1, exp2) => KindedAst.Expression.FixpointMerge(exp1, exp2, loc)
        }
      case ResolvedAst.Expression.FixpointSolve(exp0, loc) =>
        val expVal = visit(exp0)
        mapN(expVal) {
          exp => KindedAst.Expression.FixpointSolve(exp, loc)
        }
      case ResolvedAst.Expression.FixpointFilter(pred, exp0, tpe0, loc) =>
        val expVal = visit(exp0)
        val tpe = tpe0.ascribedWith(Kind.Schema) // MATT right?
        mapN(expVal) {
          exp => KindedAst.Expression.FixpointFilter(pred, exp, tpe, loc)
        }
      case ResolvedAst.Expression.FixpointProjectIn(exp0, pred, tpe0, loc) =>
        val expVal = visit(exp0)
        val tpe = tpe0.ascribedWith(Kind.Schema) // MATT right?
        mapN(expVal) {
          exp => KindedAst.Expression.FixpointProjectIn(exp, pred, tpe, loc)
        }
      case ResolvedAst.Expression.FixpointProjectOut(pred, exp10, exp20, tpe0, loc) =>
        val exp1Val = visit(exp10)
        val exp2Val = visit(exp20)
        val tpe = tpe0.ascribedWith(Kind.Schema) // MATT right?
        mapN(exp1Val, exp2Val) {
          case (exp1, exp2) => KindedAst.Expression.FixpointProjectOut(pred, exp1, exp2, tpe, loc)
        }
    }

    visit(exp000)
  }

  // MATT docs
  private def visitMatchRule(rule: ResolvedAst.MatchRule, ascriptions: Map[Int, Kind], root: ResolvedAst.Root): Validation[KindedAst.MatchRule, KindError] = rule match {
    case ResolvedAst.MatchRule(pat0, guard0, exp0) =>
      val pat = visitPattern(pat0)
      val guardVal = visitExp(guard0, ascriptions, root)
      val expVal = visitExp(exp0, ascriptions, root)
      mapN(guardVal, expVal) {
        case (guard, exp) => KindedAst.MatchRule(pat, guard, exp)
      }
  }

  // MATT docs
  private def visitChoiceRule(rule: ResolvedAst.ChoiceRule, ascriptions: Map[Int, Kind], root: ResolvedAst.Root): Validation[KindedAst.ChoiceRule, KindError] = rule match {
    case ResolvedAst.ChoiceRule(pats0, exp0) =>
      val pats = pats0.map(visitChoicePattern)
      val expVal = visitExp(exp0, ascriptions, root)
      mapN(expVal) {
        exp => KindedAst.ChoiceRule(pats, exp)
      }
  }

  // MATT docs
  private def visitCatchRule(rule: ResolvedAst.CatchRule, ascriptions: Map[Int, Kind], root: ResolvedAst.Root): Validation[KindedAst.CatchRule, KindError] = rule match {
    case ResolvedAst.CatchRule(sym, clazz, exp0) =>
      val expVal = visitExp(exp0, ascriptions, root)
      mapN(expVal) {
        exp => KindedAst.CatchRule(sym, clazz, exp)
      }
  }

  // MATT docs
  private def visitSelectChannelRule(rule: ResolvedAst.SelectChannelRule, ascriptions: Map[Int, Kind], root: ResolvedAst.Root): Validation[KindedAst.SelectChannelRule, KindError] = rule match {
    case ResolvedAst.SelectChannelRule(sym, chan0, exp0) =>
      val chanVal = visitExp(chan0, ascriptions, root)
      val expVal = visitExp(exp0, ascriptions, root)
      mapN(chanVal, expVal) {
        case (chan, exp) => KindedAst.SelectChannelRule(sym, chan, exp)
      }
  }

  private def visitConstraint(constraint: ResolvedAst.Constraint, ascriptions: Map[Int, Kind], root: ResolvedAst.Root): Validation[KindedAst.Constraint, KindError] = constraint match {
    case ResolvedAst.Constraint(cparams0, head0, body0, loc) =>
      // MATT probably need to add cparams to ascriptions
      val cparams = cparams0.map(visitConstraintParam)
      val headVal = visitPredicateHead(head0, ascriptions, root)
      val bodyVal = traverse(body0)(visitPredicateBody(_, ascriptions, root))
      mapN(headVal, bodyVal) {
        case (head, body) => KindedAst.Constraint(cparams, head, body, loc)
      }

  }

  // MATT docs
  private def visitConstraintParam(cparam: ResolvedAst.ConstraintParam): KindedAst.ConstraintParam = cparam match {
    case ResolvedAst.ConstraintParam.HeadParam(sym, tpe, loc) => KindedAst.ConstraintParam.HeadParam(sym, tpe.ascribedWith(Kind.Star), loc)
    case ResolvedAst.ConstraintParam.RuleParam(sym, tpe, loc) => KindedAst.ConstraintParam.RuleParam(sym, tpe.ascribedWith(Kind.Star), loc)
  }

  // MATT docs
  private def visitPredicateHead(head: ResolvedAst.Predicate.Head, ascriptions: Map[Int, Kind], root: ResolvedAst.Root) = head match {
    case ResolvedAst.Predicate.Head.Atom(pred, den, terms0, tvar0, loc) =>
      val termsVal = traverse(terms0)(visitExp(_, ascriptions, root))
      val tvar = tvar0.ascribedWith(Kind.Star) // MATT right?
      mapN(termsVal) {
        terms => KindedAst.Predicate.Head.Atom(pred, den, terms, tvar, loc)
      }
  }

  private def visitPredicateBody(body: ResolvedAst.Predicate.Body, ascriptions: Map[Int, Kind], root: ResolvedAst.Root): Validation[KindedAst.Predicate.Body, KindError] = body match {
    case ResolvedAst.Predicate.Body.Atom(pred, den, polarity, terms0, tvar0, loc) =>
      val terms = terms0.map(visitPattern)
      val tvar = tvar0.ascribedWith(Kind.Star) // MATT right?
      KindedAst.Predicate.Body.Atom(pred, den, polarity, terms, tvar, loc).toSuccess
    case ResolvedAst.Predicate.Body.Guard(exp0, loc) =>
      val expVal = visitExp(exp0, ascriptions, root)
      mapN(expVal) {
        exp => KindedAst.Predicate.Body.Guard(exp, loc)
      }
  }

  // MATT docs
  private def visitPattern(pat: ResolvedAst.Pattern): KindedAst.Pattern = pat match {
    case ResolvedAst.Pattern.Wild(tvar, loc) => KindedAst.Pattern.Wild(tvar.ascribedWith(Kind.Star), loc)
    case ResolvedAst.Pattern.Var(sym, tvar, loc) => KindedAst.Pattern.Var(sym, tvar.ascribedWith(Kind.Star), loc)
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
    case ResolvedAst.Pattern.Tag(sym, tag, pat, tvar, loc) => KindedAst.Pattern.Tag(sym, tag, visitPattern(pat), tvar.ascribedWith(Kind.Star), loc)
    case ResolvedAst.Pattern.Tuple(elms, loc) => KindedAst.Pattern.Tuple(elms.map(visitPattern), loc)
    case ResolvedAst.Pattern.Array(elms, tvar, loc) => KindedAst.Pattern.Array(elms.map(visitPattern), tvar.ascribedWith(Kind.Star), loc)
    case ResolvedAst.Pattern.ArrayTailSpread(elms, sym, tvar, loc) => KindedAst.Pattern.ArrayTailSpread(elms.map(visitPattern), sym, tvar.ascribedWith(Kind.Star), loc)
    case ResolvedAst.Pattern.ArrayHeadSpread(sym, elms, tvar, loc) => KindedAst.Pattern.ArrayHeadSpread(sym, elms.map(visitPattern), tvar.ascribedWith(Kind.Star), loc)
  }

  // MATT docs
  private def visitChoicePattern(pat: ResolvedAst.ChoicePattern): KindedAst.ChoicePattern = pat match {
    case ResolvedAst.ChoicePattern.Wild(loc) => KindedAst.ChoicePattern.Wild(loc)
    case ResolvedAst.ChoicePattern.Absent(loc) => KindedAst.ChoicePattern.Absent(loc)
    case ResolvedAst.ChoicePattern.Present(sym, tvar, loc) => KindedAst.ChoicePattern.Present(sym, tvar.ascribedWith(Kind.Star), loc)
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
        ascriptions.get(id) match {
          case Some(actual) =>
            checkKindsMatch(expected, actual) map {
              _ => Type.Var(id, actual, text = text)
            }
          case None =>
            Type.Var(id, KindMatch.toKind(expected), text = text).toSuccess
        }
      //        val actual = ascriptions(id)
      //        checkKindsMatch(expected, actual) map {
      //          _ => Type.Var(id, actual, text = text)
      //        }
      // MATT is it right to allow for ascription not found?
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

  private def inferType(tpe00: UnkindedType, root: KindedAst.Root)(implicit flix: Flix): KindInferMonad[Kind] = {
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
      case UnkindedType.Var(_, _) => KindInferMonad.point(Kind.freshVar())
    }

    visitType(tpe00)
  }

  private def getTyconKind(tycon: UnkindedType.Constructor, root: KindedAst.Root): Kind = tycon match {
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
          tpe match {
              // MATT case docs
            case UnkindedType.Var(id, _) => mergeAscriptions(acc, Map(id -> kind))
            case _ => acc.toSuccess
          }
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
      case (Arrow(k11, k12),  Kind.Arrow(k21, k22)) => contramatches(k11, k21) && matches(k12, k22)

      case (Star, Kind.Record) => true
      case (Star, Kind.Schema) => true

      case _ => false
    }

    // MATT docs
    // for contravariance
    // k1 >:: k2
    def contramatches(k1: KindMatch, k2: Kind): Boolean = (k1, k2) match {
      case (Wild, _) => true
      case (Star, Kind.Star) => true
      case (Bool, Kind.Bool) => true
      case (Record, Kind.Record) => true
      case (Schema, Kind.Schema) => true
      case (Arrow(k11, k12),  Kind.Arrow(k21, k22)) => matches(k11, k21) && contramatches(k12, k22)

      case (Record, Kind.Star) => true
      case (Schema, Kind.Star) => true

      case _ => false
    }

    def toKind(k: KindMatch): Kind = k match {
      case Wild => Kind.Var(-1) // MATT
      case Star => Kind.Star
      case Bool => Kind.Bool
      case Record => Kind.Record
      case Schema => Kind.Schema
      case Arrow(k1, k2) => toKind(k1) ->: toKind(k2)
    }
  }
}
