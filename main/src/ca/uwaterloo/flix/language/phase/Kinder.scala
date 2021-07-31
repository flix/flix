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
import ca.uwaterloo.flix.util.Validation.{ToFailure, ToSuccess, flatMapN, mapN, traverse}
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}

/**
  * Attributes kinds to the types in the AST.
  *
  * For enums, classes, instances, and type aliases,
  * kinds are either explicit or assumed to be Star.
  *
  * For defs,
  * kinds are either explicit or are inferred from their
  * use in the formal parameters, return type and effect, and type constraints.
  */
object Kinder extends Phase[ResolvedAst.Root, KindedAst.Root] {

  override def run(root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Root, CompilationError] = flix.phase("Kinder") {
    val enumsVal = Validation.traverse(root.enums) {
      case (sym, enum) => visitEnum(enum, root).map((sym, _))
    }

    val classesVal = Validation.traverse(root.classes) {
      case (sym, clazz) => visitClass(clazz, root).map((sym, _))
    }

    val defsVal = Validation.traverse(root.defs) {
      case (sym, defn) => visitDef(defn, KindEnv.empty, root).map((sym, _))
    }

    val instancesVal = Validation.traverse(root.instances) {
      case (sym, insts0) => traverse(insts0)(visitInstance(_, root)).map((sym, _))
    }

    val typeAliasesVal = Validation.traverseX(root.typealiases) {
      case (_, typeAlias) => visitTypeAlias(typeAlias, root)
    }

    mapN(enumsVal, classesVal, defsVal, instancesVal, typeAliasesVal) {
      case (enums, classes, defs, instances, _) =>
        KindedAst.Root(classes.toMap, instances.toMap, defs.toMap, enums.toMap, root.reachable, root.sources)
    }

  }

  /**
    * Performs kinding on the given enum.
    */
  private def visitEnum(enum: ResolvedAst.Enum, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Enum, CompilationError] = enum match {
    case ResolvedAst.Enum(doc, mod, sym, tparams0, cases0, tpeDeprecated0, sc0, loc) =>
      val kinds = getKindsFromTparamsDefaultStar(tparams0)

      val tparamsVal = Validation.traverse(tparams0.tparams)(ascribeTparam(_, kinds))
      val casesVal = Validation.traverse(cases0) {
        case (tag, case0) => mapN(ascribeCase(case0, kinds, root)) {
          caze => (tag, caze)
        }
      }
      val tpeDeprecatedVal = ascribeType(tpeDeprecated0, KindMatch.subKindOf(Kind.Star), kinds, root)
      val scVal = ascribeScheme(sc0, kinds, root)

      mapN(tparamsVal, casesVal, tpeDeprecatedVal, scVal) {
        case (tparams, cases, tpeDeprecated, sc) => KindedAst.Enum(doc, mod, sym, tparams, cases.toMap, tpeDeprecated, sc, loc)
      }
  }

  /**
    * Performs kinding on the given type alias.
    * Returns Unit since type aliases are not carried through to the next phase.
    */
  private def visitTypeAlias(alias: ResolvedAst.TypeAlias, root: ResolvedAst.Root)(implicit flix: Flix): Validation[Unit, KindError] = alias match {
    case ResolvedAst.TypeAlias(doc, mod, sym, tparams0, tpe0, loc) =>
      val kinds = getKindsFromTparamsDefaultStar(tparams0)

      val tparamsVal = Validation.traverse(tparams0.tparams)(ascribeTparam(_, kinds))
      val tpeVal = ascribeType(tpe0, KindMatch.wild, kinds, root)

      mapN(tparamsVal, tpeVal) {
        case (_, _) => ()
      }
  }

  /**
    * Performs kinding on the given type parameter under the given kind environment.
    */
  private def ascribeTparam(tparam: ResolvedAst.TypeParam, kenv: KindEnv): Validation[KindedAst.TypeParam, KindError] = tparam match {
    // MATT don't really need monad here because this should never fail
    case ResolvedAst.TypeParam.Kinded(name, tpe0, _, loc) =>
      mapN(ascribeTypeVar(tpe0, KindMatch.wild, kenv)) {
        tpe => KindedAst.TypeParam(name, tpe, loc)
      }
    case ResolvedAst.TypeParam.Unkinded(name, tpe0, loc) =>
      mapN(ascribeTypeVar(tpe0, KindMatch.wild, kenv)) {
        tpe => KindedAst.TypeParam(name, tpe, loc)
      }
  }

  /**
    * Performs kinding on the given enum case under the given kind environment.
    */
  private def ascribeCase(caze0: ResolvedAst.Case, kinds: KindEnv, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Case, KindError] = caze0 match {
    case ResolvedAst.Case(enum, tag, tpeDeprecated0, sc0) =>
      for {
        tpeDeprecated <- ascribeType(tpeDeprecated0, KindMatch.subKindOf(Kind.Star), kinds, root)
        sc <- ascribeScheme(sc0, kinds, root)
      } yield KindedAst.Case(enum, tag, tpeDeprecated, sc)
  }

  /**
    * Performs kinding on the given type variable under the given kind environment, with `kindMatch` expected from context.
    */
  private def ascribeTypeVar(tvar: UnkindedType.Var, kindMatch: KindMatch, kenv: KindEnv): Validation[Type.Var, KindError] = tvar match {
    case tvar@UnkindedType.Var(id, text) =>
      kenv.map.get(tvar) match {
        // MATT I don't know if we should be here
        // Case 1: we don't know about this kind, just ascribe it with what the context expects
        case None => tvar.ascribedWith(kindMatch.kind).toSuccess
        // Case 2: we know about this kind, make sure it's behaving as we expect
        case Some(actualKind) =>
          if (kindMatch.matches(actualKind)) {
            Type.Var(id, actualKind, text = text).toSuccess
          } else {
            val expectedKind = kindMatch.kind
            KindError.UnexpectedKind(expectedKind = expectedKind, actualKind = actualKind, SourceLocation.Unknown).toFailure
            // MATT get real source loc
          }
      }

  }

  /**
    * Performs kinding on the given free type variable, with `kindMatch` expected from context.
    */
  private def ascribeFreeTypeVar(tvar: UnkindedType.Var, kindMatch: KindMatch): Type.Var = tvar match {
    case UnkindedType.Var(id, text) =>
      val kind = kindMatch.kind
      Type.Var(id, kind, text = text)
  }

  /**
    * Performs kinding on the given type under the given kind environment, with `kindMatch` expected from context.
    */
  private def ascribeType(tpe0: UnkindedType, expectedKind: KindMatch, kenv: KindEnv, root: ResolvedAst.Root)(implicit flix: Flix): Validation[Type, KindError] = tpe0 match {
    case tvar: UnkindedType.Var => ascribeTypeVar(tvar, expectedKind, kenv)
    case UnkindedType.Cst(cst, loc) =>
      val tycon = ascribeTypeConstructor(cst, root)
      val kind = tycon.kind
      if (expectedKind.matches(kind)) {
        Type.Cst(tycon, loc).toSuccess
      } else {
        KindError.UnexpectedKind(expectedKind = expectedKind.kind, actualKind = kind, loc).toFailure
      }
    case UnkindedType.Apply(t10, t20) =>
      for {
        t2 <- ascribeType(t20, KindMatch.wild, kenv, root)
        k1 = KindMatch.subKindOf(Kind.Arrow(t2.kind, expectedKind.kind))
        t1 <- ascribeType(t10, k1, kenv, root)
      } yield Type.Apply(t1, t2)
    case UnkindedType.Lambda(t10, t20) =>
      expectedKind match {
        case KindMatch(_, Kind.Arrow(expK1, expK2)) =>
          val t1 = ascribeFreeTypeVar(t10, KindMatch.subKindOf(expK1))
          for {
            newKinds <- kenv + (t10 -> t1.kind)
            t2 <- ascribeType(t20, KindMatch.subKindOf(expK2), newKinds, root)
          } yield Type.Lambda(t1, t2)
        case _ => ??? // MATT KindError (maybe we can accept Wild here?)
      }
    case UnkindedType.Ascribe(t, k, loc) =>
      if (expectedKind.matches(k)) {
        ascribeType(t, KindMatch.subKindOf(k), kenv, root)
      } else {
        KindError.UnexpectedKind(expectedKind = expectedKind.kind, actualKind = k, loc).toFailure
      }
  }

  /**
    * Performs kinding on the given type under the given kind environment.
    */
  private def ascribeScheme(sc: ResolvedAst.Scheme, kinds: KindEnv, root: ResolvedAst.Root)(implicit flix: Flix): Validation[Scheme, KindError] = sc match {
    case ResolvedAst.Scheme(quantifiers0, constraints0, base0) =>
      for {
        quantifiers <- Validation.traverse(quantifiers0)(ascribeTypeVar(_, KindMatch.wild, kinds))
        constraints <- Validation.traverse(constraints0)(ascribeTypeConstraint(_, kinds, root))
        base <- ascribeType(base0, KindMatch.subKindOf(Kind.Star), kinds, root)
      } yield Scheme(quantifiers, constraints, base)
  }

  /**
    * Performs kinding on the given type constraint under the given kind environment.
    */
  private def ascribeTypeConstraint(tconstr: ResolvedAst.TypeConstraint, kinds: KindEnv, root: ResolvedAst.Root)(implicit flix: Flix): Validation[Ast.TypeConstraint, KindError] = tconstr match {
    case ResolvedAst.TypeConstraint(clazz, tpe0, loc) =>
      val classKind = getClassKind(root.classes(clazz))
      mapN(ascribeType(tpe0, KindMatch.subKindOf(classKind), kinds, root)) {
        tpe => Ast.TypeConstraint(clazz, tpe, loc)
      }
  }

  /**
    * Performs kinding on the given type class.
    */
  private def visitClass(clazz: ResolvedAst.Class, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Class, KindError] = clazz match {
    case ResolvedAst.Class(doc, mod, sym, tparam0, superClasses0, sigs0, laws0, loc) =>
      val kinds = getKindsFromTparamDefaultStar(tparam0)

      val tparamVal = ascribeTparam(tparam0, kinds)
      val superClassesVal = Validation.traverse(superClasses0)(ascribeTypeConstraint(_, kinds, root))
      val sigsVal = Validation.traverse(sigs0) {
        case (sigSym, sig0) => visitSig(sig0, kinds, root).map(sig => sigSym -> sig)
      }
      val lawsVal = traverse(laws0)(visitDef(_, kinds, root))

      mapN(tparamVal, superClassesVal, sigsVal, lawsVal) {
        case (tparam, superClasses, sigs, laws) => KindedAst.Class(doc, mod, sym, tparam, superClasses, sigs.toMap, laws, loc)
      }
  }

  /**
    * Performs kinding on the given instance.
    */
  private def visitInstance(inst: ResolvedAst.Instance, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Instance, KindError] = inst match {
    case ResolvedAst.Instance(doc, mod, sym, tpe0, tconstrs0, defs0, ns, loc) =>
      val kind = getClassKind(root.classes(sym))
      for {
        kinds <- inferType(tpe0, KindMatch.subKindOf(kind), root)
        tpeVal = ascribeType(tpe0, KindMatch.subKindOf(kind), kinds, root)
        tconstrsVal = Validation.traverse(tconstrs0)(ascribeTypeConstraint(_, kinds, root))
        defsVal = Validation.traverse(defs0)(visitDef(_, kinds, root))
        result <- Validation.sequenceT(tpeVal, tconstrsVal, defsVal)
        (tpe, tconstrs, defs) = result
      } yield KindedAst.Instance(doc, mod, sym, tpe, tconstrs, defs, ns, loc)
  }

  /**
    * Gets the kind of the enum.
    */
  def getEnumKind(enum: ResolvedAst.Enum)(implicit flix: Flix): Kind = enum match {
    case ResolvedAst.Enum(_, _, _, tparams, _, _, _, _) =>
      val kenv = getKindsFromTparamsDefaultStar(tparams)
      tparams.tparams.foldRight(Kind.Star: Kind) {
        case (tparam, acc) => kenv.map(tparam.tpe) ->: acc
      }
  }

  /**
    * Gets the kind of the class.
    */
  def getClassKind(clazz: ResolvedAst.Class): Kind = clazz.tparam match {
    case ResolvedAst.TypeParam.Kinded(_, _, kind, _) => kind
    case _: ResolvedAst.TypeParam.Unkinded => Kind.Star
  }


  /**
    * Performs kinding on the given type constructor under the given kind environment.
    */
  def ascribeTypeConstructor(tycon: UnkindedType.Constructor, root: ResolvedAst.Root)(implicit flix: Flix): TypeConstructor = tycon match {
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
      val kind = getEnumKind(root.enums(sym))
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


  /**
    * Performs kinding on the given def under the given kind environment.
    */
  private def visitDef(def0: ResolvedAst.Def, kinds0: KindEnv, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Def, KindError] = def0 match {
    case ResolvedAst.Def(sym, spec0, exp0) =>
      for {
        specKinds <- getKindsFromSpec(spec0, root)
        kinds <- kinds0 ++ specKinds
        spec <- ascribeSpec(spec0, kinds, root)
        exp <- ascribeExpression(exp0, kinds, root)
      } yield KindedAst.Def(sym, spec, exp)
  }

  /**
    * Performs kinding on the given sig under the given kind environment.
    */
  private def visitSig(sig0: ResolvedAst.Sig, kenv0: KindEnv, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Sig, KindError] = sig0 match {
    case ResolvedAst.Sig(sym, spec0, exp0) =>
      for {
        specKinds <- getKindsFromSpec(spec0, root)
        kinds <- kenv0 ++ specKinds
        spec <- ascribeSpec(spec0, kinds, root)
        exp <- Validation.traverse(exp0)(ascribeExpression(_, kinds, root))
      } yield KindedAst.Sig(sym, spec, exp.headOption)
  }

  /**
    * Performs kinding on the given spec under the given kind environment.
    */
  private def ascribeSpec(spec0: ResolvedAst.Spec, kenv: KindEnv, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Spec, KindError] = spec0 match {
    case ResolvedAst.Spec(doc, ann0, mod, tparams0, fparams0, sc0, tpe0, eff0, loc) =>
      val annVal = Validation.traverse(ann0)(ascribeAnnotation(_, kenv, root))
      val tparamsVal = Validation.traverse(tparams0.tparams)(ascribeTparam(_, kenv))
      val fparamsVal = Validation.traverse(fparams0)(ascribeFormalParam(_, kenv, root))
      val scVal = ascribeScheme(sc0, kenv, root)
      val tpeVal = ascribeType(tpe0, KindMatch.subKindOf(Kind.Star), kenv, root)
      val effVal = ascribeType(eff0, KindMatch.subKindOf(Kind.Bool), kenv, root)
      mapN(annVal, tparamsVal, fparamsVal, scVal, tpeVal, effVal) {
        case (ann, tparams, fparams, sc, tpe, eff) => KindedAst.Spec(doc, ann, mod, tparams, fparams, sc, tpe, eff, loc)
      }
  }

  /**
    * Performs kinding on the given formal param under the given kind environment.
    */
  private def ascribeFormalParam(fparam0: ResolvedAst.FormalParam, kinds: KindEnv, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.FormalParam, KindError] = fparam0 match {
    case ResolvedAst.FormalParam(sym, mod, tpe0, loc) =>
      mapN(ascribeType(tpe0, KindMatch.subKindOf(Kind.Star), kinds, root)) {
        tpe => KindedAst.FormalParam(sym, mod, tpe, loc)
      }
  }

  /**
    * Performs kinding on the given annotation under the given kind environment.
    */
  private def ascribeAnnotation(ann: ResolvedAst.Annotation, kinds: KindEnv, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Annotation, KindError] = ann match {
    case ResolvedAst.Annotation(name, exps0, loc) =>
      mapN(Validation.traverse(exps0)(ascribeExpression(_, kinds, root))) {
        exps => KindedAst.Annotation(name, exps, loc)
      }
  }

  /**
    * Performs kinding on the given expression under the given kind environment.
    */
  // MATT remove the tvars newly introduced in Resolver and introduce them here
  private def ascribeExpression(exp00: ResolvedAst.Expression, kenv: KindEnv, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Expression, KindError] = exp00 match {
    case ResolvedAst.Expression.Wild(tpe, loc) => KindedAst.Expression.Wild(tpe.ascribedWith(Kind.Star), loc).toSuccess
    case ResolvedAst.Expression.Var(sym, tpe0, loc) =>
      mapN(ascribeType(tpe0, KindMatch.subKindOf(Kind.Star), kenv, root)) {
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
        exp <- ascribeExpression(exp0, kenv, root)
        exps <- Validation.traverse(exps0)(ascribeExpression(_, kenv, root))
      } yield KindedAst.Expression.Apply(exp, exps, tpe.ascribedWith(Kind.Star), eff.ascribedWith(Kind.Bool), loc)
    case ResolvedAst.Expression.Lambda(fparam0, exp0, tpe, loc) =>
      for {
        fparam <- ascribeFormalParam(fparam0, kenv, root)
        exp <- ascribeExpression(exp0, kenv, root)
      } yield KindedAst.Expression.Lambda(fparam, exp, tpe.ascribedWith(Kind.Star), loc)
    case ResolvedAst.Expression.Unary(sop, exp0, tpe, loc) =>
      for {
        exp <- ascribeExpression(exp0, kenv, root)
      } yield KindedAst.Expression.Unary(sop, exp, tpe.ascribedWith(Kind.Star), loc)
    case ResolvedAst.Expression.Binary(sop, exp10, exp20, tpe, loc) =>
      for {
        exp1 <- ascribeExpression(exp10, kenv, root)
        exp2 <- ascribeExpression(exp20, kenv, root)
      } yield KindedAst.Expression.Binary(sop, exp1, exp2, tpe.ascribedWith(Kind.Star), loc)
    case ResolvedAst.Expression.IfThenElse(exp10, exp20, exp30, loc) =>
      for {
        exp1 <- ascribeExpression(exp10, kenv, root)
        exp2 <- ascribeExpression(exp20, kenv, root)
        exp3 <- ascribeExpression(exp30, kenv, root)
      } yield KindedAst.Expression.IfThenElse(exp1, exp2, exp3, loc)
    case ResolvedAst.Expression.Stm(exp10, exp20, loc) =>
      for {
        exp1 <- ascribeExpression(exp10, kenv, root)
        exp2 <- ascribeExpression(exp20, kenv, root)
      } yield KindedAst.Expression.Stm(exp1, exp2, loc)
    case ResolvedAst.Expression.Let(sym, mod, exp10, exp20, loc) =>
      for {
        exp1 <- ascribeExpression(exp10, kenv, root)
        exp2 <- ascribeExpression(exp20, kenv, root)
      } yield KindedAst.Expression.Let(sym, mod, exp1, exp2, loc)
    case ResolvedAst.Expression.LetRegion(sym, exp0, evar, loc) =>
      for {
        exp <- ascribeExpression(exp0, kenv, root)
      } yield KindedAst.Expression.LetRegion(sym, exp, evar.ascribedWith(Kind.Bool), loc)
    case ResolvedAst.Expression.Match(exp0, rules0, loc) =>
      for {
        exp <- ascribeExpression(exp0, kenv, root)
        rules <- traverse(rules0)(ascribeMatchRule(_, kenv, root))
      } yield KindedAst.Expression.Match(exp, rules, loc)
    case ResolvedAst.Expression.Choose(star, exps0, rules0, tpe, loc) =>
      for {
        exps <- Validation.traverse(exps0)(ascribeExpression(_, kenv, root))
        rules <- Validation.traverse(rules0)(ascribeChoiceRule(_, kenv, root))
      } yield KindedAst.Expression.Choose(star, exps, rules, tpe.ascribedWith(Kind.Star), loc)
    case ResolvedAst.Expression.Tag(sym, tag, exp0, tpe, loc) =>
      for {
        exp <- ascribeExpression(exp0, kenv, root)
      } yield KindedAst.Expression.Tag(sym, tag, exp, tpe.ascribedWith(Kind.Star), loc)
    case ResolvedAst.Expression.Tuple(elms0, loc) =>
      for {
        elms <- Validation.traverse(elms0)(ascribeExpression(_, kenv, root))
      } yield KindedAst.Expression.Tuple(elms, loc)
    case ResolvedAst.Expression.RecordEmpty(tpe, loc) => KindedAst.Expression.RecordEmpty(tpe.ascribedWith(Kind.Record), loc).toSuccess
    case ResolvedAst.Expression.RecordSelect(exp0, field, tpe, loc) =>
      for {
        exp <- ascribeExpression(exp0, kenv, root)
      } yield KindedAst.Expression.RecordSelect(exp, field, tpe.ascribedWith(Kind.Star), loc)
    case ResolvedAst.Expression.RecordExtend(field, value0, rest0, tpe, loc) =>
      // Ideally, if `rest` is not of record kind, we should throw a kind error.
      // But because we have subkinding, we can't do this in the Kinder.
      // Consider: { +name = 5 | id({})
      // This is OK, but would be seen as a kind error since id is `(a: *) -> (a: *)`, so `id({}) :: *`
      // This KindError will be caught later in the Typer
      for {
        value <- ascribeExpression(value0, kenv, root)
        rest <- ascribeExpression(rest0, kenv, root)
      } yield KindedAst.Expression.RecordExtend(field, value, rest, tpe.ascribedWith(Kind.Record), loc)
    case ResolvedAst.Expression.RecordRestrict(field, rest0, tpe, loc) =>
      for {
        rest <- ascribeExpression(rest0, kenv, root)
      } yield KindedAst.Expression.RecordRestrict(field, rest, tpe.ascribedWith(Kind.Record), loc)
    case ResolvedAst.Expression.ArrayLit(elms0, tpe, loc) =>
      for {
        elms <- Validation.traverse(elms0)(ascribeExpression(_, kenv, root))
      } yield KindedAst.Expression.ArrayLit(elms, tpe.ascribedWith(Kind.Star), loc)
    case ResolvedAst.Expression.ArrayNew(elm0, len0, tpe, loc) =>
      for {
        elm <- ascribeExpression(elm0, kenv, root)
        len <- ascribeExpression(len0, kenv, root)
      } yield KindedAst.Expression.ArrayNew(elm, len, tpe.ascribedWith(Kind.Star), loc)
    case ResolvedAst.Expression.ArrayLoad(base0, index0, tpe, loc) =>
      for {
        base <- ascribeExpression(base0, kenv, root)
        index <- ascribeExpression(index0, kenv, root)
      } yield KindedAst.Expression.ArrayLoad(base, index, tpe.ascribedWith(Kind.Star), loc)
    case ResolvedAst.Expression.ArrayStore(base0, index0, elm0, loc) =>
      for {
        base <- ascribeExpression(base0, kenv, root)
        index <- ascribeExpression(index0, kenv, root)
        elm <- ascribeExpression(elm0, kenv, root)
      } yield KindedAst.Expression.ArrayStore(base, index, elm, loc)
    case ResolvedAst.Expression.ArrayLength(base0, loc) =>
      for {
        base <- ascribeExpression(base0, kenv, root)
      } yield KindedAst.Expression.ArrayLength(base, loc)
    case ResolvedAst.Expression.ArraySlice(base0, beginIndex0, endIndex0, loc) =>
      for {
        base <- ascribeExpression(base0, kenv, root)
        beginIndex <- ascribeExpression(beginIndex0, kenv, root)
        endIndex <- ascribeExpression(endIndex0, kenv, root)
      } yield KindedAst.Expression.ArraySlice(base, beginIndex, endIndex, loc)
    case ResolvedAst.Expression.Ref(exp0, tvar, loc) =>
      for {
        exp <- ascribeExpression(exp0, kenv, root)
      } yield KindedAst.Expression.Ref(exp, tvar.ascribedWith(Kind.Star), loc)
    case ResolvedAst.Expression.RefWithRegion(exp10, exp20, tpe, evar, loc) =>
      for {
        exp1 <- ascribeExpression(exp10, kenv, root)
        exp2 <- ascribeExpression(exp20, kenv, root)
      } yield KindedAst.Expression.RefWithRegion(exp1, exp2, tpe.ascribedWith(Kind.Star), evar.ascribedWith(Kind.Bool), loc)
    case ResolvedAst.Expression.Deref(exp0, tvar, evar, loc) =>
      for {
        exp <- ascribeExpression(exp0, kenv, root)
      } yield KindedAst.Expression.Deref(exp, tvar.ascribedWith(Kind.Star), evar.ascribedWith(Kind.Bool), loc)
    case ResolvedAst.Expression.Assign(exp10, exp20, evar, loc) =>
      for {
        exp1 <- ascribeExpression(exp10, kenv, root)
        exp2 <- ascribeExpression(exp20, kenv, root)
      } yield KindedAst.Expression.Assign(exp1, exp2, evar.ascribedWith(Kind.Bool), loc)
    case ResolvedAst.Expression.Existential(fparam0, exp0, loc) =>
      // add the formal param kinds to the environment
      for {
        fparamKinds <- inferFparam(fparam0, root)
        kenv1 <- kenv ++ fparamKinds
        fparam <- ascribeFormalParam(fparam0, kenv1, root)
        exp <- ascribeExpression(exp0, kenv1, root)
      } yield KindedAst.Expression.Existential(fparam, exp, loc)
    case ResolvedAst.Expression.Universal(fparam0, exp0, loc) =>
      // add the formal param kinds to the environment
      for {
        fparamKinds <- inferFparam(fparam0, root)
        kenv1 <- kenv ++ fparamKinds
        fparam <- ascribeFormalParam(fparam0, kenv1, root)
        exp <- ascribeExpression(exp0, kenv1, root)
      } yield KindedAst.Expression.Universal(fparam, exp, loc)
    case ResolvedAst.Expression.Ascribe(exp0, expectedType0, expectedEff0, tpe, loc) =>
      for {
        exp <- ascribeExpression(exp0, kenv, root)
        expectedType <- Validation.traverse(expectedType0)(ascribeType(_, KindMatch.subKindOf(Kind.Star), kenv, root))
        expectedEff <- Validation.traverse(expectedEff0)(ascribeType(_, KindMatch.subKindOf(Kind.Bool), kenv, root))
      } yield KindedAst.Expression.Ascribe(exp, expectedType.headOption, expectedEff.headOption, tpe.ascribedWith(Kind.Star), loc)
    case ResolvedAst.Expression.Cast(exp0, declaredType0, declaredEff0, tpe, loc) =>
      for {
        exp <- ascribeExpression(exp0, kenv, root)
        declaredType <- Validation.traverse(declaredType0)(ascribeType(_, KindMatch.subKindOf(Kind.Star), kenv, root))
        declaredEff <- Validation.traverse(declaredEff0)(ascribeType(_, KindMatch.subKindOf(Kind.Bool), kenv, root))
      } yield KindedAst.Expression.Cast(exp, declaredType.headOption, declaredEff.headOption, tpe.ascribedWith(Kind.Star), loc)
    case ResolvedAst.Expression.TryCatch(exp0, rules0, loc) =>
      for {
        exp <- ascribeExpression(exp0, kenv, root)
        rules <- Validation.traverse(rules0)(ascribeCatchRule(_, kenv, root))
      } yield KindedAst.Expression.TryCatch(exp, rules, loc)
    case ResolvedAst.Expression.InvokeConstructor(constructor, args0, loc) =>
      for {
        args <- Validation.traverse(args0)(ascribeExpression(_, kenv, root))
      } yield KindedAst.Expression.InvokeConstructor(constructor, args, loc)
    case ResolvedAst.Expression.InvokeMethod(method, exp0, args0, loc) =>
      for {
        exp <- ascribeExpression(exp0, kenv, root)
        args <- Validation.traverse(args0)(ascribeExpression(_, kenv, root))
      } yield KindedAst.Expression.InvokeMethod(method, exp, args, loc)
    case ResolvedAst.Expression.InvokeStaticMethod(method, args0, loc) =>
      for {
        args <- Validation.traverse(args0)(ascribeExpression(_, kenv, root))
      } yield KindedAst.Expression.InvokeStaticMethod(method, args, loc)
    case ResolvedAst.Expression.GetField(field, exp0, loc) =>
      for {
        exp <- ascribeExpression(exp0, kenv, root)
      } yield KindedAst.Expression.GetField(field, exp, loc)
    case ResolvedAst.Expression.PutField(field, exp10, exp20, loc) =>
      for {
        exp1 <- ascribeExpression(exp10, kenv, root)
        exp2 <- ascribeExpression(exp20, kenv, root)
      } yield KindedAst.Expression.PutField(field, exp1, exp2, loc)
    case ResolvedAst.Expression.GetStaticField(field, loc) => KindedAst.Expression.GetStaticField(field, loc).toSuccess
    case ResolvedAst.Expression.PutStaticField(field, exp0, loc) =>
      for {
        exp <- ascribeExpression(exp0, kenv, root)
      } yield KindedAst.Expression.PutStaticField(field, exp, loc)
    case ResolvedAst.Expression.NewChannel(exp0, tpe0, loc) =>
      for {
        exp <- ascribeExpression(exp0, kenv, root)
        tpe <- ascribeType(tpe0, KindMatch.subKindOf(Kind.Star), kenv, root)
      } yield KindedAst.Expression.NewChannel(exp, tpe, loc)
    case ResolvedAst.Expression.GetChannel(exp0, tpe, loc) =>
      for {
        exp <- ascribeExpression(exp0, kenv, root)
      } yield KindedAst.Expression.GetChannel(exp, tpe.ascribedWith(Kind.Star), loc)
    case ResolvedAst.Expression.PutChannel(exp10, exp20, tpe, loc) =>
      for {
        exp1 <- ascribeExpression(exp10, kenv, root)
        exp2 <- ascribeExpression(exp20, kenv, root)
      } yield KindedAst.Expression.PutChannel(exp1, exp2, tpe.ascribedWith(Kind.Star), loc)
    case ResolvedAst.Expression.SelectChannel(rules0, default0, tpe, loc) =>
      for {
        rules <- Validation.traverse(rules0)(ascribeSelectChannelRule(_, kenv, root))
        default <- Validation.traverse(default0)(ascribeExpression(_, kenv, root))
      } yield KindedAst.Expression.SelectChannel(rules, default.headOption, tpe.ascribedWith(Kind.Star), loc)
    case ResolvedAst.Expression.Spawn(exp0, loc) =>
      for {
        exp <- ascribeExpression(exp0, kenv, root)
      } yield KindedAst.Expression.Spawn(exp, loc)
    case ResolvedAst.Expression.Lazy(exp0, loc) =>
      for {
        exp <- ascribeExpression(exp0, kenv, root)
      } yield KindedAst.Expression.Lazy(exp, loc)
    case ResolvedAst.Expression.Force(exp0, tpe, loc) =>
      for {
        exp <- ascribeExpression(exp0, kenv, root)
      } yield KindedAst.Expression.Force(exp, tpe.ascribedWith(Kind.Star), loc)
    case ResolvedAst.Expression.FixpointConstraintSet(cs0, tpe, loc) =>
      for {
        cs <- Validation.traverse(cs0)(ascribeConstraint(_, kenv, root))
      } yield KindedAst.Expression.FixpointConstraintSet(cs, tpe.ascribedWith(Kind.Schema), loc)
    case ResolvedAst.Expression.FixpointMerge(exp10, exp20, loc) =>
      for {
        exp1 <- ascribeExpression(exp10, kenv, root)
        exp2 <- ascribeExpression(exp20, kenv, root)
      } yield KindedAst.Expression.FixpointMerge(exp1, exp2, loc)
    case ResolvedAst.Expression.FixpointSolve(exp0, loc) =>
      for {
        exp <- ascribeExpression(exp0, kenv, root)
      } yield KindedAst.Expression.FixpointSolve(exp, loc)
    case ResolvedAst.Expression.FixpointFilter(pred, exp0, tpe, loc) =>
      for {
        exp <- ascribeExpression(exp0, kenv, root)
      } yield KindedAst.Expression.FixpointFilter(pred, exp, tpe.ascribedWith(Kind.Star), loc) // MATT right?
    case ResolvedAst.Expression.FixpointProjectIn(exp0, pred, tpe, loc) =>
      for {
        exp <- ascribeExpression(exp0, kenv, root)
      } yield KindedAst.Expression.FixpointProjectIn(exp, pred, tpe.ascribedWith(Kind.Star), loc) // MATT right?
    case ResolvedAst.Expression.FixpointProjectOut(pred, exp10, exp20, tpe, loc) =>
      for {
        exp1 <- ascribeExpression(exp10, kenv, root)
        exp2 <- ascribeExpression(exp20, kenv, root)
      } yield KindedAst.Expression.FixpointProjectOut(pred, exp1, exp2, tpe.ascribedWith(Kind.Star), loc) // MATT right?
    case ResolvedAst.Expression.MatchEff(exp10, exp20, exp30, loc) =>
      for {
        exp1 <- ascribeExpression(exp10, kenv, root)
        exp2 <- ascribeExpression(exp20, kenv, root)
        exp3 <- ascribeExpression(exp30, kenv, root)
      } yield KindedAst.Expression.MatchEff(exp1, exp2, exp3, loc)

  }

  /**
    * Performs kinding on the given match rule under the given kind environment.
    */
  private def ascribeMatchRule(rule0: ResolvedAst.MatchRule, kinds: KindEnv, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.MatchRule, KindError] = rule0 match {
    case ResolvedAst.MatchRule(pat0, guard0, exp0) =>
      for {
        pat <- ascribePattern(pat0, kinds, root)
        guard <- ascribeExpression(guard0, kinds, root)
        exp <- ascribeExpression(exp0, kinds, root)
      } yield KindedAst.MatchRule(pat, guard, exp)
  }

  /**
    * Performs kinding on the given choice rule under the given kind environment.
    */
  private def ascribeChoiceRule(rule0: ResolvedAst.ChoiceRule, kinds: KindEnv, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.ChoiceRule, KindError] = rule0 match {
    case ResolvedAst.ChoiceRule(pat0, exp0) =>
      for {
        pat <- Validation.traverse(pat0)(ascribeChoicePattern(_, kinds, root))
        exp <- ascribeExpression(exp0, kinds, root)
      } yield KindedAst.ChoiceRule(pat, exp)
  }

  /**
    * Performs kinding on the given catch rule under the given kind environment.
    */
  private def ascribeCatchRule(rule0: ResolvedAst.CatchRule, kinds: KindEnv, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.CatchRule, KindError] = rule0 match {
    case ResolvedAst.CatchRule(sym, clazz, exp0) =>
      for {
        exp <- ascribeExpression(exp0, kinds, root)
      } yield KindedAst.CatchRule(sym, clazz, exp)
  }

  /**
    * Performs kinding on the given select channel rule under the given kind environment.
    */
  private def ascribeSelectChannelRule(rule0: ResolvedAst.SelectChannelRule, kinds: KindEnv, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.SelectChannelRule, KindError] = rule0 match {
    case ResolvedAst.SelectChannelRule(sym, chan0, exp0) =>
      for {
        chan <- ascribeExpression(chan0, kinds, root)
        exp <- ascribeExpression(exp0, kinds, root)
      } yield KindedAst.SelectChannelRule(sym, chan, exp)
  }

  /**
    * Performs kinding on the given pattern under the given kind environment.
    */
  private def ascribePattern(pat0: ResolvedAst.Pattern, kenv: KindEnv, root: ResolvedAst.Root): Validation[KindedAst.Pattern, KindError] = pat0 match {
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
        pat <- ascribePattern(pat0, kenv, root)
      } yield KindedAst.Pattern.Tag(sym, tag, pat, tvar.ascribedWith(Kind.Star), loc)
    case ResolvedAst.Pattern.Tuple(elms0, loc) =>
      for {
        elms <- Validation.traverse(elms0)(ascribePattern(_, kenv, root))
      } yield KindedAst.Pattern.Tuple(elms, loc)
    case ResolvedAst.Pattern.Array(elms0, tvar, loc) =>
      for {
        elms <- Validation.traverse(elms0)(ascribePattern(_, kenv, root))
      } yield KindedAst.Pattern.Array(elms, tvar.ascribedWith(Kind.Star), loc)
    case ResolvedAst.Pattern.ArrayTailSpread(elms0, sym, tvar, loc) =>
      for {
        elms <- Validation.traverse(elms0)(ascribePattern(_, kenv, root))
      } yield KindedAst.Pattern.ArrayTailSpread(elms, sym, tvar.ascribedWith(Kind.Star), loc)
    case ResolvedAst.Pattern.ArrayHeadSpread(sym, elms0, tvar, loc) =>
      for {
        elms <- Validation.traverse(elms0)(ascribePattern(_, kenv, root))
      } yield KindedAst.Pattern.ArrayHeadSpread(sym, elms, tvar.ascribedWith(Kind.Star), loc)
  }


  // MATT validation unneeded here

  /**
    * Performs kinding on the given choice pattern under the given kind environment.
    */
  private def ascribeChoicePattern(pat0: ResolvedAst.ChoicePattern, kinds: KindEnv, root: ResolvedAst.Root): Validation[KindedAst.ChoicePattern, KindError] = pat0 match {
    case ResolvedAst.ChoicePattern.Wild(loc) => KindedAst.ChoicePattern.Wild(loc).toSuccess
    case ResolvedAst.ChoicePattern.Absent(loc) => KindedAst.ChoicePattern.Absent(loc).toSuccess
    case ResolvedAst.ChoicePattern.Present(sym, tvar, loc) => KindedAst.ChoicePattern.Present(sym, tvar.ascribedWith(Kind.Star), loc).toSuccess
  }

  /**
    * Performs kinding on the given constraint under the given kind environment.
    */
  private def ascribeConstraint(constraint0: ResolvedAst.Constraint, kenv: KindEnv, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Constraint, KindError] = constraint0 match {
    case ResolvedAst.Constraint(cparams0, head0, body0, loc) =>
      for {
        cparams <- Validation.traverse(cparams0)(ascribeCparam(_, kenv, root))
        head <- ascribeHeadPredicate(head0, kenv, root)
        body <- Validation.traverse(body0)(ascribeBodyPredicate(_, kenv, root))
      } yield KindedAst.Constraint(cparams, head, body, loc)
  }

  // MATT validation unneeded here
  // MATT other args unneeded here

  /**
    * Performs kinding on the given constraint param under the given kind environment.
    */
  private def ascribeCparam(cparam0: ResolvedAst.ConstraintParam, kenv: KindEnv, root: ResolvedAst.Root): Validation[KindedAst.ConstraintParam, KindError] = cparam0 match {
    case ResolvedAst.ConstraintParam.HeadParam(sym, tpe, loc) => KindedAst.ConstraintParam.HeadParam(sym, tpe.ascribedWith(Kind.Star), loc).toSuccess
    case ResolvedAst.ConstraintParam.RuleParam(sym, tpe, loc) => KindedAst.ConstraintParam.RuleParam(sym, tpe.ascribedWith(Kind.Star), loc).toSuccess
  }

  /**
    * Performs kinding on the given head predicate under the given kind environment.
    */
  private def ascribeHeadPredicate(pred: ResolvedAst.Predicate.Head, kenv: KindEnv, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Predicate.Head, KindError] = pred match {
    case ResolvedAst.Predicate.Head.Atom(pred, den, terms0, tvar, loc) =>
      for {
        terms <- Validation.traverse(terms0)(ascribeExpression(_, kenv, root))
      } yield KindedAst.Predicate.Head.Atom(pred, den, terms, tvar.ascribedWith(Kind.Star), loc)
  }

  /**
    * Performs kinding on the given body predicate under the given kind environment.
    */
  private def ascribeBodyPredicate(pred: ResolvedAst.Predicate.Body, kenv: KindEnv, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindedAst.Predicate.Body, KindError] = pred match {
    case ResolvedAst.Predicate.Body.Atom(pred, den, polarity, terms0, tvar, loc) =>
      for {
        terms <- Validation.traverse(terms0)(ascribePattern(_, kenv, root))
      } yield KindedAst.Predicate.Body.Atom(pred, den, polarity, terms, tvar.ascribedWith(Kind.Star), loc)
    case ResolvedAst.Predicate.Body.Guard(exp0, loc) =>
      for {
        exp <- ascribeExpression(exp0, kenv, root)
      } yield KindedAst.Predicate.Body.Guard(exp, loc)
  }

  /**
    * Infers a kind environment from the given spec.
    */
  private def inferSpec(spec0: ResolvedAst.Spec, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindEnv, KindError] = spec0 match {
    case ResolvedAst.Spec(_, _, _, _, fparams, sc, _, eff, _) =>
      val fparamKenvVal = Validation.fold(fparams, KindEnv.empty) {
        case (acc, fparam) => flatMapN(inferFparam(fparam, root)) {
          fparamKenv => acc ++ fparamKenv
        }
      }
      val schemeKenvVal = inferScheme(sc, root)
      val effKenvVal = inferType(eff, KindMatch.subKindOf(Kind.Bool), root)

      flatMapN(fparamKenvVal, schemeKenvVal, effKenvVal) {
        case (fparamKenv, schemeKenv, effKenv) => KindEnv.merge(fparamKenv, schemeKenv, effKenv)
      }

  }

  /**
    * Infers a kind environment from the given formal param.
    */
  private def inferFparam(fparam0: ResolvedAst.FormalParam, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindEnv, KindError] = fparam0 match {
    case ResolvedAst.FormalParam(_, _, tpe0, _) => inferType(tpe0, KindMatch.subKindOf(Kind.Star), root)
  }

  /**
    * Infers a kind environment from the given scheme.
    */
  private def inferScheme(sc0: ResolvedAst.Scheme, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindEnv, KindError] = sc0 match {
    case ResolvedAst.Scheme(_, constraints, base) =>
      val baseKenvVal = inferType(base, KindMatch.subKindOf(Kind.Star), root)
      val tconstrsKenvsVal = Validation.traverse(constraints)(inferTconstr(_, root))

      Validation.flatMapN(baseKenvVal, tconstrsKenvsVal) {
        case (baseKenv, tconstrKenvs) => Validation.fold(tconstrKenvs, baseKenv)(_ ++ _)
      }
  }

  /**
    * Infers a kind environment from the given type constraint.
    */
  private def inferTconstr(tconstr: ResolvedAst.TypeConstraint, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindEnv, KindError] = tconstr match {
    case ResolvedAst.TypeConstraint(clazz, tpe, loc) =>
      val kind = getClassKind(root.classes(clazz))
      inferType(tpe, KindMatch.subKindOf(kind), root)
  }

  /**
    * Infers a kind environment from the given type, with an expectation from context.
    */
  private def inferType(tpe: UnkindedType, expectedType: KindMatch, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindEnv, KindError] = tpe.baseType match {
    // Case 1: the type constructor is a variable: all args are * and the constructor is * -> * -> * ... -> expectedType
    case tvar: UnkindedType.Var =>
      val kind = tpe.typeArguments.foldLeft(expectedType.kind) {
        case (acc, _) => Kind.Star ->: acc
      }

      Validation.fold(tpe.typeArguments, KindEnv.singleton(tvar -> kind)) {
        case (acc, targ) => flatMapN(inferType(targ, KindMatch.subKindOf(Kind.Star), root)) {
          kenv => acc ++ kenv
        }
      }
    case UnkindedType.Cst(cst, loc) =>
      val tyconKind = getTyconKind(cst, root)
      val args = Kind.args(tyconKind)

      Validation.fold(tpe.typeArguments.zip(args), KindEnv.empty) {
        case (acc, (targ, kind)) => flatMapN(inferType(targ, KindMatch.subKindOf(kind), root)) {
          kenv => acc ++ kenv
        }
      }
    case UnkindedType.Apply(_, _) => throw InternalCompilerException("Unexpected type application.")
    case UnkindedType.Lambda(t1, t2) => ??? // MATT I'll cross this bridge when i get to it
    case UnkindedType.Ascribe(t, k, _) => inferType(t, KindMatch.subKindOf(k), root)
  }

  /**
    * Gets a kind environment from the type params, defaulting to Star kind if they are unkinded.
    */
  private def getKindsFromTparamsDefaultStar(tparams0: ResolvedAst.TypeParams)(implicit flix: Flix): KindEnv = tparams0 match {
    case tparams: ResolvedAst.TypeParams.Kinded =>
      getKindsFromKindedTparams(tparams)
    case tparams: ResolvedAst.TypeParams.Unkinded =>
      getStarKindsForTparams(tparams)
  }

  /**
    * Gets a kind environment from the type param, defaulting to Star kind if it is unkinded.
    */
  private def getKindsFromTparamDefaultStar(tparam0: ResolvedAst.TypeParam)(implicit flix: Flix): KindEnv = tparam0 match {
    case ResolvedAst.TypeParam.Kinded(_, tvar, kind, _) => KindEnv.singleton(tvar -> kind)
    case ResolvedAst.TypeParam.Unkinded(_, tvar, _) => KindEnv.singleton(tvar -> Kind.Star)
  }

  /**
    * Gets a kind environment from the spec.
    */
  private def getKindsFromSpec(spec0: ResolvedAst.Spec, root: ResolvedAst.Root)(implicit flix: Flix): Validation[KindEnv, KindError] = spec0 match {
    case ResolvedAst.Spec(_, _, _, tparams0, _, _, _, _, _) =>
      tparams0 match {
        case tparams: ResolvedAst.TypeParams.Kinded => getKindsFromKindedTparams(tparams).toSuccess
        case _: ResolvedAst.TypeParams.Unkinded => inferSpec(spec0, root)
      }
  }

  /**
    * Gets a kind environment from the kinded type params.
    */
  private def getKindsFromKindedTparams(tparams0: ResolvedAst.TypeParams.Kinded)(implicit flix: Flix): KindEnv = tparams0 match {
    case ResolvedAst.TypeParams.Kinded(tparams) =>
      // no chance of collision
      val map = tparams.foldLeft(Map.empty[UnkindedType.Var, Kind]) {
        case (acc, ResolvedAst.TypeParam.Kinded(_, tpe, kind, _)) =>
          acc + (tpe -> kind)
      }
      KindEnv(map)
  }

  /**
    * Gets a kind environment from the unkinded type params, defaulting each to Star kind.
    */
  private def getStarKindsForTparams(tparams0: ResolvedAst.TypeParams.Unkinded)(implicit flix: Flix): KindEnv = tparams0 match {
    case ResolvedAst.TypeParams.Unkinded(tparams) =>
      // no chance of collision
      val map = tparams.foldLeft(Map.empty[UnkindedType.Var, Kind]) {
        case (acc, ResolvedAst.TypeParam.Unkinded(_, tpe, _)) =>
          acc + (tpe -> Kind.Star)
      }
      KindEnv(map)
  }

  /**
    * Gets the kind associated with the type constructor.
    */
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

  /**
    * Describes a kind bound, a subkind or superkind of some kind.
    */
  private case class KindMatch(assoc: KindMatch.Association, kind: Kind) {
    /**
      * Returns true if the given kind obeys the kind boudn described by this KindMatch.
      */
    def matches(other: Kind): Boolean = assoc match {
      case KindMatch.Association.SubKind => other <:: kind
      case KindMatch.Association.SuperKind => kind <:: other
    }
  }

  private object KindMatch {


    /**
      * A KindMatch that matches every kind.
      */
    // association doesn't matter
    val wild: KindMatch = KindMatch(Association.SubKind, Kind.Wild)

    def subKindOf(kind: Kind): KindMatch = KindMatch(Association.SubKind, kind)

    def superKindOf(kind: Kind): KindMatch = KindMatch(Association.SuperKind, kind)

    /**
      * Describes whether the KindMatch is a lower or upper bound.
      */
    sealed trait Association

    object Association {
      case object SubKind extends Association

      case object SuperKind extends Association
    }
  }

  /**
    * A mapping from type variables to kinds.
    */
  private object KindEnv {
    /**
      * The empty kind environment.
      */
    val empty: KindEnv = KindEnv(Map.empty)

    /**
      * Returns a kind environment consisting of a single mapping.
      */
    def singleton(pair: (UnkindedType.Var, Kind)): KindEnv = KindEnv(Map(pair))

    /**
      * Merges all the given kind environments.
      */
    def merge(kenvs: KindEnv*): Validation[KindEnv, KindError] = {
      Validation.fold(kenvs, KindEnv.empty) {
        case (acc, kenv) => acc ++ kenv
      }
    }
  }

  private case class KindEnv(map: Map[UnkindedType.Var, Kind]) {
    /**
      * Adds the given mapping to the kind environment.
      */
    def +(pair: (UnkindedType.Var, Kind)): Validation[KindEnv, KindError] = pair match {
      case (tvar, kind) => map.get(tvar) match {
        case Some(kind0) => Kind.min(kind0, kind) match {
          case Some(minKind) => KindEnv(map + (tvar -> minKind)).toSuccess
          case None => KindError.MismatchedKinds(kind0, kind, SourceLocation.Unknown).toFailure // MATT real loc, double error
        }
        case None => KindEnv(map + (tvar -> kind)).toSuccess
      }
    }

    /**
      * Merges the given kind environment into this kind environment.
      */
    def ++(other: KindEnv): Validation[KindEnv, KindError] = {
      Validation.fold(other.map, this) {
        case (acc, pair) => acc + pair
      }
    }

  }
}
