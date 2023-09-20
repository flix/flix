/*
 * Copyright 2023 Matthew Lutze
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
import ca.uwaterloo.flix.language.ast.Ast.{CheckedCastType, Constant, Stratification}
import ca.uwaterloo.flix.language.ast.Type.getFlixType
import ca.uwaterloo.flix.language.ast.{Ast, ChangeSet, KindedAst, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.language.phase.unification.Substitution
import ca.uwaterloo.flix.util.Validation.ToSuccess
import ca.uwaterloo.flix.util.collection.ListMap
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps, Validation}

object TypeReconstruction {

  /**
    * Type checks the given AST root.
    */
  def run(root: KindedAst.Root, defSubsts: Map[Symbol.DefnSym, Substitution], sigSubsts: Map[Symbol.SigSym, Substitution], oldRoot: TypedAst.Root, changeSet: ChangeSet)(implicit flix: Flix): Validation[TypedAst.Root, TypeError] = flix.subphase("TypeReconstruction") {
    val classes = visitClasses(root, defSubsts, sigSubsts, oldRoot, changeSet)
    val instances = visitInstances(root, defSubsts)
    val defs = visitDefs(root, defSubsts, oldRoot, changeSet)
    val enums = visitEnums(root)
    val restrictableEnums = visitRestrictableEnums(root)
    val effs = visitEffs(root)
    val typeAliases = visitTypeAliases(root)
    val sigs = classes.values.flatMap(_.signatures).map(sig => sig.sym -> sig).toMap

    val modules = collectModules(root)

    val classEnv = mkClassEnv(root.classes, root.instances)
    val eqEnv = mkEqualityEnv(root.classes, root.instances)

    TypedAst.Root(modules, classes, instances, sigs, defs, enums, restrictableEnums, effs, typeAliases, root.uses, root.entryPoint, root.sources, classEnv, eqEnv, root.names).toSuccess
  }

  /**
    * Creates a class environment from the classes and instances in the root.
    */
  private def mkClassEnv(classes0: Map[Symbol.ClassSym, KindedAst.Class], instances0: Map[Symbol.ClassSym, List[KindedAst.Instance]])(implicit flix: Flix): Map[Symbol.ClassSym, Ast.ClassContext] =
    flix.subphase("ClassEnv") {
      classes0.map {
        case (classSym, clazz) =>
          val instances = instances0.getOrElse(classSym, Nil)
          val envInsts = instances.map {
            case KindedAst.Instance(_, _, _, _, tpe, tconstrs, _, _, _, _) => Ast.Instance(tpe, tconstrs)
          }
          // ignore the super class parameters since they should all be the same as the class param
          val superClasses = clazz.superClasses.map(_.head.sym)
          (classSym, Ast.ClassContext(superClasses, envInsts))
      }
    }

  /**
    * Creates an equality environment from the classes and instances in the root.
    */
  private def mkEqualityEnv(classes0: Map[Symbol.ClassSym, KindedAst.Class], instances0: Map[Symbol.ClassSym, List[KindedAst.Instance]])(implicit flix: Flix): ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef] =
    flix.subphase("EqualityEnv") {

      val assocs = for {
        (classSym, _) <- classes0.iterator
        inst <- instances0.getOrElse(classSym, Nil)
        assoc <- inst.assocs
      } yield (assoc.sym.sym, Ast.AssocTypeDef(assoc.arg, assoc.tpe))


      assocs.foldLeft(ListMap.empty[Symbol.AssocTypeSym, Ast.AssocTypeDef]) {
        case (acc, (sym, defn)) => acc + (sym -> defn)
      }
    }

  /**
    * Collects the symbols in the given root into a map.
    */
  private def collectModules(root: KindedAst.Root): Map[Symbol.ModuleSym, List[Symbol]] = root match {
    case KindedAst.Root(classes, _, defs, enums, restrictableEnums, effects, typeAliases, _, _, _, loc) =>
      val sigs = classes.values.flatMap { clazz => clazz.sigs.values.map(_.sym) }
      val ops = effects.values.flatMap { eff => eff.ops.map(_.sym) }

      val syms0 = classes.keys ++ defs.keys ++ enums.keys ++ effects.keys ++ typeAliases.keys ++ sigs ++ ops

      // collect namespaces from prefixes of other symbols
      // TODO this should be done in resolver once the duplicate namespace issue is managed
      val namespaces = syms0.collect {
        case sym: Symbol.DefnSym => sym.namespace
        case sym: Symbol.EnumSym => sym.namespace
        case sym: Symbol.RestrictableEnumSym => sym.namespace
        case sym: Symbol.ClassSym => sym.namespace
        case sym: Symbol.TypeAliasSym => sym.namespace
        case sym: Symbol.EffectSym => sym.namespace
      }.flatMap {
        fullNs =>
          fullNs.inits.collect {
            case ns@(_ :: _) => new Symbol.ModuleSym(ns)
          }
      }.toSet
      val syms = syms0 ++ namespaces

      val groups = syms.groupBy {
        case sym: Symbol.DefnSym => new Symbol.ModuleSym(sym.namespace)
        case sym: Symbol.EnumSym => new Symbol.ModuleSym(sym.namespace)
        case sym: Symbol.RestrictableEnumSym => new Symbol.ModuleSym(sym.namespace)
        case sym: Symbol.ClassSym => new Symbol.ModuleSym(sym.namespace)
        case sym: Symbol.TypeAliasSym => new Symbol.ModuleSym(sym.namespace)
        case sym: Symbol.EffectSym => new Symbol.ModuleSym(sym.namespace)

        case sym: Symbol.SigSym => new Symbol.ModuleSym(sym.clazz.namespace :+ sym.clazz.name)
        case sym: Symbol.OpSym => new Symbol.ModuleSym(sym.eff.namespace :+ sym.eff.name)
        case sym: Symbol.AssocTypeSym => new Symbol.ModuleSym(sym.clazz.namespace :+ sym.clazz.name)

        case sym: Symbol.ModuleSym => new Symbol.ModuleSym(sym.ns.init)

        case sym: Symbol.CaseSym => throw InternalCompilerException(s"unexpected symbol: $sym", sym.loc)
        case sym: Symbol.RestrictableCaseSym => throw InternalCompilerException(s"unexpected symbol: $sym", sym.loc)
        case sym: Symbol.VarSym => throw InternalCompilerException(s"unexpected symbol: $sym", sym.loc)
        case sym: Symbol.KindedTypeVarSym => throw InternalCompilerException(s"unexpected symbol: $sym", sym.loc)
        case sym: Symbol.UnkindedTypeVarSym => throw InternalCompilerException(s"unexpected symbol: $sym", sym.loc)
        case sym: Symbol.LabelSym => throw InternalCompilerException(s"unexpected symbol: $sym", SourceLocation.Unknown)
        case sym: Symbol.HoleSym => throw InternalCompilerException(s"unexpected symbol: $sym", sym.loc)
      }

      groups.map {
        case (k, v) => (k, v.toList)
      }

  }

  /**
    * Reconstructs types in the given defs.
    */
  private def visitDefs(root: KindedAst.Root, substs: Map[Symbol.DefnSym, Substitution], oldRoot: TypedAst.Root, changeSet: ChangeSet)(implicit flix: Flix): Map[Symbol.DefnSym, TypedAst.Def] = {
    flix.subphase("Defs") {
      val (staleDefs, freshDefs) = changeSet.partition(root.defs, oldRoot.defs)
      ParOps.mapValues(staleDefs) {
        case defn => visitDef(defn, root, substs(defn.sym))
      } ++ freshDefs
    }
  }

  /**
    * Reconstructs types in the given classes.
    */
  private def visitClasses(root: KindedAst.Root, defSubsts: Map[Symbol.DefnSym, Substitution], sigSubsts: Map[Symbol.SigSym, Substitution], oldRoot: TypedAst.Root, changeSet: ChangeSet)(implicit flix: Flix): Map[Symbol.ClassSym, TypedAst.Class] = {
    flix.subphase("Classes") {
      val (staleClasses, freshClasses) = changeSet.partition(root.classes, oldRoot.classes)
      ParOps.mapValues(staleClasses)(visitClass(_, root, defSubsts, sigSubsts)) ++ freshClasses
    }
  }

  /**
    * Reconstructs types in the given instances.
    */
  private def visitInstances(root: KindedAst.Root, substs: Map[Symbol.DefnSym, Substitution])(implicit flix: Flix): Map[Symbol.ClassSym, List[TypedAst.Instance]] = {
    flix.subphase("Instances") {
      ParOps.mapValues(root.instances) {
        case insts => insts.map(visitInstance(_, root, substs))
      }
    }
  }

  /**
    * Reconstructs types in the given class.
    */
  private def visitClass(clazz: KindedAst.Class, root: KindedAst.Root, defSubsts: Map[Symbol.DefnSym, Substitution], sigSubsts: Map[Symbol.SigSym, Substitution]): TypedAst.Class = clazz match {
    case KindedAst.Class(doc, ann, mod, sym, tparam0, superClasses0, assocs0, sigs0, laws0, loc) =>
      val tparam = visitTypeParam(tparam0, root) // TODO ASSOC-TYPES redundant?
      val superClasses = superClasses0 // no subst to be done
      val assocs = assocs0.map {
        case KindedAst.AssocTypeSig(doc, mod, sym, tp0, kind, loc) =>
          val tp = visitTypeParam(tp0, root) // TODO ASSOC-TYPES redundant?
          TypedAst.AssocTypeSig(doc, mod, sym, tp, kind, loc) // TODO ASSOC-TYPES trivial
      }
      val sigs = sigs0.map {
        case (sigSym, sig) => visitSig(sig, root, sigSubsts(sigSym))
      }.toList
      val laws = laws0.map {
        case law => visitDef(law, root, defSubsts(law.sym))
      }
      TypedAst.Class(doc, ann, mod, sym, tparam, superClasses, assocs, sigs, laws, loc)
  }

  /**
    * Reconstructs types in the given instance.
    */
  private def visitInstance(inst: KindedAst.Instance, root: KindedAst.Root, substs: Map[Symbol.DefnSym, Substitution]): TypedAst.Instance = inst match {
    case KindedAst.Instance(doc, ann, mod, clazz, tpe0, tconstrs0, assocs0, defs0, ns, loc) =>
      val tpe = tpe0 // TODO ASSOC-TYPES redundant?
      val tconstrs = tconstrs0 // no subst to be done
      val assocs = assocs0.map {
        case KindedAst.AssocTypeDef(doc, mod, sym, args, tpe, loc) =>
          TypedAst.AssocTypeDef(doc, mod, sym, args, tpe, loc) // TODO ASSOC-TYPES trivial
      }
      val defs = defs0.map {
        case defn => visitDef(defn, root, substs(defn.sym))
      }
      TypedAst.Instance(doc, ann, mod, clazz, tpe, tconstrs, assocs, defs, ns, loc)
  }

  /**
    * Reconstructs types in the given def.
    */
  private def visitDef(defn: KindedAst.Def, root: KindedAst.Root, subst: Substitution): TypedAst.Def = defn match {
    case KindedAst.Def(sym, spec0, exp0) =>
      val spec = visitSpec(spec0, root, subst)
      val exp = visitExp(exp0)(root, subst)
      TypedAst.Def(sym, spec, exp)
  }

  /**
    * Reconstructs types in the given sig.
    */
  private def visitSig(sig: KindedAst.Sig, root: KindedAst.Root, subst: Substitution): TypedAst.Sig = sig match {
    case KindedAst.Sig(sym, spec0, exp0) =>
      val spec = visitSpec(spec0, root, subst)
      val exp = exp0.map(visitExp(_)(root, subst))
      TypedAst.Sig(sym, spec, exp)
  }

  /**
    * Reconstructs types in the given spec.
    */
  private def visitSpec(spec: KindedAst.Spec, root: KindedAst.Root, subst: Substitution): TypedAst.Spec = spec match {
    case KindedAst.Spec(doc, ann, mod, tparams0, fparams0, sc0, tpe0, eff0, tconstrs0, econstrs0, loc) =>
      val tparams = tparams0.map(visitTypeParam(_, root))
      val fparams = fparams0.map(visitFormalParam(_, root, subst))
      val tpe = subst(tpe0)
      val eff = subst(eff0)
      val tconstrs = tconstrs0.map(subst.apply)
      val econstrs = econstrs0.map(subst.apply)
      val sc = sc0 // TODO ASSOC-TYPES get rid of type visits here and elsewhere that only go over rigid tvars
      TypedAst.Spec(doc, ann, mod, tparams, fparams, sc, tpe, eff, tconstrs, econstrs, loc)
  }

  /**
    * Reconstructs types in the given tparams.
    */
  private def visitTypeParam(tparam: KindedAst.TypeParam, root: KindedAst.Root): TypedAst.TypeParam = tparam match {
    case KindedAst.TypeParam(name, sym, loc) => TypedAst.TypeParam(name, sym, loc)
  }

  /**
    * Reconstructs types in the given fparams.
    */
  private def visitFormalParam(fparam: KindedAst.FormalParam, root: KindedAst.Root, subst: Substitution): TypedAst.FormalParam = fparam match {
    case KindedAst.FormalParam(sym, mod, tpe0, src, loc) =>
      val tpe = subst(tpe0)
      TypedAst.FormalParam(sym, mod, tpe, src, loc)
  }

  /**
    * Reconstructs types in the given enums.
    */
  private def visitEnums(root: KindedAst.Root)(implicit flix: Flix): Map[Symbol.EnumSym, TypedAst.Enum] =
    flix.subphase("Enums") {
      // Visit every enum in the ast.
      val result = root.enums.toList.map {
        case (_, enum) => visitEnum(enum, root)
      }

      // Sequence the results and convert them back to a map.
      result.toMap
    }

  /**
    * Reconstructs types in the given enum.
    */
  private def visitEnum(enum0: KindedAst.Enum, root: KindedAst.Root)(implicit flix: Flix): (Symbol.EnumSym, TypedAst.Enum) = enum0 match {
    case KindedAst.Enum(doc, ann, mod, enumSym, tparams0, derives, cases0, tpe, loc) =>
      val tparams = tparams0.map(visitTypeParam(_, root))
      val cases = cases0 map {
        case (name, KindedAst.Case(caseSym, tagType, sc, caseLoc)) =>
          name -> TypedAst.Case(caseSym, tagType, sc, caseLoc)
      }

      enumSym -> TypedAst.Enum(doc, ann, mod, enumSym, tparams, derives, cases, tpe, loc)
  }


  /**
    * Reconstructs types in the given restrictable enums.
    */
  private def visitRestrictableEnums(root: KindedAst.Root)(implicit flix: Flix): Map[Symbol.RestrictableEnumSym, TypedAst.RestrictableEnum] =
    flix.subphase("RestrictableEnums") {
      // Visit every restrictable enum in the ast.
      val result = root.restrictableEnums.toList.map {
        case (_, re) => visitRestrictableEnum(re, root)
      }

      // Sequence the results and convert them back to a map.
      result.toMap
    }

  /**
    * Reconstructs types in the given restrictable enum.
    */
  private def visitRestrictableEnum(enum0: KindedAst.RestrictableEnum, root: KindedAst.Root)(implicit flix: Flix): (Symbol.RestrictableEnumSym, TypedAst.RestrictableEnum) = enum0 match {
    case KindedAst.RestrictableEnum(doc, ann, mod, enumSym, index0, tparams0, derives, cases0, tpe, loc) =>
      val index = TypedAst.TypeParam(index0.name, index0.sym, index0.loc)
      val tparams = tparams0.map(visitTypeParam(_, root))
      val cases = cases0 map {
        case (name, KindedAst.RestrictableCase(caseSym, tagType, sc, caseLoc)) =>
          name -> TypedAst.RestrictableCase(caseSym, tagType, sc, caseLoc)
      }

      enumSym -> TypedAst.RestrictableEnum(doc, ann, mod, enumSym, index, tparams, derives, cases, tpe, loc)
  }

  /**
    * Reconstructs types in the given type aliases.
    */
  private def visitTypeAliases(root: KindedAst.Root)(implicit flix: Flix): Map[Symbol.TypeAliasSym, TypedAst.TypeAlias] =
    flix.subphase("TypeAliases") {
      def visitTypeAlias(alias: KindedAst.TypeAlias): (Symbol.TypeAliasSym, TypedAst.TypeAlias) = alias match {
        case KindedAst.TypeAlias(doc, mod, sym, tparams0, tpe, loc) =>
          val tparams = tparams0.map(visitTypeParam(_, root))
          sym -> TypedAst.TypeAlias(doc, mod, sym, tparams, tpe, loc)
      }

      root.typeAliases.values.map(visitTypeAlias).toMap
    }

  /**
    * Reconstructs types in the given effects.
    */
  private def visitEffs(root: KindedAst.Root)(implicit flix: Flix): Map[Symbol.EffectSym, TypedAst.Effect] = {
    flix.subphase("Effs") {
      root.effects.map {
        case (sym, eff) => sym -> visitEff(eff, root)
      }
    }
  }

  /**
    * Reconstructs types in the given effect.
    */
  private def visitEff(eff: KindedAst.Effect, root: KindedAst.Root)(implicit flix: Flix): TypedAst.Effect = eff match {
    case KindedAst.Effect(doc, ann, mod, sym, ops0, loc) =>
      val ops = ops0.map(visitOp(_, root))
      TypedAst.Effect(doc, ann, mod, sym, ops, loc)
  }

  /**
    * Reconstructs types in the given operation.
    */
  private def visitOp(op: KindedAst.Op, root: KindedAst.Root)(implicit flix: Flix): TypedAst.Op = op match {
    case KindedAst.Op(sym, spec0) =>
      val spec = visitSpec(spec0, root, Substitution.empty)
      TypedAst.Op(sym, spec)
  }

  /**
    * Reconstructs types in the given expression.
    */
  private def visitExp(exp0: KindedAst.Expr)(implicit root: KindedAst.Root, subst: Substitution): TypedAst.Expr = {
    case KindedAst.Expr.Var(sym, loc) =>
      TypedAst.Expr.Var(sym, subst(sym.tvar), loc)

    case KindedAst.Expr.Def(sym, tvar, loc) =>
      TypedAst.Expr.Def(sym, subst(tvar), loc)

    case KindedAst.Expr.Sig(sym, tvar, loc) =>
      TypedAst.Expr.Sig(sym, subst(tvar), loc)

    case KindedAst.Expr.Hole(sym, tpe, loc) =>
      TypedAst.Expr.Hole(sym, subst(tpe), loc)

    case KindedAst.Expr.HoleWithExp(exp, tvar, pvar, loc) =>
      val e = visitExp(exp)
      TypedAst.Expr.HoleWithExp(e, subst(tvar), subst(pvar), loc)

    case KindedAst.Expr.OpenAs(sym, exp, tvar, loc) =>
      val e = visitExp(exp)
      TypedAst.Expr.OpenAs(sym, e, subst(tvar), loc)

    case KindedAst.Expr.Use(sym, alias, exp, loc) =>
      val e = visitExp(exp)
      TypedAst.Expr.Use(sym, alias, e, loc)

    case KindedAst.Expr.Cst(Ast.Constant.Null, loc) =>
      TypedAst.Expr.Cst(Ast.Constant.Null, Type.Null, loc)

    case KindedAst.Expr.Cst(cst, loc) => TypedAst.Expr.Cst(cst, constantType(cst), loc)

    case KindedAst.Expr.Apply(exp, exps, tvar, pvar, loc) =>
      val e = visitExp(exp)
      val es = exps.map(visitExp(_))
      TypedAst.Expr.Apply(e, es, subst(tvar), subst(pvar), loc)

    case KindedAst.Expr.Lambda(fparam, exp, loc) =>
      val p = visitFormalParam(fparam)
      val e = visitExp(exp)
      val t = Type.mkArrowWithEffect(p.tpe, e.eff, e.tpe, loc)
      TypedAst.Expr.Lambda(p, e, t, loc)

    case KindedAst.Expr.Unary(sop, exp, tvar, loc) =>
      val e = visitExp(exp)
      val eff = e.eff
      TypedAst.Expr.Unary(sop, e, subst(tvar), eff, loc)

    case KindedAst.Expr.Binary(sop, exp1, exp2, tvar, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val eff = Type.mkUnion(e1.eff, e2.eff, loc)
      TypedAst.Expr.Binary(sop, e1, e2, subst(tvar), eff, loc)

    case KindedAst.Expr.IfThenElse(exp1, exp2, exp3, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      val tpe = e2.tpe
      val eff = Type.mkUnion(e1.eff, e2.eff, e3.eff, loc)
      TypedAst.Expr.IfThenElse(e1, e2, e3, tpe, eff, loc)

    case KindedAst.Expr.Stm(exp1, exp2, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val tpe = e2.tpe
      val eff = Type.mkUnion(e1.eff, e2.eff, loc)
      TypedAst.Expr.Stm(e1, e2, tpe, eff, loc)

    case KindedAst.Expr.Discard(exp, loc) =>
      val e = visitExp(exp)
      TypedAst.Expr.Discard(e, e.eff, loc)

    case KindedAst.Expr.Let(sym, mod, exp1, exp2, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val tpe = e2.tpe
      val eff = Type.mkUnion(e1.eff, e2.eff, loc)
      TypedAst.Expr.Let(sym, mod, e1, e2, tpe, eff, loc)

    case KindedAst.Expr.LetRec(sym, mod, exp1, exp2, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val tpe = e2.tpe
      val eff = Type.mkUnion(e1.eff, e2.eff, loc)
      TypedAst.Expr.LetRec(sym, mod, e1, e2, tpe, eff, loc)

    case KindedAst.Expr.Region(tpe, loc) =>
      TypedAst.Expr.Region(tpe, loc)

    case KindedAst.Expr.Scope(sym, regionVar, exp, pvar, loc) =>
      val e = visitExp(exp)
      val tpe = e.tpe
      val eff = subst(pvar)
      TypedAst.Expr.Scope(sym, regionVar, e, tpe, eff, loc)

    case KindedAst.Expr.ScopeExit(exp1, exp2, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val tpe = Type.Unit
      val eff = Type.Impure
      TypedAst.Expr.ScopeExit(e1, e2, tpe, eff, loc)

    case KindedAst.Expr.Match(matchExp, rules, loc) =>
      val e1 = visitExp(matchExp)
      val rs = rules map {
        case KindedAst.MatchRule(pat, guard, exp) =>
          val p = visitPattern(pat)
          val g = guard.map(visitExp(_))
          val b = visitExp(exp)
          TypedAst.MatchRule(p, g, b)
      }
      val tpe = rs.head.exp.tpe
      val eff = rs.foldLeft(e1.eff) {
        case (acc, TypedAst.MatchRule(_, g, b)) => Type.mkUnion(g.map(_.eff).toList ::: List(b.eff, acc), loc)
      }
      TypedAst.Expr.Match(e1, rs, tpe, eff, loc)

    case KindedAst.Expr.TypeMatch(matchExp, rules, loc) =>
      val e1 = visitExp(matchExp)
      val rs = rules map {
        case KindedAst.TypeMatchRule(sym, tpe0, exp) =>
          val t = subst(tpe0)
          val b = visitExp(exp)
          TypedAst.TypeMatchRule(sym, t, b)
      }
      val tpe = rs.head.exp.tpe
      val eff = rs.foldLeft(e1.eff) {
        case (acc, TypedAst.TypeMatchRule(_, _, b)) => Type.mkUnion(b.eff, acc, loc)
      }
      TypedAst.Expr.TypeMatch(e1, rs, tpe, eff, loc)

    case KindedAst.Expr.RelationalChoose(_, exps, rules, tvar, loc) =>
      val es = exps.map(visitExp(_))
      val rs = rules.map {
        case KindedAst.RelationalChooseRule(pat0, exp) =>
          val pat = pat0.map {
            case KindedAst.RelationalChoosePattern.Wild(loc) => TypedAst.RelationalChoosePattern.Wild(loc)
            case KindedAst.RelationalChoosePattern.Absent(loc) => TypedAst.RelationalChoosePattern.Absent(loc)
            case KindedAst.RelationalChoosePattern.Present(sym, tvar, loc) => TypedAst.RelationalChoosePattern.Present(sym, subst(tvar), loc)
          }
          TypedAst.RelationalChooseRule(pat, visitExp(exp))
      }
      val tpe = subst(tvar)
      val eff = Type.mkAnd(rs.map(_.exp.eff), loc)
      TypedAst.Expr.RelationalChoose(es, rs, tpe, eff, loc)

    case KindedAst.Expr.RestrictableChoose(star, exp, rules, tvar, loc) =>
      val e = visitExp(exp)
      val rs = rules.map {
        case KindedAst.RestrictableChooseRule(pat0, body0) =>
          val pat = pat0 match {
            case KindedAst.RestrictableChoosePattern.Tag(sym, pats, tvar, loc) =>
              val ps = pats.map {
                case KindedAst.RestrictableChoosePattern.Wild(tvar, loc) => TypedAst.RestrictableChoosePattern.Wild(subst(tvar), loc)
                case KindedAst.RestrictableChoosePattern.Var(sym, tvar, loc) => TypedAst.RestrictableChoosePattern.Var(sym, subst(tvar), loc)
              }
              TypedAst.RestrictableChoosePattern.Tag(sym, ps, subst(tvar), loc)
          }
          val body = visitExp(body0)
          TypedAst.RestrictableChooseRule(pat, body)
      }
      val eff = Type.mkUnion(rs.map(_.exp.eff), loc)
      TypedAst.Expr.RestrictableChoose(star, e, rs, subst(tvar), eff, loc)

    case KindedAst.Expr.Tag(sym, exp, tvar, loc) =>
      val e = visitExp(exp)
      val eff = e.eff
      TypedAst.Expr.Tag(sym, e, subst(tvar), eff, loc)

    case KindedAst.Expr.RestrictableTag(sym, exp, _, tvar, loc) =>
      val e = visitExp(exp)
      val eff = e.eff
      TypedAst.Expr.RestrictableTag(sym, e, subst(tvar), eff, loc)

    case KindedAst.Expr.Tuple(elms, loc) =>
      val es = elms.map(visitExp(_))
      val tpe = Type.mkTuple(es.map(_.tpe), loc)
      val eff = Type.mkUnion(es.map(_.eff), loc)
      TypedAst.Expr.Tuple(es, tpe, eff, loc)

    case KindedAst.Expr.RecordEmpty(loc) =>
      TypedAst.Expr.RecordEmpty(Type.mkRecord(Type.RecordRowEmpty, loc), loc)

    case KindedAst.Expr.RecordSelect(exp, field, tvar, loc) =>
      val e = visitExp(exp)
      val eff = e.eff
      TypedAst.Expr.RecordSelect(e, field, subst(tvar), eff, loc)

    case KindedAst.Expr.RecordExtend(field, value, rest, tvar, loc) =>
      val v = visitExp(value)
      val r = visitExp(rest)
      val eff = Type.mkUnion(v.eff, r.eff, loc)
      TypedAst.Expr.RecordExtend(field, v, r, subst(tvar), eff, loc)

    case KindedAst.Expr.RecordRestrict(field, rest, tvar, loc) =>
      val r = visitExp(rest)
      val eff = r.eff
      TypedAst.Expr.RecordRestrict(field, r, subst(tvar), eff, loc)

    case KindedAst.Expr.ArrayLit(exps, exp, tvar, pvar, loc) =>
      val es = exps.map(visitExp(_))
      val e = visitExp(exp)
      val tpe = subst(tvar)
      val eff = subst(pvar)
      TypedAst.Expr.ArrayLit(es, e, tpe, eff, loc)

    case KindedAst.Expr.ArrayNew(exp1, exp2, exp3, tvar, pvar, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      val tpe = subst(tvar)
      val eff = subst(pvar)
      TypedAst.Expr.ArrayNew(e1, e2, e3, tpe, eff, loc)

    case KindedAst.Expr.ArrayLoad(exp1, exp2, tvar, pvar, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val tpe = subst(tvar)
      val eff = subst(pvar)
      TypedAst.Expr.ArrayLoad(e1, e2, tpe, eff, loc)

    case KindedAst.Expr.ArrayStore(exp1, exp2, exp3, pvar, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      val eff = subst(pvar)
      TypedAst.Expr.ArrayStore(e1, e2, e3, eff, loc)

    case KindedAst.Expr.ArrayLength(exp, loc) =>
      val e = visitExp(exp)
      val eff = e.eff
      TypedAst.Expr.ArrayLength(e, eff, loc)

    case KindedAst.Expr.VectorLit(exps, tvar, pvar, loc) =>
      val es = exps.map(visitExp(_))
      val tpe = subst(tvar)
      val eff = subst(pvar)
      TypedAst.Expr.VectorLit(es, tpe, eff, loc)

    case KindedAst.Expr.VectorLoad(exp1, exp2, tvar, pvar, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val tpe = subst(tvar)
      val eff = subst(pvar)
      TypedAst.Expr.VectorLoad(e1, e2, tpe, eff, loc)

    case KindedAst.Expr.VectorLength(exp, loc) =>
      val e = visitExp(exp)
      val eff = e.eff
      TypedAst.Expr.VectorLength(e, loc)

    case KindedAst.Expr.Ref(exp1, exp2, tvar, pvar, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val tpe = subst(tvar)
      val eff = subst(pvar)
      TypedAst.Expr.Ref(e1, e2, tpe, eff, loc)

    case KindedAst.Expr.Deref(exp, tvar, pvar, loc) =>
      val e = visitExp(exp)
      val tpe = subst(tvar)
      val eff = subst(pvar)
      TypedAst.Expr.Deref(e, tpe, eff, loc)

    case KindedAst.Expr.Assign(exp1, exp2, pvar, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val tpe = Type.Unit
      val eff = subst(pvar)
      TypedAst.Expr.Assign(e1, e2, tpe, eff, loc)

    case KindedAst.Expr.Ascribe(exp, _, _, tvar, loc) =>
      val e = visitExp(exp)
      val eff = e.eff
      TypedAst.Expr.Ascribe(e, subst(tvar), eff, loc)

    case KindedAst.Expr.InstanceOf(exp, clazz, loc) =>
      val e1 = visitExp(exp)
      TypedAst.Expr.InstanceOf(e1, clazz, loc)

    case KindedAst.Expr.CheckedCast(cast, exp, tvar, pvar, loc) =>
      cast match {
        case CheckedCastType.TypeCast =>
          val e = visitExp(exp)
          val tpe = subst(tvar)
          TypedAst.Expr.CheckedCast(cast, e, tpe, e.eff, loc)
        case CheckedCastType.EffectCast =>
          val e = visitExp(exp)
          val eff = Type.mkUnion(e.eff, subst(pvar), loc)
          TypedAst.Expr.CheckedCast(cast, e, e.tpe, eff, loc)
      }

    case KindedAst.Expr.UncheckedCast(KindedAst.Expr.Cst(Ast.Constant.Null, _), _, _, tvar, loc) =>
      val t = subst(tvar)
      TypedAst.Expr.Cst(Ast.Constant.Null, t, loc)

    case KindedAst.Expr.UncheckedCast(exp, declaredType, declaredEff, tvar, loc) =>
      val e = visitExp(exp)
      val dt = declaredType.map(tpe => subst(tpe))
      val dp = declaredEff.map(eff => subst(eff))
      val tpe = subst(tvar)
      val eff = declaredEff.getOrElse(e.eff)
      TypedAst.Expr.UncheckedCast(e, dt, dp, tpe, eff, loc)

    case KindedAst.Expr.UncheckedMaskingCast(exp, loc) =>
      // We explicitly mark a `Mask` expression as Impure.
      val e = visitExp(exp)
      val tpe = e.tpe
      val eff = Type.Impure
      TypedAst.Expr.UncheckedMaskingCast(e, tpe, eff, loc)

    case KindedAst.Expr.Without(exp, effUse, loc) =>
      val e = visitExp(exp)
      val tpe = e.tpe
      val eff = e.eff
      TypedAst.Expr.Without(e, effUse, tpe, eff, loc)

    case KindedAst.Expr.TryCatch(exp, rules, loc) =>
      val e = visitExp(exp)
      val rs = rules map {
        case KindedAst.CatchRule(sym, clazz, body) =>
          val b = visitExp(body)
          TypedAst.CatchRule(sym, clazz, b)
      }
      val tpe = rs.head.exp.tpe
      val eff = Type.mkUnion(e.eff :: rs.map(_.exp.eff), loc)
      TypedAst.Expr.TryCatch(e, rs, tpe, eff, loc)

    case KindedAst.Expr.TryWith(exp, effUse, rules, tvar, loc) =>
      val e = visitExp(exp)
      val rs = rules map {
        case KindedAst.HandlerRule(op, fparams, hexp, htvar) =>
          val fps = fparams.map(visitFormalParam)
          val he = visitExp(hexp)
          TypedAst.HandlerRule(op, fps, he)
      }
      val tpe = subst(tvar)
      val eff = Type.mkUnion(e.eff :: rs.map(_.exp.eff), loc)
      TypedAst.Expr.TryWith(e, effUse, rs, tpe, eff, loc)

    case KindedAst.Expr.Do(op, exps, tvar, loc) =>
      val es = exps.map(visitExp(_))
      val tpe = subst(tvar)
      val eff1 = Type.Cst(TypeConstructor.Effect(op.sym.eff), op.loc.asSynthetic)
      val eff = Type.mkUnion(eff1 :: es.map(_.eff), loc)
      TypedAst.Expr.Do(op, es, tpe, eff, loc)

    case KindedAst.Expr.Resume(exp, _, retTvar, loc) =>
      val e = visitExp(exp)
      val tpe = subst(retTvar)
      TypedAst.Expr.Resume(e, tpe, loc)

    case KindedAst.Expr.InvokeConstructor(constructor, args, loc) =>
      val as = args.map(visitExp(_))
      val tpe = getFlixType(constructor.getDeclaringClass)
      val eff = Type.Impure
      TypedAst.Expr.InvokeConstructor(constructor, as, tpe, eff, loc)

    case KindedAst.Expr.InvokeMethod(method, _, exp, args, loc) =>
      val e = visitExp(exp)
      val as = args.map(visitExp(_))
      val tpe = getFlixType(method.getReturnType)
      val eff = Type.Impure
      TypedAst.Expr.InvokeMethod(method, e, as, tpe, eff, loc)

    case KindedAst.Expr.InvokeStaticMethod(method, args, loc) =>
      val as = args.map(visitExp(_))
      val tpe = getFlixType(method.getReturnType)
      val eff = Type.Impure
      TypedAst.Expr.InvokeStaticMethod(method, as, tpe, eff, loc)

    case KindedAst.Expr.GetField(field, _, exp, loc) =>
      val e = visitExp(exp)
      val tpe = getFlixType(field.getType)
      val eff = Type.Impure
      TypedAst.Expr.GetField(field, e, tpe, eff, loc)

    case KindedAst.Expr.PutField(field, _, exp1, exp2, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val tpe = Type.Unit
      val eff = Type.Impure
      TypedAst.Expr.PutField(field, e1, e2, tpe, eff, loc)

    case KindedAst.Expr.GetStaticField(field, loc) =>
      val tpe = getFlixType(field.getType)
      val eff = Type.Impure
      TypedAst.Expr.GetStaticField(field, tpe, eff, loc)

    case KindedAst.Expr.PutStaticField(field, exp, loc) =>
      val e = visitExp(exp)
      val tpe = Type.Unit
      val eff = Type.Impure
      TypedAst.Expr.PutStaticField(field, e, tpe, eff, loc)

    case KindedAst.Expr.NewObject(name, clazz, methods, loc) =>
      val tpe = getFlixType(clazz)
      val eff = Type.Impure
      val ms = methods map visitJvmMethod
      TypedAst.Expr.NewObject(name, clazz, tpe, eff, ms, loc)

    case KindedAst.Expr.NewChannel(exp1, exp2, tvar, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val eff = Type.Impure
      TypedAst.Expr.NewChannel(e1, e2, subst(tvar), eff, loc)

    case KindedAst.Expr.GetChannel(exp, tvar, loc) =>
      val e = visitExp(exp)
      val eff = Type.Impure
      TypedAst.Expr.GetChannel(e, subst(tvar), eff, loc)

    case KindedAst.Expr.PutChannel(exp1, exp2, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val tpe = Type.mkUnit(loc)
      val eff = Type.Impure
      TypedAst.Expr.PutChannel(e1, e2, tpe, eff, loc)

    case KindedAst.Expr.SelectChannel(rules, default, tvar, loc) =>
      val rs = rules map {
        case KindedAst.SelectChannelRule(sym, chan, exp) =>
          val c = visitExp(chan)
          val b = visitExp(exp)
          TypedAst.SelectChannelRule(sym, c, b)
      }
      val d = default.map(visitExp(_))
      val eff = Type.Impure
      TypedAst.Expr.SelectChannel(rs, d, subst(tvar), eff, loc)

    case KindedAst.Expr.Spawn(exp1, exp2, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val tpe = Type.Unit
      val eff = Type.Impure
      TypedAst.Expr.Spawn(e1, e2, tpe, eff, loc)

    case KindedAst.Expr.ParYield(frags, exp, loc) =>
      val e = visitExp(exp)
      val fs = frags map {
        case KindedAst.ParYieldFragment(pat, e0, l0) =>
          val p = visitPattern(pat)
          val e1 = visitExp(e0)
          TypedAst.ParYieldFragment(p, e1, l0)
      }
      val tpe = e.tpe
      val eff = fs.foldLeft(e.eff) {
        case (acc, TypedAst.ParYieldFragment(_, e1, _)) => Type.mkUnion(acc, e1.eff, loc)
      }
      TypedAst.Expr.ParYield(fs, e, tpe, eff, loc)

    case KindedAst.Expr.Lazy(exp, loc) =>
      val e = visitExp(exp)
      val tpe = Type.mkLazy(e.tpe, loc)
      TypedAst.Expr.Lazy(e, tpe, loc)

    case KindedAst.Expr.Force(exp, tvar, loc) =>
      val e = visitExp(exp)
      val tpe = subst(tvar)
      val eff = e.eff
      TypedAst.Expr.Force(e, tpe, eff, loc)

    case KindedAst.Expr.FixpointConstraintSet(cs0, tvar, loc) =>
      val cs = cs0.map(visitConstraint)
      TypedAst.Expr.FixpointConstraintSet(cs, Stratification.empty, subst(tvar), loc)

    case KindedAst.Expr.FixpointLambda(pparams, exp, tvar, loc) =>
      val ps = pparams.map(visitPredicateParam)
      val e = visitExp(exp)
      val tpe = subst(tvar)
      val eff = e.eff
      TypedAst.Expr.FixpointLambda(ps, e, Stratification.empty, tpe, eff, loc)

    case KindedAst.Expr.FixpointMerge(exp1, exp2, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val tpe = e1.tpe
      val eff = Type.mkUnion(e1.eff, e2.eff, loc)
      TypedAst.Expr.FixpointMerge(e1, e2, Stratification.empty, tpe, eff, loc)

    case KindedAst.Expr.FixpointSolve(exp, loc) =>
      val e = visitExp(exp)
      val tpe = e.tpe
      val eff = e.eff
      TypedAst.Expr.FixpointSolve(e, Stratification.empty, tpe, eff, loc)

    case KindedAst.Expr.FixpointFilter(pred, exp, tvar, loc) =>
      val e = visitExp(exp)
      val eff = e.eff
      TypedAst.Expr.FixpointFilter(pred, e, subst(tvar), eff, loc)

    case KindedAst.Expr.FixpointInject(exp, pred, tvar, loc) =>
      val e = visitExp(exp)
      val eff = e.eff
      TypedAst.Expr.FixpointInject(e, pred, subst(tvar), eff, loc)

    case KindedAst.Expr.FixpointProject(pred, exp1, exp2, tvar, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val stf = Stratification.empty
      val tpe = subst(tvar)
      val eff = Type.mkUnion(e1.eff, e2.eff, loc)

      // Note: This transformation should happen in the Weeder but it is here because
      // `#{#Result(..)` | _} cannot be unified with `#{A(..)}` (a closed row).
      // See Weeder for more details.
      val mergeExp = TypedAst.Expr.FixpointMerge(e1, e2, stf, e1.tpe, eff, loc)
      val solveExp = TypedAst.Expr.FixpointSolve(mergeExp, stf, e1.tpe, eff, loc)
      TypedAst.Expr.FixpointProject(pred, solveExp, tpe, eff, loc)

    case KindedAst.Expr.Error(m, tvar, pvar) =>
      val tpe = subst(tvar)
      val eff = subst(pvar)
      TypedAst.Expr.Error(m, tpe, eff)
  }

  /**
    * Applies the substitution to the given constraint.
    */
  def visitConstraint(c0: KindedAst.Constraint)(implicit root: KindedAst.Root, subst0: Substitution): TypedAst.Constraint = {
    // Pattern match on the constraint.
    val KindedAst.Constraint(cparams0, head0, body0, loc) = c0

    // Unification was successful. Reassemble the head and body predicates.
    val head = visitHeadPredicate(head0)
    val body = body0.map(b => visitBodyPredicate(b))

    // Reassemble the constraint parameters.
    val cparams = cparams0.map {
      case KindedAst.ConstraintParam(sym, l) =>
        TypedAst.ConstraintParam(sym, subst0(sym.tvar), l)
    }

    // Reassemble the constraint.
    TypedAst.Constraint(cparams, head, body, loc)
  }

  /**
    * Reconstructs types in the given formal param.
    */
  def visitFormalParam(fparam: KindedAst.FormalParam)(implicit root: KindedAst.Root, subst0: Substitution): TypedAst.FormalParam =
    TypedAst.FormalParam(fparam.sym, fparam.mod, subst0(fparam.tpe), fparam.src, fparam.loc)

  /**
    * Reconstructs types in the given predicate param.
    */
  def visitPredicateParam(pparam: KindedAst.PredicateParam)(implicit root: KindedAst.Root, subst0: Substitution): TypedAst.PredicateParam =
    TypedAst.PredicateParam(pparam.pred, subst0(pparam.tpe), pparam.loc)

  /**
    * Reconstructs types in the given JVM method.
    */
  def visitJvmMethod(method: KindedAst.JvmMethod)(implicit root: KindedAst.Root, subst0: Substitution): TypedAst.JvmMethod = {
    method match {
      case KindedAst.JvmMethod(ident, fparams0, exp0, tpe, eff, loc) =>
        val fparams = fparams0.map(visitFormalParam)
        val exp = visitExp(exp0)
        TypedAst.JvmMethod(ident, fparams, exp, tpe, eff, loc)
    }
  }

  /**
    * Reconstructs types in the given pattern.
    */
  private def visitPattern(pat0: KindedAst.Pattern)(implicit root: KindedAst.Root, subst0: Substitution): TypedAst.Pattern = pat0 match {
    case KindedAst.Pattern.Wild(tvar, loc) => TypedAst.Pattern.Wild(subst0(tvar), loc)
    case KindedAst.Pattern.Var(sym, tvar, loc) => TypedAst.Pattern.Var(sym, subst0(tvar), loc)
    case KindedAst.Pattern.Cst(cst, loc) => TypedAst.Pattern.Cst(cst, constantType(cst), loc)

    case KindedAst.Pattern.Tag(sym, pat, tvar, loc) => TypedAst.Pattern.Tag(sym, visitPattern(pat), subst0(tvar), loc)

    case KindedAst.Pattern.Tuple(elms, loc) =>
      val es = elms.map(visitPattern)
      val tpe = Type.mkTuple(es.map(_.tpe), loc)
      TypedAst.Pattern.Tuple(es, tpe, loc)

    case KindedAst.Pattern.Record(pats, pat, tvar, loc) =>
      val ps = pats.map {
        case KindedAst.Pattern.Record.RecordLabelPattern(field, tvar1, pat1, loc1) =>
          TypedAst.Pattern.Record.RecordLabelPattern(field, subst0(tvar1), visitPattern(pat1), loc1)
      }
      val p = visitPattern(pat)
      TypedAst.Pattern.Record(ps, p, subst0(tvar), loc)

    case KindedAst.Pattern.RecordEmpty(loc) =>
      TypedAst.Pattern.RecordEmpty(Type.mkRecord(Type.RecordRowEmpty, loc), loc)
  }


  /**
    * Reconstructs types in the given head predicate.
    */
  private def visitHeadPredicate(head0: KindedAst.Predicate.Head)(implicit root: KindedAst.Root, subst0: Substitution): TypedAst.Predicate.Head = head0 match {
    case KindedAst.Predicate.Head.Atom(pred, den0, terms, tvar, loc) =>
      val ts = terms.map(t => visitExp(t))
      TypedAst.Predicate.Head.Atom(pred, den0, ts, subst0(tvar), loc)
  }


  /**
    * Reconstructs types in the given body predicate.
    */
  private def visitBodyPredicate(body0: KindedAst.Predicate.Body)(implicit root: KindedAst.Root, subst0: Substitution): TypedAst.Predicate.Body = body0 match {
    case KindedAst.Predicate.Body.Atom(pred, den0, polarity, fixity, terms, tvar, loc) =>
      val ts = terms.map(t => visitPattern(t))
      TypedAst.Predicate.Body.Atom(pred, den0, polarity, fixity, ts, subst0(tvar), loc)

    case KindedAst.Predicate.Body.Functional(outVars, exp, loc) =>
      val e = visitExp(exp)
      TypedAst.Predicate.Body.Functional(outVars, e, loc)

    case KindedAst.Predicate.Body.Guard(exp, loc) =>
      val e = visitExp(exp)
      TypedAst.Predicate.Body.Guard(e, loc)

  }

  /**
    * Returns the type of the given constant.
    */
  private def constantType(cst: Ast.Constant): Type = cst match {
    case Constant.Unit => Type.Unit
    case Constant.Null => Type.Null
    case Constant.Bool(_) => Type.Bool
    case Constant.Char(_) => Type.Char
    case Constant.Float32(_) => Type.Float32
    case Constant.Float64(_) => Type.Float64
    case Constant.BigDecimal(_) => Type.BigDecimal
    case Constant.Int8(_) => Type.Int8
    case Constant.Int16(_) => Type.Int16
    case Constant.Int32(_) => Type.Int32
    case Constant.Int64(_) => Type.Int64
    case Constant.BigInt(_) => Type.BigInt
    case Constant.Str(_) => Type.Str
    case Constant.Regex(_) => Type.Regex
  }
}
