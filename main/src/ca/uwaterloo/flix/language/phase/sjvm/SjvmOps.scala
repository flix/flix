/*
 * Copyright 2020-2021 Jonathan Lindegaard Starup
 * Copyright 2017 Magnus Madsen
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

package ca.uwaterloo.flix.language.phase.sjvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ErasedAst.{Case, CatchRule, Def, Expression, Root}
import ca.uwaterloo.flix.language.ast.RRefType.REnum
import ca.uwaterloo.flix.language.ast.RType.RReference
import ca.uwaterloo.flix.language.ast.{PType, RType, Symbol}
import ca.uwaterloo.flix.language.phase.unification.Unification

object SjvmOps {

  /**
    * Returns `true` if the given definition `defn` is a law.
    */
  def nonLaw(defn: Def): Boolean = !defn.ann.isLaw

  /**
    * Returns the namespace info of the given definition symbol `sym`.
    */
  def getNamespace(sym: Symbol.DefnSym)(implicit root: Root, flix: Flix): NamespaceInfo = {
    NamespaceInfo(sym.namespace, Map.empty) // TODO: Magnus: Empty map.
  }

  /**
    * Returns the set of namespaces in the given AST `root`.
    */
  def namespacesOf(root: Root): Set[NamespaceInfo] = {
    // Group every symbol by namespace.
    root.defs.groupBy(_._1.namespace).map {
      case (ns, defs) =>
        // Collect all non-law definitions.
        val nonLaws = defs filter {
          case (sym, defn) => nonLaw(defn)
        }
        NamespaceInfo(ns, nonLaws)
    }.toSet
  }

  /**
    * Returns the set of closures in the given AST `root`.
    */
  def closuresOf(root: Root): Set[ClosureInfo] = {
    /**
      * Returns the set of closures in the given expression `exp0`.
      */
    def visitExp[T <: PType](exp0: Expression[T]): Set[ClosureInfo] = exp0 match {
      case Expression.Unit(_) => Set.empty

      case Expression.Null(tpe, _) => Set.empty

      case Expression.True(_) => Set.empty

      case Expression.False(_) => Set.empty

      case Expression.Char(lit, _) => Set.empty

      case Expression.Float32(lit, _) => Set.empty

      case Expression.Float64(lit, _) => Set.empty

      case Expression.Int8(lit, _) => Set.empty

      case Expression.Int16(lit, _) => Set.empty

      case Expression.Int32(lit, _) => Set.empty

      case Expression.Int64(lit, _) => Set.empty

      case Expression.BigInt(lit, _) => Set.empty

      case Expression.Str(lit, _) => Set.empty

      case Expression.Var(sym, tpe, loc) => Set.empty

      case Expression.Closure(sym, freeVars, tpe, loc) =>
        Set(ClosureInfo(sym, freeVars, tpe))

      case Expression.ApplyClo(exp, args, tpe, loc) => args.foldLeft(visitExp(exp)) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ApplyDef(sym, args, tpe, loc) => args.foldLeft(Set.empty[ClosureInfo]) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ApplyCloTail(exp, args, tpe, loc) => args.foldLeft(visitExp(exp)) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ApplyDefTail(sym, args, tpe, loc) => args.foldLeft(Set.empty[ClosureInfo]) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ApplySelfTail(sym, fparams, args, tpe, loc) => args.foldLeft(Set.empty[ClosureInfo]) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.Unary(sop, op, exp, tpe, loc) =>
        visitExp(exp)

      case Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
        visitExp(exp1) ++ visitExp(exp2)

      case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

      case Expression.Branch(exp, branches, tpe, loc) => branches.foldLeft(visitExp(exp)) {
        case (sacc, (_, e)) => sacc ++ visitExp(e)
      }

      case Expression.JumpTo(sym, tpe, loc) => Set.empty

      case Expression.Let(sym, exp1, exp2, tpe, loc) => visitExp(exp1) ++ visitExp(exp2)

      case Expression.Is(sym, tag, exp, loc) => visitExp(exp)

      case Expression.Tag(sym, tag, exp, tpe, loc) => visitExp(exp)

      case Expression.Untag(sym, tag, exp, tpe, loc) => visitExp(exp)

      case Expression.Index(base, offset, tpe, loc) => visitExp(base)

      case Expression.Tuple(elms, tpe, loc) => elms.foldLeft(Set.empty[ClosureInfo]) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.RecordEmpty(tpe, loc) => Set.empty

      case Expression.RecordExtend(_, value, rest, tpe, loc) => visitExp(value) ++ visitExp(rest)

      case Expression.RecordSelect(exp, _, tpe, loc) => visitExp(exp)

      case Expression.RecordRestrict(_, rest, tpe, loc) => visitExp(rest)

      case Expression.ArrayLit(elms, tpe, loc) => elms.foldLeft(Set.empty[ClosureInfo]) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ArrayNew(elm, len, tpe, loc) => visitExp(elm) ++ visitExp(len)

      case Expression.ArrayLoad(exp1, exp2, tpe, loc) => visitExp(exp1) ++ visitExp(exp2)

      case Expression.ArrayStore(exp1, exp2, exp3, tpe, loc) => visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

      case Expression.ArrayLength(exp, tpe, loc) => visitExp(exp)

      case Expression.ArraySlice(exp1, exp2, exp3, tpe, loc) => visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

      case Expression.Ref(exp, tpe, loc) => visitExp(exp)

      case Expression.Deref(exp, tpe, loc) => visitExp(exp)

      case Expression.Assign(exp1, exp2, tpe, loc) => visitExp(exp1) ++ visitExp(exp2)

      case Expression.Existential(fparam, exp, loc) => visitExp(exp)

      case Expression.Universal(fparam, exp, loc) => visitExp(exp)

      case Expression.Cast(exp, tpe, loc) => visitExp(exp)

      case Expression.TryCatch(exp, rules, tpe, loc) =>
        rules.foldLeft(visitExp(exp)) {
          case (sacc, CatchRule(sym, clazz, body)) => sacc ++ visitExp(body)
        }

      case Expression.InvokeConstructor(constructor, args, tpe, loc) => args.foldLeft(Set.empty[ClosureInfo]) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.InvokeMethod(method, exp, args, tpe, loc) =>
        args.foldLeft(visitExp(exp)) {
          case (sacc, e) => sacc ++ visitExp(e)
        }

      case Expression.InvokeStaticMethod(method, args, tpe, loc) =>
        args.foldLeft(Set.empty[ClosureInfo]) {
          case (sacc, e) => sacc ++ visitExp(e)
        }

      case Expression.GetField(field, exp, tpe, loc) =>
        visitExp(exp)

      case Expression.PutField(field, exp1, exp2, tpe, loc) =>
        visitExp(exp1) ++ visitExp(exp2)

      case Expression.GetStaticField(field, tpe, loc) =>
        Set.empty

      case Expression.PutStaticField(field, exp, tpe, loc) =>
        visitExp(exp)

      case Expression.NewChannel(exp, tpe, loc) => visitExp(exp)

      case Expression.GetChannel(exp, tpe, loc) => visitExp(exp)

      case Expression.PutChannel(exp1, exp2, tpe, loc) => visitExp(exp1) ++ visitExp(exp2)

      case Expression.SelectChannel(rules, default, tpe, loc) =>
        val rs = rules.foldLeft(Set.empty[ClosureInfo])((old, rule) =>
          old ++ visitExp(rule.chan) ++ visitExp(rule.exp))
        val d = default.map(visitExp).getOrElse(Set.empty)
        rs ++ d

      case Expression.Spawn(exp, tpe, loc) => visitExp(exp)

      case Expression.Lazy(exp, tpe, loc) => visitExp(exp)

      case Expression.Force(exp, tpe, loc) => visitExp(exp)

      case Expression.HoleError(sym, tpe, loc) => Set.empty

      case Expression.MatchError(tpe, loc) => Set.empty
    }

    // TODO: Look for closures in other places.

    // Visit every definition.
    root.defs.foldLeft(Set.empty[ClosureInfo]) {
      case (sacc, (sym, defn)) => sacc ++ visitExp(defn.exp)
    }
  }

  /**
    * Returns the set of tags in the given AST `root`.
    */
  def tagsOf(root: Root)(implicit flix: Flix): Set[TagInfo] = {
    typesOf(root).flatMap(tpe => getTagsOf(tpe)(root, flix))
  }

  /**
    * Returns the set of tags associated with the given type.
    */
  def getTagsOf(tpe: RType[PType])(implicit root: Root, flix: Flix): Set[TagInfo] = tpe match {
    case enumType@RReference(REnum(sym, args)) =>
      // Retrieve the enum.
      val enum = root.enums(enumType.sym)

      // Compute the tag info.
      enum.cases.foldLeft(Set.empty[TagInfo]) {
        case (sacc, (_, Case(enumSym, tagName, uninstantiatedTagType, loc))) =>
          // TODO: Magnus: It would be nice if this information could be stored somewhere...
          val subst = Unification.unifyTypes(hackMonoType2Type(enum.tpeDeprecated), hackMonoType2Type(tpe)).get
          val tagType = subst(hackMonoType2Type(uninstantiatedTagType))

          sacc + TagInfo(enumSym, tagName.name, args, tpe, hackType2MonoType(tagType))
      }
    case _ => Set.empty
  }

  /**
    * Returns the set of all instantiated types in the given AST `root`.
    *
    * This include type components. For example, if the program contains
    * the type (Bool, (Char, Int)) this includes the type (Char, Int).
    */
  def typesOf(root: Root)(implicit flix: Flix): Set[RType[PType]] = {
    /**
      * Returns the set of types which occur in the given definition `defn0`.
      */
    def visitDefn(defn: Def): Set[MonoType] = {
      // Compute the types in the formal parameters.
      val formalParamTypes = defn.formals.foldLeft(Set.empty[MonoType]) {
        case (sacc, FormalParam(sym, tpe)) => sacc + tpe
      }

      // Compute the types in the expression.
      val expressionTypes = visitExp(defn.exp)

      // Return the types in the defn.
      formalParamTypes ++ expressionTypes + defn.tpe
    }

    /**
      * Returns the set of types which occur in the given expression `exp0`.
      */
    def visitExp(exp0: Expression): Set[MonoType] = exp0 match {
      case Expression.Unit(_) => Set(MonoType.Unit)

      case Expression.Null(tpe, _) => Set(tpe)

      case Expression.True(_) => Set(MonoType.Bool)

      case Expression.False(_) => Set(MonoType.Bool)

      case Expression.Char(lit, _) => Set(MonoType.Char)

      case Expression.Float32(lit, _) => Set(MonoType.Float32)

      case Expression.Float64(lit, _) => Set(MonoType.Float64)

      case Expression.Int8(lit, _) => Set(MonoType.Int8)

      case Expression.Int16(lit, _) => Set(MonoType.Int16)

      case Expression.Int32(lit, _) => Set(MonoType.Int32)

      case Expression.Int64(lit, _) => Set(MonoType.Int64)

      case Expression.BigInt(lit, _) => Set(MonoType.BigInt)

      case Expression.Str(lit, _) => Set(MonoType.Str)

      case Expression.Var(sym, tpe, loc) => Set(tpe)

      case Expression.Closure(sym, freeVars, fnType, tpe, loc) => Set(tpe)

      case Expression.ApplyClo(exp, args, tpe, loc) => args.foldLeft(visitExp(exp) + tpe) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ApplyDef(sym, args, tpe, loc) => args.foldLeft(Set(tpe)) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ApplyCloTail(exp, args, tpe, loc) => args.foldLeft(visitExp(exp) + tpe) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ApplyDefTail(sym, args, tpe, loc) => args.foldLeft(Set(tpe)) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ApplySelfTail(sym, fparams, args, tpe, loc) => args.foldLeft(Set(tpe)) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.Unary(sop, op, exp, tpe, loc) =>
        visitExp(exp) + tpe

      case Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
        visitExp(exp1) ++ visitExp(exp2) + tpe

      case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3) + tpe

      case Expression.Branch(exp, branches, tpe, loc) => branches.foldLeft(visitExp(exp)) {
        case (sacc, (_, e)) => sacc ++ visitExp(e)
      }

      case Expression.JumpTo(sym, tpe, loc) => Set(tpe)

      case Expression.Let(sym, exp1, exp2, tpe, loc) => visitExp(exp1) ++ visitExp(exp2) + tpe

      case Expression.Is(sym, tag, exp, loc) => visitExp(exp)

      case Expression.Tag(sym, tag, exp, tpe, loc) => visitExp(exp) + tpe

      case Expression.Untag(sym, tag, exp, tpe, loc) => visitExp(exp) + tpe

      case Expression.Index(base, offset, tpe, loc) => visitExp(base) + tpe

      case Expression.Tuple(elms, tpe, loc) => elms.foldLeft(Set(tpe)) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.RecordEmpty(tpe, loc) => Set(tpe)

      case Expression.RecordSelect(exp, _, tpe, loc) => Set(tpe) ++ visitExp(exp)

      case Expression.RecordExtend(_, value, rest, tpe, loc) => Set(tpe) ++ visitExp(value) ++ visitExp(rest)

      case Expression.RecordRestrict(_, rest, tpe, loc) => Set(tpe) ++ visitExp(rest)

      case Expression.ArrayLit(elms, tpe, loc) => elms.foldLeft(Set(tpe)) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.ArrayNew(elm, len, tpe, loc) => visitExp(elm) ++ visitExp(len)

      case Expression.ArrayLoad(exp1, exp2, tpe, loc) => visitExp(exp1) ++ visitExp(exp2)

      case Expression.ArrayStore(exp1, exp2, exp3, tpe, loc) => visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

      case Expression.ArrayLength(exp, tpe, loc) => visitExp(exp)

      case Expression.ArraySlice(exp1, exp2, exp3, tpe, loc) => visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

      case Expression.Ref(exp, tpe, loc) => visitExp(exp) + tpe

      case Expression.Deref(exp, tpe, loc) => visitExp(exp) + tpe

      case Expression.Assign(exp1, exp2, tpe, loc) => visitExp(exp1) ++ visitExp(exp2) + tpe

      case Expression.Existential(fparam, exp, loc) => visitExp(exp) + fparam.tpe

      case Expression.Universal(fparam, exp, loc) => visitExp(exp) + fparam.tpe

      case Expression.Cast(exp, tpe, loc) => visitExp(exp) + tpe

      case Expression.TryCatch(exp, rules, tpe, loc) =>
        rules.foldLeft(visitExp(exp)) {
          case (sacc, CatchRule(sym, clazz, body)) => sacc ++ visitExp(body)
        }

      case Expression.InvokeConstructor(constructor, args, tpe, loc) => args.foldLeft(Set(tpe)) {
        case (sacc, e) => sacc ++ visitExp(e)
      }

      case Expression.InvokeMethod(method, exp, args, tpe, loc) =>
        args.foldLeft(visitExp(exp) + tpe) {
          case (sacc, e) => sacc ++ visitExp(e)
        }

      case Expression.InvokeStaticMethod(method, args, tpe, loc) =>
        args.foldLeft(Set(tpe)) {
          case (sacc, e) => sacc ++ visitExp(e)
        }

      case Expression.GetField(field, exp, tpe, loc) =>
        visitExp(exp) + tpe

      case Expression.PutField(field, exp1, exp2, tpe, loc) =>
        visitExp(exp1) ++ visitExp(exp2) + tpe

      case Expression.GetStaticField(field, tpe, loc) =>
        Set(tpe)

      case Expression.PutStaticField(field, exp, tpe, loc) =>
        visitExp(exp) + tpe

      case Expression.NewChannel(exp, tpe, loc) => visitExp(exp) + tpe

      case Expression.GetChannel(exp, tpe, loc) => visitExp(exp) + tpe

      case Expression.PutChannel(exp1, exp2, tpe, loc) => visitExp(exp1) ++ visitExp(exp2) + tpe

      case Expression.SelectChannel(rules, default, tpe, loc) =>
        val rs = rules.foldLeft(Set(tpe))((old, rule) => old ++ visitExp(rule.chan) ++ visitExp(rule.exp))
        val d = default.map(visitExp).getOrElse(Set.empty)
        rs ++ d

      case Expression.Spawn(exp, tpe, loc) => visitExp(exp) + tpe

      case Expression.Lazy(exp, tpe, loc) => visitExp(exp) + tpe

      case Expression.Force(exp, tpe, loc) => visitExp(exp) + tpe

      case Expression.HoleError(sym, tpe, loc) => Set(tpe)

      case Expression.MatchError(tpe, loc) => Set(tpe)
    }

    // TODO: Magnus: Look for types in other places.

    // Visit every definition.
    val result = root.defs.foldLeft(Set.empty[MonoType]) {
      case (sacc, (_, defn)) => sacc ++ visitDefn(defn)
    }

    result.flatMap(t => nestedTypesOf(t)(root, flix))
  }

}
