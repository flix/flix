/*
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

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Ast.Modifiers
import ca.uwaterloo.flix.language.ast.shared.Scope
import ca.uwaterloo.flix.language.ast.{Ast, Kind, LoweredAst, MonoAst, Name, RigidityEnv, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.dbg.AstPrinter.*
import ca.uwaterloo.flix.language.phase.unification.{EqualityEnvironment, Substitution, Unification}
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.collection.{ListMap, ListOps}
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps, Result}

import java.util.concurrent.ConcurrentLinkedQueue
import scala.collection.immutable.SortedSet
import scala.collection.mutable

/**
  * Monomorphization is a whole-program compilation strategy that replaces every reference to a
  * parametric function with a reference to a non-parametric version (of that function) specialized
  * to the concrete types of the reference.
  *
  * Additionally it eliminates all type variables and normalizes types. This means that types have
  * unique representations, without aliases and associated types.
  *
  * Additionally it resolves type class methods into actual function calls.
  *
  * For example, the polymorphic program:
  *
  *   -   def fst[a, b](p: (a, b)): a = let (x, y) = p ; x
  *   -   def f: Bool = fst((true, 'a'))
  *   -   def g: Int32 = fst((42, "foo"))
  *
  * is, roughly speaking, translated to:
  *
  *   -   def fst$1(p: (Bool, Char)): Bool = let (x, y) = p ; x
  *   -   def fst$2(p: (Int32, String)): Int32 = let (x, y) = p ; x
  *   -   def f: Bool = fst$1((true, 'a'))
  *   -   def g: Bool = fst$2((42, "foo"))
  *
  * Additionally for things like record types and effect formulas, equivalent types are flattened
  * and ordered. This means that `{b = String, a = String}` becomes `{a = String, b = String}` and
  * that `Print + (Crash + Print)` becomes `Crash + Print`.
  *
  * At a high-level, monomorphization works as follows:
  *
  *   1. We maintain a queue of functions and the concrete, normalized types they must be
  *      specialized to.
  *   1. We populate the queue by specialization of non-parametric function definitions.
  *   1. We iteratively extract a function from the queue and specialize it:
  *      a. We replace every type variable appearing anywhere in the definition by its concrete
  *         type.
  *      a. We create new fresh local variable symbols (since the function is effectively being
  *         copied).
  *      a. We enqueue (or re-used) other functions referenced by the current function which require
  *         specialization.
  *   1. We reconstruct the AST from the specialized functions and remove all parametric functions
  *      (Enum declarations are deleted to avoid specializing them).
  *
  * Type normalization details:
  *
  *   - Record fields are in alphabetical order
  *   - Schema fields are in alphabetical order
  *   - Effect formulas are flat unions of effects in alphabetical order.
  *   - Case set formulas are a single CaseSet literal.
  *
  */
object Monomorpher {

  // TODO levels trying top scope to get it to compile. Revisit.
  private implicit val S: Scope = Scope.Top

  /**
    * A strict substitution is similar to a regular substitution except that free type variables are
    * replaced by an appropriate default type. In other words, when performing a type substitution,
    * if there is no requirement on a polymorphic type, we assume it to be the default type of its
    * kind. This is safe since otherwise the type would not be polymorphic after type-inference.
    *
    * Properties of normalized types (which is returned by apply):
    *   - No type variables
    *   - No associated types
    *   - No type aliases
    *   - Equivalent types are uniquely represented (e.g. fields in records types are alphabetized)
    *
    * Notes on `s`: It is only applied to variables directly in the apply of the strict
    * substitution. They image of `s` does not have type aliases or associated types but can have
    * type variables and non-unique types. Variables in the image are unconstrained and will be
    * mapped to their default representation. `s` is used without the [[StrictSubstitution]] to
    * support variables in typematch, where it is not only used on variables.
    */
  private case class StrictSubstitution(s: Substitution, eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix) {

    /**
      * Returns the normalized default type for the kind of `tpe0`.
      */
    private def default(tpe0: Type): Type = tpe0.kind match {
      case Kind.Wild => Type.mkAnyType(tpe0.loc)
      case Kind.WildCaseSet => Type.mkAnyType(tpe0.loc)
      case Kind.Star => Type.mkAnyType(tpe0.loc)
      case Kind.Eff =>
        // If an effect variable is free, we may assume its Pure due to the subst. lemma.
        Type.Pure
      case Kind.Bool => Type.mkAnyType(tpe0.loc)
      case Kind.RecordRow => Type.RecordRowEmpty
      case Kind.SchemaRow => Type.SchemaRowEmpty
      case Kind.Predicate => Type.mkAnyType(tpe0.loc)
      case Kind.CaseSet(sym) => Type.Cst(TypeConstructor.CaseSet(SortedSet.empty, sym), tpe0.loc)
      case Kind.Arrow(_, _) => Type.mkAnyType(tpe0.loc)
      case Kind.Jvm => throw InternalCompilerException(s"Unexpected type: '$tpe0'.", tpe0.loc)
      case Kind.Error => throw InternalCompilerException(s"Unexpected type '$tpe0'.", tpe0.loc)
    }

    /**
      * Applies `this` substitution to the given type `tpe`, returning a normalized type.
      *
      * Performance Note: We are on a hot path. We take extra care to avoid redundant type objects.
      */
    def apply(tpe0: Type): Type = tpe0 match {
      case x: Type.Var =>
        // Remove variables by substitution, otherwise by default type.
        s.m.get(x.sym) match {
          case Some(t) =>
            // Use default since variables in the output of `s.m` are unconstrained.
            // It is important to do this before apply to avoid looping on variables.
            val t1 = t.map(default)
            // Use apply to normalize the type, `t1` is ground.
            apply(t1)
          case None =>
            // Default types are normalized.
            default(x)
        }

      case Type.Cst(_, _) =>
        // Performance: Reuse tpe0.
        tpe0

      case Type.Apply(t1, t2, loc) =>

        (apply(t1), apply(t2)) match {
          // Simplify boolean equations.
          case (Type.Cst(TypeConstructor.Complement, _), y) => Type.mkComplement(y, loc)
          case (Type.Apply(Type.Cst(TypeConstructor.Union, _), x, _), y) => Type.mkUnion(x, y, loc)
          case (Type.Apply(Type.Cst(TypeConstructor.Intersection, _), x, _), y) => Type.mkIntersection(x, y, loc)

          case (Type.Cst(TypeConstructor.CaseComplement(sym), _), y) => Type.mkCaseComplement(y, sym, loc)
          case (Type.Apply(Type.Cst(TypeConstructor.CaseIntersection(sym), _), x, _), y) => Type.mkCaseIntersection(x, y, sym, loc)
          case (Type.Apply(Type.Cst(TypeConstructor.CaseUnion(sym), _), x, _), y) => Type.mkCaseUnion(x, y, sym, loc)

          // Put records in alphabetical order
          case (Type.Apply(Type.Cst(TypeConstructor.RecordRowExtend(label), _), tpe, _), rest) => mkRecordExtendSorted(label, tpe, rest, loc)

          // Put schemas in alphabetical order
          case (Type.Apply(Type.Cst(TypeConstructor.SchemaRowExtend(label), _), tpe, _), rest) => mkSchemaExtendSorted(label, tpe, rest, loc)

          // Else just apply.
          case (x, y) =>
            // Performance: Reuse tpe0, if possible.
            if ((x eq t1) && (y eq t2)) {
              tpe0
            } else {
              Type.Apply(x, y, loc)
            }
        }

      case Type.Alias(_, _, t, _) =>
        // Remove the Alias.
        apply(t)

      case Type.AssocType(cst, arg0, _, loc) =>
        // Remove the associated type.
        val arg = apply(arg0)
        EqualityEnvironment.reduceAssocType(cst, arg, eqEnv) match {
          case Ok(t) =>
            // Use apply to normalize the type, `t` is ground.
            apply(t)
          case Err(_) => throw InternalCompilerException("unexpected associated type reduction failure", loc)
        }

      case Type.JvmToType(_, loc) => throw InternalCompilerException("unexpected JVM type", loc)
      case Type.JvmToEff(_, loc) => throw InternalCompilerException("unexpected JVM eff", loc)
      case Type.UnresolvedJvmType(_, loc) => throw InternalCompilerException("unexpected JVM type", loc)
    }

    /**
      * Adds the given mapping to the substitution.
      *
      * The type must be a normalized type.
      */
    def +(kv: (Symbol.KindedTypeVarSym, Type)): StrictSubstitution = kv match {
      case (tvar, tpe) => StrictSubstitution(s ++ Substitution.singleton(tvar, tpe), eqEnv)
    }

    /**
      * Removes the binding for the given type variable `tvar` (if it exists).
      */
    def unbind(sym: Symbol.KindedTypeVarSym): StrictSubstitution =
      StrictSubstitution(s.unbind(sym), eqEnv)

    /**
      * Returns the non-strict version of this substitution.
      *
      * Mapping added must not contain type aliases or associated types. Variables in the image of
      * the substitution are considered unconstrained.
      */
    def nonStrict: Substitution = s
  }

  /**
    * Holds the mutable data used throughout monomorphization.
    *
    * This class is thread-safe.
    */
  private class Context() {

    /**
      * A queue of pending (fresh symbol, function definition, and substitution)-triples.
      *
      * For example, if the queue contains the entry:
      *
      *   -   (f$1, f, [a -> Int32])
      *
      * it means that the function definition f should be specialized w.r.t. the map [a -> Int32] under the fresh name f$1.
      *
      * Note: We use a concurrent linked queue (which is non-blocking) so threads can enqueue items without contention.
      */
    private val defQueue: ConcurrentLinkedQueue[(Symbol.DefnSym, LoweredAst.Def, StrictSubstitution)] = new ConcurrentLinkedQueue

    /**
      * Returns `true` if the queue is non-empty.
      *
      * Note: This is not synchronized.
      */
    def nonEmpty: Boolean = synchronized {
      !defQueue.isEmpty
    }

    /**
      * Enqueues the given symbol, def, and substitution triple.
      *
      * Note: This is not synchronized.
      */
    def enqueue(sym: Symbol.DefnSym, defn: LoweredAst.Def, subst: StrictSubstitution): Unit = synchronized {
      defQueue.add((sym, defn, subst))
    }

    /**
      * Dequeues an element from the queue.
      *
      * Note: This is not synchronized.
      */
    def dequeueAll: Array[(Symbol.DefnSym, LoweredAst.Def, StrictSubstitution)] = synchronized {
      val r = defQueue.toArray(Array.empty[(Symbol.DefnSym, LoweredAst.Def, StrictSubstitution)])
      defQueue.clear()
      r
    }

    /**
      * A map from a symbol and a normalized type to the fresh symbol for the specialized version
      * of that function.
      *
      * For example, if the function:
      *
      *   -   def fst[a, b](x: a, y: b): a = ...
      *
      * has been specialized w.r.t. to `Int32` and `String` then this map will contain an entry:
      *
      *   -   (fst, (Int32, String) -> Int32) -> fst$1
      */
    private val def2def: mutable.Map[(Symbol.DefnSym, Type), Symbol.DefnSym] = mutable.Map.empty

    /**
      * Optionally returns the specialized def symbol for the given symbol `sym` and type `tpe`.
      */
    def getDef2Def(sym: Symbol.DefnSym, tpe: Type): Option[Symbol.DefnSym] = synchronized {
      def2def.get((sym, tpe))
    }

    /**
      * Adds a new def2def binding for the given symbol `sym1` and type `tpe`.
      */
    def putDef2Def(sym1: Symbol.DefnSym, tpe: Type, sym2: Symbol.DefnSym): Unit = synchronized {
      def2def.put((sym1, tpe), sym2)
    }

    /**
      * A map used to collect specialized definitions, etc.
      */
    private val specializedDefns: mutable.Map[Symbol.DefnSym, MonoAst.Def] = mutable.Map.empty

    /**
      * Adds a new specialized definition for the given def symbol `sym`.
      */
    def putSpecializedDef(sym: Symbol.DefnSym, defn: MonoAst.Def): Unit = synchronized {
      specializedDefns.put(sym, defn)
    }

    /**
      * Returns the specialized definitions as an immutable map.
      */
    def toMap: Map[Symbol.DefnSym, MonoAst.Def] = synchronized {
      specializedDefns.toMap
    }
  }

  /**
    * Returns a sorted record, assuming that `rest` is sorted.
    * Sorting is stable on duplicate fields.
    *
    * Assumes that rest does not contain variables, aliases, or associated types.
    */
  private def mkRecordExtendSorted(label: Name.Label, tpe: Type, rest: Type, loc: SourceLocation): Type = rest match {
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordRowExtend(l), loc1), t, loc2), r, loc3) if l.name < label.name =>
      // Push extend further.
      val newRest = mkRecordExtendSorted(label, tpe, r, loc)
      Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordRowExtend(l), loc1), t, loc2), newRest, loc3)
    case Type.Cst(_, _) | Type.Apply(_, _, _) =>
      // Non-record related types or a record in correct order.
      Type.mkRecordRowExtend(label, tpe, rest, loc)
    case Type.Var(_, _) => throw InternalCompilerException(s"Unexpected type variable '$rest'", rest.loc)
    case Type.Alias(_, _, _, _) => throw InternalCompilerException(s"Unexpected alias '$rest'", rest.loc)
    case Type.AssocType(_, _, _, _) => throw InternalCompilerException(s"Unexpected associated type '$rest'", rest.loc)
    case Type.JvmToType(_, _) => throw InternalCompilerException(s"Unexpected JVM type '$rest'", rest.loc)
    case Type.JvmToEff(_, _) => throw InternalCompilerException(s"Unexpected JVM eff '$rest'", rest.loc)
    case Type.UnresolvedJvmType(_, _) => throw InternalCompilerException(s"Unexpected JVM type '$rest'", rest.loc)
  }

  /**
    * Returns a sorted schema, assuming that `rest` is sorted.
    * Sorting is stable on duplicate predicates.
    *
    * Assumes that rest does not contain variables, aliases, or associated types.
    */
  private def mkSchemaExtendSorted(label: Name.Pred, tpe: Type, rest: Type, loc: SourceLocation): Type = rest match {
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaRowExtend(l), loc1), t, loc2), r, loc3) if l.name < label.name =>
      // Push extend further.
      val newRest = mkSchemaExtendSorted(label, tpe, r, loc)
      Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaRowExtend(l), loc1), t, loc2), newRest, loc3)
    case Type.Cst(_, _) | Type.Apply(_, _, _) =>
      // Non-record related types or a record in correct order.
      Type.mkSchemaRowExtend(label, tpe, rest, loc)
    case Type.Var(_, _) => throw InternalCompilerException(s"Unexpected type variable '$rest'", rest.loc)
    case Type.Alias(_, _, _, _) => throw InternalCompilerException(s"Unexpected alias '$rest'", rest.loc)
    case Type.AssocType(_, _, _, _) => throw InternalCompilerException(s"Unexpected associated type '$rest'", rest.loc)
    case Type.JvmToType(_, _) => throw InternalCompilerException(s"Unexpected JVM type '$rest'", rest.loc)
    case Type.JvmToEff(_, _) => throw InternalCompilerException(s"Unexpected JVM eff '$rest'", rest.loc)
    case Type.UnresolvedJvmType(_, _) => throw InternalCompilerException(s"Unexpected JVM type '$rest'", rest.loc)
  }

  /**
    * Performs monomorphization of the given AST `root`.
    */
  def run(root: LoweredAst.Root)(implicit flix: Flix): MonoAst.Root = flix.phase("Monomorpher") {

    implicit val r: LoweredAst.Root = root
    implicit val ctx: Context = new Context()
    val empty = StrictSubstitution(Substitution.empty, root.eqEnv)

    /*
     * Collect all non-parametric function definitions.
     */
    val nonParametricDefns = root.defs.filter {
      case (_, defn) => defn.spec.tparams.isEmpty
    }

    /*
     * Perform specialization of all non-parametric function definitions.
     *
     * We perform specialization in parallel.
     *
     * This will enqueue additional functions for specialization.
     */
    ParOps.parMap(nonParametricDefns) {
      case (sym, defn) =>
        // We use an empty substitution because the defs are non-parametric.
        // its important that non-parametric functions keep their symbol to not
        // invalidate the set of reachable functions.
        mkFreshDefn(sym, defn, empty)
    }

    /*
     * Perform function specialization until the queue is empty.
     *
     * We perform specialization in parallel along the frontier, i.e. each frontier is done in parallel.
     */
    while (ctx.nonEmpty) {
      // Extract a function from the queue and specializes it w.r.t. its substitution.
      val queue = ctx.dequeueAll
      ParOps.parMap(queue) {
        case (freshSym, defn, subst) => mkFreshDefn(freshSym, defn, subst)
      }
    }

    val effects = ParOps.parMapValues(root.effects) {
      case LoweredAst.Effect(doc, ann, mod, sym, ops0, loc) =>
        val ops = ops0.map {
          case LoweredAst.Op(sym, spec) =>
            MonoAst.Op(sym, visitEffectOpSpec(spec, empty))
        }
        MonoAst.Effect(doc, ann, mod, sym, ops, loc)
    }

    val structs =  ParOps.parMapValues(root.structs) {
      case LoweredAst.Struct(doc, ann, mod, sym, tparams0, fields, loc) =>
        val newFields = fields.map(visitStructField)
        val tparams = tparams0.map(_.sym)
        MonoAst.Struct(doc, ann, mod, sym, tparams, newFields, loc)
    }

    // Reassemble the AST.
    MonoAst.Root(
      ctx.toMap,
      structs,
      effects,
      root.entryPoint,
      root.reachable,
      root.sources
    )
  }

  def visitStructField(field: LoweredAst.StructField): MonoAst.StructField = {
    field match {
      case LoweredAst.StructField(fieldSym, tpe, loc) =>
        MonoAst.StructField(fieldSym, tpe, loc)
    }
  }

  /**
    * Converts the given effect op spec. Effect operations are monomorphic -
    * they have no variables - so the substitution can be empty.
    */
  private def visitEffectOpSpec(spec: LoweredAst.Spec, subst: StrictSubstitution): MonoAst.Spec = spec match {
    case LoweredAst.Spec(doc, ann, mod, _, fparams0, declaredScheme, retTpe, eff, _, loc) =>
      val fparams = fparams0.map {
        case LoweredAst.FormalParam(sym, mod, tpe, src, loc) =>
          MonoAst.FormalParam(sym, mod, subst(tpe), src, loc)
      }
      MonoAst.Spec(doc, ann, mod, fparams, declaredScheme.base, subst(retTpe), subst(eff), loc)
  }

  /**
    * Adds a specialized def for the given symbol `freshSym` and def `defn` with the given substitution `subst`.
    */
  private def mkFreshDefn(freshSym: Symbol.DefnSym, defn: LoweredAst.Def, subst: StrictSubstitution)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): Unit = {
    // Specialize the formal parameters and introduce fresh local variable symbols.
    val (fparams, env0) = specializeFormalParams(defn.spec.fparams, subst)

    // Specialize the body expression.
    val specializedExp = visitExp(defn.exp, env0, subst)

    // Reassemble the definition.
    // NB: Removes the type parameters as the function is now monomorphic.
    val spec0 = defn.spec
    val spec = MonoAst.Spec(
      spec0.doc,
      spec0.ann,
      spec0.mod,
      fparams,
      subst(defn.spec.declaredScheme.base),
      subst(spec0.retTpe),
      subst(spec0.eff),
      spec0.loc
    )
    val specializedDefn = MonoAst.Def(freshSym, spec, specializedExp)

    // Save the specialized function.
    ctx.putSpecializedDef(freshSym, specializedDefn)
  }

  /**
    * Performs specialization of the given expression `exp0` under the environment `env0` w.r.t. the
    * given substitution `subst`.
    *
    * Replaces every reference to a parametric function with a reference to its specialized version.
    *
    * Replaces every local variable symbol with a fresh local variable symbol.
    *
    * If a specialized version of a function does not yet exist, a fresh symbol is created for it,
    * and the definition and substitution is enqueued.
    */
  private def visitExp(exp0: LoweredAst.Expr, env0: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): MonoAst.Expr = exp0 match {
    case LoweredAst.Expr.Var(sym, tpe, loc) =>
      MonoAst.Expr.Var(env0(sym), subst(tpe), loc)

    case LoweredAst.Expr.Def(sym, tpe, loc) =>
      /*
       * !! This is where all the magic happens !!
       */
      val newSym = specializeDefSym(sym, subst(tpe))
      MonoAst.Expr.Def(newSym, subst(tpe), loc)

    case LoweredAst.Expr.Sig(sym, tpe, loc) =>
      val newSym = specializeSigSym(sym, subst(tpe))
      MonoAst.Expr.Def(newSym, subst(tpe), loc)

    case LoweredAst.Expr.Cst(cst, tpe, loc) =>
      MonoAst.Expr.Cst(cst, subst(tpe), loc)

    case LoweredAst.Expr.Lambda(fparam, exp, tpe, loc) =>
      val (p, env1) = specializeFormalParam(fparam, subst)
      val e = visitExp(exp, env0 ++ env1, subst)
      MonoAst.Expr.Lambda(p, e, subst(tpe), loc)

    case LoweredAst.Expr.Apply(exp, exps, tpe, eff, loc) =>
      val e = visitExp(exp, env0, subst)
      val es = exps.map(visitExp(_, env0, subst))
      MonoAst.Expr.Apply(e, es, subst(tpe), subst(eff), loc)

    case LoweredAst.Expr.ApplyDef(sym, exps, itpe, tpe, eff, loc2) =>
      val ft = subst(itpe)
      val newSym = specializeDefSym(sym, ft)
      val es = exps.map(visitExp(_, env0, subst))
      MonoAst.Expr.ApplyDef(newSym, es, ft, subst(tpe), subst(eff), loc2)

    case LoweredAst.Expr.ApplyAtomic(op, exps, tpe, eff, loc) =>
      val es = exps.map(visitExp(_, env0, subst))
      MonoAst.Expr.ApplyAtomic(op, es, subst(tpe), subst(eff), loc)

    case LoweredAst.Expr.Let(sym, mod, exp1, exp2, tpe, eff, loc) =>
      // Generate a fresh symbol for the let-bound variable.
      val freshSym = Symbol.freshVarSym(sym)
      val env1 = env0 + (sym -> freshSym)
      MonoAst.Expr.Let(freshSym, mod, visitExp(exp1, env0, subst), visitExp(exp2, env1, subst), subst(tpe), subst(eff), loc)

    case LoweredAst.Expr.LetRec(sym, mod, exp1, exp2, tpe, eff, loc) =>
      // Generate a fresh symbol for the let-bound variable.
      val freshSym = Symbol.freshVarSym(sym)
      val env1 = env0 + (sym -> freshSym)
      MonoAst.Expr.LetRec(freshSym, mod, visitExp(exp1, env1, subst), visitExp(exp2, env1, subst), subst(tpe), subst(eff), loc)

    case LoweredAst.Expr.Scope(sym, regionVar, exp, tpe, eff, loc) =>
      val freshSym = Symbol.freshVarSym(sym)
      val env1 = env0 + (sym -> freshSym)
      // forcedly mark the region variable as IO inside the region
      val subst1 = subst.unbind(regionVar.sym) + (regionVar.sym -> Type.IO)
      MonoAst.Expr.Scope(freshSym, regionVar, visitExp(exp, env1, subst1), subst(tpe), subst(eff), loc)

    case LoweredAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      val e1 = visitExp(exp1, env0, subst)
      val e2 = visitExp(exp2, env0, subst)
      val e3 = visitExp(exp3, env0, subst)
      MonoAst.Expr.IfThenElse(e1, e2, e3, subst(tpe), subst(eff), loc)

    case LoweredAst.Expr.Stm(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1, env0, subst)
      val e2 = visitExp(exp2, env0, subst)
      MonoAst.Expr.Stm(e1, e2, subst(tpe), subst(eff), loc)

    case LoweredAst.Expr.Discard(exp, eff, loc) =>
      val e = visitExp(exp, env0, subst)
      MonoAst.Expr.Discard(e, subst(eff), loc)

    case LoweredAst.Expr.Match(exp, rules, tpe, eff, loc) =>
      val rs = rules map {
        case LoweredAst.MatchRule(pat, guard, body) =>
          val (p, env1) = visitPat(pat, subst)
          val extendedEnv = env0 ++ env1
          val g = guard.map(visitExp(_, extendedEnv, subst))
          val b = visitExp(body, extendedEnv, subst)
          MonoAst.MatchRule(p, g, b)
      }
      MonoAst.Expr.Match(visitExp(exp, env0, subst), rs, subst(tpe), subst(eff), loc)

    case LoweredAst.Expr.TypeMatch(exp, rules, tpe, _, loc) =>
      // use the non-strict substitution
      // to allow free type variables to match with anything
      val expTpe = subst.nonStrict(exp.tpe)
      // make the tvars in `exp`'s type rigid
      // so that Nil: List[x%123] can only match List[_]
      val renv = expTpe.typeVars.foldLeft(RigidityEnv.empty) {
        case (acc, Type.Var(sym, _)) => acc.markRigid(sym)
      }
      ListOps.findMap(rules) {
        case LoweredAst.TypeMatchRule(sym, t, body0) =>
          // try to unify
          Unification.unifyTypes(expTpe, subst.nonStrict(t), renv) match {
            // Case 1: types don't unify; just continue
            case Result.Err(_) => None
            // Case 2: types unify; use the substitution in the body
            case Result.Ok((caseSubst, econstrs)) => // TODO ASSOC-TYPES consider econstrs
              // visit the base expression under the initial environment
              val e = visitExp(exp, env0, subst)
              // Generate a fresh symbol for the let-bound variable.
              val freshSym = Symbol.freshVarSym(sym)
              val env1 = env0 + (sym -> freshSym)
              val subst1 = caseSubst @@ subst.nonStrict
              // visit the body under the extended environment
              val body = visitExp(body0, env1, StrictSubstitution(subst1, root.eqEnv))
              val eff = Type.mkUnion(e.eff, body.eff, loc.asSynthetic)
              Some(MonoAst.Expr.Let(freshSym, Modifiers.Empty, e, body, StrictSubstitution(subst1, root.eqEnv).apply(tpe), subst1(eff), loc))
          }
      }.get // We are safe to call get because the last case will always match

    case LoweredAst.Expr.VectorLit(exps, tpe, eff, loc) =>
      val es = exps.map(visitExp(_, env0, subst))
      MonoAst.Expr.VectorLit(es, subst(tpe), subst(eff), loc)

    case LoweredAst.Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1, env0, subst)
      val e2 = visitExp(exp2, env0, subst)
      MonoAst.Expr.VectorLoad(e1, e2, subst(tpe), subst(eff), loc)

    case LoweredAst.Expr.VectorLength(exp, loc) =>
      val e = visitExp(exp, env0, subst)
      MonoAst.Expr.VectorLength(e, loc)

    case LoweredAst.Expr.Ascribe(exp, tpe, eff, loc) =>
      val e = visitExp(exp, env0, subst)
      MonoAst.Expr.Ascribe(e, subst(tpe), subst(eff), loc)

    case LoweredAst.Expr.Cast(exp, _, _, tpe, eff, loc) =>
      // We drop the declaredType and declaredEff here.
      val e = visitExp(exp, env0, subst)
      MonoAst.Expr.Cast(e, None, None, subst(tpe), subst(eff), loc)

    case LoweredAst.Expr.TryCatch(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp, env0, subst)
      val rs = rules map {
        case LoweredAst.CatchRule(sym, clazz, body) =>
          // Generate a fresh symbol.
          val freshSym = Symbol.freshVarSym(sym)
          val env1 = env0 + (sym -> freshSym)
          val b = visitExp(body, env1, subst)
          MonoAst.CatchRule(freshSym, clazz, b)
      }
      MonoAst.Expr.TryCatch(e, rs, subst(tpe), subst(eff), loc)

    case LoweredAst.Expr.TryWith(exp, effect, rules, tpe, eff, loc) =>
      val e = visitExp(exp, env0, subst)
      val rs = rules map {
        case LoweredAst.HandlerRule(op, fparams0, body0) =>
          val (fparams, fparamEnv) = specializeFormalParams(fparams0, subst)
          val env1 = env0 ++ fparamEnv
          val body = visitExp(body0, env1, subst)
          MonoAst.HandlerRule(op, fparams, body)
      }
      MonoAst.Expr.TryWith(e, effect, rs, subst(tpe), subst(eff), loc)

    case LoweredAst.Expr.Do(op, exps, tpe, eff, loc) =>
      val es = exps.map(visitExp(_, env0, subst))
      MonoAst.Expr.Do(op, es, subst(tpe), subst(eff), loc)

    case LoweredAst.Expr.NewObject(name, clazz, tpe, eff, methods0, loc) =>
      val methods = methods0.map(visitJvmMethod(_, env0, subst))
      MonoAst.Expr.NewObject(name, clazz, subst(tpe), subst(eff), methods, loc)

  }

  /**
    * Specializes the given pattern `p0` w.r.t. the current substitution.
    *
    * Returns the new pattern and a mapping from variable symbols to fresh variable symbols.
    */
  private def visitPat(p0: LoweredAst.Pattern, subst: StrictSubstitution)(implicit flix: Flix): (MonoAst.Pattern, Map[Symbol.VarSym, Symbol.VarSym]) = p0 match {
    case LoweredAst.Pattern.Wild(tpe, loc) => (MonoAst.Pattern.Wild(subst(tpe), loc), Map.empty)
    case LoweredAst.Pattern.Var(sym, tpe, loc) =>
      // Generate a fresh variable symbol for the pattern-bound variable.
      val freshSym = Symbol.freshVarSym(sym)
      (MonoAst.Pattern.Var(freshSym, subst(tpe), loc), Map(sym -> freshSym))
    case LoweredAst.Pattern.Cst(cst, tpe, loc) => (MonoAst.Pattern.Cst(cst, subst(tpe), loc), Map.empty)
    case LoweredAst.Pattern.Tag(sym, pat, tpe, loc) =>
      val (p, env1) = visitPat(pat, subst)
      (MonoAst.Pattern.Tag(sym, p, subst(tpe), loc), env1)
    case LoweredAst.Pattern.Tuple(elms, tpe, loc) =>
      val (ps, envs) = elms.map(visitPat(_, subst)).unzip
      (MonoAst.Pattern.Tuple(ps, subst(tpe), loc), envs.reduce(_ ++ _))
    case LoweredAst.Pattern.Record(pats, pat, tpe, loc) =>
      val (ps, envs) = pats.map {
        case LoweredAst.Pattern.Record.RecordLabelPattern(label, tpe1, pat1, loc1) =>
          val (p1, env1) = visitPat(pat1, subst)
          (MonoAst.Pattern.Record.RecordLabelPattern(label, subst(tpe1), p1, loc1), env1)
      }.unzip
      val (p, env1) = visitPat(pat, subst)
      val finalEnv = env1 :: envs
      (MonoAst.Pattern.Record(ps, p, subst(tpe), loc), finalEnv.reduce(_ ++ _))
    case LoweredAst.Pattern.RecordEmpty(tpe, loc) => (MonoAst.Pattern.RecordEmpty(subst(tpe), loc), Map.empty)
  }

  /**
    * Specializes the given method `method` w.r.t. the current substitution.
    *
    * Returns the new method.
    */
  private def visitJvmMethod(method: LoweredAst.JvmMethod, env0: Map[Symbol.VarSym, Symbol.VarSym], subst: StrictSubstitution)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): MonoAst.JvmMethod = method match {
    case LoweredAst.JvmMethod(ident, fparams0, exp0, tpe, eff, loc) =>
      val (fparams, env1) = specializeFormalParams(fparams0, subst)
      val exp = visitExp(exp0, env0 ++ env1, subst)
      MonoAst.JvmMethod(ident, fparams, exp, subst(tpe), subst(eff), loc)
  }

  /**
    * Returns the def symbol corresponding to the specialized symbol `sym` w.r.t. to the type `tpe`.
    *
    * The given type must be a normalized type.
    */
  private def specializeDefSym(sym: Symbol.DefnSym, tpe: Type)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): Symbol.DefnSym = {
    // Lookup the definition and its declared type.
    val defn = root.defs(sym)

    // Check if the function is non-polymorphic.
    if (defn.spec.tparams.isEmpty) {
      defn.sym
    } else {
      specializeDef(defn, tpe)
    }
  }

  /**
    * Returns the def symbol corresponding to the specialized symbol `sym` w.r.t. to the type `tpe`.
    *
    * The given type must be a normalized type.
    */
  private def specializeSigSym(sym: Symbol.SigSym, tpe: Type)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): Symbol.DefnSym = {
    val sig = root.sigs(sym)

    // lookup the instance corresponding to this type
    val instances = root.instances(sig.sym.trt)

    val defns = instances.flatMap {
      inst =>
        inst.defs.find {
          defn =>
            defn.sym.text == sig.sym.name && Unification.unifiesWith(defn.spec.declaredScheme.base, tpe, RigidityEnv.empty, root.eqEnv)
        }
    }

    (sig.exp, defns) match {
      // Case 1: An instance implementation exists. Use it.
      case (_, defn :: Nil) => specializeDef(defn, tpe)
      // Case 2: No instance implementation, but a default implementation exists. Use it.
      case (Some(impl), Nil) => specializeDef(sigToDef(sig.sym, sig.spec, impl), tpe)
      // Case 3: Multiple matching defs. Should have been caught previously.
      case (_, _ :: _ :: _) => throw InternalCompilerException(s"Expected at most one matching definition for '$sym', but found ${defns.size} signatures.", sym.loc)
      // Case 4: No matching defs and no default. Should have been caught previously.
      case (None, Nil) => throw InternalCompilerException(s"No default or matching definition found for '$sym'.", sym.loc)
    }
  }

  /**
    * Converts a signature with an implementation into the equivalent definition.
    */
  private def sigToDef(sigSym: Symbol.SigSym, spec: LoweredAst.Spec, exp: LoweredAst.Expr): LoweredAst.Def = {
    LoweredAst.Def(sigSymToDefnSym(sigSym), spec, exp)
  }

  /**
    * Converts a SigSym into the equivalent DefnSym.
    */
  private def sigSymToDefnSym(sigSym: Symbol.SigSym): Symbol.DefnSym = {
    val ns = sigSym.trt.namespace :+ sigSym.trt.name
    new Symbol.DefnSym(None, ns, sigSym.name, sigSym.loc)
  }

  /**
    * Returns the def symbol corresponding to the specialized def `defn` w.r.t. to the type `tpe`.
    *
    * The given type must be a normalized type.
    */
  private def specializeDef(defn: LoweredAst.Def, tpe: Type)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): Symbol.DefnSym = {
    // Unify the declared and actual type to obtain the substitution map.
    val subst = infallibleUnify(defn.spec.declaredScheme.base, tpe, defn.sym)

    // Check whether the function definition has already been specialized.
    ctx synchronized {
      ctx.getDef2Def(defn.sym, tpe) match {
        case None =>
          // Case 1: The function has not been specialized.
          // Generate a fresh specialized definition symbol.
          val freshSym = Symbol.freshDefnSym(defn.sym)

          // Register the fresh symbol (and actual type) in the symbol2symbol map.
          ctx.putDef2Def(defn.sym, tpe, freshSym)

          // Enqueue the fresh symbol with the definition and substitution.
          ctx.enqueue(freshSym, defn, subst)

          // Now simply refer to the freshly generated symbol.
          freshSym
        case Some(specializedSym) =>
          // Case 2: The function has already been specialized.
          // Simply refer to the already existing specialized symbol.
          specializedSym
      }
    }

  }

  /**
    * Specializes the given formal parameters `fparams0` w.r.t. the given substitution `subst0`.
    *
    * Returns the new formal parameters and an environment mapping the variable symbol for each parameter to a fresh symbol.
    */
  private def specializeFormalParams(fparams0: List[LoweredAst.FormalParam], subst0: StrictSubstitution)(implicit flix: Flix): (List[MonoAst.FormalParam], Map[Symbol.VarSym, Symbol.VarSym]) = {
    // Return early if there are no formal parameters.
    if (fparams0.isEmpty)
      return (Nil, Map.empty)

    // Specialize each formal parameter and recombine the results.
    val (params, envs) = fparams0.map(p => specializeFormalParam(p, subst0)).unzip
    (params, envs.reduce(_ ++ _))
  }

  /**
    * Specializes the given formal parameter `fparam0` w.r.t. the given substitution `subst0`.
    *
    * Returns the new formal parameter and an environment mapping the variable symbol to a fresh variable symbol.
    */
  private def specializeFormalParam(fparam0: LoweredAst.FormalParam, subst0: StrictSubstitution)(implicit flix: Flix): (MonoAst.FormalParam, Map[Symbol.VarSym, Symbol.VarSym]) = {
    val LoweredAst.FormalParam(sym, mod, tpe, src, loc) = fparam0
    val freshSym = Symbol.freshVarSym(sym)
    (MonoAst.FormalParam(freshSym, mod, subst0(tpe), src, loc), Map(sym -> freshSym))
  }

  /**
    * Unifies `tpe1` and `tpe2` which must be unifiable.
    */
  private def infallibleUnify(tpe1: Type, tpe2: Type, sym: Symbol.DefnSym)(implicit root: LoweredAst.Root, flix: Flix): StrictSubstitution = {
    Unification.unifyTypes(tpe1, tpe2, RigidityEnv.empty) match {
      case Result.Ok((subst, econstrs)) => // TODO ASSOC-TYPES consider econstrs
        StrictSubstitution(subst, root.eqEnv)
      case Result.Err(_) =>
        throw InternalCompilerException(s"Unable to unify: '$tpe1' and '$tpe2'.\nIn '${sym}'", tpe1.loc)
    }
  }

}
