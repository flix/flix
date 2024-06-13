/*
 * Copyright 2023 Magnus Madsen
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
package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.Ast.Constant
import ca.uwaterloo.flix.language.ast.{Ast, Kind, SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.language.ast.TypedAst.{Def, Expr, Predicate, Root, Spec}
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.language.phase.typer.TypeConstraint

import scala.collection.concurrent.TrieMap
import scala.collection.immutable.SortedSet

object Summary {

  private val effVarCount: TrieMap[Symbol, Int] = TrieMap.empty

  def addEffCount(sym: Symbol, tc: List[TypeConstraint]): Unit = {
    if (effVarCount.contains(sym)) ??? // shouldn't happen
    else effVarCount.update(sym, effVars(tc).size)
  }

  private def combine(l: List[SortedSet[Type.Var]]): SortedSet[Type.Var] = {
    if (l.isEmpty) SortedSet.empty
    else l.reduce((s1, s2) => s1.union(s2))
  }

  private def effVars(tcs: List[TypeConstraint]): SortedSet[Type.Var] = {
    combine(tcs.map(effVars))
  }

  private def effVars(tc: TypeConstraint): SortedSet[Type.Var] = tc match {
    case TypeConstraint.Equality(tpe1, tpe2, _) =>
      effVars(tpe1).union(effVars(tpe2))
    case TypeConstraint.EqJvmConstructor(mvar, _, tpes, _) =>
      effVars(mvar).union(combine(tpes.map(effVars)))
    case TypeConstraint.EqJvmMethod(mvar, tpe0, _, tpes, _) =>
      effVars(mvar).union(effVars(tpe0)).union(combine(tpes.map(effVars)))
    case TypeConstraint.Trait(_, tpe, _) =>
      effVars(tpe)
    case TypeConstraint.Purification(_, eff1, eff2, _, nested) =>
      effVars(eff1).union(effVars(eff2)).union(effVars(nested))
  }

  /**
    * OBS: not counting associated types as variables.
    */
  private def effVars(tpe: Type): SortedSet[Type.Var] = tpe match {
    case v@Type.Var(_, _) if tpe.kind == Kind.Eff =>
      SortedSet(v)
    case Type.Var(_, _) =>
      SortedSet.empty
    case Type.Cst(_, _) =>
      SortedSet.empty
    case Type.Apply(tpe1, tpe2, _) =>
      effVars(tpe1).union(effVars(tpe2))
    case Type.Alias(_, args, tpe, _) =>
      combine(args.map(effVars)).union(effVars(tpe))
    case Type.AssocType(_, arg, _, _) =>
      effVars(arg)
  }

  /**
    * The column separator.
    */
  private val Separator = " & "

  /**
    * The end of line separator.
    */
  private val EndOfLine = " \\\\"

  /**
    * The width of the module column.
    */
  private val ModWidth = 35

  /**
    * The width of every other column.
    */
  private val ColWidth = 12

  private val fillerExpr = Expr.Cst(Constant.Unit, Type.Unit, SourceLocation.Unknown)

  /**
    * Returns `true` if the given module `mod` should be printed.
    */
  private def include(mod: String, lines: Int): Boolean =
    lines >= 125

  def printSummary(v: Validation[Root, CompilationMessage]): Unit = mapN(v) {
    case root =>

      print(padR("Module", ModWidth))
      print(Separator)
      print(padL("Lines", ColWidth))
      print(Separator)
      print(padL("Functions", ColWidth))
      print(Separator)
      print(padL("Pure", ColWidth))
      print(Separator)
      print(padL("IO", ColWidth))
      print(Separator)
      print(padL("Polymorphic", ColWidth))
      print(Separator)
      print(padL("Lambdas", ColWidth))
      print(Separator)
      print(padL("Eff. Vars.", ColWidth))
      println(EndOfLine)

      var totalLines = 0
      var totalFunctions = 0
      var totalPureFunctions = 0
      var totalUnivFunctions = 0
      var totalEffPolymorphicFunctions = 0
      var totalLambdas = 0
      var totalEffVars = 0
      var totalCasts = empty

      for ((source, loc) <- root.sources.toList.sortBy(_._1.name)) {
        val module = source.name
        val defs = getFunctions(source, root) ++
          getTraitFunctions(source, root) ++
          getInstanceFunctions(source, root)

        val numberOfLines = loc.endLine
        val numberOfFunctions = defs.size
        val numberOfPureFunctions = defs.count(defn => isPure(defn._2))
        val numberOfIOFunctions = defs.count(defn => isIO(defn._2))
        val numberOfEffectPolymorphicFunctions = defs.count(defn => isEffectPolymorphic(defn._2))
        val numberOfLambdas = defs.map(defn => lambdaCount(defn._3)).sum
        val numberOfEffVars = defs.map(defn => effVarCount.getOrElse(defn._1, -1)).sum
        val numberOfCasts = castCount(defs.map(_._3).toList)

        totalLines = totalLines + numberOfLines
        totalFunctions = totalFunctions + numberOfFunctions
        totalPureFunctions = totalPureFunctions + numberOfPureFunctions
        totalUnivFunctions = totalUnivFunctions + numberOfIOFunctions
        totalEffPolymorphicFunctions = totalEffPolymorphicFunctions + numberOfEffectPolymorphicFunctions
        totalLambdas = totalLambdas + numberOfLambdas
        totalEffVars = totalEffVars + numberOfEffVars
        totalCasts = combine(totalCasts, numberOfCasts)

        if (include(module, numberOfLines)) {
          print(padR(module, ModWidth))
          print(Separator)
          print(padL(format(numberOfLines), ColWidth))
          print(Separator)
          print(padL(format(numberOfFunctions), ColWidth))
          print(Separator)
          print(padL(format(numberOfPureFunctions), ColWidth))
          print(Separator)
          print(padL(format(numberOfIOFunctions), ColWidth))
          print(Separator)
          print(padL(format(numberOfEffectPolymorphicFunctions), ColWidth))
          print(Separator)
          print(padL(format(numberOfLambdas), ColWidth))
          print(Separator)
          print(padL(format(numberOfEffVars), ColWidth))
          println(EndOfLine)
        }
      }

      println("---")
      print(padR("Totals (incl. filtered)", ModWidth))
      print(Separator)
      print(padL(format(totalLines), ColWidth))
      print(Separator)
      print(padL(format(totalFunctions), ColWidth))
      print(Separator)
      print(padL(format(totalPureFunctions), ColWidth))
      print(Separator)
      print(padL(format(totalUnivFunctions), ColWidth))
      print(Separator)
      print(padL(format(totalEffPolymorphicFunctions), ColWidth))
      print(Separator)
      print(padL(format(totalLambdas), ColWidth))
      print(Separator)
      print(padL(format(totalEffVars), ColWidth))
      println(EndOfLine)

      println()
      printCastCountSummary(totalLines, totalCasts)
  }

  private def printCastCountSummary(totalLines: Int, totalCasts: Casts): Unit = {
    print(padL("Lines", ColWidth))
    print(Separator)
    print(padL("Casts", ColWidth))
    print(Separator)
    print(padL("Type Upcast", ColWidth))
    print(Separator)
    print(padL("Type Downcast", ColWidth))
    print(Separator)
    print(padL("Eff. Upcast", ColWidth))
    print(Separator)
    print(padL("Eff. Downcast", ColWidth))
    println(EndOfLine)


    val sumOfCasts = total(totalCasts)
    print(padL(format(totalLines), ColWidth))
    print(Separator)
    print(padL(format(sumOfCasts), ColWidth))
    print(Separator)
    print(padL(s"${format(totalCasts.typeUpcast)} ${formatOfSum(totalCasts.typeUpcast, sumOfCasts)}", ColWidth))
    print(Separator)
    print(padL(s"${format(totalCasts.typeDowncast)} ${formatOfSum(totalCasts.typeDowncast, sumOfCasts)}", ColWidth))
    print(Separator)
    print(padL(s"${format(totalCasts.effectUpcast)} ${formatOfSum(totalCasts.effectUpcast, sumOfCasts)}", ColWidth))
    print(Separator)
    print(padL(s"${format(totalCasts.effectDowncast)} ${formatOfSum(totalCasts.effectDowncast, sumOfCasts)}", ColWidth))
    println(EndOfLine)
  }

  /**
    * Returns the [[Spec]] of all top-level functions in the given `source`.
    */
  private def getFunctions(source: Ast.Source, root: Root): Iterable[(Symbol, Spec, Expr)] =
    root.defs.collect {
      case (sym, defn) if sym.loc.source == source => (defn.sym, defn.spec, defn.exp)
    }

  /**
    * Returns the [[Spec]] of all trait functions in the given `source`.
    *
    * Note: This means signatures that have an implementation (i.e. body expression).
    */
  private def getTraitFunctions(source: Ast.Source, root: Root): Iterable[(Symbol, Spec, Expr)] =
    root.traits.collect {
      case (sym, trt) if sym.loc.source == source =>
        trt.sigs.collect {
          case sig if sig.exp.nonEmpty =>
            (sig.sym, sig.spec, sig.exp.getOrElse(fillerExpr))
        }
    }.flatten

  /**
    * Returns the [[Spec]] of all instance functions in the given `source`.
    */
  private def getInstanceFunctions(source: Ast.Source, root: Root): Iterable[(Symbol, Spec, Expr)] =
    root.instances.values.flatMap {
      case instances => instances.filter(_.loc.source == source).flatMap(_.defs).map(defn => (defn.sym, defn.spec, defn.exp))
    }

  /**
    * Formats the given number `n`.
    */
  private def format(n: Int): String = f"$n%,d".replace(".", ",")

  private def formatOfSum(before: Int, sum: Int): String = {
    val percentage = if (sum == 0) "inf" else f"${100 * before / (1.0 * sum)}%2.0f"
    s"($percentage \\%)"
  }

  /**
    * Returns `true` if the given `spec` is pure.
    */
  private def isPure(spec: Spec): Boolean = spec.eff == Type.Pure

  /**
    * Returns `true` if the given `spec` has the top effect.
    */
  private def isIO(spec: Spec): Boolean = spec.eff == Type.IO

  /**
    * Returns `true` if the given `spec` is effect polymorphic (neither pure or impure).
    */
  private def isEffectPolymorphic(spec: Spec): Boolean = !isPure(spec) && !isIO(spec)

  /**
    * Right-pads the given string `s` to length `l`.
    */
  private def padR(s: String, l: Int): String = s.padTo(l, ' ')

  /**
    * Left-pads the given string `s` to length `l`.
    */
  private def padL(s: String, l: Int): String = {
    if (s.length >= l) {
      return s
    }
    val sb = new StringBuilder
    while (sb.length < l - s.length) {
      sb.append(' ')
    }
    sb.append(s)
    sb.toString
  }

  /**
    * Returns the number of syntactic lambdas in the function body.
    *
    * OBS: newObject are not counted as a lambda.
    */
  private def lambdaCount(e: Expr): Int = e match {
    case Expr.Cst(cst, tpe, loc) => 0
    case Expr.Var(sym, tpe, loc) => 0
    case Expr.Def(sym, tpe, loc) => 0
    case Expr.Sig(sym, tpe, loc) => 0
    case Expr.Hole(sym, tpe, loc) => -0
    case Expr.HoleWithExp(exp, tpe, eff, loc) => lambdaCount(exp)
    case Expr.OpenAs(symUse, exp, tpe, loc) => lambdaCount(exp)
    case Expr.Use(sym, alias, exp, loc) => lambdaCount(exp)
    case Expr.Lambda(fparam, exp, tpe, loc) => 1 + lambdaCount(exp)
    case Expr.Apply(exp, exps, tpe, eff, loc) => (exp :: exps).map(lambdaCount).sum
    case Expr.Unary(sop, exp, tpe, eff, loc) => lambdaCount(exp)
    case Expr.Binary(sop, exp1, exp2, tpe, eff, loc) => lambdaCount(exp1) + lambdaCount(exp2)
    case Expr.Let(sym, mod, exp1, exp2, tpe, eff, loc) => lambdaCount(exp1) + lambdaCount(exp2)
    case Expr.LetRec(sym, ann, mod, exp1, exp2, tpe, eff, loc) => lambdaCount(exp1) + lambdaCount(exp2)
    case Expr.Region(tpe, loc) => 0
    case Expr.Scope(sym, regionVar, exp, tpe, eff, loc) => lambdaCount(exp)
    case Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) => lambdaCount(exp1) + lambdaCount(exp2) + lambdaCount(exp3)
    case Expr.Stm(exp1, exp2, tpe, eff, loc) => lambdaCount(exp1) + lambdaCount(exp2)
    case Expr.Discard(exp, eff, loc) => lambdaCount(exp)
    case Expr.Match(exp, rules, tpe, eff, loc) => (exp :: rules.flatMap(r => r.exp :: r.guard.toList)).map(lambdaCount).sum
    case Expr.TypeMatch(exp, rules, tpe, eff, loc) => (exp :: rules.map(_.exp)).map(lambdaCount).sum
    case Expr.RestrictableChoose(star, exp, rules, tpe, eff, loc) => (exp :: rules.map(_.exp)).map(lambdaCount).sum
    case Expr.Tag(sym, exp, tpe, eff, loc) => lambdaCount(exp)
    case Expr.RestrictableTag(sym, exp, tpe, eff, loc) => lambdaCount(exp)
    case Expr.Tuple(elms, tpe, eff, loc) => elms.map(lambdaCount).sum
    case Expr.RecordEmpty(tpe, loc) => 0
    case Expr.RecordSelect(exp, label, tpe, eff, loc) => lambdaCount(exp)
    case Expr.RecordExtend(label, exp1, exp2, tpe, eff, loc) => lambdaCount(exp1) + lambdaCount(exp2)
    case Expr.RecordRestrict(label, exp, tpe, eff, loc) => lambdaCount(exp)
    case Expr.ArrayLit(exps, exp, tpe, eff, loc) => (exp :: exps).map(lambdaCount).sum
    case Expr.ArrayNew(exp1, exp2, exp3, tpe, eff, loc) => lambdaCount(exp1) + lambdaCount(exp2) + lambdaCount(exp3)
    case Expr.ArrayLoad(exp1, exp2, tpe, eff, loc) => lambdaCount(exp1) + lambdaCount(exp2)
    case Expr.ArrayLength(exp, eff, loc) => lambdaCount(exp)
    case Expr.ArrayStore(exp1, exp2, exp3, eff, loc) => lambdaCount(exp1) + lambdaCount(exp2) + lambdaCount(exp3)
    case Expr.VectorLit(exps, tpe, eff, loc) => exps.map(lambdaCount).sum
    case Expr.VectorLoad(exp1, exp2, tpe, eff, loc) => lambdaCount(exp1) + lambdaCount(exp2)
    case Expr.VectorLength(exp, loc) => lambdaCount(exp)
    case Expr.Ref(exp1, exp2, tpe, eff, loc) => lambdaCount(exp1) + lambdaCount(exp2)
    case Expr.Deref(exp, tpe, eff, loc) => lambdaCount(exp)
    case Expr.Assign(exp1, exp2, tpe, eff, loc) => lambdaCount(exp1) + lambdaCount(exp2)
    case Expr.Ascribe(exp, tpe, eff, loc) => lambdaCount(exp)
    case Expr.InstanceOf(exp, clazz, loc) => lambdaCount(exp)
    case Expr.CheckedCast(cast, exp, tpe, eff, loc) => lambdaCount(exp)
    case Expr.UncheckedCast(exp, declaredType, declaredEff, tpe, eff, loc) => lambdaCount(exp)
    case Expr.UncheckedMaskingCast(exp, tpe, eff, loc) => lambdaCount(exp)
    case Expr.Without(exp, effUse, tpe, eff, loc) => lambdaCount(exp)
    case Expr.TryCatch(exp, rules, tpe, eff, loc) => (exp :: rules.map(_.exp)).map(lambdaCount).sum
    case Expr.TryWith(exp, effUse, rules, tpe, eff, loc) => (exp :: rules.map(_.exp)).map(lambdaCount).sum
    case Expr.Do(op, exps, tpe, eff, loc) => exps.map(lambdaCount).sum
    case Expr.InvokeConstructor(constructor, exps, tpe, eff, loc) => exps.map(lambdaCount).sum
    case Expr.InvokeMethod(method, exp, exps, tpe, eff, loc) => (exp :: exps).map(lambdaCount).sum
    case Expr.InvokeStaticMethod(method, exps, tpe, eff, loc) => exps.map(lambdaCount).sum
    case Expr.GetField(field, exp, tpe, eff, loc) => lambdaCount(exp)
    case Expr.PutField(field, exp1, exp2, tpe, eff, loc) => lambdaCount(exp1) + lambdaCount(exp2)
    case Expr.GetStaticField(field, tpe, eff, loc) => 0
    case Expr.PutStaticField(field, exp, tpe, eff, loc) => lambdaCount(exp)
    case Expr.NewObject(name, clazz, tpe, eff, methods, loc) => methods.map(_.exp).map(lambdaCount).sum
    case Expr.NewChannel(exp1, exp2, tpe, eff, loc) => lambdaCount(exp1) + lambdaCount(exp2)
    case Expr.GetChannel(exp, tpe, eff, loc) => lambdaCount(exp)
    case Expr.PutChannel(exp1, exp2, tpe, eff, loc) => lambdaCount(exp1) + lambdaCount(exp2)
    case Expr.SelectChannel(rules, default, tpe, eff, loc) => (default.toList ++ rules.flatMap(r => List(r.exp, r.chan))).map(lambdaCount).sum
    case Expr.Spawn(exp1, exp2, tpe, eff, loc) => lambdaCount(exp1) + lambdaCount(exp2)
    case Expr.ParYield(frags, exp, tpe, eff, loc) => (exp :: frags.map(_.exp)).map(lambdaCount).sum
    case Expr.Lazy(exp, tpe, loc) => lambdaCount(exp)
    case Expr.Force(exp, tpe, eff, loc) => lambdaCount(exp)
    case Expr.FixpointConstraintSet(cs, tpe, loc) => cs.flatMap(_.head match {
      case Predicate.Head.Atom(pred, den, terms, tpe, loc) => terms
    }).map(lambdaCount).sum
    case Expr.FixpointLambda(pparams, exp, tpe, eff, loc) => lambdaCount(exp)
    case Expr.FixpointMerge(exp1, exp2, tpe, eff, loc) => lambdaCount(exp1) + lambdaCount(exp2)
    case Expr.FixpointSolve(exp, tpe, eff, loc) => lambdaCount(exp)
    case Expr.FixpointFilter(pred, exp, tpe, eff, loc) => lambdaCount(exp)
    case Expr.FixpointInject(exp, pred, tpe, eff, loc) => lambdaCount(exp)
    case Expr.FixpointProject(pred, exp, tpe, eff, loc) => lambdaCount(exp)
    case Expr.Error(m, tpe, eff) => 0
  }

  private sealed case class Casts(typeUpcast: Int, typeDowncast: Int, effectUpcast: Int, effectDowncast: Int)

  private val empty: Casts = Casts(0, 0, 0, 0)
  private val typeUpcast: Casts = Casts(1, 0, 0, 0)
  private val typeDowncast: Casts = Casts(0, 1, 0, 0)
  private val effectUpcast: Casts = Casts(0, 0, 1, 0)
  private val effectDowncast: Casts = Casts(0, 0, 0, 1)

  private def total(c: Casts): Int = c.typeUpcast + c.typeDowncast + c.effectUpcast + c.effectDowncast

  private def combine(c1: Casts, c2: Casts): Casts = {
    val Casts(tu1, td1, eu1, ed1) = c1
    val Casts(tu2, td2, eu2, ed2) = c2
    Casts(tu1 + tu2, td1 + td2, eu1 + eu2, ed1 + ed2)
  }


  private def castCount(l: List[Expr]): Casts = {
    l.map(castCount).foldLeft(empty)(combine)
  }

  private def castCount(e1: Expr, e2: Expr): Casts = {
    combine(castCount(e1), castCount(e2))
  }

  private def castCount(e1: Expr, e2: Expr, e3: Expr): Casts = {
    combine(combine(castCount(e1), castCount(e2)), castCount(e3))
  }

  private def castCount(e: Expr): Casts = e match {
    case Expr.CheckedCast(Ast.CheckedCastType.TypeCast, exp, tpe, eff, loc) => combine(typeUpcast, castCount(exp))
    case Expr.CheckedCast(Ast.CheckedCastType.EffectCast, exp, tpe, eff, loc) => combine(effectUpcast, castCount(exp))
    case Expr.UncheckedCast(exp, declaredType, declaredEff, tpe, eff, loc) if jvmThing(exp) => empty
    case Expr.UncheckedCast(exp, None, Some(_), tpe, eff, loc) => combine(effectDowncast, castCount(exp))
    case Expr.UncheckedCast(exp, Some(_), None, tpe, eff, loc) => combine(typeDowncast, castCount(exp))
    case Expr.UncheckedCast(exp, None, None, tpe, eff, loc) => ???
    case Expr.UncheckedCast(exp, Some(_), Some(_), tpe, eff, loc) => ???

    case Expr.Cst(cst, tpe, loc) => empty
    case Expr.Var(sym, tpe, loc) => empty
    case Expr.Def(sym, tpe, loc) => empty
    case Expr.Sig(sym, tpe, loc) => empty
    case Expr.Hole(sym, tpe, loc) => empty
    case Expr.HoleWithExp(exp, tpe, eff, loc) => castCount(exp)
    case Expr.OpenAs(symUse, exp, tpe, loc) => castCount(exp)
    case Expr.Use(sym, alias, exp, loc) => castCount(exp)
    case Expr.Lambda(fparam, exp, tpe, loc) => castCount(exp)
    case Expr.Apply(exp, exps, tpe, eff, loc) => castCount(exp :: exps)
    case Expr.Unary(sop, exp, tpe, eff, loc) => castCount(exp)
    case Expr.Binary(sop, exp1, exp2, tpe, eff, loc) => castCount(exp1, exp2)
    case Expr.Let(sym, mod, exp1, exp2, tpe, eff, loc) => castCount(exp1, exp2)
    case Expr.LetRec(sym, ann, mod, exp1, exp2, tpe, eff, loc) => castCount(exp1, exp2)
    case Expr.Region(tpe, loc) => empty
    case Expr.Scope(sym, regionVar, exp, tpe, eff, loc) => castCount(exp)
    case Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) => castCount(List(exp1, exp2, exp3))
    case Expr.Stm(exp1, exp2, tpe, eff, loc) => castCount(exp1, exp2)
    case Expr.Discard(exp, eff, loc) => castCount(exp)
    case Expr.Match(exp, rules, tpe, eff, loc) => castCount(exp :: rules.flatMap(r => r.exp :: r.guard.toList))
    case Expr.TypeMatch(exp, rules, tpe, eff, loc) => castCount(exp :: rules.map(_.exp))
    case Expr.RestrictableChoose(star, exp, rules, tpe, eff, loc) => castCount(exp :: rules.map(_.exp))
    case Expr.Tag(sym, exp, tpe, eff, loc) => castCount(exp)
    case Expr.RestrictableTag(sym, exp, tpe, eff, loc) => castCount(exp)
    case Expr.Tuple(elms, tpe, eff, loc) => castCount(elms)
    case Expr.RecordEmpty(tpe, loc) => empty
    case Expr.RecordSelect(exp, label, tpe, eff, loc) => castCount(exp)
    case Expr.RecordExtend(label, exp1, exp2, tpe, eff, loc) => castCount(exp1, exp2)
    case Expr.RecordRestrict(label, exp, tpe, eff, loc) => castCount(exp)
    case Expr.ArrayLit(exps, exp, tpe, eff, loc) => castCount(exp :: exps)
    case Expr.ArrayNew(exp1, exp2, exp3, tpe, eff, loc) => castCount(exp1, exp2, exp3)
    case Expr.ArrayLoad(exp1, exp2, tpe, eff, loc) => castCount(exp1, exp2)
    case Expr.ArrayLength(exp, eff, loc) => castCount(exp)
    case Expr.ArrayStore(exp1, exp2, exp3, eff, loc) => castCount(exp1, exp2, exp3)
    case Expr.VectorLit(exps, tpe, eff, loc) => castCount(exps)
    case Expr.VectorLoad(exp1, exp2, tpe, eff, loc) => castCount(exp1, exp2)
    case Expr.VectorLength(exp, loc) => castCount(exp)
    case Expr.Ref(exp1, exp2, tpe, eff, loc) => castCount(exp1, exp2)
    case Expr.Deref(exp, tpe, eff, loc) => castCount(exp)
    case Expr.Assign(exp1, exp2, tpe, eff, loc) => castCount(exp1, exp2)
    case Expr.Ascribe(exp, tpe, eff, loc) => castCount(exp)
    case Expr.InstanceOf(exp, clazz, loc) => castCount(exp)
    case Expr.UncheckedMaskingCast(exp, tpe, eff, loc) => castCount(exp)
    case Expr.Without(exp, effUse, tpe, eff, loc) => castCount(exp)
    case Expr.TryCatch(exp, rules, tpe, eff, loc) => castCount(exp :: rules.map(_.exp))
    case Expr.TryWith(exp, effUse, rules, tpe, eff, loc) => castCount(exp :: rules.map(_.exp))
    case Expr.Do(op, exps, tpe, eff, loc) => castCount(exps)
    case Expr.InvokeConstructor(constructor, exps, tpe, eff, loc) => castCount(exps)
    case Expr.InvokeMethod(method, exp, exps, tpe, eff, loc) => castCount(exp :: exps)
    case Expr.InvokeStaticMethod(method, exps, tpe, eff, loc) => castCount(exps)
    case Expr.GetField(field, exp, tpe, eff, loc) => castCount(exp)
    case Expr.PutField(field, exp1, exp2, tpe, eff, loc) => castCount(exp1, exp2)
    case Expr.GetStaticField(field, tpe, eff, loc) => empty
    case Expr.PutStaticField(field, exp, tpe, eff, loc) => castCount(exp)
    case Expr.NewObject(name, clazz, tpe, eff, methods, loc) => castCount(methods.map(_.exp))
    case Expr.NewChannel(exp1, exp2, tpe, eff, loc) => castCount(exp1, exp2)
    case Expr.GetChannel(exp, tpe, eff, loc) => castCount(exp)
    case Expr.PutChannel(exp1, exp2, tpe, eff, loc) => castCount(exp1, exp2)
    case Expr.SelectChannel(rules, default, tpe, eff, loc) => castCount(default.toList ++ rules.flatMap(r => List(r.exp, r.chan)))
    case Expr.Spawn(exp1, exp2, tpe, eff, loc) => castCount(exp1, exp2)
    case Expr.ParYield(frags, exp, tpe, eff, loc) => castCount(exp :: frags.map(_.exp))
    case Expr.Lazy(exp, tpe, loc) => castCount(exp)
    case Expr.Force(exp, tpe, eff, loc) => castCount(exp)
    case Expr.FixpointConstraintSet(cs, tpe, loc) => castCount(cs.flatMap(_.head match {
      case Predicate.Head.Atom(pred, den, terms, tpe, loc) => terms
    }))
    case Expr.FixpointLambda(pparams, exp, tpe, eff, loc) => castCount(exp)
    case Expr.FixpointMerge(exp1, exp2, tpe, eff, loc) => castCount(exp1, exp2)
    case Expr.FixpointSolve(exp, tpe, eff, loc) => castCount(exp)
    case Expr.FixpointFilter(pred, exp, tpe, eff, loc) => castCount(exp)
    case Expr.FixpointInject(exp, pred, tpe, eff, loc) => castCount(exp)
    case Expr.FixpointProject(pred, exp, tpe, eff, loc) => castCount(exp)
    case Expr.Error(m, tpe, eff) => empty
  }

  private def jvmThing(e: Expr): Boolean = e match {
    case Expr.InvokeConstructor(constructor, exps, tpe, eff, loc) => true
    case Expr.InvokeMethod(method, exp, exps, tpe, eff, loc) => true
    case Expr.InvokeStaticMethod(method, exps, tpe, eff, loc) => true
    case Expr.GetField(field, exp, tpe, eff, loc) => true
    case Expr.PutField(field, exp1, exp2, tpe, eff, loc) => true
    case Expr.GetStaticField(field, tpe, eff, loc) => true
    case Expr.PutStaticField(field, exp, tpe, eff, loc) => true
    case _ => false
  }

}
