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
import ca.uwaterloo.flix.language.ast.TypedAst.{Expr, Predicate, Root, Spec}
import ca.uwaterloo.flix.language.ast.{Ast, Kind, SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.language.phase.typer.TypeConstraint
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

import scala.collection.concurrent.TrieMap
import scala.collection.immutable.SortedSet

object Summary {

  private val effVarCount: TrieMap[Symbol, (Int, Int)] = TrieMap.empty

  def addEffCount(sym: Symbol, tc: List[TypeConstraint]): Unit = {
    if (effVarCount.contains(sym)) ??? // shouldn't happen
    else {
      val effVarSet = effVars(tc)
      val (slackVars, nonSlackVars) = effVarSet.partition(_.sym.isSlack)
      effVarCount.update(sym, (nonSlackVars.size, slackVars.size))
    }
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
  private val ColWidth = 13

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
      print(padL("Eff. Poly", ColWidth))
      print(Separator)
      print(padL("Lambdas", ColWidth))
      print(Separator)
      print(padL("Eff. Vars.", ColWidth))
      print(Separator)
      print(padL("Slack Vars.", ColWidth))
      println(EndOfLine)

      var totalLines = 0
      var totalFunctions = 0
      var totalPureFunctions = 0
      var totalUnivFunctions = 0
      var totalEffPolymorphicFunctions = 0
      var totalLambdas = 0
      var totalEffVars = 0
      var totalSlackEffVars = 0

      for ((source, loc) <- root.sources.toList.sortBy(_._1.name)) {
        val module = source.name
        val defs = getFunctions(source, root) ++
          getTraitFunctions(source, root) ++
          getInstanceFunctions(source, root)

        if (defs.nonEmpty) {

          val numberOfLines = loc.endLine
          val numberOfFunctions = defs.size
          val numberOfPureFunctions = defs.count(defn => isPure(defn._2))
          val numberOfIOFunctions = defs.count(defn => isIO(defn._2))
          val numberOfEffectPolymorphicFunctions = defs.count(defn => isEffectPolymorphic(defn._2))
          val numberOfLambdas = defs.map(defn => lambdaCount(defn._3)).sum
          val (numberOfEffVars, numberOfSlackEffVars) = defs.map(defn => effVarCount.getOrElse(defn._1, (-1, -1))).reduce((x, y) => (x._1 + y._1, x._2 + y._2))

          totalLines = totalLines + numberOfLines
          totalFunctions = totalFunctions + numberOfFunctions
          totalPureFunctions = totalPureFunctions + numberOfPureFunctions
          totalUnivFunctions = totalUnivFunctions + numberOfIOFunctions
          totalEffPolymorphicFunctions = totalEffPolymorphicFunctions + numberOfEffectPolymorphicFunctions
          totalLambdas = totalLambdas + numberOfLambdas
          totalEffVars = totalEffVars + numberOfEffVars
          totalSlackEffVars = totalSlackEffVars + numberOfSlackEffVars

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
            print(Separator)
            print(padL(s"${format(numberOfSlackEffVars, sign = true)} ${format(numberOfEffVars, numberOfSlackEffVars)}", ColWidth))
            println(EndOfLine)
          }
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
      print(Separator)
      print(padL(s"${format(totalSlackEffVars, sign = true)} ${format(totalEffVars, totalSlackEffVars)}", ColWidth))
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
            (sig.sym, sig.spec, sig.exp.getOrElse(???))
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
  private def format(n: Int, sign: Boolean = false): String = {
    if (sign) f"$n%+,d".replace(".", ",")
    else f"$n%,d".replace(".", ",")
  }

  private def format(before: Int, inc: Int): String = {
    val after = before + inc
    // k * before = after
    // k = after / before
    val percentage = if (before == 0) "inf" else f"${(100 * after / (1.0 * before))-100}%+3.0f"
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

}
