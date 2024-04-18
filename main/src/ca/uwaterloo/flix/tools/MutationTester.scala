package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.Ast.{Constant, Input, Polarity, Source}
import ca.uwaterloo.flix.language.ast.SemanticOp._
import ca.uwaterloo.flix.language.ast.TypedAst.Pattern.Record.RecordLabelPattern
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.{Body, Head}
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.phase.util.PredefinedClasses
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.tools.Tester.ConsoleRedirection
import ca.uwaterloo.flix.util.{Formatter, Result, Validation}

import java.util.concurrent.ConcurrentLinkedQueue
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{Duration, DurationInt, DurationLong}
import scala.concurrent.{Await, Future, TimeoutException}

object MutationTester {

  // todo: is it good returned type ?
  def run(root: Root, sourceCompilationResult: CompilationResult)(implicit flix: Flix): Result[Unit, String] = {
    val testTimeout: Duration = MutantRunner.testSource(sourceCompilationResult) match {
      case Result.Ok(t) => t
      case Result.Err(s) => return Result.Err(s"Mutation testing not started: $s")
    }

    val mutants = Mutator.mutateRoot(root)

    val queue = new ConcurrentLinkedQueue[ReportEvent]()
    val reporter = new MutantReporter(queue)
    val runner = new MutantRunner(queue, mutants, testTimeout)

    runner.start()
    reporter.start()

    runner.join()
    reporter.join()

    Result.Ok(())
  }

  private case class Mutant[+T](value: T, printed: PrintedDiff)

  private object Mutator {

    import Helper._
    import PrintedDiff._

    def mutateRoot(root: Root)(implicit flix: Flix): LazyList[Mutant[Root]] = {
      val filteredInstances = LazyList.from(root.instances).filter(pair => !isLibSource(pair._1.loc))
      val instancesMutants = filteredInstances.flatMap { pair =>
        mutateElms(pair._2, mutateInstance, { list: List[Instance] => root.copy(instances = root.instances + (pair._1 -> list)) })
      }

      val filteredDefs = LazyList.from(root.defs.values).filter(defn => !isLibSource(defn.exp.loc) && !isTestDef(defn))
      val defsMutants = filteredDefs.flatMap(mutateMap(_, mutateDef, { value: Def => root.copy(defs = root.defs + (value.sym -> value)) }))

      instancesMutants #::: defsMutants
    }

    private def mutateInstance(instance: Instance)(implicit flix: Flix): LazyList[Mutant[Instance]] = {
      val defs = instance.defs.filter(defn => !isLibSource(defn.exp.loc) && !isTestDef(defn))

      mutateElms(defs, mutateDef, { list: List[Def] => instance.copy(defs = list) })
    }

    private def mutateDef(defn: Def)(implicit flix: Flix): LazyList[Mutant[Def]] =
      mutateMap(defn.exp, mutateExpr, Def(defn.sym, defn.spec, _))

    private def mutateExpr(expr: Expr)(implicit flix: Flix): LazyList[Mutant[Expr]] = {
      if (expr.tpe.toString == "Unit") {
        return LazyList.empty
      }

      expr match {
        case Expr.Cst(cst, tpe, loc) => mutateMap(cst, mutateConstant, Expr.Cst(_, tpe, loc), _.mapLoc(loc))
        case Expr.Var(sym, tpe, loc) => LazyList.empty
        case Expr.Def(sym, tpe, loc) => mutateExprDef(expr)
        case Expr.Sig(sym, tpe, loc) => mutateExprSig(expr)
        case Expr.Hole(sym, tpe, loc) => LazyList.empty
        case Expr.HoleWithExp(exp, tpe, eff, loc) => mutateMap(exp, mutateExpr, Expr.HoleWithExp(_, tpe, eff, loc))
        case Expr.OpenAs(symUse, exp, tpe, loc) => mutateMap(exp, mutateExpr, Expr.OpenAs(symUse, _, tpe, loc))
        case Expr.Use(sym, alias, exp, loc) => mutateMap(exp, mutateExpr, Expr.Use(sym, alias, _, loc))
        case Expr.Lambda(fparam, exp, tpe, loc) => mutateMap(exp, mutateExpr, Expr.Lambda(fparam, _, tpe, loc))
        case Expr.Apply(exp, exps, tpe, eff, loc) => mutateExprApply(expr)
        case Expr.Unary(sop, exp, tpe, eff, loc) => mutateExprUnary(expr)
        case Expr.Binary(sop, exp1, exp2, tpe, eff, loc) => mutateExprBinary(expr)
        case Expr.Let(sym, mod, exp1, exp2, tpe, eff, loc) =>
          mutateMap(exp1, mutateExpr, Expr.Let(sym, mod, _, exp2, tpe, eff, loc)) #:::
            mutateMap(exp2, mutateExpr, Expr.Let(sym, mod, exp1, _, tpe, eff, loc))
        case Expr.LetRec(sym, ann, mod, exp1, exp2, tpe, eff, loc) =>
          mutateMap(exp1, mutateExpr, Expr.LetRec(sym, ann, mod, _, exp2, tpe, eff, loc)) #:::
            mutateMap(exp2, mutateExpr, Expr.LetRec(sym, ann, mod, exp1, _, tpe, eff, loc))
        case Expr.Region(tpe, loc) => LazyList.empty
        case Expr.Scope(sym, regionVar, exp, tpe, eff, loc) =>
          mutateMap(exp, mutateExpr, Expr.Scope(sym, regionVar, _, tpe, eff, loc))
        case Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
          mutateMap(exp1, mutateExpr, Expr.IfThenElse(_, exp2, exp3, tpe, eff, loc)) #:::
            mutateMap(exp2, mutateExpr, Expr.IfThenElse(exp1, _, exp3, tpe, eff, loc)) #:::
            mutateMap(exp3, mutateExpr, Expr.IfThenElse(exp1, exp2, _, tpe, eff, loc))
        case Expr.Stm(exp1, exp2, tpe, eff, loc) =>
          mutateMap(exp1, mutateExpr, Expr.Stm(_, exp2, tpe, eff, loc)) #:::
            mutateMap(exp2, mutateExpr, Expr.Stm(exp1, _, tpe, eff, loc))
        case Expr.Discard(exp, eff, loc) => mutateMap(exp, mutateExpr, Expr.Discard(_, eff, loc))
        case Expr.Match(exp, rules, tpe, eff, loc) =>
          mutateMap(exp, mutateExpr, Expr.Match(_, rules, tpe, eff, loc)) #:::
            mutateElms(rules, mutateMatchRule, Expr.Match(exp, _, tpe, eff, loc))
        case Expr.TypeMatch(exp, rules, tpe, eff, loc) =>
          mutateMap(exp, mutateExpr, Expr.TypeMatch(_, rules, tpe, eff, loc)) #:::
            mutateElms(rules, mutateTypeMatchRule, Expr.TypeMatch(exp, _, tpe, eff, loc))
        case Expr.RestrictableChoose(star, exp, rules, tpe, eff, loc) =>
          mutateMap(exp, mutateExpr, Expr.RestrictableChoose(star, _, rules, tpe, eff, loc)) #:::
            mutateElms(rules, mutateRestrictableChooseRule, Expr.RestrictableChoose(star, exp, _, tpe, eff, loc))
        case Expr.Tag(sym, exp, tpe, eff, loc) =>
          mutateMap(exp, mutateExpr, Expr.Tag(sym, _, tpe, eff, loc))
        case Expr.RestrictableTag(sym, exp, tpe, eff, loc) =>
          mutateMap(exp, mutateExpr, Expr.RestrictableTag(sym, _, tpe, eff, loc))
        case Expr.Tuple(elms, tpe, eff, loc) =>
          mutateElms(elms, mutateExpr, Expr.Tuple(_, tpe, eff, loc))
        case Expr.RecordEmpty(tpe, loc) => LazyList.empty
        case Expr.RecordSelect(exp, label, tpe, eff, loc) =>
          mutateMap(exp, mutateExpr, Expr.RecordSelect(_, label, tpe, eff, loc))
        case Expr.RecordExtend(label, exp1, exp2, tpe, eff, loc) =>
          mutateMap(exp1, mutateExpr, Expr.RecordExtend(label, _, exp2, tpe, eff, loc)) #:::
            mutateMap(exp2, mutateExpr, Expr.RecordExtend(label, exp1, _, tpe, eff, loc))
        case Expr.RecordRestrict(label, exp, tpe, eff, loc) =>
          mutateMap(exp, mutateExpr, Expr.RecordRestrict(label, _, tpe, eff, loc))
        case Expr.ArrayLit(exps, exp, tpe, eff, loc) =>
          mutateElms(exps, mutateExpr, Expr.ArrayLit(_, exp, tpe, eff, loc)) #:::
            mutateMap(exp, mutateExpr, Expr.ArrayLit(exps, _, tpe, eff, loc))
        case Expr.ArrayNew(exp1, exp2, exp3, tpe, eff, loc) =>
          mutateMap(exp1, mutateExpr, Expr.ArrayNew(_, exp2, exp3, tpe, eff, loc)) #:::
            mutateMap(exp2, mutateExpr, Expr.ArrayNew(exp1, _, exp3, tpe, eff, loc)) #:::
            mutateMap(exp3, mutateExpr, Expr.ArrayNew(exp1, exp2, _, tpe, eff, loc))
        case Expr.ArrayLoad(exp1, exp2, tpe, eff, loc) =>
          mutateMap(exp1, mutateExpr, Expr.ArrayLoad(_, exp2, tpe, eff, loc)) #:::
            mutateMap(exp2, mutateExpr, Expr.ArrayLoad(exp1, _, tpe, eff, loc))
        case Expr.ArrayLength(exp, eff, loc) =>
          mutateMap(exp, mutateExpr, Expr.ArrayLength(_, eff, loc))
        case Expr.ArrayStore(exp1, exp2, exp3, eff, loc) =>
          mutateMap(exp1, mutateExpr, Expr.ArrayStore(_, exp2, exp3, eff, loc)) #:::
            mutateMap(exp2, mutateExpr, Expr.ArrayStore(exp1, _, exp3, eff, loc)) #:::
            mutateMap(exp3, mutateExpr, Expr.ArrayStore(exp1, exp2, _, eff, loc))
        case Expr.VectorLit(exps, tpe, eff, loc) =>
          mutateElms(exps, mutateExpr, Expr.VectorLit(_, tpe, eff, loc))
        case Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
          mutateMap(exp1, mutateExpr, Expr.VectorLoad(_, exp2, tpe, eff, loc)) #:::
            mutateMap(exp2, mutateExpr, Expr.VectorLoad(exp1, _, tpe, eff, loc))
        case Expr.VectorLength(exp, loc) =>
          mutateMap(exp, mutateExpr, Expr.VectorLength(_, loc))
        case Expr.Ref(exp1, exp2, tpe, eff, loc) =>
          mutateMap(exp1, mutateExpr, Expr.Ref(_, exp2, tpe, eff, loc)) #:::
            mutateMap(exp2, mutateExpr, Expr.Ref(exp1, _, tpe, eff, loc))
        case Expr.Deref(exp, tpe, eff, loc) =>
          mutateMap(exp, mutateExpr, Expr.Deref(_, tpe, eff, loc))
        case Expr.Assign(exp1, exp2, tpe, eff, loc) =>
          mutateMap(exp1, mutateExpr, Expr.Assign(_, exp2, tpe, eff, loc)) #:::
            mutateMap(exp2, mutateExpr, Expr.Assign(exp1, _, tpe, eff, loc))
        case Expr.Ascribe(exp, tpe, eff, loc) =>
          mutateMap(exp, mutateExpr, Expr.Ascribe(_, tpe, eff, loc))
        case Expr.InstanceOf(exp, clazz, loc) =>
          mutateMap(exp, mutateExpr, Expr.InstanceOf(_, clazz, loc))
        case Expr.CheckedCast(cast, exp, tpe, eff, loc) =>
          mutateMap(exp, mutateExpr, Expr.CheckedCast(cast, _, tpe, eff, loc))
        case Expr.UncheckedCast(exp, declaredType, declaredEff, tpe, eff, loc) =>
          mutateMap(exp, mutateExpr, Expr.UncheckedCast(_, declaredType, declaredEff, tpe, eff, loc))
        case Expr.UncheckedMaskingCast(exp, tpe, eff, loc) =>
          mutateMap(exp, mutateExpr, Expr.UncheckedMaskingCast(_, tpe, eff, loc))
        case Expr.Without(exp, effUse, tpe, eff, loc) =>
          mutateMap(exp, mutateExpr, Expr.Without(_, effUse, tpe, eff, loc))
        case Expr.TryCatch(exp, rules, tpe, eff, loc) =>
          mutateMap(exp, mutateExpr, Expr.TryCatch(_, rules, tpe, eff, loc)) #:::
            mutateElms(rules, mutateCatchRule, Expr.TryCatch(exp, _, tpe, eff, loc))
        case Expr.TryWith(exp, effUse, rules, tpe, eff, loc) =>
          mutateMap(exp, mutateExpr, Expr.TryWith(_, effUse, rules, tpe, eff, loc)) #:::
            mutateElms(rules, mutateHandlerRule, Expr.TryWith(exp, effUse, _, tpe, eff, loc))
        case Expr.Do(op, exps, tpe, eff, loc) =>
          mutateElms(exps, mutateExpr, Expr.Do(op, _, tpe, eff, loc))
        case Expr.InvokeConstructor(constructor, exps, tpe, eff, loc) =>
          mutateElms(exps, mutateExpr, Expr.InvokeConstructor(constructor, _, tpe, eff, loc))
        case Expr.InvokeMethod(method, exp, exps, tpe, eff, loc) =>
          mutateMap(exp, mutateExpr, Expr.InvokeMethod(method, _, exps, tpe, eff, loc)) #:::
            mutateElms(exps, mutateExpr, Expr.InvokeMethod(method, exp, _, tpe, eff, loc))
        case Expr.InvokeStaticMethod(method, exps, tpe, eff, loc) =>
          mutateElms(exps, mutateExpr, Expr.InvokeStaticMethod(method, _, tpe, eff, loc))
        case Expr.GetField(field, exp, tpe, eff, loc) =>
          mutateMap(exp, mutateExpr, Expr.GetField(field, _, tpe, eff, loc))
        case Expr.PutField(field, exp1, exp2, tpe, eff, loc) =>
          mutateMap(exp1, mutateExpr, Expr.PutField(field, _, exp2, tpe, eff, loc)) #:::
            mutateMap(exp2, mutateExpr, Expr.PutField(field, exp1, _, tpe, eff, loc))
        case Expr.GetStaticField(field, tpe, eff, loc) => LazyList.empty
        case Expr.PutStaticField(field, exp, tpe, eff, loc) =>
          mutateMap(exp, mutateExpr, Expr.PutStaticField(field, _, tpe, eff, loc))
        case Expr.NewObject(name, clazz, tpe, eff, methods, loc) => LazyList.empty
        case Expr.NewChannel(exp1, exp2, tpe, eff, loc) =>
          mutateMap(exp1, mutateExpr, Expr.NewChannel(_, exp2, tpe, eff, loc)) #:::
            mutateMap(exp2, mutateExpr, Expr.NewChannel(exp1, _, tpe, eff, loc))
        case Expr.GetChannel(exp, tpe, eff, loc) =>
          mutateMap(exp, mutateExpr, Expr.GetChannel(_, tpe, eff, loc))
        case Expr.PutChannel(exp1, exp2, tpe, eff, loc) =>
          mutateMap(exp1, mutateExpr, Expr.PutChannel(_, exp2, tpe, eff, loc)) #:::
            mutateMap(exp2, mutateExpr, Expr.PutChannel(exp1, _, tpe, eff, loc))
        case Expr.SelectChannel(rules, default, tpe, eff, loc) =>
          mutateElms(rules, mutateSelectChannelRule, Expr.SelectChannel(_, default, tpe, eff, loc)) #:::
            default.map(
              mutateMap(_, mutateExpr, { value: Expr => Expr.SelectChannel(rules, Some(value), tpe, eff, loc) })
            ).getOrElse(LazyList.empty)
        case Expr.Spawn(exp1, exp2, tpe, eff, loc) =>
          mutateMap(exp1, mutateExpr, Expr.Spawn(_, exp2, tpe, eff, loc)) #:::
            mutateMap(exp2, mutateExpr, Expr.Spawn(exp1, _, tpe, eff, loc))
        case Expr.ParYield(frags, exp, tpe, eff, loc) =>
          mutateElms(frags, mutateParYieldFragment, Expr.ParYield(_, exp, tpe, eff, loc)) #:::
            mutateMap(exp, mutateExpr, Expr.ParYield(frags, _, tpe, eff, loc))
        case Expr.Lazy(exp, tpe, loc) => mutateMap(exp, mutateExpr, Expr.Lazy(_, tpe, loc))
        case Expr.Force(exp, tpe, eff, loc) => mutateMap(exp, mutateExpr, Expr.Force(_, tpe, eff, loc))
        case Expr.FixpointConstraintSet(cs, tpe, loc) => mutateExprFixpointConstraintSet(expr)
        case Expr.FixpointLambda(pparams, exp, tpe, eff, loc) => LazyList.empty
        case Expr.FixpointMerge(exp1, exp2, tpe, eff, loc) => LazyList.empty
        case Expr.FixpointSolve(exp, tpe, eff, loc) => LazyList.empty
        case Expr.FixpointFilter(pred, exp, tpe, eff, loc) => LazyList.empty
        case Expr.FixpointInject(exp, pred, tpe, eff, loc) => LazyList.empty
        case Expr.FixpointProject(pred, exp, tpe, eff, loc) => LazyList.empty
        case Expr.Error(m, tpe, eff) => LazyList.empty
      }
    }

    private def mutateConstant(cst: Constant): LazyList[Mutant[Constant]] = cst match {
      case Constant.Bool(lit) => LazyList(Mutant(Constant.Bool(!lit), PrintedReplace(SourceLocation.Unknown, (!lit).toString)))
      case Constant.Float32(lit) => mutateConstantNum(lit + 1, lit - 1, Constant.Float32, { s => s"${s}f32" })
      case Constant.Float64(lit) => mutateConstantNum(lit + 1, lit - 1, Constant.Float64, { s => s })
      case Constant.BigDecimal(lit) => mutateConstantNum(
        lit.add(java.math.BigDecimal.ONE),
        lit.subtract(java.math.BigDecimal.ONE),
        Constant.BigDecimal,
        { s => s"${s}ff" }
      )
      case Constant.Int8(lit) => mutateConstantNum(lit + 1, lit - 1, { n: Int => Constant.Int8(n.toByte) }, { s => s"${s}i8" })
      case Constant.Int16(lit) => mutateConstantNum(lit + 1, lit - 1, { n: Int => Constant.Int16(n.toShort) }, { s => s"${s}i16" })
      case Constant.Int32(lit) => mutateConstantNum(lit + 1, lit - 1, Constant.Int32, { s => s })
      case Constant.Int64(lit) => mutateConstantNum(lit + 1, lit - 1, Constant.Int64, { s => s"${s}i64" })
      case Constant.BigInt(lit) => mutateConstantNum(
        lit.add(java.math.BigInteger.ONE),
        lit.subtract(java.math.BigInteger.ONE),
        Constant.BigInt,
        { s => s"${s}ii" }
      )
      case Constant.Str(lit) =>
        val pair = if (lit != "") ("", "") else ("Mutant", "\"Mutant\"")
        LazyList(Mutant(Constant.Str(pair._1), PrintedReplace(SourceLocation.Unknown, pair._2)))
      case _ => LazyList.empty
    }

    private def mutateExprDef(expr: Expr)(implicit flix: Flix): LazyList[Mutant[Expr]] = expr match {
      case Expr.Def(sym, tpe, loc) =>
        if (defnIntNamespaces.contains(sym.namespace.mkString("."))) {
          val symText = sym.text
          defnToDefn.get(symText)
            .map { s =>
              val defn = findDef(sym.namespace, s)
              LazyList(Mutant(Expr.Def(defn, tpe, loc), PrintedReplace(loc, defn.text)))
            }
            .orElse {
              defnToSig.get(symText)
                .map { pair =>
                  val sig = findSig(pair._1, pair._2)
                  val loc1 = loc.copy(beginCol = loc.endCol - sym.toString.length)
                  LazyList(Mutant(Expr.Sig(sig, tpe, loc), PrintedReplace(loc1, sig.toString)))
                }
            }
            .getOrElse(LazyList.empty)
        } else {
          LazyList.empty
        }
      case _ => LazyList.empty
    }

    private def mutateExprSig(expr: Expr)(implicit flix: Flix): LazyList[Mutant[Expr]] = expr match {
      case Expr.Sig(sym, tpe, loc) =>
        var l = List(conditionalNegateSigToSig, conditionalBoundarySigToSig)

        if (arithmeticSigTypes.contains(tpe.toString)) {
          l = arithmeticSigToSig :: l
        }

        val symText = sym.toString
        l.foldLeft(LazyList.empty[Mutant[Expr]]) {
          (acc, sigMap) =>
            acc #::: sigMap.get(symText)
              .map { pair =>
                val sig = findSig(pair._1, pair._2)
                val locText = getText(loc)
                val printedDiff = if (symText.endsWith(locText)) {
                  PrintedReplace(loc.copy(beginCol = loc.beginCol - (symText.length - locText.length)), sig.toString)
                } else {
                  PrintedReplace(loc, sigToSymbol.getOrElse(sig.toString, s"\'${sig.toString}\'"))
                }
                LazyList(Mutant(Expr.Sig(sig, tpe, loc), printedDiff))
              }
              .getOrElse(LazyList.empty)
        }
      case _ => LazyList.empty
    }

    private def mutateExprApply(expr: TypedAst.Expr)(implicit flix: Flix): LazyList[Mutant[Expr]] = expr match {
      case Expr.Apply(exp, exps, tpe, eff, loc) =>
        exp match {
          case Expr.Sig(sym, _, _) if sym.toString == "Neg.neg" =>
            LazyList(Mutant(exps.head, PrintedReplace(loc, getText(exps.head))))
          case Expr.Def(sym, _, _) if defnIntNamespaces.contains(sym.namespace.mkString(".")) && sym.text == "bitwiseNot" =>
            LazyList(Mutant(exps.head, PrintedReplace(loc, getText(exps.head))))
          case _ =>
            val res = mutateMap(exp, mutateExpr, Expr.Apply(_, exps, tpe, eff, loc)) #:::
              mutateElms(exps, mutateExprWithoutCst, Expr.Apply(exp, _, tpe, eff, loc))

            if (res.nonEmpty) res
            else if (tpe.toString == "Bool")
              LazyList(Mutant(
                Expr.Unary(BoolOp.Not, expr, tpe, eff, loc),
                PrintedAdd(loc.copy(endLine = loc.beginLine, endCol = loc.beginCol), "not ")
              ))
            else LazyList.empty
        }
      case _ => LazyList.empty
    }

    private def mutateExprUnary(expr: Expr): LazyList[Mutant[Expr]] =
      expr match {
        case Expr.Unary(sop, exp, tpe, eff, loc) => sop match {
          case BoolOp.Not =>
            LazyList(Mutant(exp, PrintedRemove(loc.copy(endLine = loc.beginLine, endCol = loc.beginCol + 4))))
          case _ => LazyList.empty
        }
        case _ => LazyList.empty
      }

    private def mutateExprBinary(expr: Expr)(implicit flix: Flix): LazyList[Mutant[Expr]] = expr match {
      case Expr.Binary(op, exp1, exp2, tpe, eff, loc) =>
        op match {
          case BoolOp.And =>
            val loc1 = locBetween(loc, exp1, exp2, "and")

            Mutant(Expr.Binary(BoolOp.Or, exp1, exp2, tpe, eff, loc), PrintedReplace(loc1, "or")) #::
              mutateMap(exp1, mutateExprWithoutCst, Expr.Binary(op, _, exp2, tpe, eff, loc)) #:::
              mutateMap(exp2, mutateExprWithoutCst, Expr.Binary(op, exp1, _, tpe, eff, loc))
          case BoolOp.Or =>
            val loc1 = locBetween(loc, exp1, exp2, "or")

            Mutant(Expr.Binary(BoolOp.And, exp1, exp2, tpe, eff, loc), PrintedReplace(loc1, "and")) #::
              mutateMap(exp1, mutateExprWithoutCst, Expr.Binary(op, _, exp2, tpe, eff, loc)) #:::
              mutateMap(exp2, mutateExprWithoutCst, Expr.Binary(op, exp1, _, tpe, eff, loc))
          case _ => LazyList.empty
        }
      case _ => LazyList.empty
    }

    private def mutateExprFixpointConstraintSet(expr: Expr)(implicit flix: Flix): LazyList[Mutant[Expr]] =
      expr match {
        case Expr.FixpointConstraintSet(cs, tpe, loc1) =>
          val csWithIndex = cs.zipWithIndex
          LazyList.from(csWithIndex).flatMap { case (constraint, i) =>
            // remove constraint
            val removedCS = csWithIndex.filterNot(_._2 == i).map(_._1)
            val removedExpr = Expr.FixpointConstraintSet(removedCS, tpe, loc1)
            val removedMutant = Mutant(removedExpr, PrintedRemove(constraint.loc))

            // mutate constraint
            val constraintMutants = mutateConstraint(constraint).map { m =>
              val updatedCS: List[Constraint] = cs.updated(i, m.value)
              val updatedExpr = Expr.FixpointConstraintSet(updatedCS, tpe, loc1)
              Mutant(updatedExpr, m.printed)
            }

            removedMutant #:: constraintMutants
          }
        case _ => LazyList.empty
      }

    private def mutateConstraint(constraint: Constraint)(implicit flix: Flix): LazyList[Mutant[Constraint]] = {
      // mutate head
      val headMutants = mutatePredicateHead(constraint.head).map { m =>
        Mutant(Constraint(constraint.cparams, m.value, constraint.body, constraint.loc), m.printed)
      }

      // mutate body
      val bodiesMutants = mutatePredicateBodyList(constraint.body).map { m =>
        Mutant(Constraint(constraint.cparams, constraint.head, m.value, constraint.loc), m.printed)
      }

      headMutants #::: bodiesMutants
    }

    private def mutatePredicateHead(head: Head)(implicit flix: Flix): LazyList[Mutant[Head]] = head match {
      case Head.Atom(pred, den, terms, tpe, loc) =>
        // mutate terms
        LazyList.from(terms.zipWithIndex).flatMap { case (exp, i) =>
          mutateExpr(exp).map { m =>
            val updatedTerms = terms.updated(i, m.value)
            val updatedHeadAtom = Head.Atom(pred, den, updatedTerms, tpe, loc)
            Mutant(updatedHeadAtom, m.printed)
          }
        }
    }

    private def mutatePredicateBodyList(bodyList: List[Body])(implicit flix: Flix): LazyList[Mutant[List[Body]]] = {
      val bodyListWithIndex = bodyList.zipWithIndex
      LazyList.from(bodyListWithIndex).flatMap { case (body, i) =>
        // remove item
        val removedMutant = Mutant(bodyListWithIndex.filterNot(_._2 == i).map(_._1), PrintedRemove(body.loc))

        // mutate item
        val bodyMutants = mutatePredicateBody(body).map { m =>
          Mutant(bodyList.updated(i, m.value), m.printed)
        }

        removedMutant #:: bodyMutants
      }
    }

    private def mutatePredicateBody(body: Body)(implicit flix: Flix): LazyList[Mutant[Body]] = body match {
      case Body.Atom(pred, den, polarity, fixity, terms, tpe, loc) =>
        // inverse polarity
        val inversedPolarity = inverseAstPolarity(polarity)
        val inversedBodyAtom = Body.Atom(pred, den, inversedPolarity, fixity, terms, tpe, loc)
        val printedDiff = inversedPolarity match {
          case Polarity.Positive => PrintedRemove(loc.copy(endLine = loc.beginLine, endCol = loc.beginCol + 4))
          case Polarity.Negative => PrintedAdd(loc.copy(endLine = loc.beginLine, endCol = loc.beginCol), "not ")
        }
        val inversedMutant = Mutant(inversedBodyAtom, printedDiff)

        // mutate terms
        val termsMutants = LazyList.from(terms.zipWithIndex).flatMap { case (pattern, i) =>
          mutatePattern(pattern).map { m =>
            val updatedTerms = terms.updated(i, m.value)
            val updatedBodyAtom = Body.Atom(pred, den, polarity, fixity, updatedTerms, tpe, loc)
            Mutant(updatedBodyAtom, m.printed)
          }
        }

        inversedMutant #:: termsMutants
      case Body.Functional(outVars, exp, loc) => mutateMap(exp, mutateExpr, Body.Functional(outVars, _, loc))
      case Body.Guard(exp, loc) => mutateMap(exp, mutateExpr, Body.Guard(_, loc))
    }

    private def mutateCatchRule(catchRule: CatchRule)(implicit flix: Flix): LazyList[Mutant[CatchRule]] =
      mutateMap(catchRule.exp, mutateExpr, CatchRule(catchRule.sym, catchRule.clazz, _))

    private def mutateHandlerRule(handlerRule: HandlerRule)(implicit flix: Flix): LazyList[Mutant[HandlerRule]] =
      mutateMap(handlerRule.exp, mutateExpr, HandlerRule(handlerRule.op, handlerRule.fparams, _))

    private def mutateRestrictableChooseRule(rcr: RestrictableChooseRule)(implicit flix: Flix): LazyList[Mutant[RestrictableChooseRule]] =
      mutateMap(rcr.exp, mutateExpr, RestrictableChooseRule(rcr.pat, _))

    private def mutateMatchRule(matchRule: MatchRule)(implicit flix: Flix): LazyList[Mutant[MatchRule]] =
      mutateMap(matchRule.pat, mutatePattern, MatchRule(_, matchRule.guard, matchRule.exp)) #:::
        matchRule.guard.map(
          mutateMap(_, mutateExpr, { value: Expr => MatchRule(matchRule.pat, Some(value), matchRule.exp) })
        ).getOrElse(LazyList.empty) #:::
        mutateMap(matchRule.exp, mutateExpr, MatchRule(matchRule.pat, matchRule.guard, _))

    private def mutateTypeMatchRule(typeMatchRule: TypeMatchRule)(implicit flix: Flix): LazyList[Mutant[TypeMatchRule]] =
      mutateMap(typeMatchRule.exp, mutateExpr, TypeMatchRule(typeMatchRule.sym, typeMatchRule.tpe, _))

    private def mutateSelectChannelRule(scr: SelectChannelRule)(implicit flix: Flix): LazyList[Mutant[SelectChannelRule]] =
      mutateMap(scr.chan, mutateExpr, SelectChannelRule(scr.sym, _, scr.exp)) #:::
        mutateMap(scr.exp, mutateExpr, SelectChannelRule(scr.sym, scr.chan, _))

    private def mutateParYieldFragment(pyf: ParYieldFragment)(implicit flix: Flix): LazyList[Mutant[ParYieldFragment]] =
      mutateMap(pyf.pat, mutatePattern, ParYieldFragment(_, pyf.exp, pyf.loc)) #:::
        mutateMap(pyf.exp, mutateExpr, ParYieldFragment(pyf.pat, _, pyf.loc))

    private def mutatePattern(pattern: Pattern): LazyList[Mutant[Pattern]] = {
      pattern match {
        case Pattern.Wild(tpe, loc) => LazyList.empty
        case Pattern.Var(sym, tpe, loc) => LazyList.empty
        case Pattern.Cst(cst, tpe, loc) => mutateMap(cst, mutateConstant, Pattern.Cst(_, tpe, loc), _.mapLoc(loc))
        case Pattern.Tag(sym, pat, tpe, loc) => mutateMap(pat, mutatePattern, Pattern.Tag(sym, _, tpe, loc))
        case Pattern.Tuple(elms, tpe, loc) => mutateElms(elms, mutatePattern, Pattern.Tuple(_, tpe, loc))
        case Pattern.Record(pats, pat, tpe, loc) =>
          mutateElms(pats, mutateRecordLabelPattern, Pattern.Record(_, pat, tpe, loc)) #:::
            mutateMap(pat, mutatePattern, Pattern.Record(pats, _, tpe, loc))
        case Pattern.RecordEmpty(tpe, loc) => LazyList.empty
        case Pattern.Error(tpe, loc) => LazyList.empty
      }
    }

    private def mutateRecordLabelPattern(rp: RecordLabelPattern): LazyList[Mutant[RecordLabelPattern]] =
      mutateMap(rp.pat, mutatePattern, RecordLabelPattern(rp.label, rp.tpe, _, rp.loc))

    private object Helper {
      val arithmeticSigTypes: Set[String] = Set(
        "Float32 -> (Float32 -> Float32)",
        "Float64 -> (Float64 -> Float64)",
        "BigDecimal -> (BigDecimal -> BigDecimal)",
        "Int8 -> (Int8 -> Int8)",
        "Int16 -> (Int16 -> Int16)",
        "Int32 -> (Int32 -> Int32)",
        "Int64 -> (Int64 -> Int64)",
        "BigInt -> (BigInt -> BigInt)",
      )

      val arithmeticSigToSig: Map[String, (String, String)] = Map(
        "Add.add" -> ("Sub", "sub"),
        "Sub.sub" -> ("Add", "add"),
        "Mul.mul" -> ("Div", "div"),
        "Div.div" -> ("Mul", "mul"),
      )

      val conditionalNegateSigToSig: Map[String, (String, String)] = Map(
        "Eq.eq" -> ("Eq", "neq"),
        "Eq.neq" -> ("Eq", "eq"),
        "Order.less" -> ("Order", "greaterEqual"),
        "Order.lessEqual" -> ("Order", "greater"),
        "Order.greater" -> ("Order", "lessEqual"),
        "Order.greaterEqual" -> ("Order", "less"),
      )

      val conditionalBoundarySigToSig: Map[String, (String, String)] = Map(
        "Order.less" -> ("Order", "lessEqual"),
        "Order.lessEqual" -> ("Order", "less"),
        "Order.greater" -> ("Order", "greaterEqual"),
        "Order.greaterEqual" -> ("Order", "greater"),
      )

      val sigToSymbol: Map[String, String] = Map(
        "Add.add" -> "+",
        "Sub.sub" -> "-",
        "Mul.mul" -> "*",
        "Div.div" -> "/",
        "Eq.eq" -> "==",
        "Eq.neq" -> "!=",
        "Order.less" -> "<",
        "Order.lessEqual" -> "<=",
        "Order.greater" -> ">",
        "Order.greaterEqual" -> ">=",
      )

      val defnIntNamespaces: Set[String] = Set(
        "Int8",
        "Int16",
        "Int32",
        "Int64",
      )

      val defnToSig: Map[String, (String, String)] = Map(
        "modulo" -> ("Mul", "mul"),
        "remainder" -> ("Mul", "mul"),
      )

      val defnToDefn: Map[String, String] = Map(
        "bitwiseAnd" -> "bitwiseOr",
        "bitwiseOr" -> "bitwiseAnd",
        "bitwiseXor" -> "bitwiseAnd",
        "leftShift" -> "rightShift",
        "rightShift" -> "leftShift",
      )

      def mutateExprWithoutCst(expr: Expr)(implicit flix: Flix): LazyList[Mutant[Expr]] = expr match {
        case Expr.Cst(_, _, _) => LazyList.empty
        case expr => mutateExpr(expr)
      }

      def isLibSource(loc: SourceLocation): Boolean = loc.source.input match {
        case Input.Text(_, _, _) => true
        case Input.TxtFile(_) => false
        case Input.PkgFile(_) => false
      }

      def isTestDef(defn: Def): Boolean = defn.spec.ann.isTest

      def mutateMap[T, E](e: E,
                          mutF: E => LazyList[Mutant[E]],
                          mapF: E => T,
                          printedF: PrintedDiff => PrintedDiff = { p => p }): LazyList[Mutant[T]] = {
        mutF(e).map(m => Mutant(mapF(m.value), printedF(m.printed)))
      }

      def mutateElms[T, E](elms: List[E], mutF: E => LazyList[Mutant[E]], mapF: List[E] => T): LazyList[Mutant[T]] =
        LazyList.tabulate(elms.length)(i =>
          mutF(elms(i)).map(m => Mutant(mapF(elms.updated(i, m.value)), m.printed))
        ).flatten

      def mutateConstantNum[T](inc: T, dec: T, mapF: T => Constant, mapString: String => String): LazyList[Mutant[Constant]] =
        LazyList(inc, dec).map(value => Mutant(mapF(value), PrintedReplace(SourceLocation.Unknown, mapString(value.toString))))

      def locBetween(sl: SourceLocation, exp1: Expr, exp2: Expr, sub: String)(implicit flix: Flix): SourceLocation = {
        val loc = sl.copy(
          beginLine = exp1.loc.endLine,
          beginCol = exp1.loc.endCol,
          endLine = exp2.loc.beginLine,
          endCol = exp2.loc.beginCol
        )
        val i = getText(loc).indexOf(sub)

        loc.copy(
          beginCol = loc.beginCol + i,
          endCol = loc.beginCol + i + sub.length
        )
      }

      def findSig(clazz: String, sig: String)(implicit flix: Flix): Symbol.SigSym = try {
        PredefinedClasses.lookupSigSym(clazz, sig, flix.getKinderAst)
      } catch {
        case e: Exception => throw new MutantException(s"Mutant creation failed: ${e.getMessage}", e)
      }

      def findDef(namespace: List[String], name: String)(implicit flix: Flix): Symbol.DefnSym = try {
        PredefinedClasses.lookupDefSym(namespace, name, flix.getKinderAst)
      } catch {
        case e: Exception => throw new MutantException(s"Mutant creation failed: ${e.getMessage}", e)
      }

      def inverseAstPolarity(polarity: Polarity): Polarity =
        polarity match {
          case Polarity.Positive => Polarity.Negative
          case Polarity.Negative => Polarity.Positive
        }

      def getText(expr: Expr): String = expr.loc.text.getOrElse("<unknown>")

      def getText(loc: SourceLocation): String = loc.text.getOrElse("<unknown>")
    }

    private class MutantException(message: String, cause: Throwable) extends Exception(message, cause)
  }

  private class MutantRunner(queue: ConcurrentLinkedQueue[ReportEvent],
                             mutants: LazyList[Mutant[Root]],
                             testTimeout: Duration)(implicit flix: Flix) extends Thread {

    import MutantRunner._
    import MutantStatus._

    override def run(): Unit = {
      val timeBefore = System.currentTimeMillis()

      mutants.foreach { m =>
        val compilationResult = compileMutant(m.value)

        val status: MutantStatus = compilationResult.toHardResult match {
          case Result.Ok(c) => try {
            Await.result(Future(testMutant(c)), testTimeout)
          } catch {
            case _: TimeoutException => TimedOut
          }
          case Result.Err(_) => CompilationFailed
        }

        queue.add(ReportEvent.Processed(status, m.printed))
      }

      queue.add(ReportEvent.Finished((System.currentTimeMillis() - timeBefore).milliseconds))
    }
  }

  private object MutantRunner {

    import MutantStatus._

    def testSource(cr: CompilationResult): Result[Duration, String] = {
      val timeBefore = System.currentTimeMillis()

      testMutant(cr) match {
        case Survived => Result.Ok(configureTimeOut((System.currentTimeMillis() - timeBefore).milliseconds))
        case _ => Result.Err("Source tests failed")
      }
    }

    // todo: is it necessary and possible run phases before `Typer.run` for mutant too ?
    private def compileMutant(mutant: Root)(implicit flix: Flix): Validation[CompilationResult, CompilationMessage] = {
      val result = flix.checkMutant(mutant).toHardFailure
      Validation.flatMapN(result)(flix.codeGenMutant)
    }

    private def testMutant(compilationResult: CompilationResult): MutantStatus = {
      val tests = compilationResult.getTests.values.filter(!_.skip)

      for (test <- tests) {
        // Taken from Tester.scala.
        val redirect = new ConsoleRedirection
        redirect.redirect()

        try {
          val result = test.run()
          redirect.restore()

          result match {
            case java.lang.Boolean.FALSE => return Killed
            case _ => if (redirect.stdErr.nonEmpty) return Killed
          }
        } catch {
          case _: Throwable =>
            redirect.restore()

            return Killed
        }
      }

      Survived
    }

    private def configureTimeOut(duration: Duration): Duration = (duration + 4.seconds) * 1.5
  }

  private class MutantReporter(queue: ConcurrentLinkedQueue[ReportEvent])(implicit flix: Flix) extends Thread {
    private var killed: Int = 0
    private var survived: Int = 0
    private var compilationFailed: Int = 0
    private var timedOut: Int = 0
    private val source2LinesNum = new mutable.HashMap[Source, Int]
    private val lineSeparator = System.lineSeparator()
    private val outputDivider = "-" * 80
    private val formatter: Formatter = flix.getFormatter

    import formatter._
    import MutantStatus._

    override def run(): Unit = {
      println("Running mutation testing...")

      var finished = false
      while (!finished) {
        queue.poll() match {
          case ReportEvent.Processed(status, printed) =>
            print(report(status, printed))
          case ReportEvent.Finished(duration: Duration) =>
            println(printStats(duration))
            finished = true
          case _ => // nop
        }
      }
    }

    private def report(status: MutantStatus, printedDiff: PrintedDiff): String = {
      val sb = new mutable.StringBuilder

      status match {
        case Killed =>
          killed += 1
          sb.append(green("KILLED"))
        case Survived =>
          survived += 1
          diff(sb, printedDiff)
          sb.append(red("SURVIVED"))
        case CompilationFailed =>
          compilationFailed += 1
          sb.append(blue("COMPILATION FAILED"))
        case TimedOut =>
          timedOut += 1
          sb.append(blue("TIMED OUT"))
        case _ =>
          sb.append(yellow("UNKNOWN STATUS"))
      }

      sb.append(s" ${printedDiff.sourceLocation.format}")
        .append(lineSeparator)
        .append(outputDivider)
        .append(lineSeparator)

      sb.toString()
    }

    private def printStats(duration: Duration): String =
      new StringBuilder()
        .append(lineSeparator)
        .append(cyan("Mutation testing results:"))
        .append(lineSeparator)
        .append(s"Total = ${total().toString}")
        .append(s", Killed = ${killed.toString}")
        .append(s", Survived = ${survived.toString}")
        .append(s", Compilation Failed = ${compilationFailed.toString}")
        .append(s", Timed out = ${timedOut.toString}")
        .append(lineSeparator)
        .append(s"Mutations score ${
          val tested = killed.doubleValue() + survived.doubleValue()
          if (tested != 0) s"= ${cyan(f"${killed.doubleValue() / tested}%.2f")}"
          else "was not calculated from 0 mutants"
        }")
        .append(lineSeparator)
        .append(s"Calculated in ${duration.toSeconds} seconds")
        .toString()

    private def total(): Int = killed + survived + compilationFailed + timedOut

    private def diff(sb: StringBuilder, printedDiff: PrintedDiff): Unit = {
      val sl = printedDiff.sourceLocation

      if (sl == SourceLocation.Unknown) {
        sb.append(yellow("Diff printing error: SourceLocation.Unknown was not expected"))
          .append(lineSeparator)
          .append(lineSeparator)
        return
      }

      val beginLine = sl.beginLine
      val endLine = sl.endLine
      val linesNum = source2LinesNum.getOrElseUpdate(sl.source, sl.source.data.count(c => c == '\n' || c == '\r') + 1)
      val numWidth = {
        if (endLine + 2 <= linesNum) endLine + 2
        else if (endLine + 1 <= linesNum) endLine + 1
        else endLine
      }.toString.length

      if (0 < beginLine - 2) diffNumberedLine(sb, sl, numWidth, beginLine - 2)
      if (0 < beginLine - 1) diffNumberedLine(sb, sl, numWidth, beginLine - 1)

      diffRemove(sb, sl, numWidth)
      diffAdd(sb, sl, numWidth, printedDiff.newStr)

      if (endLine + 1 <= linesNum) diffNumberedLine(sb, sl, numWidth, endLine + 1)
      if (endLine + 2 <= linesNum) diffNumberedLine(sb, sl, numWidth, endLine + 2)

      sb.append(lineSeparator)
    }

    private def diffRemove(sb: StringBuilder, loc: SourceLocation, numWidth: Int): Unit = {
      val beginLine = loc.beginLine
      val endLine = loc.endLine
      val beginCol = loc.beginCol
      val endCol = loc.endCol
      val beginL = loc.lineAt(beginLine)
      val endL = loc.lineAt(endLine)

      sb.append(padLeft(numWidth, beginLine.toString))
        .append(verticalBar(red("-")))
        .append(beginL.substring(0, beginCol - 1))

      if (loc.isSingleLine) {
        sb.append(red(beginL.substring(beginCol - 1, endCol - 1)))
      } else {
        sb.append(red(beginL.substring(beginCol - 1)))
          .append(lineSeparator)

        for (l <- beginLine + 1 until endLine) {
          diffNumberedLine(sb, loc, numWidth, l, "-", red)
        }

        sb.append(padLeft(numWidth, endLine.toString))
          .append(verticalBar(red("-")))
          .append(red(endL.substring(0, endCol - 1)))
      }

      sb.append(endL.substring(endCol - 1))
        .append(lineSeparator)
    }

    private def diffAdd(sb: StringBuilder, loc: SourceLocation, numWidth: Int, str: String): Unit = {
      sb.append(padLeft(numWidth, ""))
        .append(verticalBar(green("+")))
        .append(loc.lineAt(loc.beginLine).substring(0, loc.beginCol - 1))
        .append(green(str))
        .append(loc.lineAt(loc.endLine).substring(loc.endCol - 1))
        .append(lineSeparator)
    }

    private def diffNumberedLine(sb: StringBuilder,
                                 loc: SourceLocation,
                                 numWidth: Int,
                                 lineNo: Int,
                                 afterBar: String = " ",
                                 colorize: String => String = { s => s }): Unit = {
      sb.append(padLeft(numWidth, lineNo.toString))
        .append(verticalBar(colorize(afterBar)))
        .append(colorize(loc.lineAt(lineNo)))
        .append(lineSeparator)
    }

    private def padLeft(width: Int, s: String): String = String.format("%" + width + "s", s)

    private def verticalBar(s: String = " "): String = s" |$s "
  }

  sealed trait PrintedDiff {

    import PrintedDiff._

    def sourceLocation: SourceLocation = this match {
      case PrintedReplace(loc, _) => loc
      case PrintedRemove(loc) => loc
      case PrintedAdd(loc, _) => loc
    }

    def newStr: String = this match {
      case PrintedReplace(_, str) => str
      case PrintedRemove(_) => ""
      case PrintedAdd(_, str) => str
    }

    def mapLoc(loc1: SourceLocation): PrintedDiff = {
      this match {
        case PrintedReplace(_, newStr) => PrintedReplace(loc1, newStr)
        case PrintedRemove(_) => PrintedRemove(loc1)
        case PrintedAdd(_, newStr) => PrintedAdd(loc1, newStr)
      }
    }

    def mapStr(f: String => String): PrintedDiff = this match {
      case PrintedReplace(loc, newStr) => PrintedReplace(loc, f(newStr))
      case PrintedRemove(loc) => PrintedRemove(loc)
      case PrintedAdd(loc, newStr) => PrintedAdd(loc, f(newStr))
    }
  }

  private object PrintedDiff {

    case class PrintedReplace(loc: SourceLocation, str: String) extends PrintedDiff

    case class PrintedRemove(loc: SourceLocation) extends PrintedDiff

    case class PrintedAdd(loc: SourceLocation, str: String) extends PrintedDiff
  }

  sealed trait MutantStatus

  private object MutantStatus {

    case object Killed extends MutantStatus

    case object Survived extends MutantStatus

    case object CompilationFailed extends MutantStatus

    case object TimedOut extends MutantStatus
  }

  sealed trait ReportEvent

  private object ReportEvent {

    case class Processed(status: MutantStatus, printed: PrintedDiff) extends ReportEvent

    case class Finished(d: Duration) extends ReportEvent
  }
}
