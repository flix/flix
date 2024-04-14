package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Ast.{Constant, Input, Polarity, Source}
import ca.uwaterloo.flix.language.ast.SemanticOp._
import ca.uwaterloo.flix.language.ast.TypedAst.Pattern.Record.RecordLabelPattern
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.{Body, Head}
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.phase.util.PredefinedClasses
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.tools.Tester.ConsoleRedirection
import ca.uwaterloo.flix.util.{Formatter, Result}

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future, TimeoutException}

/* TODO: think also:
 * Check mutant compilation. Create 'MutantStatus' enum
 * Add reporter with statistics
 * Compile source and run tests on it before mutation testing
 * Run tests on mutant with timeout (calculate it by time spent to run tests on source)
 * Mutate Root.instances.defs (?)
 * Are mutants SourceLocations correct ?
 * Separate mutations by kind (?)
 * Is it necessary to mutate SemanticOp other than BoolOp.Not, BoolOp.And, BoolOp.Or ??
 * Helper functions for tests without @Test annotation.
*/

object MutationTester {
  def run(root: Root)(implicit flix: Flix): Result[Unit, String] = {
    val testTimeout = 10.seconds // todo: calculate bu run tests on source

    //     todo: configure (how) ?
    //    val numThreads = 2
    //    val executor = Executors.newFixedThreadPool(numThreads)

    val reporter = new MutationReporter()
    reporter.setStartTime()

    /*
     * Idea:
     *
     * Mutants can also be created by a separate thread and pushed into a separate queue,
     * which will be processed by the thread performing the compilation
     *
     * flix.codeGen can evaluates in only single thread, because implicit flix
     *
     * One thread compiles mutatedExps to compilationResults and push to ConcurrentLinkedQueue
     * Other thread takes them and run tests by executorService
     * Queue should have limited size to avoid memory problems
     *
     * I'm not sure if this will be effective, since compilation may take longer than testing (?)
     *
     * I would like to achieve full parallelism at all stages, including compilation,
     * but I'm not sure what will be achieved
     */

    val mutants = Mutator.mutateRoot(root)
    mutants.foreach { m =>
      //    redirect.redirect()
      // TODO: need run phases after `Typer.run` for mutants too
      //      val codeGenResult = flix.codeGen(m.value) // todo: is it possible to parallelize it ??
      //    if (redirect.stdErr.nonEmpty) return CompilationFailed
      //    redirect.restore()

      val compilationResult = flix.compileMutant(m.value)

      //          executor.execute(() => {
      val status: MutantStatus = compilationResult.toHardResult match {
        case Result.Ok(c) => try {
          Await.result(Future(testMutant(c)), testTimeout)
        } catch {
          case _: TimeoutException => TimedOut
        }
        case Result.Err(_) => CompilationFailed
      }

      println(reporter.report(status, m.printed))
      //          })
    }

    //    executor.shutdown()
    //    while (!executor.isTerminated) {}

    reporter.setFinishTime()
    println(reporter.printStats())

    Result.Ok(())
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

  private object Mutator {

    import Helper._

    def mutateRoot(root: Root)(implicit flix: Flix): LazyList[Mutant[Root]] = {
      // don't want to mutate library defs and tests
      val defs = LazyList.from(root.defs.values).filter(defn => !isLibDef(defn) && !isTestDef(defn))

      defs.flatMap(mutateMap(_, mutateDef, { value: Def => root.copy(defs = root.defs + (value.sym -> value)) }))
    }

    private def mutateDef(defn: Def)(implicit flix: Flix): LazyList[Mutant[Def]] =
      mutateMap(defn.exp, mutateExpr, Def(defn.sym, defn.spec, _))

    // todo: collections: mutate to empty (?)
    private def mutateExpr(expr: Expr)(implicit flix: Flix): LazyList[Mutant[Expr]] = {
      //      if (expr.tpe.toString == "Unit") {
      //        return LazyList.empty
      //      }

      expr match {
        case Expr.Cst(cst, tpe, loc) => mutateMap(cst, mutateConstant, Expr.Cst(_, tpe, loc), _.mapLoc(loc))
        case Expr.Var(sym, tpe, loc) => LazyList.empty
        case Expr.Def(sym, tpe, loc) => mutateExprDef(expr)
        case Expr.Sig(sym, tpe, loc) => mutateExprSig(expr)
        case Expr.Hole(sym, tpe, loc) => LazyList.empty // todo
        case Expr.HoleWithExp(exp, tpe, eff, loc) => LazyList.empty // todo
        case Expr.OpenAs(symUse, exp, tpe, loc) => LazyList.empty // todo
        case Expr.Use(sym, alias, exp, loc) => LazyList.empty // todo
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
        // todo: think if need other mutants for 2 below
        case Expr.Match(exp, rules, tpe, eff, loc) =>
          mutateMap(exp, mutateExpr, Expr.Match(_, rules, tpe, eff, loc)) #:::
            mutateElms(rules, mutateMatchRule, Expr.Match(exp, _, tpe, eff, loc))
        case Expr.TypeMatch(exp, rules, tpe, eff, loc) =>
          mutateMap(exp, mutateExpr, Expr.TypeMatch(_, rules, tpe, eff, loc)) #:::
            mutateElms(rules, mutateTypeMatchRule, Expr.TypeMatch(exp, _, tpe, eff, loc))
        case Expr.RestrictableChoose(star, exp, rules, tpe, eff, loc) => LazyList.empty // todo
        case Expr.Tag(sym, exp, tpe, eff, loc) =>
          mutateMap(exp, mutateExpr, Expr.Tag(sym, _, tpe, eff, loc))
        case Expr.RestrictableTag(sym, exp, tpe, eff, loc) => LazyList.empty // todo
        case Expr.Tuple(elms, tpe, eff, loc) => mutateElms(elms, mutateExpr, Expr.Tuple(_, tpe, eff, loc))
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
        case Expr.Assign(exp1, exp2, tpe, eff, loc) => LazyList.empty // todo
        case Expr.Ascribe(exp, tpe, eff, loc) =>
          mutateMap(exp, mutateExpr, Expr.Ascribe(_, tpe, eff, loc))
        case Expr.InstanceOf(exp, clazz, loc) => LazyList.empty // todo
        case Expr.CheckedCast(cast, exp, tpe, eff, loc) => LazyList.empty // todo
        case Expr.UncheckedCast(exp, declaredType, declaredEff, tpe, eff, loc) =>
          mutateMap(exp, mutateExpr, Expr.UncheckedCast(_, declaredType, declaredEff, tpe, eff, loc))
        case Expr.UncheckedMaskingCast(exp, tpe, eff, loc) => LazyList.empty // todo
        case Expr.Without(exp, effUse, tpe, eff, loc) => LazyList.empty // todo
        case Expr.TryCatch(exp, rules, tpe, eff, loc) => LazyList.empty // todo
        case Expr.TryWith(exp, effUse, rules, tpe, eff, loc) => LazyList.empty // todo
        case Expr.Do(op, exps, tpe, eff, loc) => LazyList.empty // todo
        case Expr.InvokeConstructor(constructor, exps, tpe, eff, loc) => LazyList.empty // todo
        case Expr.InvokeMethod(method, exp, exps, tpe, eff, loc) => LazyList.empty // todo
        case Expr.InvokeStaticMethod(method, exps, tpe, eff, loc) => LazyList.empty // todo
        case Expr.GetField(field, exp, tpe, eff, loc) => LazyList.empty // todo
        case Expr.PutField(field, exp1, exp2, tpe, eff, loc) => LazyList.empty // todo
        case Expr.GetStaticField(field, tpe, eff, loc) => LazyList.empty // todo
        case Expr.PutStaticField(field, exp, tpe, eff, loc) => LazyList.empty // todo
        case Expr.NewObject(name, clazz, tpe, eff, methods, loc) => LazyList.empty // todo
        case Expr.NewChannel(exp1, exp2, tpe, eff, loc) => LazyList.empty // todo
        case Expr.GetChannel(exp, tpe, eff, loc) => LazyList.empty // todo
        case Expr.PutChannel(exp1, exp2, tpe, eff, loc) => LazyList.empty // todo
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
        case Expr.FixpointLambda(pparams, exp, tpe, eff, loc) =>
          mutateMap(exp, mutateExpr, Expr.FixpointLambda(pparams, _, tpe, eff, loc))
        case Expr.FixpointMerge(exp1, exp2, tpe, eff, loc) =>
          mutateMap(exp1, mutateExpr, Expr.FixpointMerge(_, exp2, tpe, eff, loc)) #:::
            mutateMap(exp2, mutateExpr, Expr.FixpointMerge(exp1, _, tpe, eff, loc))
        case Expr.FixpointSolve(exp, tpe, eff, loc) =>
          mutateMap(exp, mutateExpr, Expr.FixpointSolve(_, tpe, eff, loc))
        case Expr.FixpointFilter(pred, exp, tpe, eff, loc) =>
          mutateMap(exp, mutateExpr, Expr.FixpointFilter(pred, _, tpe, eff, loc))
        case Expr.FixpointInject(exp, pred, tpe, eff, loc) =>
          mutateMap(exp, mutateExpr, Expr.FixpointInject(_, pred, tpe, eff, loc))
        case Expr.FixpointProject(pred, exp, tpe, eff, loc) =>
          mutateMap(exp, mutateExpr, Expr.FixpointProject(pred, _, tpe, eff, loc))
        case Expr.Error(m, tpe, eff) => LazyList.empty
      }
    }

    private def mutateConstant(cst: Constant): LazyList[Mutant[Constant]] = cst match {
      case Constant.Bool(lit) => LazyList(Mutant(Constant.Bool(!lit), PrintedReplace(SourceLocation.Unknown, (!lit).toString)))
      case Constant.Float32(lit) => LazyList(lit + 1, lit - 1).map(value => Mutant(Constant.Float32(value), PrintedReplace(SourceLocation.Unknown, s"${value.toString}f32")))
      case Constant.Float64(lit) => LazyList(lit + 1, lit - 1).map(value => Mutant(Constant.Float64(value), PrintedReplace(SourceLocation.Unknown, value.toString)))
      case Constant.BigDecimal(lit) => LazyList(
        lit.add(java.math.BigDecimal.ONE),
        lit.subtract(java.math.BigDecimal.ONE)
      ).map(value => Mutant(Constant.BigDecimal(value), PrintedReplace(SourceLocation.Unknown, s"${value.toString}ff")))
      case Constant.Int8(lit) => LazyList(lit + 1, lit - 1).map(value => Mutant(Constant.Int8(value.toByte), PrintedReplace(SourceLocation.Unknown, s"${value.toString}i8")))
      case Constant.Int16(lit) => LazyList(lit + 1, lit - 1).map(value => Mutant(Constant.Int16(value.toShort), PrintedReplace(SourceLocation.Unknown, s"${value.toString}i16")))
      case Constant.Int32(lit) => LazyList(lit + 1, lit - 1).map(value => Mutant(Constant.Int32(value), PrintedReplace(SourceLocation.Unknown, value.toString)))
      case Constant.Int64(lit) => LazyList(lit + 1, lit - 1).map(value => Mutant(Constant.Int64(value), PrintedReplace(SourceLocation.Unknown, s"${value.toString}i64")))
      case Constant.BigInt(lit) => LazyList(
        lit.add(java.math.BigInteger.ONE),
        lit.subtract(java.math.BigInteger.ONE)
      ).map(value => Mutant(Constant.BigInt(value), PrintedReplace(SourceLocation.Unknown, s"${value.toString}ii")))
      case Constant.Str(lit) =>
        val pair = if (lit != "") ("", "") else ("Flix", "\"Flix\"")
        LazyList(Mutant(Constant.Str(pair._1), PrintedReplace(SourceLocation.Unknown, pair._2)))
      case _ => LazyList.empty
    }

    private def mutateExprDef(expr: Expr)(implicit flix: Flix): LazyList[Mutant[Expr]] = expr match {
      case Expr.Def(sym, tpe, loc) if defnIntNamespaces.contains(sym.namespace.mkString(".")) =>
        // todo: refactor to imperative
        defnToDefn.get(sym.text)
          .flatMap(findDef(sym.namespace, _))
          .map(s => LazyList(Mutant(Expr.Def(s, tpe, loc), PrintedReplace(loc, s.toString))))
          .orElse {
            defnToSig.get(sym.text)
              .flatMap(p => findSig(p._1, p._2))
              .map(s => LazyList(Mutant(Expr.Sig(s, tpe, loc), PrintedReplace(loc, s.toString))))
          }
          .getOrElse(LazyList.empty)
      case _ => LazyList.empty
    }

    private def mutateExprSig(expr: Expr)(implicit flix: Flix): LazyList[Mutant[Expr]] = expr match {
      case Expr.Sig(sym, tpe, loc) =>
        // todo: refactor to imperative
        var l = List(conditionalNegateSigToSig, conditionalBoundarySigToSig)
        if (arithmeticSigTypes.contains(tpe.toString)) {
          l = arithmeticSigToSig :: l
        }

        l.foldLeft(LazyList.empty[Mutant[Expr]]) {
          (acc, sigMap) =>
            acc #::: sigMap.get(sym.toString)
              .flatMap(p => findSig(p._1, p._2))
              .map(s => LazyList(Mutant(Expr.Sig(s, tpe, loc), PrintedReplace(loc, s.toString))))
              .getOrElse(LazyList.empty)
        }
      case _ => LazyList.empty
    }

    private def mutateExprApply(expr: TypedAst.Expr)(implicit flix: Flix): LazyList[Mutant[Expr]] = expr match {
      case Expr.Apply(exp, exps, tpe, eff, loc) =>
        exp match {
          case Expr.Sig(sym, _, _) if sym.toString == "Neg.neg" =>
            LazyList(Mutant(exps.head, PrintedReplace(loc, exps.head.loc.text.get)))
          case Expr.Def(sym, _, _) if defnIntNamespaces.contains(sym.namespace.mkString(".")) && sym.text == "bitwiseNot" =>
            LazyList(Mutant(exps.head, PrintedReplace(loc, exps.head.loc.text.get)))
          case Expr.Sig(_, _, _) | Expr.Def(_, _, _) =>
            val res = mutateMap(
              exp,
              mutateExpr,
              Expr.Apply(_, exps, tpe, eff, loc),
              _.mapStr(s => s"$s${exps.map(_.loc.text.get).mkString("(", ", ", ")")}").mapLoc(loc)
            )

            // todo: think if need other mutants
            if (res.nonEmpty) res
            else if (tpe.toString == "Bool")
              LazyList(Mutant(
                Expr.Unary(BoolOp.Not, expr, tpe, eff, loc),
                PrintedAdd(loc.copy(endLine = loc.beginLine, endCol = loc.beginCol), "not ")
              ))
            else mutateElms(exps, mutateExpr, Expr.Apply(exp, _, tpe, eff, loc))
          case _ =>
            mutateMap(exp, mutateExpr, Expr.Apply(_, exps, tpe, eff, loc)) #:::
              mutateElms(exps, mutateExpr, Expr.Apply(exp, _, tpe, eff, loc))
        }
      case _ => LazyList.empty
    }

    private def mutateExprUnary(expr: Expr): LazyList[Mutant[Expr]] = expr match {
      case Expr.Unary(BoolOp.Not, exp, _, _, loc) => LazyList(Mutant(exp, PrintedReplace(loc, exp.loc.text.get)))
      case _ => LazyList.empty
    }

    private def mutateExprBinary(expr: Expr): LazyList[Mutant[Expr]] = expr match {
      case Expr.Binary(BoolOp.And, exp1, exp2, tpe, eff, loc) =>
        LazyList(Mutant(Expr.Binary(BoolOp.Or, exp1, exp2, tpe, eff, loc), PrintedReplace(loc, s"${exp1.loc.text.get} or ${exp2.loc.text.get}")))
      case Expr.Binary(BoolOp.Or, exp1, exp2, tpe, eff, loc) =>
        LazyList(Mutant(Expr.Binary(BoolOp.And, exp1, exp2, tpe, eff, loc), PrintedReplace(loc, s"${exp1.loc.text.get} and ${exp2.loc.text.get}")))
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

      def isLibDef(defn: Def): Boolean = defn.exp.loc.source.input match {
        case Input.Text(_, _, _) => true
        case Input.TxtFile(_) => false
        case Input.PkgFile(_) => false
      }

      def isTestDef(defn: Def): Boolean = defn.spec.ann.isTest

      def mutateMap[T, E](e: E, mutF: E => LazyList[Mutant[E]], mapF: E => T, printedF: PrintedDiff => PrintedDiff = { p => p }): LazyList[Mutant[T]] =
        mutF(e).map(m => Mutant(mapF(m.value), printedF(m.printed)))

      def mutateElms[T, E](elms: List[E], mutF: E => LazyList[Mutant[E]], mapF: List[E] => T): LazyList[Mutant[T]] =
        LazyList.tabulate(elms.length)(i =>
          mutF(elms(i)).map(m => Mutant(mapF(elms.updated(i, m.value)), m.printed))
        ).flatten

      def findSig(clazz: String, sig: String)(implicit flix: Flix): Option[Symbol.SigSym] = try {
        Some(PredefinedClasses.lookupSigSym(clazz, sig, flix.getKinderAst))
      } catch {
        case _: Exception => None // todo: refactor to return info that mutant creation failed ?
      }

      def findDef(namespace: List[String], name: String)(implicit flix: Flix): Option[Symbol.DefnSym] = try {
        Some(PredefinedClasses.lookupDefSym(namespace, name, flix.getKinderAst))
      } catch {
        case _: Exception => None // todo: refactor to return info that mutant creation failed ?
      }

      def inverseAstPolarity(polarity: Polarity): Polarity =
        polarity match {
          case Polarity.Positive => Polarity.Negative
          case Polarity.Negative => Polarity.Positive
        }
    }
  }

  // todo: create MutationKind enum (?)
  private case class Mutant[+T](value: T, printed: PrintedDiff)

  private sealed trait PrintedDiff {
    def sourceLocation: SourceLocation = this match {
      case PrintedReplace(loc, _) => loc
      case PrintedRemove(loc) => loc
      case PrintedAdd(loc, _) => loc
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

  private case class PrintedReplace(loc: SourceLocation, newStr: String) extends PrintedDiff

  private case class PrintedRemove(loc: SourceLocation) extends PrintedDiff

  private case class PrintedAdd(loc: SourceLocation, newStr: String) extends PrintedDiff

  private sealed trait MutantStatus

  private case object Killed extends MutantStatus

  private case object Survived extends MutantStatus

  private case object CompilationFailed extends MutantStatus

  private case object TimedOut extends MutantStatus

  private class MutationReporter(implicit flix: Flix) {
    private var startTime: Long = 0
    private var finishTime: Long = 0
    private val all: AtomicInteger = new AtomicInteger(0)
    private val killed: AtomicInteger = new AtomicInteger(0)
    private val survived: AtomicInteger = new AtomicInteger(0)
    private val compilationFailed: AtomicInteger = new AtomicInteger(0)
    private val timedOut: AtomicInteger = new AtomicInteger(0)

    private val formatter: Formatter = flix.getFormatter
    private val lineSeparator = System.lineSeparator()
    private val verticalBar = " | "
    private val verticalBarFormatter = "%4d"
    private val outputDivider = "-" * 70

    private val source2LinesNum = new mutable.HashMap[Source, Int]

    import formatter._

    def setStartTime(time: Long = System.currentTimeMillis()): Unit =
      startTime = time

    def setFinishTime(time: Long = System.currentTimeMillis()): Unit =
      finishTime = time

    def report(status: MutantStatus, printedDiff: PrintedDiff): String = {
      val sb = new mutable.StringBuilder

      diff(sb, printedDiff)

      all.getAndIncrement()
      status match {
        case Killed =>
          killed.getAndIncrement()
          sb.append(green("KILLED"))
        case Survived =>
          survived.getAndIncrement()
          sb.append(red("SURVIVED"))
        case CompilationFailed =>
          compilationFailed.getAndIncrement()
          sb.append(yellow("COMPILATION FAILED"))
        case TimedOut =>
          timedOut.getAndIncrement()
          sb.append(yellow("TIMED OUT"))
        case _ =>
          sb.append(blue("UNKNOWN MUTANT STATUS"))
      }

      sb.append(lineSeparator)
        .append(outputDivider)

      // Todo: print and clear after each N mutants. Remember, that multiple threads can perform this function at a time
      // Look 'ProgressBar.SampleRate`
      sb.toString()
    }

    // colorize ?
    def printStats(): String = new StringBuilder()
      .append(lineSeparator)
      .append("Mutation testing:")
      .append(lineSeparator)
      .append(s"All = ${all.intValue().toString}")
      .append(s", Killed = ${killed.intValue().toString}")
      .append(s", Survived = ${survived.intValue().toString}")
      .append(s", CompilationFailed = ${compilationFailed.intValue().toString}")
      .append(s", CompilationFailed = ${timedOut.intValue().toString}")
      .append(lineSeparator)
      .append(s"Mutations score = ${f"${killed.doubleValue() / (killed.doubleValue() + survived.doubleValue()) * 100}%.2f"} %")
      .append(lineSeparator)
      .append(s"Calculated in ${(finishTime - startTime) / 1000.0} seconds")
      .toString()

    // todo: refactor hardcoded '.loc.text.get' in this file
    // todo: refactor SigSym newStr. For example, `Eq.neq(x, 5)` to `x != 5`
    // todo: move to Formatter class like Formatter.code() ??
    private def diff(sb: StringBuilder, printedDiff: PrintedDiff): Unit = {
      val sourceLocation = printedDiff.sourceLocation
      if (sourceLocation == SourceLocation.Unknown) {
        sb.append("Diff printing error: SourceLocation.Unknown was not expected")
          .append(lineSeparator)
          .append(lineSeparator)
        return
      }

      diffBefore(sb, sourceLocation)
      printedDiff match {
        case PrintedReplace(loc, newStr) => diffRemove(sb, loc); diffAdd(sb, loc, newStr)
        case PrintedRemove(loc) => diffRemove(sb, loc)
        case PrintedAdd(loc, newStr) => diffAdd(sb, loc, newStr)
      }
      diffAfter(sb, sourceLocation)

      sb.append(lineSeparator)
    }

    private def diffBefore(sb: StringBuilder, loc: SourceLocation): Unit = {
      val beginLine = loc.beginLine

      diffNumberedLine(sb, loc, beginLine - 2)
      diffNumberedLine(sb, loc, beginLine - 1)
    }

    private def diffRemove(sb: StringBuilder, loc: SourceLocation): Unit = {
      val beginLine = loc.beginLine
      val endLine = loc.endLine
      val beginCol = loc.beginCol
      val endCol = loc.endCol
      val beginL = loc.lineAt(beginLine)
      val endL = loc.lineAt(endLine)

      sb.append(red("   -"))
        .append(verticalBar)
        .append(beginL.substring(0, beginCol - 1))

      if (loc.isSingleLine) {
        sb.append(red(beginL.substring(beginCol - 1, endCol - 1)))
      } else {
        sb.append(red(beginL.substring(beginCol - 1)))

        for (l <- beginLine + 1 until endLine) {
          sb.append(lineSeparator)
            .append(red(verticalBarFormatter.format(l)))
            .append(verticalBar)
            .append(red(loc.lineAt(l)))
        }

        sb.append(lineSeparator)
          .append(red(verticalBarFormatter.format(endLine)))
          .append(verticalBar)
          .append(red(endL.substring(0, endCol - 1)))
      }

      sb.append(endL.substring(endCol - 1))
        .append(lineSeparator)
    }

    // correct display is not guaranteed for multiline str
    private def diffAdd(sb: StringBuilder, loc: SourceLocation, str: String): Unit = {
      sb.append(green("   +"))
        .append(verticalBar)
        .append(loc.lineAt(loc.beginLine).substring(0, loc.beginCol - 1))
        .append(green(str))
        .append(loc.lineAt(loc.endLine).substring(loc.endCol - 1))
        .append(lineSeparator)
    }

    private def diffAfter(sb: StringBuilder, loc: SourceLocation): Unit = {
      val endLine = loc.endLine

      diffNumberedLine(sb, loc, endLine + 1)
      diffNumberedLine(sb, loc, endLine + 2)
    }

    private def diffNumberedLine(sb: StringBuilder, loc: SourceLocation, n: Int): Unit = {
      val linesNum = source2LinesNum.getOrElseUpdate(loc.source, loc.source.data.count(c => c == '\n' || c == '\r') + 1)

      if (0 < n && n <= linesNum) {
        sb.append(verticalBarFormatter.format(n))
          .append(verticalBar)
          .append(loc.lineAt(n))
          .append(lineSeparator)
      }
    }
  }
}
