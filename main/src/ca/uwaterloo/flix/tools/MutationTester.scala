package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Ast.{Constant, Input, Polarity}
import ca.uwaterloo.flix.language.ast.SemanticOp._
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.{Body, Head}
import ca.uwaterloo.flix.language.ast.TypedAst.{Constraint, Def, Expr, Pattern, Root}
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.phase.util.PredefinedClasses
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.tools.Tester.ConsoleRedirection
import ca.uwaterloo.flix.util.{Formatter, Result}

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

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

  private sealed trait MutantStatus

  private case object Killed extends MutantStatus

  private case object Survived extends MutantStatus

  private case object CompilationFailed extends MutantStatus

  def run(root: Root)(implicit flix: Flix): Result[Unit, String] = {
    // don't want to mutate library defs and tests
    val defs = root.defs.values.filter(defn => !isLibDef(defn) && !isTestDef(defn))

    //     todo: configure by defs count (?)
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
    defs.foreach { defn =>
      // todo: move generation to separated thread or ExecutorService
      Mutator.mutateDef(defn).foreach { m =>
        val mutatedDef = m.value
        val mutant = root.copy(defs = root.defs + (mutatedDef.sym -> mutatedDef))

        //    val redirect = new ConsoleRedirection
        //    redirect.redirect()
        val codeGenResult = flix.codeGen(mutant) // todo: is it possible to parallelize it ??
        //    if (redirect.stdErr.nonEmpty) return CompilationFailed
        //    redirect.restore()

        //          executor.execute(() => {
        codeGenResult.toHardResult match {
          case Result.Ok(c) =>
            val status = testMutant(c)
            println(reporter.report(status, mutatedDef.sym.toString, m.printed))
          case Result.Err(_) =>
            println(reporter.report(CompilationFailed, mutatedDef.sym.toString, m.printed))
        }
        //          })
      }
    }

    //    executor.shutdown()
    //    while (!executor.isTerminated) {}

    reporter.setFinishTime()
    println(reporter.printStats())

    Result.Ok(())
  }

  private def isLibDef(defn: Def): Boolean = defn.exp.loc.source.input match {
    case Input.Text(_, _, _) => true
    case Input.TxtFile(_) => false
    case Input.PkgFile(_) => false
  }

  private def isTestDef(defn: Def): Boolean = defn.spec.ann.isTest

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

    def mutateDef(defn: Def)(implicit flix: Flix): LazyList[Mutation[Def]] = {
      mutateExpr(defn.exp).map(m => Mutation(defn.copy(exp = m.value), m.printed))
    }

    // todo: collections: for example, Set constructor
    private def mutateExpr(expr: Expr)(implicit flix: Flix): LazyList[Mutation[Expr]] = {
      if (expr.tpe.toString == "Unit") {
        LazyList.empty
      } else {
        expr match {
          case Expr.Cst(cst, tpe, loc) => mutateExprCst(expr)
          // TODO: think mutation vars by type. for example in def calls 'Add.add(x, y)'
          case Expr.Var(sym, tpe, loc) => LazyList.empty
          case Expr.Def(sym, tpe, loc) => mutateExprDef(expr)
          case Expr.Sig(sym, tpe, loc) => mutateExprSig(expr)
          case Expr.Hole(sym, tpe, loc) => LazyList.empty
          case Expr.HoleWithExp(exp, tpe, eff, loc) => LazyList.empty
          case Expr.OpenAs(symUse, exp, tpe, loc) => LazyList.empty
          case Expr.Use(sym, alias, exp, loc) => LazyList.empty
          case Expr.Lambda(fparam, exp, tpe, loc) => LazyList.empty
          case Expr.Apply(exp, exps, tpe, eff, loc) => mutateExprApply(expr)
          case Expr.Unary(sop, exp, tpe, eff, loc) => mutateExprUnary(expr)
          case Expr.Binary(sop, exp1, exp2, tpe, eff, loc) => mutateExprBinary(expr)
          case Expr.Let(sym, mod, exp1, exp2, tpe, eff, loc) =>
            mutateExpr(exp1).map(m => Mutation(Expr.Let(sym, mod, m.value, exp2, tpe, eff, loc), m.printed)) #:::
              mutateExpr(exp2).map(m => Mutation(Expr.Let(sym, mod, exp1, m.value, tpe, eff, loc), m.printed))
          case Expr.LetRec(sym, ann, mod, exp1, exp2, tpe, eff, loc) => // find example code
            mutateExpr(exp1).map(m => Mutation(Expr.LetRec(sym, ann, mod, m.value, exp2, tpe, eff, loc), m.printed)) #:::
              mutateExpr(exp2).map(m => Mutation(Expr.LetRec(sym, ann, mod, exp1, m.value, tpe, eff, loc), m.printed))
          case Expr.Region(tpe, loc) => LazyList.empty
          case Expr.Scope(sym, regionVar, exp, tpe, eff, loc) =>
            mutateExpr(exp).map(m => Mutation(Expr.Scope(sym, regionVar, m.value, tpe, eff, loc), m.printed))
          case Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
            mutateExpr(exp1).map(m => Mutation(Expr.IfThenElse(m.value, exp2, exp3, tpe, eff, loc), m.printed)) #:::
              mutateExpr(exp2).map(m => Mutation(Expr.IfThenElse(exp1, m.value, exp3, tpe, eff, loc), m.printed)) #:::
              mutateExpr(exp3).map(m => Mutation(Expr.IfThenElse(exp1, exp2, m.value, tpe, eff, loc), m.printed))
          case Expr.Stm(exp1, exp2, tpe, eff, loc) =>
            mutateExpr(exp1).map(m => Mutation(Expr.Stm(m.value, exp2, tpe, eff, loc), m.printed)) #:::
              mutateExpr(exp2).map(m => Mutation(Expr.Stm(exp1, m.value, tpe, eff, loc), m.printed))
          case Expr.Discard(exp, eff, loc) => LazyList.empty
          case Expr.Match(exp, rules, tpe, eff, loc) => LazyList.empty
          case Expr.TypeMatch(exp, rules, tpe, eff, loc) => LazyList.empty
          case Expr.RestrictableChoose(star, exp, rules, tpe, eff, loc) => LazyList.empty
          case Expr.Tag(sym, exp, tpe, eff, loc) => LazyList.empty
          case Expr.RestrictableTag(sym, exp, tpe, eff, loc) => LazyList.empty
          case Expr.Tuple(elms, tpe, eff, loc) => mutateExprTuple(expr)
          case Expr.RecordEmpty(tpe, loc) => LazyList.empty
          case Expr.RecordSelect(exp, label, tpe, eff, loc) => LazyList.empty
          case Expr.RecordExtend(label, exp1, exp2, tpe, eff, loc) => LazyList.empty
          case Expr.RecordRestrict(label, exp, tpe, eff, loc) => LazyList.empty
          case Expr.ArrayLit(exps, exp, tpe, eff, loc) => LazyList.empty
          case Expr.ArrayNew(exp1, exp2, exp3, tpe, eff, loc) => LazyList.empty
          case Expr.ArrayLoad(exp1, exp2, tpe, eff, loc) => LazyList.empty
          case Expr.ArrayLength(exp, eff, loc) => LazyList.empty
          case Expr.ArrayStore(exp1, exp2, exp3, eff, loc) => LazyList.empty
          case Expr.VectorLit(exps, tpe, eff, loc) => LazyList.empty
          case Expr.VectorLoad(exp1, exp2, tpe, eff, loc) => LazyList.empty
          case Expr.VectorLength(exp, loc) => LazyList.empty
          case Expr.Ref(exp1, exp2, tpe, eff, loc) => LazyList.empty // Todo
          case Expr.Deref(exp, tpe, eff, loc) => LazyList.empty // Todo
          case Expr.Assign(exp1, exp2, tpe, eff, loc) => LazyList.empty
          case Expr.Ascribe(exp, tpe, eff, loc) => LazyList.empty
          case Expr.InstanceOf(exp, clazz, loc) => LazyList.empty
          case Expr.CheckedCast(cast, exp, tpe, eff, loc) => LazyList.empty
          case Expr.UncheckedCast(exp, declaredType, declaredEff, tpe, eff, loc) => LazyList.empty
          case Expr.UncheckedMaskingCast(exp, tpe, eff, loc) => LazyList.empty
          case Expr.Without(exp, effUse, tpe, eff, loc) => LazyList.empty
          case Expr.TryCatch(exp, rules, tpe, eff, loc) => LazyList.empty // Todo
          case Expr.TryWith(exp, effUse, rules, tpe, eff, loc) => LazyList.empty // Todo
          case Expr.Do(op, exps, tpe, eff, loc) => LazyList.empty
          case Expr.InvokeConstructor(constructor, exps, tpe, eff, loc) => LazyList.empty
          case Expr.InvokeMethod(method, exp, exps, tpe, eff, loc) => LazyList.empty
          case Expr.InvokeStaticMethod(method, exps, tpe, eff, loc) => LazyList.empty
          case Expr.GetField(field, exp, tpe, eff, loc) => LazyList.empty
          case Expr.PutField(field, exp1, exp2, tpe, eff, loc) => LazyList.empty
          case Expr.GetStaticField(field, tpe, eff, loc) => LazyList.empty
          case Expr.PutStaticField(field, exp, tpe, eff, loc) => LazyList.empty
          case Expr.NewObject(name, clazz, tpe, eff, methods, loc) => LazyList.empty
          case Expr.NewChannel(exp1, exp2, tpe, eff, loc) => LazyList.empty
          case Expr.GetChannel(exp, tpe, eff, loc) => LazyList.empty
          case Expr.PutChannel(exp1, exp2, tpe, eff, loc) => LazyList.empty
          case Expr.SelectChannel(rules, default, tpe, eff, loc) => LazyList.empty
          case Expr.Spawn(exp1, exp2, tpe, eff, loc) => LazyList.empty
          case Expr.ParYield(frags, exp, tpe, eff, loc) => LazyList.empty
          case Expr.Lazy(exp, tpe, loc) => LazyList.empty
          case Expr.Force(exp, tpe, eff, loc) => LazyList.empty

          case Expr.FixpointConstraintSet(cs, tpe, loc) => mutateExprFixpointConstraintSet(expr)
          case Expr.FixpointLambda(pparams, exp, tpe, eff, loc) => LazyList.empty // find example
          case Expr.FixpointMerge(exp1, exp2, tpe, eff, loc) => LazyList.empty
          case Expr.FixpointSolve(exp, tpe, eff, loc) => LazyList.empty
          case Expr.FixpointFilter(pred, exp, tpe, eff, loc) => LazyList.empty
          case Expr.FixpointInject(exp, pred, tpe, eff, loc) => LazyList.empty
          case Expr.FixpointProject(pred, exp, tpe, eff, loc) => LazyList.empty

          case Expr.Error(m, tpe, eff) => LazyList.empty
        }
      }
    }

    private def mutateExprCst(expr: Expr): LazyList[Mutation[Expr]] = expr match {
      case Expr.Cst(cst, tpe, loc) => mutateConstant(cst).map(p => Mutation(Expr.Cst(p.value, tpe, loc), p.printed.mapLoc(loc)))
      case _ => LazyList.empty
    }

    private def mutateConstant(cst: Constant): LazyList[Mutation[Constant]] = cst match {
      case Constant.Bool(lit) => LazyList(Mutation(Constant.Bool(!lit), PrintedReplace(SourceLocation.Unknown, (!lit).toString)))
      case Constant.Float32(lit) => LazyList(lit + 1, lit - 1).map(value => Mutation(Constant.Float32(value), PrintedReplace(SourceLocation.Unknown, s"${value.toString}f32")))
      case Constant.Float64(lit) => LazyList(lit + 1, lit - 1).map(value => Mutation(Constant.Float64(value), PrintedReplace(SourceLocation.Unknown, value.toString)))
      case Constant.BigDecimal(lit) => LazyList(
        lit.add(java.math.BigDecimal.ONE),
        lit.subtract(java.math.BigDecimal.ONE)
      ).map(value => Mutation(Constant.BigDecimal(value), PrintedReplace(SourceLocation.Unknown, s"${value.toString}ff")))
      case Constant.Int8(lit) => LazyList(lit + 1, lit - 1).map(value => Mutation(Constant.Int8(value.toByte), PrintedReplace(SourceLocation.Unknown, s"${value.toString}i8")))
      case Constant.Int16(lit) => LazyList(lit + 1, lit - 1).map(value => Mutation(Constant.Int16(value.toShort), PrintedReplace(SourceLocation.Unknown, s"${value.toString}i16")))
      case Constant.Int32(lit) => LazyList(lit + 1, lit - 1).map(value => Mutation(Constant.Int32(value), PrintedReplace(SourceLocation.Unknown, value.toString)))
      case Constant.Int64(lit) => LazyList(lit + 1, lit - 1).map(value => Mutation(Constant.Int64(value), PrintedReplace(SourceLocation.Unknown, s"${value.toString}i64")))
      case Constant.BigInt(lit) => LazyList(
        lit.add(java.math.BigInteger.ONE),
        lit.subtract(java.math.BigInteger.ONE)
      ).map(value => Mutation(Constant.BigInt(value), PrintedReplace(SourceLocation.Unknown, s"${value.toString}ii")))
      case Constant.Str(lit) =>
        val pair = if (lit != "") ("", PrintedRemove(SourceLocation.Unknown)) else ("Flix", PrintedAdd(SourceLocation.Unknown, "\"Flix\""))
        LazyList(Mutation(Constant.Str(pair._1), pair._2))
      case _ => LazyList.empty
    }

    private def mutateExprDef(expr: Expr)(implicit flix: Flix): LazyList[Mutation[Expr]] = expr match {
      case Expr.Def(sym, tpe, loc) if defnIntNamespaces.contains(sym.namespace.mkString(".")) =>
        // todo: refactor to imperative
        defnToDefn.get(sym.text)
          .flatMap(findDef(sym.namespace, _))
          .map(s => LazyList(Mutation(Expr.Def(s, tpe, loc), PrintedReplace(loc, s.toString))))
          .orElse {
            defnToSig.get(sym.text)
              .flatMap(p => findSig(p._1, p._2))
              .map(s => LazyList(Mutation(Expr.Sig(s, tpe, loc), PrintedReplace(loc, s.toString))))
          }
          .getOrElse(LazyList.empty)
      case _ => LazyList.empty
    }

    private def mutateExprSig(expr: Expr)(implicit flix: Flix): LazyList[Mutation[Expr]] = expr match {
      case Expr.Sig(sym, tpe, loc) =>
        // todo: refactor to imperative
        var l = List(conditionalNegateSigToSig, conditionalBoundarySigToSig)
        if (arithmeticSigTypes.contains(tpe.toString)) {
          l = arithmeticSigToSig :: l
        }

        l.foldLeft(LazyList.empty[Mutation[Expr]]) {
          (acc, sigMap) =>
            acc #::: sigMap.get(sym.toString)
              .flatMap(p => findSig(p._1, p._2))
              .map(s => LazyList(Mutation(Expr.Sig(s, tpe, loc), PrintedReplace(loc, s.toString))))
              .getOrElse(LazyList.empty)
        }
      case _ => LazyList.empty
    }

    // what if there is an expression other than Def and Sig ?
    // take Apply's SourceLocation and refactor Mutation.newStr creating
    private def mutateExprApply(expr: TypedAst.Expr)(implicit flix: Flix): LazyList[Mutation[Expr]] = expr match {
      case Expr.Apply(exp, exps, tpe, eff, loc) =>
        exp match {
          case Expr.Sig(sym, _, _) if sym.toString == "Neg.neg" =>
            LazyList(Mutation(exps.head, PrintedReplace(loc, exps.head.loc.text.get)))
          case Expr.Def(sym, _, _) if defnIntNamespaces.contains(sym.namespace.mkString(".")) && sym.text == "bitwiseNot" =>
            LazyList(Mutation(exps.head, PrintedReplace(loc, exps.head.loc.text.get)))
          case _ =>
            val res = Mutator.mutateExpr(exp).map { m =>
              val printedDiff = m.printed.mapStr(s => s"$s${exps.map(_.loc.text.get).mkString("(", ", ", ")")}").mapLoc(loc)
              Mutation(Expr.Apply(m.value, exps, tpe, eff, loc), printedDiff)
            }

            if (res.nonEmpty) res
            else LazyList.tabulate(exps.length)(i =>
              Mutator.mutateExpr(exps(i)).map { m =>
                val updatedExps = exps.updated(i, m.value)
                Mutation(Expr.Apply(exp, updatedExps, tpe, eff, loc), m.printed)
              }
            ).flatten
        }
      case _ => LazyList.empty
    }

    private def mutateExprUnary(expr: Expr): LazyList[Mutation[Expr]] = expr match {
      case Expr.Unary(BoolOp.Not, exp, _, _, loc) => LazyList(Mutation(exp, PrintedReplace(loc, exp.loc.text.get)))
      case _ => LazyList.empty
    }

    private def mutateExprBinary(expr: Expr): LazyList[Mutation[Expr]] = expr match {
      case Expr.Binary(BoolOp.And, exp1, exp2, tpe, eff, loc) =>
        LazyList(Mutation(Expr.Binary(BoolOp.Or, exp1, exp2, tpe, eff, loc), PrintedReplace(loc, s"${exp1.loc.text.get} or ${exp2.loc.text.get}")))
      case Expr.Binary(BoolOp.Or, exp1, exp2, tpe, eff, loc) =>
        LazyList(Mutation(Expr.Binary(BoolOp.And, exp1, exp2, tpe, eff, loc), PrintedReplace(loc, s"${exp1.loc.text.get} and ${exp2.loc.text.get}")))
      case _ => LazyList.empty
    }

    private def mutateExprTuple(expr: Expr)(implicit flix: Flix): LazyList[Mutation[Expr]] = expr match {
      case Expr.Tuple(elms, tpe, eff, loc) =>
        LazyList.tabulate(elms.length)(i =>
          mutateExpr(elms(i)).map(m => Mutation(Expr.Tuple(elms.updated(i, m.value), tpe, eff, loc), m.printed))
        ).flatten
      case _ => LazyList.empty
    }

    private def mutateExprFixpointConstraintSet(expr: Expr)(implicit flix: Flix): LazyList[Mutation[Expr]] =
      expr match {
        case Expr.FixpointConstraintSet(cs, tpe, loc1) =>
          val csWithIndex = cs.zipWithIndex
          LazyList.from(csWithIndex).flatMap { case (constraint, i) =>
            // remove constraint
            val removedCS = csWithIndex.filterNot(_._2 == i).map(_._1)
            val removedExpr = Expr.FixpointConstraintSet(removedCS, tpe, loc1)
            val removedMutation = Mutation(removedExpr, PrintedRemove(constraint.loc))

            // mutate constraint
            val constraintMutations = mutateConstraint(constraint).map { m =>
              val updatedCS: List[Constraint] = cs.updated(i, m.value)
              val updatedExpr = Expr.FixpointConstraintSet(updatedCS, tpe, loc1)
              Mutation(updatedExpr, m.printed)
            }

            removedMutation #:: constraintMutations
          }
        case _ => LazyList.empty
      }

    private def mutateConstraint(constraint: Constraint)(implicit flix: Flix): LazyList[Mutation[Constraint]] = {
      // mutate head
      val headMutations = mutatePredicateHead(constraint.head).map { m =>
        Mutation(Constraint(constraint.cparams, m.value, constraint.body, constraint.loc), m.printed)
      }

      // mutate body
      val bodiesMutations = mutatePredicateBodyList(constraint.body).map { m =>
        Mutation(Constraint(constraint.cparams, constraint.head, m.value, constraint.loc), m.printed)
      }

      headMutations #::: bodiesMutations
    }

    private def mutatePredicateHead(head: Head)(implicit flix: Flix): LazyList[Mutation[Head]] = head match {
      case Head.Atom(pred, den, terms, tpe, loc) =>
        // mutate terms
        LazyList.from(terms.zipWithIndex).flatMap { case (exp, i) =>
          mutateExpr(exp).map { m =>
            val updatedTerms = terms.updated(i, m.value)
            val updatedHeadAtom = Head.Atom(pred, den, updatedTerms, tpe, loc)
            Mutation(updatedHeadAtom, m.printed)
          }
        }
    }

    private def mutatePredicateBodyList(bodyList: List[Body])(implicit flix: Flix): LazyList[Mutation[List[Body]]] = {
      val bodyListWithIndex = bodyList.zipWithIndex
      LazyList.from(bodyListWithIndex).flatMap { case (body, i) =>
        // remove item
        val removedMutation = Mutation(bodyListWithIndex.filterNot(_._2 == i).map(_._1), PrintedRemove(body.loc))

        // mutate item
        val bodyMutations = mutatePredicateBody(body).map { m =>
          Mutation(bodyList.updated(i, m.value), m.printed)
        }

        removedMutation #:: bodyMutations
      }
    }

    private def mutatePredicateBody(body: Body)(implicit flix: Flix): LazyList[Mutation[Body]] = body match {
      case Body.Atom(pred, den, polarity, fixity, terms, tpe, loc) =>
        // inverse polarity
        val inversedPolarity = inverseAstPolarity(polarity)
        val inversedBodyAtom = Body.Atom(pred, den, inversedPolarity, fixity, terms, tpe, loc)
        val printedDiff = inversedPolarity match {
          case Polarity.Positive =>
            val inversedLoc = loc.copy(endLine = loc.beginLine, endCol = loc.beginCol + 4)
            PrintedRemove(inversedLoc)
          case Polarity.Negative =>
            val inversedLoc = loc.copy(endLine = loc.beginLine, endCol = loc.beginCol)
            PrintedAdd(inversedLoc, "not ")
        }
        val inversedMutation = Mutation(inversedBodyAtom, printedDiff)

        // mutate terms
        val termsMutations = LazyList.from(terms.zipWithIndex).flatMap { case (pattern, i) =>
          mutatePattern(pattern).map { m =>
            val updatedTerms = terms.updated(i, m.value)
            val updatedBodyAtom = Body.Atom(pred, den, polarity, fixity, updatedTerms, tpe, loc)
            Mutation(updatedBodyAtom, m.printed)
          }
        }

        inversedMutation #:: termsMutations
      case Body.Functional(outVars, exp, loc) =>
        mutateExpr(exp).map(m => Mutation(Body.Functional(outVars, m.value, loc), m.printed))
      case Body.Guard(exp, loc) =>
        mutateExpr(exp).map(m => Mutation(Body.Guard(m.value, loc), m.printed))
    }

    private def mutatePattern(pattern: Pattern): LazyList[Mutation[Pattern]] =
      pattern match {
        case Pattern.Cst(cst, tpe, loc) => mutateConstant(cst).map(m => Mutation(Pattern.Cst(m.value, tpe, loc), m.printed.mapLoc(loc)))
        case _ => LazyList.empty
      }

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
  private case class Mutation[+T](value: T, printed: PrintedDiff)

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

  private class MutationReporter(implicit flix: Flix) {
    private var startTime: Long = 0
    private var finishTime: Long = 0
    private val all: AtomicInteger = new AtomicInteger(0)
    private val killed: AtomicInteger = new AtomicInteger(0)
    private val survived: AtomicInteger = new AtomicInteger(0)
    private val compilationFailed: AtomicInteger = new AtomicInteger(0)

    private val formatter: Formatter = flix.getFormatter
    private val verticalBar = " | "
    private val verticalBarFormatter = "%4d"
    private val outputDivider = "-" * 70

    import formatter._

    def setStartTime(time: Long = System.currentTimeMillis()): Unit =
      startTime = time

    def setFinishTime(time: Long = System.currentTimeMillis()): Unit =
      finishTime = time

    def report(status: MutantStatus, defName: String, printedDiff: PrintedDiff): String = {
      val sb = new mutable.StringBuilder

      sb.append(s"Mutant created for ${blue(defName)}")
        .append(System.lineSeparator())
        .append(System.lineSeparator())

      val loc = printedDiff.sourceLocation
      diffBefore(sb, loc)
      if (loc != SourceLocation.Unknown) {
        diff(sb, printedDiff)
      } else {
        sb.append("Diff printing error: SourceLocation.Unknown was not expected")
          .append(System.lineSeparator())
      }
      diffAfter(sb, loc)

      sb.append(System.lineSeparator())

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
      }

      sb.append(System.lineSeparator())
        .append(outputDivider)

      // Todo: print and clear after each N mutants. Remember, that multiple threads can perform this function at a time
      sb.toString()
    }

    // colorize ?
    def printStats(): String = new StringBuilder()
      .append(System.lineSeparator())
      .append("Mutation testing:")
      .append(System.lineSeparator())
      .append(s"All = ${all.intValue().toString}")
      .append(s", Killed = ${killed.intValue().toString}")
      .append(s", Survived = ${survived.intValue().toString}")
      .append(s", CompilationFailed = ${compilationFailed.intValue().toString}")
      .append(System.lineSeparator())
      .append(s"Mutations score = ${f"${killed.doubleValue() / all.doubleValue() * 100}%.2f"} %")
      .append(System.lineSeparator())
      .append(s"Calculated in ${(finishTime - startTime) / 1000.0} seconds")
      .toString()

    // todo: refactor SigSym newStr. For example, `Eq.neq(x, 5)` to `x != 5`
    // todo: refactor hardcoded '.loc.text.get' in this file
    // todo: refactor for multiline. Example: examples/larger-examples/Reachable.flix
    // todo: move to Formatter class like Formatter.code() ??
    // todo: `9 |` and `10 | `
    private def diff(sb: StringBuilder, printedDiff: PrintedDiff): Unit = printedDiff match {
      case PrintedReplace(loc, newStr) => diffRemove(sb, loc); diffAdd(sb, loc, newStr)
      case PrintedRemove(loc) => diffRemove(sb, loc)
      case PrintedAdd(loc, newStr) => diffAdd(sb, loc, newStr)
    }

    private def diffBefore(sb: StringBuilder, loc: SourceLocation): Unit = {
      val beginLine = loc.beginLine

      if (beginLine - 2 > 0) {
        sb.append(verticalBarFormatter.format(beginLine - 2))
          .append(verticalBar)
          .append(loc.lineAt(beginLine - 2))
          .append(System.lineSeparator())
      }

      if (beginLine - 1 > 0) {
        sb.append(verticalBarFormatter.format(beginLine - 1))
          .append(verticalBar)
          .append(loc.lineAt(beginLine - 1))
          .append(System.lineSeparator())
      }
    }

    private def diffRemove(sb: StringBuilder, loc: SourceLocation): Unit = {
      val beginLine = loc.beginLine
      val beginCol = loc.beginCol
      val endCol = loc.endCol

      if (loc.isSingleLine) {
        val line = loc.lineAt(beginLine)

        sb.append(red(verticalBarFormatter.format(beginLine)))
          .append(verticalBar)
          .append(line.substring(0, beginCol - 1))
          .append(red(line.substring(beginCol - 1, endCol - 1)))
          .append(line.substring(endCol - 1))
          .append(System.lineSeparator())
      } else {
        val endLine = loc.endLine
        val beginL = loc.lineAt(loc.beginLine)
        val endL = loc.lineAt(loc.endLine)

        sb.append(red(verticalBarFormatter.format(beginLine)))
          .append(verticalBar)
          .append(beginL.substring(0, beginCol - 1))
          .append(red(beginL.substring(beginCol - 1)))

        for (l <- beginLine + 1 until endLine) {
          sb.append(System.lineSeparator())
            .append(red(verticalBarFormatter.format(l)))
            .append(verticalBar)
            .append(red(loc.lineAt(l)))
        }

        sb.append(System.lineSeparator())
          .append(red(verticalBarFormatter.format(endLine)))
          .append(verticalBar)
          .append(red(endL.substring(0, endCol - 1)))
          .append(endL.substring(endCol - 1))
          .append(System.lineSeparator())
      }
    }

    // correct display is not guaranteed for multiline strings
    private def diffAdd(sb: StringBuilder, loc: SourceLocation, str: String): Unit = {
      val beginLine = loc.beginLine
      val beginCol = loc.beginCol
      val endCol = loc.endCol

      if (loc.isSingleLine) {
        val line = loc.lineAt(beginLine)

        sb.append(green(verticalBarFormatter.format(beginLine)))
          .append(verticalBar)
          .append(line.substring(0, beginCol - 1))
          .append(green(str))
          .append(line.substring(endCol - 1))
          .append(System.lineSeparator())
      } else {
        val endLine = loc.endLine
        val beginL = loc.lineAt(beginLine)
        val endL = loc.lineAt(endLine)

        sb.append(green("   +"))
          .append(verticalBar)
          .append(beginL.substring(0, beginCol - 1))
          .append(green(str))
          .append(endL.substring(endCol - 1))
          .append(System.lineSeparator())
      }
    }

    private def diffAfter(sb: StringBuilder, loc: SourceLocation): Unit = {
      val endLine = loc.endLine

      if (endLine + 1 > 0) {
        sb.append(verticalBarFormatter.format(endLine + 1))
          .append(verticalBar)
          .append(loc.lineAt(endLine + 1))
          .append(System.lineSeparator())
      }

      if (endLine + 2 > 0) {
        sb.append(verticalBarFormatter.format(endLine + 2))
          .append(verticalBar)
          .append(loc.lineAt(endLine + 2))
          .append(System.lineSeparator())
      }
    }
  }
}
