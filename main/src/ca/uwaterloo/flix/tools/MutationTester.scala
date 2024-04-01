package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Ast.{Constant, Input}
import ca.uwaterloo.flix.language.ast.SemanticOp._
import ca.uwaterloo.flix.language.ast.TypedAst.{Def, Expr, Root}
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

  def run(root: Root)(implicit flix: Flix): Result[(MutationReporter, Double), Int] = {
    // don't want to mutate library defs and tests
    val defs = root.defs.values.filter(defn => !isLibDef(defn) && !isTestDef(defn))

    //     todo: configure by defs count (?)
    //    val numThreads = 2
    //    val executor = Executors.newFixedThreadPool(numThreads)

    val reporter = new MutationReporter()
    val startTime = System.currentTimeMillis()

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
    defs.foreach {
      defn =>
        // todo: move generation to separated thread or ExecutorService
        Mutator.mutateExpr(defn.exp).foreach { mutation =>
          val mutatedDef = defn.copy(exp = mutation.expr)
          val mutant = root.copy(defs = root.defs + (mutatedDef.sym -> mutatedDef))

          //    val redirect = new ConsoleRedirection
          //    redirect.redirect()
          val codeGenResult = flix.codeGen(mutant) // todo: is it possible to parallelize it ??
          //    if (redirect.stdErr.nonEmpty) return CompilationFailed
          //    redirect.restore()

          //          executor.execute(() => {
          codeGenResult.toSoftResult match {
            case Result.Ok(c) =>
              val status = testMutant(c._1)
              reporter.report(status, mutatedDef.sym.toString, mutation.loc, mutation.newStr)
            case Result.Err(_) => reporter.report(CompilationFailed, mutatedDef.sym.toString, mutation.loc, mutation.newStr)
          }
          //          })
        }
    }

    //    executor.shutdown()
    //    while (!executor.isTerminated) {}

    val finishTime = System.currentTimeMillis()
    val elapsedTimeInSeconds = (finishTime - startTime) / 1000.0

    Result.Ok(reporter, elapsedTimeInSeconds)
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

    // todo: collections: for example, Set constructor
    def mutateExpr(expr: Expr)(implicit flix: Flix): LazyList[Mutation] = {
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
            mutateExpr(exp1).map(m => Mutation(Expr.Let(sym, mod, m.expr, exp2, tpe, eff, loc), m.loc, m.newStr)) #:::
              mutateExpr(exp2).map(m => Mutation(Expr.Let(sym, mod, exp1, m.expr, tpe, eff, loc), m.loc, m.newStr))
          case Expr.LetRec(sym, ann, mod, exp1, exp2, tpe, eff, loc) => // find example code
            mutateExpr(exp1).map(m => Mutation(Expr.LetRec(sym, ann, mod, m.expr, exp2, tpe, eff, loc), m.loc, m.newStr)) #:::
              mutateExpr(exp2).map(m => Mutation(Expr.LetRec(sym, ann, mod, exp1, m.expr, tpe, eff, loc), m.loc, m.newStr))
          case Expr.Region(tpe, loc) => LazyList.empty
          case Expr.Scope(sym, regionVar, exp, tpe, eff, loc) =>
            mutateExpr(exp).map(m => Mutation(Expr.Scope(sym, regionVar, m.expr, tpe, eff, loc), m.loc, m.newStr))
          case Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
            mutateExpr(exp1).map(m => Mutation(Expr.IfThenElse(m.expr, exp2, exp3, tpe, eff, loc), m.loc, m.newStr)) #:::
              mutateExpr(exp2).map(m => Mutation(Expr.IfThenElse(exp1, m.expr, exp3, tpe, eff, loc), m.loc, m.newStr)) #:::
              mutateExpr(exp3).map(m => Mutation(Expr.IfThenElse(exp1, exp2, m.expr, tpe, eff, loc), m.loc, m.newStr))
          case Expr.Stm(exp1, exp2, tpe, eff, loc) =>
            mutateExpr(exp1).map(m => Mutation(Expr.Stm(m.expr, exp2, tpe, eff, loc), m.loc, m.newStr)) #:::
              mutateExpr(exp2).map(m => Mutation(Expr.Stm(exp1, m.expr, tpe, eff, loc), m.loc, m.newStr))
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
          case Expr.FixpointInject(exp, pred, tpe, eff, loc) => LazyList.empty // todo: think
          case Expr.FixpointProject(pred, exp, tpe, eff, loc) => LazyList.empty

          case Expr.Error(m, tpe, eff) => LazyList.empty
        }
      }
    }

    private def mutateExprCst(exp: Expr): LazyList[Mutation] = exp match {
      case Expr.Cst(cst, tpe, loc) => mutateConstant(cst).map(p => Mutation(Expr.Cst(p._1, tpe, loc), loc, p._2))
      case _ => LazyList.empty
    }

    private def mutateConstant(cst: Constant): LazyList[(Constant, String)] = cst match {
      case Constant.Bool(lit) => LazyList((Constant.Bool(!lit), (!lit).toString))
      case Constant.Float32(lit) => LazyList(lit + 1, lit - 1).map(value => (Constant.Float32(value), s"${value.toString}f32"))
      case Constant.Float64(lit) => LazyList(lit + 1, lit - 1).map(value => (Constant.Float64(value), s"${value.toString}f64"))
      case Constant.BigDecimal(lit) => LazyList(
        lit.add(java.math.BigDecimal.ONE),
        lit.subtract(java.math.BigDecimal.ONE)
      ).map(value => (Constant.BigDecimal(value), s"${value.toString}ff"))
      case Constant.Int8(lit) => LazyList(lit + 1, lit - 1).map(value => (Constant.Int8(value.toByte), s"${value.toString}i8"))
      case Constant.Int16(lit) => LazyList(lit + 1, lit - 1).map(value => (Constant.Int16(value.toShort), s"${value.toString}i16"))
      case Constant.Int32(lit) => LazyList(lit + 1, lit - 1).map(value => (Constant.Int32(value), s"${value.toString}i32"))
      case Constant.Int64(lit) => LazyList(lit + 1, lit - 1).map(value => (Constant.Int64(value), s"${value.toString}i64"))
      case Constant.BigInt(lit) => LazyList(
        lit.add(java.math.BigInteger.ONE),
        lit.subtract(java.math.BigInteger.ONE)
      ).map(value => (Constant.BigInt(value), s"${value.toString}ii"))
      case _ => LazyList.empty
    }

    private def mutateExprDef(exp: Expr)(implicit flix: Flix): LazyList[Mutation] = exp match {
      case Expr.Def(sym, tpe, loc) if defnIntNamespaces.contains(sym.namespace.mkString(".")) =>
        defnToDefn.get(sym.text)
          .flatMap(findDef(sym.namespace, _))
          .map(s => LazyList(Mutation(Expr.Def(s, tpe, loc), loc, s.toString)))
          .orElse {
            defnToSig.get(sym.text)
              .flatMap(p => findSig(p._1, p._2))
              .map(s => LazyList(Mutation(Expr.Sig(s, tpe, loc), loc, s.toString)))
          }
          .getOrElse(LazyList.empty)
      case _ => LazyList.empty
    }

    private def mutateExprSig(exp: Expr)(implicit flix: Flix): LazyList[Mutation] = exp match {
      case Expr.Sig(sym, tpe, loc) =>
        var l = List(conditionalNegateSigToSig, conditionalBoundarySigToSig)
        if (arithmeticSigTypes.contains(tpe.toString)) {
          l = arithmeticSigToSig :: l
        }

        l.foldLeft(LazyList.empty[Mutation]) {
          (acc, sigMap) =>
            acc #::: sigMap.get(sym.toString)
              .flatMap(p => findSig(p._1, p._2))
              .map(s => LazyList(Mutation(Expr.Sig(s, tpe, loc), loc, s.toString)))
              .getOrElse(LazyList.empty)
        }
      case _ => LazyList.empty
    }

    // what if there is an expression other than Def and Sig ?
    // take Apply's SourceLocation and refactor Mutation.newStr creating
    private def mutateExprApply(exp: TypedAst.Expr)(implicit flix: Flix): LazyList[Mutation] = exp match {
      case Expr.Apply(exp, exps, tpe, eff, loc) =>
        exp match {
          case Expr.Sig(sym, _, _) if sym.toString == "Neg.neg" => LazyList(Mutation(exps.head, loc, exps.head.loc.text.get))
          case Expr.Def(sym, _, _) if defnIntNamespaces.contains(sym.namespace.mkString(".")) && sym.text == "bitwiseNot" => LazyList(Mutation(exps.head, loc, exps.head.loc.text.get))
          case _ =>
            val res = Mutator.mutateExpr(exp).map { m =>
              val str = s"${m.newStr}${exps.map(_.loc.text.get).mkString("(", ", ", ")")}"
              Mutation(Expr.Apply(m.expr, exps, tpe, eff, loc), loc, str)
            }

            if (res.nonEmpty) res
            else LazyList.tabulate(exps.length)(i =>
              Mutator.mutateExpr(exps(i)).map { m =>
                val updatedExps = exps.updated(i, m.expr)
                val str = s"${exp.loc.text.get}${updatedExps.map(_.loc.text.get).updated(i, m.newStr).mkString("(", ", ", ")")}"
                Mutation(Expr.Apply(exp, updatedExps, tpe, eff, loc), loc, str)
              }
            ).flatten
        }
      case _ => LazyList.empty
    }

    private def mutateExprUnary(exp: Expr): LazyList[Mutation] = exp match {
      case Expr.Unary(BoolOp.Not, exp, _, _, loc) => LazyList(Mutation(exp, loc, exp.loc.text.get))
      case _ => LazyList.empty
    }

    private def mutateExprBinary(exp: Expr): LazyList[Mutation] = exp match {
      case Expr.Binary(BoolOp.And, exp1, exp2, tpe, eff, loc) => LazyList(Mutation(Expr.Binary(BoolOp.Or, exp1, exp2, tpe, eff, loc), loc, s"${exp1.loc.text.get} or ${exp2.loc.text.get}"))
      case Expr.Binary(BoolOp.Or, exp1, exp2, tpe, eff, loc) => LazyList(Mutation(Expr.Binary(BoolOp.And, exp1, exp2, tpe, eff, loc), loc, s"${exp1.loc.text.get} and ${exp2.loc.text.get}"))
      case _ => LazyList.empty
    }

    private def mutateExprTuple(exp: Expr)(implicit flix: Flix): LazyList[Mutation] = exp match {
      case Expr.Tuple(elms, tpe, eff, loc) =>
        LazyList.tabulate(elms.length)(i =>
          mutateExpr(elms(i)).map(m => Mutation(Expr.Tuple(elms.updated(i, m.expr), tpe, eff, loc), m.loc, m.newStr))
        ).flatten
      case _ => LazyList.empty
    }

    /* todo:
     * check Atom.Denotation: Relational or Latticenal
     *
     * there needs mutate only FixpointConstraintSet
     * or also mutate injected structure in FixpointInject (?)
     * or rename injected pred.Name (eq deletion inject)
     *
     * for Predicate.Body.Functional and Predicate.Body.Guard mutate inner exp
     */
    private def mutateExprFixpointConstraintSet(exp: Expr)(implicit flix: Flix): LazyList[Mutation] = exp match {
      case Expr.FixpointConstraintSet(cs, tpe, loc) =>
        LazyList.empty
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
    }
  }

  // create MutationKind enum
  private case class Mutation(expr: Expr, loc: SourceLocation, newStr: String)

  class MutationReporter(implicit flix: Flix) {
    private val all: AtomicInteger = new AtomicInteger(0)
    private val killed: AtomicInteger = new AtomicInteger(0)
    private val survived: AtomicInteger = new AtomicInteger(0)
    private val compilationFailed: AtomicInteger = new AtomicInteger(0)
    private val formatter: Formatter = flix.getFormatter

    import formatter._

    def report(status: MutantStatus, defName: String, loc: SourceLocation, newStr: String): Unit = {
      val sb = new mutable.StringBuilder

      sb.append(s"Mutant created for ${blue(defName)}${System.lineSeparator()}")
        .append(diff(loc, newStr))

      all.getAndIncrement()
      status match {
        case Killed =>
          killed.getAndIncrement()
          sb.append(green("KILLED") + System.lineSeparator())
        case Survived =>
          survived.getAndIncrement()
          sb.append(red("SURVIVED") + System.lineSeparator())
        case CompilationFailed =>
          compilationFailed.getAndIncrement()
          sb.append(yellow("COMPILATION FAILED") + System.lineSeparator())
      }

      sb.append("-" * 50)

      // Todo: print and clear after each N mutants. Remember, that multiple threads can perform this function at a time
      println(sb.toString())
    }

    def printStats(): String = new StringBuilder()
      .append(blue("Mutation testing:"))
      .append(System.lineSeparator())
      .append(blue(s"All = $all, Killed = $killed, Survived = $survived, CompilationFailed = $compilationFailed."))
      .toString()

    // todo: refactor SigSym newStr. For example, `Eq.neq(x, 5)` to `x != 5`
    // todo: refactor hardcoded '.loc.text.get' in this file
    // todo: refactor for multiline. Example: examples/larger-examples/Reachable.flix
    // todo: specify a more precise location or add 2 lines of code before and after
    // todo: move to Formatter class like Formatter.code() ??
    private def diff(loc: SourceLocation, newStr: String): String = {
      val beginLine = loc.beginLine
      val beginCol = loc.beginCol
      val endLine = loc.endLine
      val endCol = loc.endCol

      val line = loc.lineAt(beginLine)

      val sb = new mutable.StringBuilder
      //      if (beginLine - 2 > 0) {
      //        sb.append(System.lineSeparator())
      //          .append((beginLine - 2).toString)
      //          .append(" | ")
      //          .append(loc.lineAt(beginLine - 2))
      //      }
      //
      //      if (beginLine - 1 > 0) {
      //        sb.append(System.lineSeparator())
      //          .append((beginLine - 1).toString)
      //          .append(" | ")
      //          .append(loc.lineAt(beginLine - 1))
      //      }

      sb.append(System.lineSeparator())
        .append(red(beginLine.toString))
        .append(" | ")
        .append(line.substring(0, beginCol - 1))
        .append(red(line.substring(beginCol - 1, endCol - 1)))
        .append(line.substring(endCol - 1))
        .append(System.lineSeparator())
        .append(green(beginLine.toString))
        .append(" | ")
        .append(line.substring(0, beginCol - 1))
        .append(green(newStr))
        .append(line.substring(endCol - 1))
        //        .append(System.lineSeparator())
        //        .append((beginLine + 1).toString)
        //        .append(" | ")
        //        .append(loc.lineAt(beginLine + 1))
        //        .append(System.lineSeparator())
        //        .append((beginLine + 2).toString)
        //        .append(" | ")
        //        .append(loc.lineAt(beginLine + 2))
        .append(System.lineSeparator())
        .append(System.lineSeparator())
        .toString()
    }
  }
}
