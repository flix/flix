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

import java.util.concurrent.Executors
import java.util.concurrent.atomic.AtomicInteger

/* TODO: think also:
 * Check mutant compilation. Create 'MutantStatus' enum
 * Add reporter with statistics
 * Compile source and run tests on it before mutation testing
 * Run tests on mutant with timeout (calculate it by time spent to run tests on source)
 * Mutate Root.instances.defs (?)
 * Are mutants SourceLocations correct ?
 * Separate mutations by kind (?)
 * is it necessary to mutate SemanticOp other than BoolOp.Not, BoolOp.And, BoolOp.Or ??
*/

object MutationTester {

  private sealed trait MutantStatus

  private case object Killed extends MutantStatus

  private case object Survived extends MutantStatus

  private case object CompilationFailed extends MutantStatus

  def run(root: Root)(implicit flix: Flix): Result[MutationReporter, Int] = {
    // don't want to mutate library defs and tests
    val defs = root.defs.values.filter(defn => !isLibDef(defn) && !isTestDef(defn))

    // todo: configure by defs count (?)
    val numThreads = 2
    val executor = Executors.newFixedThreadPool(numThreads)

    val reporter = new MutationReporter()

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
        Mutator.mutateExpr(defn.exp).foreach { mutatedExp =>
          val mutatedDef = defn.copy(exp = mutatedExp)
          val mutant = root.copy(defs = root.defs + (mutatedDef.sym -> mutatedDef))

          //    val redirect = new ConsoleRedirection
          //    redirect.redirect()
          val codeGenResult = flix.codeGen(mutant) // todo: is it possible to parallelize it ??
          //    if (redirect.stdErr.nonEmpty) return CompilationFailed
          //    redirect.restore()

          executor.execute(() => {
            codeGenResult.toSoftResult match {
              case Result.Ok(c) =>
                val status = testMutant(c._1)
                reporter.report(status, mutatedDef.sym.toString)
              case Result.Err(_) => reporter.report(CompilationFailed, mutatedDef.sym.toString)
            }
          })
        }
    }

    executor.shutdown()
    while (!executor.isTerminated) {}

    Result.Ok(reporter)
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
    // todo: collections: for example, Set constructor
    def mutateExpr(expr: Expr)(implicit flix: Flix): LazyList[Expr] = {
      if (expr.tpe.toString == "Unit") {
        LazyList.empty
      } else {
        expr match {
          case Expr.Cst(cst, tpe, loc) => mutateExprCst(expr)
          case Expr.Var(sym, tpe, loc) => LazyList.empty
          case Expr.Def(sym, tpe, loc) => MutatorHelper.mutateExprDef(expr)
          case Expr.Sig(sym, tpe, loc) => MutatorHelper.mutateExprSig(expr)
          case Expr.Hole(sym, tpe, loc) => LazyList.empty
          case Expr.HoleWithExp(exp, tpe, eff, loc) => LazyList.empty
          case Expr.OpenAs(symUse, exp, tpe, loc) => LazyList.empty
          case Expr.Use(sym, alias, exp, loc) => LazyList.empty
          case Expr.Lambda(fparam, exp, tpe, loc) => LazyList.empty
          case Expr.Apply(exp, exps, tpe, eff, loc) => MutatorHelper.mutateExprApply(expr)
          case Expr.Unary(sop, exp, tpe, eff, loc) => mutateExprUnary(expr)
          case Expr.Binary(sop, exp1, exp2, tpe, eff, loc) => mutateExprBinary(expr)
          case Expr.Let(sym, mod, exp1, exp2, tpe, eff, loc) =>
            mutateExpr(exp1).map(Expr.Let(sym, mod, _, exp2, tpe, eff, loc)) #:::
              mutateExpr(exp2).map(Expr.Let(sym, mod, exp1, _, tpe, eff, loc))
          case Expr.LetRec(sym, ann, mod, exp1, exp2, tpe, eff, loc) => LazyList.empty // todo
          case Expr.Region(tpe, loc) => LazyList.empty // todo
          case Expr.Scope(sym, regionVar, exp, tpe, eff, loc) => LazyList.empty // todo
          case Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
            mutateExpr(exp1).map(Expr.IfThenElse(_, exp2, exp3, tpe, eff, loc)) #:::
              mutateExpr(exp2).map(Expr.IfThenElse(exp1, _, exp3, tpe, eff, loc)) #:::
              mutateExpr(exp3).map(Expr.IfThenElse(exp1, exp2, _, tpe, eff, loc))
          case Expr.Stm(exp1, exp2, tpe, eff, loc) =>
            mutateExpr(exp1).map(Expr.Stm(_, exp2, tpe, eff, loc)) #:::
              mutateExpr(exp2).map(Expr.Stm(exp1, _, tpe, eff, loc))
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
          case Expr.Ref(exp1, exp2, tpe, eff, loc) => LazyList.empty
          case Expr.Deref(exp, tpe, eff, loc) => LazyList.empty
          case Expr.Assign(exp1, exp2, tpe, eff, loc) => LazyList.empty
          case Expr.Ascribe(exp, tpe, eff, loc) => LazyList.empty
          case Expr.InstanceOf(exp, clazz, loc) => LazyList.empty
          case Expr.CheckedCast(cast, exp, tpe, eff, loc) => LazyList.empty
          case Expr.UncheckedCast(exp, declaredType, declaredEff, tpe, eff, loc) => LazyList.empty
          case Expr.UncheckedMaskingCast(exp, tpe, eff, loc) => LazyList.empty
          case Expr.Without(exp, effUse, tpe, eff, loc) => LazyList.empty
          case Expr.TryCatch(exp, rules, tpe, eff, loc) => LazyList.empty
          case Expr.TryWith(exp, effUse, rules, tpe, eff, loc) => LazyList.empty
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
          case Expr.FixpointConstraintSet(cs, tpe, loc) => LazyList.empty
          case Expr.FixpointLambda(pparams, exp, tpe, eff, loc) => LazyList.empty
          case Expr.FixpointMerge(exp1, exp2, tpe, eff, loc) => LazyList.empty
          case Expr.FixpointSolve(exp, tpe, eff, loc) => LazyList.empty
          case Expr.FixpointFilter(pred, exp, tpe, eff, loc) => LazyList.empty
          case Expr.FixpointInject(exp, pred, tpe, eff, loc) => LazyList.empty
          case Expr.FixpointProject(pred, exp, tpe, eff, loc) => LazyList.empty
          case Expr.Error(m, tpe, eff) => LazyList.empty
        }
      }
    }

    private def mutateExprCst(exp: Expr): LazyList[Expr] = exp match {
      case Expr.Cst(cst, tpe, loc) => cst match {
        case Constant.Bool(lit) => LazyList(Expr.Cst(Constant.Bool(!lit), tpe, loc))
        case Constant.Float32(lit) => LazyList(lit + 1, lit - 1).map(value => Expr.Cst(Constant.Float32(value), tpe, loc))
        case Constant.Float64(lit) => LazyList(lit + 1, lit - 1).map(value => Expr.Cst(Constant.Float64(value), tpe, loc))
        case Constant.BigDecimal(lit) => LazyList(
          lit.add(java.math.BigDecimal.ONE),
          lit.subtract(java.math.BigDecimal.ONE)
        ).map(value => Expr.Cst(Constant.BigDecimal(value), tpe, loc))
        case Constant.Int8(lit) => LazyList(lit + 1, lit - 1).map(value => Expr.Cst(Constant.Int8(value.toByte), tpe, loc))
        case Constant.Int16(lit) => LazyList(lit + 1, lit - 1).map(value => Expr.Cst(Constant.Int16(value.toShort), tpe, loc))
        case Constant.Int32(lit) => LazyList(lit + 1, lit - 1).map(value => Expr.Cst(Constant.Int32(value), tpe, loc))
        case Constant.Int64(lit) => LazyList(lit + 1, lit - 1).map(value => Expr.Cst(Constant.Int64(value), tpe, loc))
        case Constant.BigInt(lit) => LazyList(
          lit.add(java.math.BigInteger.ONE),
          lit.subtract(java.math.BigInteger.ONE)
        ).map(value => Expr.Cst(Constant.BigInt(value), tpe, loc))
        case _ => LazyList.empty
      }
      case _ => LazyList.empty
    }

    private def mutateExprUnary(exp: Expr): LazyList[Expr] = exp match {
      case Expr.Unary(BoolOp.Not, exp, _, _, _) => LazyList(exp)
      case _ => LazyList.empty
    }

    private def mutateExprBinary(exp: Expr): LazyList[Expr] = exp match {
      case Expr.Binary(BoolOp.And, exp1, exp2, tpe, eff, loc) => LazyList(Expr.Binary(BoolOp.Or, exp1, exp2, tpe, eff, loc))
      case Expr.Binary(BoolOp.Or, exp1, exp2, tpe, eff, loc) => LazyList(Expr.Binary(BoolOp.And, exp1, exp2, tpe, eff, loc))
      case _ => LazyList.empty
    }

    private def mutateExprTuple(exp: Expr)(implicit flix: Flix): LazyList[Expr] = exp match {
      case Expr.Tuple(elms, tpe, eff, loc) =>
        LazyList.tabulate(elms.length)(i =>
          mutateExpr(elms(i)).map(e => Expr.Tuple(elms.updated(i, e), tpe, eff, loc))
        ).flatten
      case _ => LazyList.empty
    }
  }

  private object MutatorHelper {

    private val arithmeticSigTypes: Set[String] = Set(
      "Float32 -> (Float32 -> Float32)",
      "Float64 -> (Float64 -> Float64)",
      "BigDecimal -> (BigDecimal -> BigDecimal)",
      "Int8 -> (Int8 -> Int8)",
      "Int16 -> (Int16 -> Int16)",
      "Int32 -> (Int32 -> Int32)",
      "Int64 -> (Int64 -> Int64)",
      "BigInt -> (BigInt -> BigInt)",
    )

    private val arithmeticSigToSig: Map[String, (String, String)] = Map(
      "Add.add" -> ("Sub", "sub"),
      "Sub.sub" -> ("Add", "add"),
      "Mul.mul" -> ("Div", "div"),
      "Div.div" -> ("Mul", "mul"),
    )

    private val conditionalNegateSigToSig: Map[String, (String, String)] = Map(
      "Eq.eq" -> ("Eq", "neq"),
      "Eq.neq" -> ("Eq", "eq"),
      "Order.less" -> ("Order", "greaterEqual"),
      "Order.lessEqual" -> ("Order", "greater"),
      "Order.greater" -> ("Order", "lessEqual"),
      "Order.greaterEqual" -> ("Order", "less"),
    )

    private val conditionalBoundarySigToSig: Map[String, (String, String)] = Map(
      "Order.less" -> ("Order", "lessEqual"),
      "Order.lessEqual" -> ("Order", "less"),
      "Order.greater" -> ("Order", "greaterEqual"),
      "Order.greaterEqual" -> ("Order", "greater"),
    )

    private val defnIntNamespaces: Set[String] = Set(
      "Int8",
      "Int16",
      "Int32",
      "Int64",
    )

    private val defnToSig: Map[String, (String, String)] = Map(
      "modulo" -> ("Mul", "mul"),
      "remainder" -> ("Mul", "mul"),
    )

    private val defnToDefn: Map[String, String] = Map(
      "bitwiseAnd" -> "bitwiseOr",
      "bitwiseOr" -> "bitwiseAnd",
      "bitwiseXor" -> "bitwiseAnd",
      "leftShift" -> "rightShift",
      "rightShift" -> "leftShift",
    )

    // what if there is an expression other than Def and Sig ?
    def mutateExprApply(exp: TypedAst.Expr)(implicit flix: Flix): LazyList[TypedAst.Expr] = exp match {
      case Expr.Apply(exp, exps, tpe, eff, loc) =>
        exp match {
          case Expr.Sig(sym, _, _) if sym.toString == "Neg.neg" => LazyList(exps.head)
          case Expr.Def(sym, _, _) if defnIntNamespaces.contains(sym.namespace.mkString(".")) && sym.text == "bitwiseNot" => LazyList(exps.head)
          case _ =>
            val res = Mutator.mutateExpr(exp).map(Expr.Apply(_, exps, tpe, eff, loc))
            if (res.nonEmpty) {
              res
            } else {
              LazyList.tabulate(exps.length)(i =>
                Mutator.mutateExpr(exps(i)).map(e => Expr.Apply(exp, exps.updated(i, e), tpe, eff, loc))
              ).flatten
            }
        }
      case _ => LazyList.empty
    }

    def mutateExprSig(exp: Expr)(implicit flix: Flix): LazyList[Expr] =
      exp match {
        case Expr.Sig(sym, tpe, _) =>
          var l = List(conditionalNegateSigToSig, conditionalBoundarySigToSig)
          if (arithmeticSigTypes.contains(tpe.toString)) {
            l = arithmeticSigToSig :: l
          }

          l.foldLeft(LazyList.empty[Expr.Sig]) {
            (acc, sigMap) =>
              acc #::: sigMap.get(sym.toString)
                .flatMap(p => findSig(p._1, p._2))
                .map(s => LazyList(Expr.Sig(s, tpe, SourceLocation.Unknown)))
                .getOrElse(LazyList.empty)
          }
        case _ => LazyList.empty
      }

    def mutateExprDef(exp: Expr)(implicit flix: Flix): LazyList[Expr] = exp match {
      case Expr.Def(sym, tpe, _) if defnIntNamespaces.contains(sym.namespace.mkString(".")) =>
        defnToDefn.get(sym.text)
          .flatMap(findDef(sym.namespace, _))
          .map(s => LazyList(Expr.Def(s, tpe, SourceLocation.Unknown)))
          .orElse {
            defnToSig.get(sym.text)
              .flatMap(p => findSig(p._1, p._2))
              .map(s => LazyList(Expr.Sig(s, tpe, SourceLocation.Unknown)))
          }
          .getOrElse(LazyList.empty)
      case _ => LazyList.empty
    }

    private def findSig(clazz: String, sig: String)(implicit flix: Flix): Option[Symbol.SigSym] = try {
      Some(PredefinedClasses.lookupSigSym(clazz, sig, flix.getKinderAst))
    } catch {
      case _: Exception => None // todo: refactor to return info that mutant creation failed ?
    }

    private def findDef(namespace: List[String], name: String)(implicit flix: Flix): Option[Symbol.DefnSym] = try {
      Some(PredefinedClasses.lookupDefSym(namespace, name, flix.getKinderAst))
    } catch {
      case _: Exception => None // todo: refactor to return info that mutant creation failed ?
    }
  }

  class MutationReporter(implicit flix: Flix) {
    private var all: AtomicInteger = new AtomicInteger(0)
    private var killed: AtomicInteger = new AtomicInteger(0)
    private var survived: AtomicInteger = new AtomicInteger(0)
    private var compilationFailed: AtomicInteger = new AtomicInteger(0)
    private val formatter: Formatter = flix.getFormatter

    import formatter._

    def report(status: MutantStatus, defName: String): Unit = {
      val sb = new StringBuilder()
      sb.append("mutant created for " + blue(defName) + System.lineSeparator())

      all.getAndIncrement()
      status match {
        case Killed =>
          killed.getAndIncrement()
          sb.append(green("killed") + System.lineSeparator())
        case Survived =>
          survived.getAndIncrement()
          sb.append(red("survived") + System.lineSeparator())
        case CompilationFailed =>
          compilationFailed.getAndIncrement()
          sb.append(yellow("compilation failed") + System.lineSeparator())
      }

      sb.append("-" * 50)
      println(sb.toString())
    }

    def printStats(): String = new StringBuilder()
      .append(blue("Mutation testing:"))
      .append(System.lineSeparator())
      .append(blue(s"All = $all, Killed = $killed, Survived = $survived, CompilationFailed = $compilationFailed."))
      .toString()
  }
}
