package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Ast.{Constant, Input}
import ca.uwaterloo.flix.language.ast.SemanticOp._
import ca.uwaterloo.flix.language.ast.TypedAst.{Def, Expr, Root}
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.phase.util.PredefinedClasses
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.tools.Tester.ConsoleRedirection
import ca.uwaterloo.flix.util.Result

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicInteger

/* TODO: think also:
 * Check mutant compilation. Create 'MutantStatus' enum
 * Add statistics
 * Add reporter
 * Recursive operators mutation
 * Compile source and run tests on it before mutation testing
 * Save source tests to run them on mutants. Can run them before first failure. Think about creating LocalTester
 * If possible, try running only tests related to mutants
 * Run tests on mutant with timeout (calculate it by time spent to run tests on source)
 * Mutate Root.instances.defs (?)
 * Are mutants SourceLocations correct ?
 * Separate mutations by kind
 * is it necessary to mutate SemanticOp other than BoolOp.Not, BoolOp.And, BoolOp.Or ??
*/

object MutationTester {

  private sealed trait MutantStatus

  private case object Killed extends MutantStatus

  private case object Survived extends MutantStatus

  private case object CompilationFailed extends MutantStatus

  def run(root: Root)(implicit flix: Flix): Result[MutationReporter, Int] = {
    val queue = new ConcurrentLinkedQueue[Def]()

    //    val executor = Executors.newSingleThreadExecutor()

    //    val numThreads = 1 // fixme: compiler crashes with more than 1 threads
    //    val executor = Executors.newFixedThreadPool(numThreads)
    //    val fjPool = new ForkJoinPool(numThreads)

    val reporter = new MutationReporter()

    // don't want to mutate library defs and tests
    val defs = root.defs.values.filter(defn => !isLibDef(defn) && !isTestDef(defn))

    defs.foreach {
      defn =>
        // todo: move generation to separated thread or ExecutorService
        Mutator.mutateExpr(defn.exp).foreach { mutatedExp =>
          queue.add(defn.copy(exp = mutatedExp))
        }
    }

    // todo: move prints to reporter
    val formatter = flix.getFormatter
    import formatter._

    while (!queue.isEmpty) {
      val mutatedDef = queue.poll()

      //      executor.execute(() => {
      val mutant = root.copy(defs = root.defs + (mutatedDef.sym -> mutatedDef))

      reporter.all.getAndIncrement()

      println("mutant created for " + blue(mutatedDef.sym.toString))

      try {
        checkMutant(mutant) match {
          case Killed =>
            reporter.killed.getAndIncrement()
            println(green("mutant killed"))
          case CompilationFailed =>
            reporter.compilationFailed.getAndIncrement()
            println(yellow("mutant compilation failed"))
          case Survived =>
            println(red("mutant survived"))
        }

        println("-" * 80)
      } catch {
        case _: Exception =>
          reporter.compilationFailed.getAndIncrement()
          println(yellow("mutant compilation failed"))
      }
      //      })
    }

    //    executor.shutdown()
    //    while (!executor.isTerminated) {}

    Result.Ok(reporter)
  }

  private def isLibDef(defn: Def): Boolean = defn.exp.loc.source.input match {
    case Input.Text(_, _, _) => true
    case Input.TxtFile(_) => false
    case Input.PkgFile(_) => false
  }

  private def isTestDef(defn: Def): Boolean = defn.spec.ann.isTest

  private def checkMutant(mutant: Root)(implicit flix: Flix): MutantStatus = {
    flix.codeGen(mutant).toSoftResult match {
      case Result.Ok(c) => testMutant(c._1)
      case Result.Err(_) => CompilationFailed
    }
  }

  /**
    * returns true if all tests passed, false otherwise
    */
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
    def mutateExpr(expr: Expr)(implicit flix: Flix): LazyList[Expr] = {
      expr match {
        case Expr.Cst(cst, tpe, loc) => mutateExprCst(expr)
        case Expr.Var(sym, tpe, loc) => LazyList.empty
        case Expr.Def(sym, tpe, loc) => LazyList.empty
        case Expr.Sig(sym, tpe, loc) => LazyList.empty
        case Expr.Hole(sym, tpe, loc) => LazyList.empty
        case Expr.HoleWithExp(exp, tpe, eff, loc) => LazyList.empty
        case Expr.OpenAs(symUse, exp, tpe, loc) => LazyList.empty
        case Expr.Use(sym, alias, exp, loc) => LazyList.empty
        case Expr.Lambda(fparam, exp, tpe, loc) => LazyList.empty
        case Expr.Apply(exp, exps, tpe, eff, loc) => ApplyMutator.mutateExpr(expr)
        // if success mutate(exp) -> not mutate exps
        case Expr.Unary(sop, exp, tpe, eff, loc) => mutateExprUnary(expr)
        case Expr.Binary(sop, exp1, exp2, tpe, eff, loc) => mutateExprBinary(expr)
        case Expr.Let(sym, mod, exp1, exp2, tpe, eff, loc) =>
          mutateExpr(exp1).map(Expr.Let(sym, mod, _, exp2, tpe, eff, loc)) #:::
            mutateExpr(exp2).map(Expr.Let(sym, mod, exp1, _, tpe, eff, loc))
        case Expr.LetRec(sym, ann, mod, exp1, exp2, tpe, eff, loc) => LazyList.empty
        case Expr.Region(tpe, loc) => LazyList.empty
        case Expr.Scope(sym, regionVar, exp, tpe, eff, loc) => LazyList.empty
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
        case Expr.Tuple(elms, tpe, eff, loc) => LazyList.empty
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

    private def mutateExprCst(exp: Expr): LazyList[Expr] = exp match {
      case Expr.Cst(cst, tpe, loc) => cst match {
        case Constant.Bool(lit) => LazyList(Expr.Cst(Constant.Bool(!lit), tpe, loc))
        case Constant.Float32(lit) => LazyList(
          Expr.Cst(Constant.Float32(lit + 1), tpe, loc),
          Expr.Cst(Constant.Float32(lit - 1), tpe, loc)
        )
        case Constant.Float64(lit) => LazyList(
          Expr.Cst(Constant.Float64(lit + 1), tpe, loc),
          Expr.Cst(Constant.Float64(lit - 1), tpe, loc)
        )
        case Constant.BigDecimal(lit) => LazyList(
          Expr.Cst(Constant.BigDecimal(lit.add(java.math.BigDecimal.ONE)), tpe, loc),
          Expr.Cst(Constant.BigDecimal(lit.subtract(java.math.BigDecimal.ONE)), tpe, loc)
        )
        case Constant.Int8(lit) => LazyList(
          Expr.Cst(Constant.Int8((lit + 1).toByte), tpe, loc),
          Expr.Cst(Constant.Int8((lit - 1).toByte), tpe, loc)
        )
        case Constant.Int16(lit) => LazyList(
          Expr.Cst(Constant.Int16((lit + 1).toShort), tpe, loc),
          Expr.Cst(Constant.Int16((lit - 1).toShort), tpe, loc)
        )
        case Constant.Int32(lit) => LazyList(
          Expr.Cst(Constant.Int32(lit + 1), tpe, loc),
          Expr.Cst(Constant.Int32(lit - 1), tpe, loc)
        )
        case Constant.Int64(lit) => LazyList(
          Expr.Cst(Constant.Int64(lit + 1), tpe, loc),
          Expr.Cst(Constant.Int64(lit - 1), tpe, loc)
        )
        case Constant.BigInt(lit) => LazyList(
          Expr.Cst(Constant.BigInt(lit.add(java.math.BigInteger.ONE)), tpe, loc),
          Expr.Cst(Constant.BigInt(lit.subtract(java.math.BigInteger.ONE)), tpe, loc)
        )
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
  }

  private object ApplyMutator {

    // def foo(): Int32 = -baz() compiles to Sig(Neg.neg)
    private val ArithmeticSigToSig: Map[String, (String, String)] = Map(
      // todo: crashes for add string. Allow there only number types
      "Add.add" -> ("Sub", "sub"),
      "Sub.sub" -> ("Add", "add"),
      "Mul.mul" -> ("Div", "div"),
      "Div.div" -> ("Mul", "mul"),
    )

    private val ConditionalNegateSigToSig: Map[String, (String, String)] = Map(
      "Eq.eq" -> ("Eq", "neq"),
      "Eq.neq" -> ("Eq", "eq"),
      "Order.less" -> ("Order", "greaterEqual"),
      "Order.lessEqual" -> ("Order", "greater"),
      "Order.greater" -> ("Order", "lessEqual"),
      "Order.greaterEqual" -> ("Order", "less"),
    )

    private val ConditionalBoundarySigToSig: Map[String, (String, String)] = Map(
      "Order.less" -> ("Order", "lessEqual"),
      "Order.lessEqual" -> ("Order", "less"),
      "Order.greater" -> ("Order", "greaterEqual"),
      "Order.greaterEqual" -> ("Order", "greater"),
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

    def mutateExpr(exp: TypedAst.Expr)(implicit flix: Flix): LazyList[TypedAst.Expr] = exp match {
      case Expr.Apply(exp, exps, tpe, eff, loc) =>
        exp match {
          case Expr.Sig(sym, _, _) =>
            if (sym.toString == "Neg.neg") LazyList(exps.head)
            else mutateExprSig(exp).map(Expr.Apply(_, exps, tpe, eff, loc))
          case Expr.Def(sym, _, _) =>
            if (sym.text == "bitwiseNot") LazyList(exps.head)
            else mutateExprDef(exp).map(Expr.Apply(_, exps, tpe, eff, loc))
          case _ => LazyList.empty
        }
      case _ => LazyList.empty
    }

    def mutateExprSig(exp: Expr)(implicit flix: Flix): LazyList[Expr] =
      exp match {
        case Expr.Sig(sym, tpe, _) =>
          List(ArithmeticSigToSig, ConditionalNegateSigToSig, ConditionalBoundarySigToSig)
            .foldLeft(LazyList.empty[Expr.Sig]) { (acc, sigMap) =>
              acc #::: sigMap.get(sym.toString)
                .flatMap(p => findSig(p._1, p._2))
                .map(s => LazyList(Expr.Sig(s, tpe, SourceLocation.Unknown)))
                .getOrElse(LazyList.empty)
            }
        case _ => LazyList.empty
      }

    def mutateExprDef(exp: Expr)(implicit flix: Flix): LazyList[Expr] =
      exp match {
        case Expr.Def(sym, tpe, _) =>
          defnToDefn.get(sym.text)
            .flatMap(findDef(sym.namespace, _))
            .map(s => Expr.Def(s, tpe, SourceLocation.Unknown))
            .orElse {
              defnToSig.get(sym.text)
                .flatMap(p => findSig(p._1, p._2))
                .map(s => Expr.Sig(s, tpe, SourceLocation.Unknown))
            } match {
            case Some(value) => LazyList(value)
            case None => LazyList.empty
          }
        case _ => LazyList.empty
      }

    def findSig(clazz: String, sig: String)(implicit flix: Flix): Option[Symbol.SigSym] = try {
      Some(PredefinedClasses.lookupSigSym(clazz, sig, flix.getKinderAst))
    } catch {
      case _: Exception => None // todo: refactor to return info that mutant creation failed ?
    }

    // todo: do we need to verify namespace ? For example: MyNotNumberType.bitwiseAnd()
    def findDef(namespace: List[String], name: String)(implicit flix: Flix): Option[Symbol.DefnSym] = try {
      Some(PredefinedClasses.lookupDefSym(namespace, name, flix.getKinderAst))
    } catch {
      case _: Exception => None // todo: refactor to return info that mutant creation failed ?
    }
  }

  class MutationReporter {
    var all, killed, compilationFailed: AtomicInteger = new AtomicInteger(0) // TODO: refactor
  }
}
