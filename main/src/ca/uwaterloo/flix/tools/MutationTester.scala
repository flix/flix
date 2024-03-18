package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Ast.{Constant, Input}
import ca.uwaterloo.flix.language.ast.SemanticOp._
import ca.uwaterloo.flix.language.ast.TypedAst.{Def, Expr, Root}
import ca.uwaterloo.flix.language.ast.{SemanticOp, SourceLocation, Type, TypedAst}
import ca.uwaterloo.flix.language.phase.util.PredefinedClasses
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.tools.Tester.ConsoleRedirection
import ca.uwaterloo.flix.util.Result

import java.io.IOException
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{ConcurrentLinkedQueue, Executors}

/* TODO: think also:
 * Check mutant compilation. Create 'MutantStatus' enum
 * Add statistics
 * Add reporter
 * Recursive operators mutation
 * Compile source and run tests on it before mutation testing
 * Save source tests to run them on mutants. Can run them before first failure. Think about creating LocalTester
 * If possible, try running only tests related to mutants
 * Run tests on mutant with timeout (calculate it by time spent to run tests on source)
*/

object MutationTester {

  private sealed trait MutantStatus

  private case object Killed extends MutantStatus

  private case object Survived extends MutantStatus

  private case object CompilationFailed extends MutantStatus

  def run(root: Root)(implicit flix: Flix): Result[MutationReporter, Int] = {
    val queue = new ConcurrentLinkedQueue[Def]()

    val executor = Executors.newSingleThreadExecutor()

    //    val numThreads = 1 // fixme: compiler crashes with more than 1 threads
    //    val executor = Executors.newFixedThreadPool(numThreads)
    //    val fjPool = new ForkJoinPool(numThreads)

    val reporter = new MutationReporter()

    // don't want to mutate library defs and tests
    val defs = root.defs.values.filter(defn => !isLibDef(defn) && !isTestDef(defn))

    defs.foreach {
      defn =>
        Mutator.mutateExpr(defn.exp).foreach { mutatedExp =>
          queue.add(defn.copy(exp = mutatedExp))
        }
    }

    // todo: move prints to reporter
    val formatter = flix.getFormatter
    import formatter._

    while (!queue.isEmpty) {
      val mutatedDef = queue.poll()

      executor.execute(() => {
        val mutant = root.copy(defs = root.defs + (mutatedDef.sym -> mutatedDef))

        reporter.all.getAndIncrement()

        println("mutant created for " + blue(mutatedDef.sym.toString))

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
      })
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

  private def checkMutant(mutant: Root)(implicit flix: Flix): MutantStatus =
    flix.codeGen(mutant).toSoftResult match {
      case Result.Ok(c) => testMutant(c._1)
      case Result.Err(_) => CompilationFailed
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
    private val mutators: LazyList[ExprMutator] = LazyList(
      ExprMutator.Incrementer,
      ExprMutator.Decrementer,
      ExprMutator.BooleanMutator,
      ExprMutator.ArithmeticUnaryMutator,
      ExprMutator.ArithmeticBinaryMutator,
      ExprMutator.ConditionalNegateMutator,
      ExprMutator.ConditionalBoundaryMutator,
    )

    def mutateExpr(expr: Expr)(implicit flix: Flix): LazyList[Expr] = {
      expr match {
        case Expr.Cst(cst, tpe, loc) => mutateByMutators(expr)
        case Expr.Var(sym, tpe, loc) => ???
        case Expr.Def(sym, tpe, loc) => ???
        case Expr.Sig(sym, tpe, loc) => ???
        case Expr.Hole(sym, tpe, loc) => ???
        case Expr.HoleWithExp(exp, tpe, eff, loc) => ???
        case Expr.OpenAs(symUse, exp, tpe, loc) => ???
        case Expr.Use(sym, alias, exp, loc) => ???
        case Expr.Lambda(fparam, exp, tpe, loc) => ???
        case Expr.Apply(exp, exps, tpe, eff, loc) => mutateByMutators(expr)
        case Expr.Unary(sop, exp, tpe, eff, loc) => mutateByMutators(expr)
        case Expr.Binary(sop, exp1, exp2, tpe, eff, loc) => mutateByMutators(expr)
        case Expr.Let(sym, mod, exp1, exp2, tpe, eff, loc) => ???
        case Expr.LetRec(sym, ann, mod, exp1, exp2, tpe, eff, loc) => ???
        case Expr.Region(tpe, loc) => ???
        case Expr.Scope(sym, regionVar, exp, tpe, eff, loc) => ???
        case Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
          mutateExpr(exp1).map(Expr.IfThenElse(_, exp2, exp3, tpe, eff, loc)) #:::
            mutateExpr(exp2).map(Expr.IfThenElse(exp1, _, exp3, tpe, eff, loc)) #:::
            mutateExpr(exp3).map(Expr.IfThenElse(exp1, exp2, _, tpe, eff, loc))
        case Expr.Stm(exp1, exp2, tpe, eff, loc) => ???
        case Expr.Discard(exp, eff, loc) => ???
        case Expr.Match(exp, rules, tpe, eff, loc) => ???
        case Expr.TypeMatch(exp, rules, tpe, eff, loc) => ???
        case Expr.RestrictableChoose(star, exp, rules, tpe, eff, loc) => ???
        case Expr.Tag(sym, exp, tpe, eff, loc) => ???
        case Expr.RestrictableTag(sym, exp, tpe, eff, loc) => ???
        case Expr.Tuple(elms, tpe, eff, loc) => ???
        case Expr.RecordEmpty(tpe, loc) => ???
        case Expr.RecordSelect(exp, label, tpe, eff, loc) => ???
        case Expr.RecordExtend(label, exp1, exp2, tpe, eff, loc) => ???
        case Expr.RecordRestrict(label, exp, tpe, eff, loc) => ???
        case Expr.ArrayLit(exps, exp, tpe, eff, loc) => ???
        case Expr.ArrayNew(exp1, exp2, exp3, tpe, eff, loc) => ???
        case Expr.ArrayLoad(exp1, exp2, tpe, eff, loc) => ???
        case Expr.ArrayLength(exp, eff, loc) => ???
        case Expr.ArrayStore(exp1, exp2, exp3, eff, loc) => ???
        case Expr.VectorLit(exps, tpe, eff, loc) => ???
        case Expr.VectorLoad(exp1, exp2, tpe, eff, loc) => ???
        case Expr.VectorLength(exp, loc) => ???
        case Expr.Ref(exp1, exp2, tpe, eff, loc) => ???
        case Expr.Deref(exp, tpe, eff, loc) => ???
        case Expr.Assign(exp1, exp2, tpe, eff, loc) => ???
        case Expr.Ascribe(exp, tpe, eff, loc) => ???
        case Expr.InstanceOf(exp, clazz, loc) => ???
        case Expr.CheckedCast(cast, exp, tpe, eff, loc) => ???
        case Expr.UncheckedCast(exp, declaredType, declaredEff, tpe, eff, loc) => ???
        case Expr.UncheckedMaskingCast(exp, tpe, eff, loc) => ???
        case Expr.Without(exp, effUse, tpe, eff, loc) => ???
        case Expr.TryCatch(exp, rules, tpe, eff, loc) => ???
        case Expr.TryWith(exp, effUse, rules, tpe, eff, loc) => ???
        case Expr.Do(op, exps, tpe, eff, loc) => ???
        case Expr.InvokeConstructor(constructor, exps, tpe, eff, loc) => ???
        case Expr.InvokeMethod(method, exp, exps, tpe, eff, loc) => ???
        case Expr.InvokeStaticMethod(method, exps, tpe, eff, loc) => ???
        case Expr.GetField(field, exp, tpe, eff, loc) => ???
        case Expr.PutField(field, exp1, exp2, tpe, eff, loc) => ???
        case Expr.GetStaticField(field, tpe, eff, loc) => ???
        case Expr.PutStaticField(field, exp, tpe, eff, loc) => ???
        case Expr.NewObject(name, clazz, tpe, eff, methods, loc) => ???
        case Expr.NewChannel(exp1, exp2, tpe, eff, loc) => ???
        case Expr.GetChannel(exp, tpe, eff, loc) => ???
        case Expr.PutChannel(exp1, exp2, tpe, eff, loc) => ???
        case Expr.SelectChannel(rules, default, tpe, eff, loc) => ???
        case Expr.Spawn(exp1, exp2, tpe, eff, loc) => ???
        case Expr.ParYield(frags, exp, tpe, eff, loc) => ???
        case Expr.Lazy(exp, tpe, loc) => ???
        case Expr.Force(exp, tpe, eff, loc) => ???
        case Expr.FixpointConstraintSet(cs, tpe, loc) => ???
        case Expr.FixpointLambda(pparams, exp, tpe, eff, loc) => ???
        case Expr.FixpointMerge(exp1, exp2, tpe, eff, loc) => ???
        case Expr.FixpointSolve(exp, tpe, eff, loc) => ???
        case Expr.FixpointFilter(pred, exp, tpe, eff, loc) => ???
        case Expr.FixpointInject(exp, pred, tpe, eff, loc) => ???
        case Expr.FixpointProject(pred, exp, tpe, eff, loc) => ???
        case Expr.Error(m, tpe, eff) => ???
      }
    }

    // todo: try refactor and optimize structure
    private def mutateByMutators(exp: Expr)(implicit flix: Flix): LazyList[Expr] = mutators.flatMap(_.mutateExpr(exp))
  }

  private sealed trait ExprMutator {
    def mutateExpr(exp: Expr)(implicit flix: Flix): Option[Expr]
  }

  private object ExprMutator {
    case object Incrementer extends ExprMutator {

      override def mutateExpr(exp: Expr)(implicit flix: Flix): Option[Expr] = exp match {
        case Expr.Cst(cst, tpe, loc) => cst match {
          case Constant.Float32(lit) => Some(Expr.Cst(Constant.Float32(lit + 1), tpe, loc))
          case Constant.Float64(lit) => Some(Expr.Cst(Constant.Float64(lit + 1), tpe, loc))
          case Constant.BigDecimal(lit) => Some(Expr.Cst(Constant.BigDecimal(lit.add(java.math.BigDecimal.ONE)), tpe, loc))
          case Constant.Int8(lit) => Some(Expr.Cst(Constant.Int8((lit + 1).toByte), tpe, loc))
          case Constant.Int16(lit) => Some(Expr.Cst(Constant.Int16((lit + 1).toShort), tpe, loc))
          case Constant.Int32(lit) => Some(Expr.Cst(Constant.Int32(lit + 1), tpe, loc))
          case Constant.Int64(lit) => Some(Expr.Cst(Constant.Int64(lit + 1), tpe, loc))
          case Constant.BigInt(lit) => Some(Expr.Cst(Constant.BigInt(lit.add(java.math.BigInteger.ONE)), tpe, loc))
          case _ => None
        }
        case _ => None
      }
    }

    case object Decrementer extends ExprMutator {

      override def mutateExpr(exp: Expr)(implicit flix: Flix): Option[Expr] = exp match {
        case Expr.Cst(cst, tpe, loc) => cst match {
          case Constant.Float32(lit) => Some(Expr.Cst(Constant.Float32(lit - 1), tpe, loc))
          case Constant.Float64(lit) => Some(Expr.Cst(Constant.Float64(lit - 1), tpe, loc))
          case Constant.BigDecimal(lit) => Some(Expr.Cst(Constant.BigDecimal(lit.subtract(java.math.BigDecimal.ONE)), tpe, loc))
          case Constant.Int8(lit) => Some(Expr.Cst(Constant.Int8((lit - 1).toByte), tpe, loc))
          case Constant.Int16(lit) => Some(Expr.Cst(Constant.Int16((lit - 1).toShort), tpe, loc))
          case Constant.Int32(lit) => Some(Expr.Cst(Constant.Int32(lit - 1), tpe, loc))
          case Constant.Int64(lit) => Some(Expr.Cst(Constant.Int64(lit - 1), tpe, loc))
          case Constant.BigInt(lit) => Some(Expr.Cst(Constant.BigInt(lit.subtract(java.math.BigInteger.ONE)), tpe, loc))
          case _ => None
        }
        case _ => None
      }
    }

    case object BooleanMutator extends ExprMutator {

      override def mutateExpr(exp: Expr)(implicit flix: Flix): Option[Expr] = exp match {
        case Expr.Cst(Constant.Bool(lit), tpe, loc) => Some(Expr.Cst(Constant.Bool(!lit), tpe, loc))
        case Expr.Unary(BoolOp.Not, exp, _, _, _) => Some(exp)
        case Expr.Binary(BoolOp.And, exp1, exp2, tpe, eff, loc) => Some(Expr.Binary(BoolOp.Or, exp1, exp2, tpe, eff, loc))
        case Expr.Binary(BoolOp.Or, exp1, exp2, tpe, eff, loc) => Some(Expr.Binary(BoolOp.And, exp1, exp2, tpe, eff, loc))
        case _ => None
      }
    }

    case object ArithmeticUnaryMutator extends ExprMutator {

      private val ops: Set[SemanticOp] = Set(
        Float32Op.Neg,
        Float64Op.Neg,
        Int8Op.Neg,
        Int16Op.Neg,
        Int32Op.Neg,
        Int64Op.Neg,
        Int8Op.Not,
        Int16Op.Not,
        Int32Op.Not,
        Int64Op.Not,
      )

      override def mutateExpr(exp: Expr)(implicit flix: Flix): Option[Expr] = exp match {
        case Expr.Apply(exp, exps, _, _, _) => exp match {
          case Expr.Def(sym, _, _) => if (sym.text == "bitwiseNot") Some(exps.head) else None
          case _ => None
        }
        case Expr.Unary(sop: SemanticOp, exp, _, _, _) => if (ops.contains(sop)) Some(exp) else None
        case _ => None
      }
    }

    case object ArithmeticBinaryMutator extends ExprMutator {

      private val sigToSig: Map[String, (String, String)] = Map(
        "Add.add" -> ("Sub", "sub"),
        "Sub.sub" -> ("Add", "add"),
        "Mul.mul" -> ("Div", "div"),
        "Div.div" -> ("Mul", "mul"),
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

      private val float32Ops: Map[Float32Op, Float32Op] = Map(
        Float32Op.Add -> Float32Op.Sub,
        Float32Op.Sub -> Float32Op.Add,
        Float32Op.Mul -> Float32Op.Div,
        Float32Op.Div -> Float32Op.Mul,
      )

      private val float64Ops: Map[Float64Op, Float64Op] = Map(
        Float64Op.Add -> Float64Op.Sub,
        Float64Op.Sub -> Float64Op.Add,
        Float64Op.Mul -> Float64Op.Div,
        Float64Op.Div -> Float64Op.Mul,
      )

      private val int8Ops: Map[Int8Op, Int8Op] = Map(
        Int8Op.Add -> Int8Op.Sub,
        Int8Op.Sub -> Int8Op.Add,
        Int8Op.Mul -> Int8Op.Div,
        Int8Op.Div -> Int8Op.Mul,
        Int8Op.Rem -> Int8Op.Mul,
        // bitwise
        Int8Op.And -> Int8Op.Or,
        Int8Op.Or -> Int8Op.And,
        Int8Op.Xor -> Int8Op.And,
        Int8Op.Shr -> Int8Op.Shl,
        Int8Op.Shl -> Int8Op.Shr,
      )

      private val int16Ops: Map[Int16Op, Int16Op] = Map(
        Int16Op.Add -> Int16Op.Sub,
        Int16Op.Sub -> Int16Op.Add,
        Int16Op.Mul -> Int16Op.Div,
        Int16Op.Div -> Int16Op.Mul,
        Int16Op.Rem -> Int16Op.Mul,
        // bitwise
        Int16Op.And -> Int16Op.Or,
        Int16Op.Or -> Int16Op.And,
        Int16Op.Xor -> Int16Op.And,
        Int16Op.Shr -> Int16Op.Shl,
        Int16Op.Shl -> Int16Op.Shr,
      )

      private val int32Ops: Map[Int32Op, Int32Op] = Map(
        Int32Op.Add -> Int32Op.Sub,
        Int32Op.Sub -> Int32Op.Add,
        Int32Op.Mul -> Int32Op.Div,
        Int32Op.Div -> Int32Op.Mul,
        Int32Op.Rem -> Int32Op.Mul,
        // bitwise
        Int32Op.And -> Int32Op.Or,
        Int32Op.Or -> Int32Op.And,
        Int32Op.Xor -> Int32Op.And,
        Int32Op.Shr -> Int32Op.Shl,
        Int32Op.Shl -> Int32Op.Shr,
      )

      private val int64Ops: Map[Int64Op, Int64Op] = Map(
        Int64Op.Add -> Int64Op.Sub,
        Int64Op.Sub -> Int64Op.Add,
        Int64Op.Mul -> Int64Op.Div,
        Int64Op.Div -> Int64Op.Mul,
        Int64Op.Rem -> Int64Op.Mul,
        // bitwise
        Int64Op.And -> Int64Op.Or,
        Int64Op.Or -> Int64Op.And,
        Int64Op.Xor -> Int64Op.And,
        Int64Op.Shr -> Int64Op.Shl,
        Int64Op.Shl -> Int64Op.Shr,
      )

      override def mutateExpr(exp: TypedAst.Expr)(implicit flix: Flix): Option[TypedAst.Expr] = exp match {
        case Expr.Apply(exp, exps, tpe, eff, loc) =>
          exp match {
            case Expr.Sig(sym, tpe1, _) =>
              sigToSig.get(sym.toString)
                .flatMap(p => findSig(p._1, p._2, tpe1))
                .map(Expr.Apply(_, exps, tpe, eff, loc))
            case Expr.Def(sym, tpe1, _) =>
              defnToDefn.get(sym.text)
                .flatMap(findDef(sym.namespace, _, tpe1))
                .orElse {
                  defnToSig.get(sym.text)
                    .flatMap(p => findSig(p._1, p._2, tpe1))
                }
                .map(Expr.Apply(_, exps, tpe, eff, loc))
            case _ => None
          }
        case Expr.Binary(sop: SemanticOp, exp1, exp2, tpe, eff, loc) =>
          sop match {
            case op: Float32Op => float32Ops.get(op).map(Expr.Binary(_, exp1, exp2, tpe, eff, loc))
            case op: Float64Op => float64Ops.get(op).map(Expr.Binary(_, exp1, exp2, tpe, eff, loc))
            case op: Int8Op => int8Ops.get(op).map(Expr.Binary(_, exp1, exp2, tpe, eff, loc))
            case op: Int16Op => int16Ops.get(op).map(Expr.Binary(_, exp1, exp2, tpe, eff, loc))
            case op: Int32Op => int32Ops.get(op).map(Expr.Binary(_, exp1, exp2, tpe, eff, loc))
            case op: Int64Op => int64Ops.get(op).map(Expr.Binary(_, exp1, exp2, tpe, eff, loc))
            case _ => None
          }
        case _ => None
      }
    }

    case object ConditionalNegateMutator extends ExprMutator {

      private val sigToSig: Map[String, (String, String)] = Map(
        "Eq.eq" -> ("Eq", "neq"),
        "Eq.neq" -> ("Eq", "eq"),
        "Order.less" -> ("Order", "greaterEqual"),
        "Order.lessEqual" -> ("Order", "greater"),
        "Order.greater" -> ("Order", "lessEqual"),
        "Order.greaterEqual" -> ("Order", "less"),
      )

      private val boolOps: Map[BoolOp, BoolOp] = Map(
        BoolOp.Eq -> BoolOp.Neq,
        BoolOp.Neq -> BoolOp.Eq,
      )

      private val float32Ops: Map[Float32Op, Float32Op] = Map(
        Float32Op.Eq -> Float32Op.Neq,
        Float32Op.Neq -> Float32Op.Eq,
        Float32Op.Lt -> Float32Op.Ge,
        Float32Op.Le -> Float32Op.Gt,
        Float32Op.Gt -> Float32Op.Le,
        Float32Op.Ge -> Float32Op.Lt,
      )

      private val float64Ops: Map[Float64Op, Float64Op] = Map(
        Float64Op.Eq -> Float64Op.Neq,
        Float64Op.Neq -> Float64Op.Eq,
        Float64Op.Lt -> Float64Op.Ge,
        Float64Op.Le -> Float64Op.Gt,
        Float64Op.Gt -> Float64Op.Le,
        Float64Op.Ge -> Float64Op.Lt,
      )

      private val int8Ops: Map[Int8Op, Int8Op] = Map(
        Int8Op.Eq -> Int8Op.Neq,
        Int8Op.Neq -> Int8Op.Eq,
        Int8Op.Lt -> Int8Op.Ge,
        Int8Op.Le -> Int8Op.Gt,
        Int8Op.Gt -> Int8Op.Le,
        Int8Op.Ge -> Int8Op.Lt,
      )

      private val int16Ops: Map[Int16Op, Int16Op] = Map(
        Int16Op.Eq -> Int16Op.Neq,
        Int16Op.Neq -> Int16Op.Eq,
        Int16Op.Lt -> Int16Op.Ge,
        Int16Op.Le -> Int16Op.Gt,
        Int16Op.Gt -> Int16Op.Le,
        Int16Op.Ge -> Int16Op.Lt,
      )

      private val int32Ops: Map[Int32Op, Int32Op] = Map(
        Int32Op.Eq -> Int32Op.Neq,
        Int32Op.Neq -> Int32Op.Eq,
        Int32Op.Lt -> Int32Op.Ge,
        Int32Op.Le -> Int32Op.Gt,
        Int32Op.Gt -> Int32Op.Le,
        Int32Op.Ge -> Int32Op.Lt,
      )

      private val int64Ops: Map[Int64Op, Int64Op] = Map(
        Int64Op.Eq -> Int64Op.Neq,
        Int64Op.Neq -> Int64Op.Eq,
        Int64Op.Lt -> Int64Op.Ge,
        Int64Op.Le -> Int64Op.Gt,
        Int64Op.Gt -> Int64Op.Le,
        Int64Op.Ge -> Int64Op.Lt,
      )

      override def mutateExpr(exp: TypedAst.Expr)(implicit flix: Flix): Option[TypedAst.Expr] = exp match {
        case Expr.Apply(exp, exps, tpe, eff, loc) =>
          exp match {
            case Expr.Sig(sym, tpe1, _) =>
              sigToSig.get(sym.toString)
                .flatMap(p => findSig(p._1, p._2, tpe1))
                .map(Expr.Apply(_, exps, tpe, eff, loc))
            case _ => None
          }
        case Expr.Binary(sop: SemanticOp, exp1, exp2, tpe, eff, loc) =>
          sop match {
            case op: BoolOp => boolOps.get(op).map(Expr.Binary(_, exp1, exp2, tpe, eff, loc))
            case op: Float32Op => float32Ops.get(op).map(Expr.Binary(_, exp1, exp2, tpe, eff, loc))
            case op: Float64Op => float64Ops.get(op).map(Expr.Binary(_, exp1, exp2, tpe, eff, loc))
            case op: Int8Op => int8Ops.get(op).map(Expr.Binary(_, exp1, exp2, tpe, eff, loc))
            case op: Int16Op => int16Ops.get(op).map(Expr.Binary(_, exp1, exp2, tpe, eff, loc))
            case op: Int32Op => int32Ops.get(op).map(Expr.Binary(_, exp1, exp2, tpe, eff, loc))
            case op: Int64Op => int64Ops.get(op).map(Expr.Binary(_, exp1, exp2, tpe, eff, loc))
            case _ => None
          }
        case _ => None
      }
    }

    case object ConditionalBoundaryMutator extends ExprMutator {

      private val sigToSig: Map[String, (String, String)] = Map(
        "Order.less" -> ("Order", "lessEqual"),
        "Order.lessEqual" -> ("Order", "less"),
        "Order.greater" -> ("Order", "greaterEqual"),
        "Order.greaterEqual" -> ("Order", "greater"),
      )

      private val float32Ops: Map[Float32Op, Float32Op] = Map(
        Float32Op.Lt -> Float32Op.Le,
        Float32Op.Le -> Float32Op.Lt,
        Float32Op.Gt -> Float32Op.Ge,
        Float32Op.Ge -> Float32Op.Gt,
      )

      private val float64Ops: Map[Float64Op, Float64Op] = Map(
        Float64Op.Lt -> Float64Op.Le,
        Float64Op.Le -> Float64Op.Lt,
        Float64Op.Gt -> Float64Op.Ge,
        Float64Op.Ge -> Float64Op.Gt,
      )

      private val int8Ops: Map[Int8Op, Int8Op] = Map(
        Int8Op.Lt -> Int8Op.Le,
        Int8Op.Le -> Int8Op.Lt,
        Int8Op.Gt -> Int8Op.Ge,
        Int8Op.Ge -> Int8Op.Gt,
      )

      private val int16Ops: Map[Int16Op, Int16Op] = Map(
        Int16Op.Lt -> Int16Op.Le,
        Int16Op.Le -> Int16Op.Lt,
        Int16Op.Gt -> Int16Op.Ge,
        Int16Op.Ge -> Int16Op.Gt,
      )

      private val int32Ops: Map[Int32Op, Int32Op] = Map(
        Int32Op.Lt -> Int32Op.Le,
        Int32Op.Le -> Int32Op.Lt,
        Int32Op.Gt -> Int32Op.Ge,
        Int32Op.Ge -> Int32Op.Gt,
      )

      private val int64Ops: Map[Int64Op, Int64Op] = Map(
        Int64Op.Lt -> Int64Op.Le,
        Int64Op.Le -> Int64Op.Lt,
        Int64Op.Gt -> Int64Op.Ge,
        Int64Op.Ge -> Int64Op.Gt,
      )

      override def mutateExpr(exp: TypedAst.Expr)(implicit flix: Flix): Option[TypedAst.Expr] = exp match {
        case Expr.Apply(exp, exps, tpe, eff, loc) =>
          exp match {
            case Expr.Sig(sym, tpe1, _) =>
              sigToSig.get(sym.toString)
                .flatMap(p => findSig(p._1, p._2, tpe1))
                .map(Expr.Apply(_, exps, tpe, eff, loc))
            case _ => None
          }
        case Expr.Binary(sop: SemanticOp, exp1, exp2, tpe, eff, loc) =>
          sop match {
            case op: Float32Op => float32Ops.get(op).map(Expr.Binary(_, exp1, exp2, tpe, eff, loc))
            case op: Float64Op => float64Ops.get(op).map(Expr.Binary(_, exp1, exp2, tpe, eff, loc))
            case op: Int8Op => int8Ops.get(op).map(Expr.Binary(_, exp1, exp2, tpe, eff, loc))
            case op: Int16Op => int16Ops.get(op).map(Expr.Binary(_, exp1, exp2, tpe, eff, loc))
            case op: Int32Op => int32Ops.get(op).map(Expr.Binary(_, exp1, exp2, tpe, eff, loc))
            case op: Int64Op => int64Ops.get(op).map(Expr.Binary(_, exp1, exp2, tpe, eff, loc))
            case _ => None
          }
        case _ => None
      }
    }

    private def findSig(clazz: String, sig: String, tpe: Type)(implicit flix: Flix): Option[Expr.Sig] = try {
      Some(Expr.Sig(PredefinedClasses.lookupSigSym(clazz, sig, flix.getKinderAst), tpe, SourceLocation.Unknown))
    } catch {
      case _: IOException => None // todo: refactor ?
    }

    // todo: do we need to verify namespace ? For example: MyNotNumberType.bitwiseAnd()
    private def findDef(namespace: List[String], name: String, tpe: Type)(implicit flix: Flix): Option[Expr.Def] = try {
      Some(Expr.Def(PredefinedClasses.lookupDefSym(namespace, name, flix.getKinderAst), tpe, SourceLocation.Unknown))
    } catch {
      case _: IOException => None // todo: refactor ?
    }
  }

  class MutationReporter {
    var all, killed, compilationFailed: AtomicInteger = new AtomicInteger(0) // TODO: refactor
  }
}
