/*
 * Copyright 2024 Lukas Schröder, Samuel Skovbakke & Alexander Sommer
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
package ca.uwaterloo.flix

import ca.uwaterloo.flix.runtime.TestFn
import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.Location
import ca.uwaterloo.flix.language.ast.Ast.Constant
import ca.uwaterloo.flix.language.ast.Type.{Apply, False, Int32, Null, Str, True, mkBigInt}
import ca.uwaterloo.flix.language.ast.{Ast, Name, SemanticOp, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.ast.TypedAst.{Expr, Root}
import dev.flix.runtime.Global

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.math.BigInteger


object MutationTester {

    sealed trait TestRes
    object TestRes {
        case object MutantKilled extends TestRes
        case object MutantSurvived extends TestRes
        case object Unknown extends TestRes
    }

    def run(flix: Flix, tester: String, testee: String): Unit = {
        val root = flix.check().unsafeGet
        val start = System.nanoTime()
        println(root.sigs.filter(t => t._1.toString.equals("Add.add")))
        val mutations = mutateRoot(root, testee)
        val end = System.nanoTime() - start
        val timeSec = end.toFloat / 1_000_000_000.0
        println(s"time to generate mutations: $timeSec")
        val lastRoot = insertDeckAndCheckIntoRoot(root)
        runMutations2(flix, tester, lastRoot, mutations)

        /**
          * val result = root1.map(r => flix.codeGen(r).unsafeGet)
          * val tests = result.map(res => res.getTests)
          */
    }
    private def insertDeckAndCheckIntoRoot(root: Root): Root = {
        val newDefs = root.defs.map({
          case (sym, fun) =>
            sym -> insertDeckAndCheckInDef(fun)
        })
        root.copy(defs = newDefs)
    }

    private def insertDeckAndCheckInDef(d: TypedAst.Def): TypedAst.Def ={
        val loc = d.exp.loc
        val method = classOf[Global].getMethods.apply(3)
        val InvokeMethod = Expr.InvokeStaticMethod(method, Nil, Type.Int64, Type.IO, loc)
        val mask = Expr.UncheckedMaskingCast(InvokeMethod, Type.Int64, Type.Pure, loc)
        val statement = Expr.Stm(mask, d.exp, d.exp.tpe, d.exp.eff, d.exp.loc)
        //println(s"name of function ${method.getName}")
        d.copy(exp = statement)
    }

    private def progressUpdate(message: String, timePassed: Long): Long = {
        var temp = timePassed
        val now = System.nanoTime()
        // print update if a minute has passed since last print
        if (now - temp > 60_000_000_000.0) {
            println(message)
            temp = now
        }
        temp
    }

    private case class TestKit(flix: Flix, root: Root, testModule: String)

    private def runMutations2(flix: Flix, tester: String, root: TypedAst.Root, mutatedDefs: List[(Symbol.DefnSym, List[TypedAst.Def])]): Unit = {
        val totalStartTime = System.nanoTime()
        val date = LocalDateTime.now()
        val temp = totalStartTime
        val amountOfMutants = mutatedDefs.map(m => m._2.length).sum
        val f = DateTimeFormatter.ofPattern("yyyy-MM-dd: HH:mm")
        // (survivorCount, startTime, tempTime for progress updates, date for progress updates, amount of mutants currently tested)
        val localAcc = (0, totalStartTime.toDouble, temp, date, 0)
        val (totalSurvivorCount, timeTemp, _, _, _) = mutatedDefs.foldLeft(localAcc)((acc, mut) => {
            val kit = TestKit(flix, root, tester)
            testMutantsAndUpdateProgress(acc, mut, kit, f)
        })
        val totalEndTime = System.nanoTime() - totalStartTime
        println(s"there where $totalSurvivorCount surviving mutations, out of $amountOfMutants mutations")
        val average = timeTemp / amountOfMutants
        println(s"average time to test a mutant:  $average sec")
        val time = totalEndTime.toFloat / 1_000_000_000
        println(s"total time to test all mutants: $time sec")
    }

    private def testMutantsAndUpdateProgress(acc: (Int, Double, Long, LocalDateTime, Int), mut: (Symbol.DefnSym, List[TypedAst.Def]), testKit: TestKit, f: DateTimeFormatter) = {
        //println(s"testing ${mut._1.toString} with $mutationAmount mutations")
        mut._2.foldLeft(acc)((acc2, mDef) => {
            val (survivorCount, time, accTemp, accDate, mAmount) = acc2
            val mutationAmount = mAmount + 1
            val start = System.nanoTime()
            val testResults = testMutant(mDef, mut, testKit)
            val newTime = time + (System.nanoTime() - start).toDouble / 1_000_000_000
            val newSurvivorCount = if (testResults) survivorCount + 1 else survivorCount
            val newDate = LocalDateTime.now()
            val message = s"[${f.format(newDate)}] Mutants: $mutationAmount, Killed: ${mutationAmount - survivorCount}, Survived: $survivorCount"
            val newTemp = progressUpdate(message, accTemp)
            (newSurvivorCount, newTime, newTemp, newDate, mutationAmount)
            //val sym = mDef.sym.toString
            //println(s"mutation in $sym survived")
        })
    }

    private def testMutant(mDef: TypedAst.Def, mut: (Symbol.DefnSym, List[TypedAst.Def]), testKit: TestKit): Boolean = {
        val defs = testKit.root.defs
        val n = defs + (mut._1 -> mDef)
        //println(s"mutation: $mDef")
        val newRoot = testKit.root.copy(defs = n)
        val cRes = testKit.flix.codeGen(newRoot).unsafeGet
        val testsFromTester = cRes.getTests.filter { case (s, _) => s.toString.contains(testKit.testModule) }.toList // change all "contains" to something more substantial
        runTest(testsFromTester)
    }

    private def runTest(testsFromTester: List[(Symbol.DefnSym, TestFn)]): Boolean = {
        testsFromTester.forall(c =>
            try {
                c._2.run() match {
                    case java.lang.Boolean.TRUE => true // TestRes.MutantSurvived
                    case java.lang.Boolean.FALSE => false // TestRes.MutantKilled
                    case _ => true // TestRes.MutantSurvived
                }
            } catch {
                // case OurException: Throwable => TestRes.Unknown
                case _: Throwable =>
                  println("nonterminating mutation")
                  false // TestRes.MutantKilled
            }
        )
    }


    private def mutateRoot(root: TypedAst.Root, testee: String): List[(Symbol.DefnSym, List[TypedAst.Def])] = {
        val defs = root.defs
        val defSyms = root.modules.filter(pair => (pair._1.toString.equals(testee))).values.toList.flatten
        mutateDefs(defs, defSyms).flatten
    }


    private def mutateDefs(defs: Map[Symbol.DefnSym, TypedAst.Def], defSyms: List[Symbol]) = {
        defs.toList.map(d => (d._1, d._2) match {
            case (s, fun) =>
                if (defSyms.contains(s)) {
                    //println(s, fun.exp)
                    val mutExps = mutateExpr(fun.exp)//.map(m => addDecAndCheck(m))
                    val mutDefs = mutExps.map(mexp =>{
                      val mutatedDef = fun.copy(exp = mexp)
                      insertDeckAndCheckInDef(mutatedDef)
                    })
                    Some(d._1 -> mutDefs)
                } else None
            case _ => None
        })
    }


  def mutateSig(sig: Expr.Sig): List[TypedAst.Expr.Sig] = {
    val tpe = sig.tpe
    val sym = sig.sym
    val sub = sig.copy(sym = Symbol.mkSigSym(Symbol.mkClassSym("Sub"), Name.Ident(sym.loc.sp1, "sub", sym.loc.sp2)))
    val mul = sig.copy(sym = Symbol.mkSigSym(Symbol.mkClassSym("Mul"), Name.Ident(sym.loc.sp1, "mul", sym.loc.sp2)))
    val div = sig.copy(sym = Symbol.mkSigSym(Symbol.mkClassSym("Div"), Name.Ident(sym.loc.sp1, "div", sym.loc.sp2)))
    val add = sig.copy(sym = Symbol.mkSigSym(Symbol.mkClassSym("Add"), Name.Ident(sym.loc.sp1, "add", sym.loc.sp2)))
    val clazz = Symbol.mkClassSym("Order")
    val le = sig.copy(sym = Symbol.mkSigSym(clazz, Name.Ident(sym.loc.sp1, "less", sym.loc.sp2)))
    val leq = sig.copy(sym = Symbol.mkSigSym(clazz, Name.Ident(sym.loc.sp1, "lessEqual", sym.loc.sp2)))
    val gre = sig.copy(sym = Symbol.mkSigSym(clazz, Name.Ident(sym.loc.sp1, "greater", sym.loc.sp2)))
    val greq = sig.copy(sym = Symbol.mkSigSym(clazz, Name.Ident(sym.loc.sp1, "greaterEqual", sym.loc.sp2)))
    val compare = sig.copy(sym = Symbol.mkSigSym(clazz, Name.Ident(sym.loc.sp1, "compare", sym.loc.sp2)))

    (sym.toString(), tpe.arrowArgTypes) match {
      case ("Add.add", List(Str, Str)) => Nil
      case ("Add.add", _) =>
        sub :: div :: mul :: Nil
      case ("Sub.sub", _) =>
        add :: div :: mul :: Nil
      case ("Div.div", _) =>
        mul :: add :: sub :: Nil
      case ("Mul.mul", _) =>
        div :: add :: sub :: Nil
      case ("Eq.eq", _) =>
        sig.copy(sym = Symbol.mkSigSym(Symbol.mkClassSym("Eq"), Name.Ident(sym.loc.sp1, "neq", sym.loc.sp2))) :: Nil
      case ("Eq.neq", _) =>
        sig.copy(sym = Symbol.mkSigSym(Symbol.mkClassSym("Eq"), Name.Ident(sym.loc.sp1, "eq", sym.loc.sp2))) :: Nil
      case ("Order.less", _) =>
        leq :: gre :: greq :: compare :: Nil
      case("Order.lessEqual",_ ) =>
        le :: gre :: greq :: compare :: Nil
      case("Order.greaterEqual",_ ) =>
        leq :: le :: gre :: compare :: Nil
      case ("Order.greater", _ ) =>
        leq :: le :: greq :: compare :: Nil
      case ("Order.compare",_) =>
        leq :: le :: greq :: gre :: Nil
      case _ => Nil
    }
  }
    private def addDecAndCheck(expr: Expr) : Expr = {
        val decAndCheck = Expr.Def(Symbol.mkDefnSym("decAndCheck"), Type.Unit, expr.loc)
        val apply = Expr.Apply(decAndCheck, Nil, Type.Unit, Type.Pure, expr.loc)
        Expr.Stm(apply, expr, expr.tpe, expr.eff, expr.loc)
    }

    private def mutateExpr(e: TypedAst.Expr): List[TypedAst.Expr] = e match {
        case Expr.Cst(cst, tpe, loc) =>
            mutateCst(cst).map(m => Expr.Cst(m, tpe, loc))
        case exp@Expr.Var(sym, tpe, loc) =>
            mutateVar(exp)
        case Expr.Def(_, _, _) => Nil
        case original@Expr.Sig(sym, tpe, loc) =>
            mutateSig(original)
        case Expr.Hole(sym, _, _) => Nil
        case Expr.HoleWithExp(exp, _, _, _) => Nil
        case Expr.OpenAs(symUse, exp, _, _) => Nil
        case original@Expr.Use(sym, alias, exp, _) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.Lambda(fparam, exp, _, _) =>
            mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.Apply(exp, exps, _, _, _) =>
            // Look at this, determine if it is correct.
            val mut = mutateExpr(exp).map(m => original.copy(exp = m))
            val mutateExps = exps.zipWithIndex.flatMap {
                case (exp, index) =>
                    val mutations = mutateExpr(exp)
                    mutations.map(m => original.copy(exps = exps.updated(index, m)))
            }
            val lengths = mutateExps.map(mr => mr.exps.length)
            lengths.foreach(l => assert(exps.length == l, "fail in apply"))
            mut ::: mutateExps

        case original@Expr.Unary(sop, exp, tpe, eff, loc) =>
            val mut1 = Expr.Unary(sop, original, tpe, eff, loc)
            mut1 :: mutateExpr(exp).map(m => original.copy(exp = m))

        case original@Expr.Binary(sop, exp1, exp2, _, _, _) =>
            val mut2 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
            val mut3 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
            mut2 ::: mut3
        case original@Expr.Let(sym, mod, exp1, exp2, _, _, _) =>
            val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
            val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
            mut1 ::: mut2
        case original@Expr.LetRec(sym, ann, mod, exp1, exp2, _, _, _) =>
            val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
            val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
            mut1 ::: mut2
        case Expr.Region(tpe, loc) => Nil
        case Expr.Scope(sym, regionVar, exp, _, _, _) => Nil
        case original@Expr.IfThenElse(exp1, exp2, exp3, _, _, loc) =>
            val ifTrue = original.copy(exp1 = Expr.Cst(Constant.Bool(true), True, exp1.loc))
            val ifFalse = original.copy(exp1 = Expr.Cst(Constant.Bool(false), False, exp1.loc))
            val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
            val mut3 = mutateExpr(exp3).map(m => original.copy(exp3 = m))
            ifTrue :: ifFalse :: mut2 ::: mut3
        case original@Expr.Stm(exp1, exp2, _, _, _) =>
            val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
            val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
            mut1 ::: mut2
        case Expr.Discard(exp, _, _) => Nil
        case original@Expr.Match(_, rules, _, _, _) =>
            // refactor to permutation
            val permutations = rules.permutations.toList.map(m => original.copy(rules = m))
            val deletedCasesMutation = rules.indices
                .map(index =>
                    rules.filter(e => rules.indexOf(e) != index || rules.indexOf(e) == rules.length - 1))
                .toList.map(m => original.copy(rules = m))
            val mutateRules = rules.zipWithIndex.flatMap { case (rule, index) => {
                val mutations = mutateMatchrule(rule)
                mutations.map(m => original.copy(rules = rules.updated(index, m)))
            }
            }
            val lengths = mutateRules.map(mr => mr.rules.length)
            lengths.foreach(l => assert(rules.length == l, "fail in match"))
            permutations ::: mutateRules ::: deletedCasesMutation.reverse.tail
        case Expr.TypeMatch(exp, rules, _, _, _) => Nil
        case Expr.RestrictableChoose(star, exp, rules, _, _, _) => Nil
        case Expr.Tag(sym, exp, _, _, _) => Nil
        case Expr.RestrictableTag(sym, exp, _, _, _) => Nil
        case original@Expr.Tuple(elms, _, _, _) =>
            elms.zipWithIndex.flatMap {
                case (exp, index) =>
                    val mutations = mutateExpr(exp)
                    mutations.map(m => original.copy(elms = elms.updated(index, m)))
            }
        case Expr.RecordEmpty(tpe, loc) => Nil
        case Expr.RecordSelect(exp, label, _, _, _) => Nil
        case Expr.RecordExtend(label, exp1, exp2, _, _, _) => Nil
        case Expr.RecordRestrict(label, exp, _, _, _) => Nil
        case original@Expr.ArrayLit(exps, exp, _, _, _) =>
            val mut = mutateExpr(exp).map(m => original.copy(exp = m))
            val mutateExps = exps.map(e => mutateExpr(e))
            mutateExps.map(m => original.copy(exps = m)) ::: mut
        case original@Expr.ArrayNew(exp1, exp2, exp3, _, _, _) =>
            val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
            val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
            val mut3 = mutateExpr(exp3).map(m => original.copy(exp3 = m))
            mut1 ::: mut2 ::: mut3
        case original@Expr.ArrayLoad(exp1, exp2, _, _, _) =>
            val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
            val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
            mut1 ::: mut2
        case Expr.ArrayLength(exp, _, _) => Nil
        case original@Expr.ArrayStore(exp1, exp2, exp3, _, _) =>
            val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
            val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
            val mut3 = mutateExpr(exp3).map(m => original.copy(exp3 = m))
            mut1 ::: mut2 ::: mut3
        case original@Expr.VectorLit(exps, _, _, _) =>
            exps.zipWithIndex.flatMap {
                case (exp, index) =>
                    val mutations = mutateExpr(exp)
                    mutations.map(m => original.copy(exps = exps.updated(index, m)))
            }
        case original@Expr.VectorLoad(exp1, exp2, _, _, _) =>
            val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
            val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
            mut1 ::: mut2
        case original@Expr.VectorLength(exp, _) =>
            mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.Ref(exp1, exp2, _, _, _) =>
            val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
            val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
            mut1 ::: mut2
        case original@Expr.Deref(exp, _, _, _) =>
            mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.Assign(exp1, exp2, _, _, _) =>
            val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
            val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
            mut1 ::: mut2
        case original@Expr.Ascribe(exp, _, _, _) =>
            mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.InstanceOf(exp, clazz, _) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.CheckedCast(cast, exp, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.UncheckedCast(exp, declaredType, declaredEff, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.UncheckedMaskingCast(exp, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.Without(exp, effUse, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.TryCatch(exp, rules, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.TryWith(exp, effUse, rules, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.Do(op, exps, _, _, _) =>
            exps.zipWithIndex.flatMap {
                case (exp, index) =>
                    val mutations = mutateExpr(exp)
                    mutations.map(m => original.copy(exps = exps.updated(index, m)))
            }
        case original@Expr.InvokeConstructor(constructor, exps, _, _, _) =>
            val mutateExps = exps.map(e => mutateExpr(e))
            mutateExps.map(m => original.copy(exps = m))
        case original@Expr.InvokeMethod(method, exp, exps, _, _, _) =>
            val mut = mutateExpr(exp).map(m => original.copy(exp = m))
            val mutateExps = exps.map(e => mutateExpr(e))
            mutateExps.map(m => original.copy(exps = m)) ::: mut
        case original@Expr.InvokeStaticMethod(method, exps, _, _, _) =>
            val mutateExps = exps.map(e => mutateExpr(e))
            mutateExps.map(m => original.copy(exps = m))
        case original@Expr.GetField(field, exp, _, _, _) =>
            mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.PutField(field, exp1, exp2, _, _, _) =>
            val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
            val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
            mut1 ::: mut2
        case Expr.GetStaticField(field, _, _, _) => Nil
        case original@Expr.PutStaticField(field, exp, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
        case Expr.NewObject(name, clazz, _, _, methods, _) => Nil
        case original@Expr.NewChannel(exp1, exp2, _, _, _) =>
            val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
            val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
            mut1 ::: mut2
        case original@Expr.GetChannel(exp, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.PutChannel(exp1, exp2, _, _, _) =>
            val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
            val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
            mut1 ::: mut2
        case Expr.SelectChannel(rules, default, _, _, _) => Nil
        case original@Expr.Spawn(exp1, exp2, _, _, _) =>
            val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
            val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
            mut1 ::: mut2
        case original@Expr.ParYield(frags, exp, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.Lazy(exp, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.Force(exp, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
        case Expr.FixpointConstraintSet(cs, _, _) => Nil
        case original@Expr.FixpointLambda(pparams, exp, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.FixpointMerge(exp1, exp2, _, _, _) =>
            val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
            val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
            mut1 ::: mut2
        case original@Expr.FixpointSolve(exp, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.FixpointFilter(pred, exp, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.FixpointInject(exp, pred, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.FixpointProject(pred, exp, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
        case Expr.Error(m, _, _) => Nil
    }

    private def mutateMatchrule(mr: TypedAst.MatchRule): List[TypedAst.MatchRule] = {
        val mut1 = mutateExpr(mr.exp).map(m => mr.copy(exp = m))
        val patterns = mutatePattern(mr.pat).map(m => mr.copy(pat = m))
        patterns ::: mut1
    }

    private def mutatePattern(pattern: TypedAst.Pattern): List[TypedAst.Pattern] = {
        pattern match {
            case original@TypedAst.Pattern.Cst(cst, _, _) => mutateCst(cst).map(m => original.copy(m))
            case _ => Nil
        }
    }


    private def mutateVar(varexp: Expr.Var): List[Expr] = varexp match {
        case Expr.Var(_, tpe, loc) =>
            //val typ = if (tpe.size > 1) tpe.arrowResultType else tpe
            val typ= tpe
            println(s"before mutating var: $typ")
            //val newtpe = Type.mkPureCurriedArrow(tpe :: tpe :: Nil, tpe, loc)
            val one = tpe match {
                case Type.Cst(tc, _) => tc match {
                    case TypeConstructor.Char =>
                        Expr.Cst(Constant.Char(1), tpe, loc) :: Nil
                    case TypeConstructor.Float32 =>
                        Expr.Cst(Constant.Float32(1), tpe, loc) :: Nil
                    case TypeConstructor.Float64 =>
                        Expr.Cst(Constant.Float64(1), tpe, loc) :: Nil
                    case TypeConstructor.BigDecimal =>
                        Expr.Cst(Constant.BigDecimal(java.math.BigDecimal.valueOf(1)), tpe, loc) :: Nil
                    case TypeConstructor.Int8 =>
                        Expr.Cst(Constant.Int8(1), tpe, loc) :: Nil
                    case TypeConstructor.Int16 =>
                        Expr.Cst(Constant.Int16(1), tpe, loc) :: Nil
                    case TypeConstructor.Int32 =>
                        Expr.Cst(Constant.Int32(1), tpe, loc) :: Nil
                    case TypeConstructor.Int64 =>
                        Expr.Cst(Constant.Int64(1), tpe, loc) :: Nil
                    case TypeConstructor.BigInt =>
                        Expr.Cst(Constant.BigInt(java.math.BigInteger.valueOf(1)), tpe, loc) :: Nil
                    case _ => Nil
                }
                case _ => Nil
            }
            if (one == Nil) return mutateVarToConstantByType(tpe)

            val newtpe = Type.mkPureUncurriedArrow(tpe :: tpe :: Nil, tpe, loc)
            println(s"after mutating var: $newtpe")
            val sub = Expr.Sig(Symbol.mkSigSym(Symbol.mkClassSym("Sub"), Name.Ident(loc.sp1, "sub", loc.sp2)), newtpe, loc)
            val add = Expr.Sig(Symbol.mkSigSym(Symbol.mkClassSym("Add"), Name.Ident(loc.sp1, "add", loc.sp2)), newtpe, loc)

            val appSub = Expr.Apply(sub, varexp :: one, tpe, Type.Pure, loc)
            val appAdd = Expr.Apply(add, varexp :: one, tpe, Type.Pure, loc)
            val ret = appSub ::  appAdd :: Nil
            println(s"ret: $ret")
            ret ::: mutateVarToConstantByType(tpe)
        case _ => Nil
    }


    private def mutateVarToConstantByType(constType: Type): List[Expr.Cst] = constType match {
        case Type.Cst(tc, loc) => tc match {
            case TypeConstructor.Bool =>
                val mut = Constant.Bool(true)
                val mutations = mut :: mutateCst(mut)
                mutations.map(m => Expr.Cst(m, Type.Bool, loc))
            case TypeConstructor.Char =>
                val mut = Constant.Char(0)
                val mutations = mut :: mutateCst(mut)
                mutations.map(m => Expr.Cst(m, Type.Bool, loc))
            case TypeConstructor.Float32 =>
                val mut = Constant.Float32(0)
                val mutations = mut :: mutateCst(mut)
                mutations.map(m => Expr.Cst(m, Type.Float32, loc))
            case TypeConstructor.Float64 =>
                val mut = Constant.Float64(0)
                val mutations = mut :: mutateCst(mut)
                mutations.map(m => Expr.Cst(m, Type.Float64, loc))
            case TypeConstructor.BigDecimal =>
                val mut = Constant.BigDecimal(java.math.BigDecimal.ZERO)
                val mutations = mut :: mutateCst(mut)
                mutations.map(m => Expr.Cst(m, Type.BigDecimal, loc))
            case TypeConstructor.Int8 =>
                val mut = Constant.Int8(0)
                val mutations = mut :: mutateCst(mut)
                mutations.map(m => Expr.Cst(m, Type.Int8, loc))
            case TypeConstructor.Int16 =>
                val mut = Constant.Int16(0)
                val mutations = mut :: mutateCst(mut)
                mutations.map(m => Expr.Cst(m, Type.Int16, loc))
            case TypeConstructor.Int32 =>
                val mut = Constant.Int32(0)
                val mutations = mut :: mutateCst(mut)
                mutations.map(m => Expr.Cst(m, Type.Int32, loc))
            case TypeConstructor.Int64 =>
                val mut = Constant.Int64(0)
                val mutations = mut :: mutateCst(mut)
                mutations.map(m => Expr.Cst(m, Type.Int64, loc))
            case TypeConstructor.BigInt =>
                val mut = Constant.BigInt(BigInteger.ZERO)
                val mutations = mut :: mutateCst(mut)
                mutations.map(m => Expr.Cst(m, Type.BigInt, loc))
            case TypeConstructor.Str =>
                val mut = Constant.Str("")
                val mutations = mut :: mutateCst(mut)
                mutations.map(m => Expr.Cst(m, Type.Str, loc))
            case TypeConstructor.Regex =>
                val mut = Constant.Regex(java.util.regex.Pattern.compile("b"))
                val mutations = mut :: mutateCst(mut)
                mutations.map(m => Expr.Cst(m, Type.Regex, loc))
            case e => println(s"$e not implemented in mutateConstantByType"); Nil
        }
        case _ => Nil
    }

    private def mutateCst(cst: Ast.Constant): List[Ast.Constant] = {
        cst match {
            case Constant.Unit => Nil
            case Constant.Null => Nil
            case Constant.Bool(lit) => Constant.Bool(!lit) :: Nil
            case Constant.Char(lit) => Constant.Char((lit ^ Char.MaxValue).toChar) :: Nil
            case Constant.Float32(lit) =>
                val litCand = Set(0, -1, 1, 2, 4, 6, 8, 16, Float.MinValue, Float.MaxValue, lit + 1, lit - 1).filter(i => lit != i)
                litCand.toList.map(i => Constant.Float32(i))
            case Constant.Float64(lit) =>
                val litCand = Set(0, -1, 1, 2, 4, 6, 8, 16, Double.MaxValue, Double.MinValue, lit + 1, lit - 1).filter(i => lit != i)
                litCand.toList.map(i => Constant.Float64(i))
            case Constant.BigDecimal(lit) =>
                val litCand = Set(java.math.BigDecimal.ZERO,
                    java.math.BigDecimal.valueOf(-1),
                    java.math.BigDecimal.ONE,
                    lit.subtract(java.math.BigDecimal.ONE),
                    java.math.BigDecimal.valueOf(2),
                    java.math.BigDecimal.valueOf(4),
                    java.math.BigDecimal.valueOf(6),
                    java.math.BigDecimal.valueOf(8),
                    java.math.BigDecimal.valueOf(16),
                    lit.add(java.math.BigDecimal.ONE),
                    lit.subtract(java.math.BigDecimal.ONE)).filter(i => lit != i)
                litCand.toList.map(i => Constant.BigDecimal(i))
            case Constant.Int8(lit) =>
                val litCand = Set(0, -1, 1, 2, 4, 6, 8, 16, Byte.MaxValue, Byte.MinValue, lit + 1, lit - 1).map(i => i.toByte).filter(i => lit != i)
                litCand.toList.map(i => Constant.Int8(i))
            case Constant.Int16(lit) =>
                val litCand = Set(0, -1, 1, 2, 4, 6, 8, Short.MinValue, Short.MaxValue, lit + 1, lit - 1).map(i => i.toShort).filter(i => lit != i)
                litCand.toList.map(i => Constant.Int16(i))
            case Constant.Int32(lit) =>
                val litCand = Set(0, -1, 1, 2, 4, 6, 8, 16, Int.MaxValue, Int.MinValue, lit + 1, lit - 1).filter(i => lit != i)
                litCand.toList.map(i => Constant.Int32(i))
            case Constant.Int64(lit) =>
                val litCand = Set(0, -1, 1, 2, 4, 6, 8, 16, Long.MaxValue, Long.MinValue, lit + 1, lit - 1).filter(i => lit != i)
                litCand.toList.map(i => Constant.Int64(i))
            case Constant.BigInt(lit) =>
                val litCand = Set(java.math.BigInteger.ZERO,
                    java.math.BigInteger.valueOf(-1),
                    java.math.BigInteger.ONE,
                    lit.subtract(java.math.BigInteger.ONE),
                    java.math.BigInteger.TWO,
                    java.math.BigInteger.valueOf(4),
                    java.math.BigInteger.valueOf(6),
                    java.math.BigInteger.valueOf(8),
                    java.math.BigInteger.valueOf(16),
                    lit.add(java.math.BigInteger.ONE),
                    lit.subtract(java.math.BigInteger.ONE)).filter(i => lit != i)
                litCand.toList.map(i => Constant.BigInt(i))
            case Constant.Str(lit) => Constant.Str(lit + "\b") :: Nil
            case Constant.Regex(reg) => Constant.Regex(java.util.regex.Pattern.compile(".*")) :: Nil
        }
    }
}
