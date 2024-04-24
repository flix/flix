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
import ca.uwaterloo.flix.MutationGenerator.{MutatedDef, mutateRoot}
import ca.uwaterloo.flix.runtime.TestFn
import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Ast.Constant
import ca.uwaterloo.flix.language.ast.{Symbol, Type, TypedAst}
import ca.uwaterloo.flix.language.ast.TypedAst.{Expr, MutationType, Root}
import dev.flix.runtime.Global

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import ca.uwaterloo.flix.language.phase.jvm.BackendObjType

import java.io.{File, FileWriter}
import scala.util.Random

///
/// A Mutation Tester can be used to evaluate ones test suite.
/// It is based on the following:
///     - given a source and test module we can test the source code.
///     - A competent programmer writes almost correct code and only
///       needs to make few changes to achieve the correct program.
///     - We can simulate the competent programmer mistakes by making single changes to the source code,
///       which is called a mutation.
///     - Running the mutants with the given tests, we can estimate the quality of the test suite.
/// Mutants are created by greedily going through the AST and making all sensible changes.
///

object MutationTester {

    /**
      * When the test suite is ran, a mutant can either be killed or survive.
      *
      * A special case is added when the test doesn't terminate.
      */
    sealed trait TestRes

    object TestRes {
        case object MutantKilled extends TestRes

        case object MutantSurvived extends TestRes

        case object Unknown extends TestRes
    }
    var nonKilledStrList: List[String] = List.empty

    /**
      * Compiles until after the typing stage, creates mutants and then runs them.
      *
      * It also keeps track of the time it took to generate all the mutations
      */
    def run(flix: Flix, testModule: String, productionModule: String, percentage: Int): Unit = {
      println(s"mutating module: $productionModule")
        val root = flix.check().unsafeGet
        val start = System.nanoTime()
        // println(root.sigs.filter(t => t._1.toString.equals("Add.add")))
        val mutations = MutationGenerator.mutateRoot(root, productionModule)
        val mutationsAreNotMade = mutations.forall{
          case (_, md) => md.isEmpty
        }
        if (mutationsAreNotMade) { // fast exit if no mutations were made
            println("No mutations were made. Please verify that you have supplied the correct module names.")
            return
        }
        val selectedMutants = randomSelection(percentage, mutations)
        val end = System.nanoTime() - start
        val timeSec = end.toFloat / 1_000_000_000.0
        println(s"time to generate mutations: $timeSec")
        val lastRoot = insertDecAndCheckIntoRoot(root)
        runMutations(flix, testModule, lastRoot, selectedMutants)
        writeReportsToFile(nonKilledStrList)
    }


    def writeReportsToFile(reportsList: List[String]): Unit = {
      if (reportsList.isEmpty) return
      val stars = "*".repeat(60)
      var strForFile = reportsList.foldLeft("")((acc, str) => s"$acc\n$stars\n\n$str")
      strForFile = strForFile.substring(2)
      val fileWriter = new FileWriter(new File(s"mutation_report.txt"))
      fileWriter.write(strForFile)
      fileWriter.close()
    }

    private def randomSelection(percentage: Int, mutants: List[(Symbol.DefnSym, List[MutatedDef])]): List[(Symbol.DefnSym, List[MutatedDef])] =  {
      mutants.map{
        case (sym, mDefs) =>
          val shuffled = Random.shuffle(mDefs)
          val toRemain = (mDefs.length * percentage) / 100
          (sym, shuffled.take(toRemain))
      }
    }


  /**
      * Inserts a masked call to the static function decAndCheck into all defs in the root
      *
      * @param root: The TAST before the insertion of decAndCheck
      * @return The root with the call inserted into all defs
      */
    private def insertDecAndCheckIntoRoot(root: Root): Root = {
        val newDefs = root.defs.map({
            case (sym, fun) =>
                sym -> insertDecAndCheckInDef(fun)
        })
        root.copy(defs = newDefs)
    }
    private def printMutation(mutType: TypedAst.MutationType): String = {
      mutType match {
        case MutationType.CstMut(cst) => cst match {
          case Constant.Unit => "Unit"
          case Constant.Null => "Null"
          case Constant.Bool(lit) => s"Changed to ${lit.toString}"
          case Constant.Char(lit) => s"Changed to ${lit.toString}"
          case Constant.Float32(lit) => s"Changed to ${lit.toString}"
          case Constant.Float64(lit) => s"Changed to ${lit.toString}"
          case Constant.BigDecimal(lit) => s"Changed to ${lit.toString}"
          case Constant.Int8(lit) => s"Changed to ${lit.toString}"
          case Constant.Int16(lit) => s"Changed to ${lit.toString}"
          case Constant.Int32(lit) => s"Changed to ${lit.toString}"
          case Constant.Int64(lit) => s"Changed to ${lit.toString}"
          case Constant.BigInt(lit) => s"Changed to ${lit.toString}"
          case Constant.Str(lit) => s"Changed to \"${lit.toString}\""
          case Constant.Regex(lit) => s"Changed to ${lit.toString}"
        }
        case MutationType.SigMut(sig) =>
          val op = sig.name match {
          case "sub" => "-"
          case "mul" =>"*"
          case "div" =>"/"
          case "add" =>"+"
          case e => e
        }
          s"Changed to ${op}"
        case MutationType.IfMut(bool) => s"Changed the if-then-else condition to ${bool.toString}"
        case MutationType.CompMut(comp) => s"Changed to ${comp.name}"
        case MutationType.CaseSwitch(x, y) => s"Switched case ${x + 1} and case ${y + 1}"
        case MutationType.VarMut(apply, varName) =>  apply.sym.name match {
            case "sub" => s"Changed variable to ${varName.text} - 1"
            case "add" =>s"Changed variable to ${varName.text} + 1"
            case e => e
          }
        case MutationType.CaseDeletion(i) => s"Deleted case number ${i + 1}"
        case MutationType.RecordSelectMut(name) => s"selects ${name.name}"
        case MutationType.ListMut() => "Changed to the empty list"
      }
    }

    def insertDecAndCheckInDef(d: TypedAst.Def): TypedAst.Def = {
      val loc = d.exp.loc
      val method = classOf[Global].getMethods.find(m => m.getName.equals("decAndCheck")).get
      val InvokeMethod = Expr.InvokeStaticMethod(method, Nil, Type.Int64, Type.IO, loc)
      val mask = Expr.UncheckedMaskingCast(InvokeMethod, Type.Int64, Type.Pure, loc)
      val statement = Expr.Stm(mask, d.exp, d.exp.tpe, d.exp.eff, d.exp.loc)
      //println(s"name of function ${method.getName}")
      d.copy(exp = statement)
    }

    private def progressUpdate(message: String, timePassed: Long): Long = {
        var temp = timePassed
        val now = System.nanoTime()
        val nanoMin = 60_000_000_000.0
        // print update if a minute has passed since last print
        if (now - temp > nanoMin) {
            println(message)
            temp = now
        }
        temp
    }

    private case class TestKit(flix: Flix, root: Root, testModule: String)

    private def runMutations(flix: Flix, testModule: String, root: TypedAst.Root, mutatedDefs: List[(Symbol.DefnSym, List[MutatedDef])]): Unit = {
        val totalStartTime = System.nanoTime()
        val temp = totalStartTime
        val amountOfMutants = mutatedDefs.map(m => m._2.length).sum
        val f = DateTimeFormatter.ofPattern("yyyy-MM-dd: HH:mm")
        // (survivorCount, startTime, tempTime for progress updates, date for progress updates, amount of mutants currently tested)
        val localAcc = (0, 0, totalStartTime.toDouble, temp, 0)
        val (totalSurvivorCount, totalUnknowns, _, _, _) = mutatedDefs.foldLeft(localAcc)((acc, mut) => {
            val kit = TestKit(flix, root, testModule)
            println(s"testing ${mut._2.length} mutations in: ${mut._1}")
            testMutantsAndUpdateProgress(acc, mut, kit, f)
        })
        val totalEndTime = System.nanoTime() - totalStartTime
        println(s"mutation score: ${(amountOfMutants - totalSurvivorCount - totalUnknowns).toFloat/(amountOfMutants - totalUnknowns).toFloat}")
        println(s"There where $totalSurvivorCount surviving mutations, out of $amountOfMutants mutations")
        println(s"There where $totalUnknowns mutations that did not finish terminating, out of $amountOfMutants mutations")
        val nano = 1_000_000_000.0
        val ifMutants = amountOfMutants > 0
        val average = if (ifMutants) (totalEndTime / nano) / amountOfMutants else 0
        println(s"Average time to test a mutant:  $average seconds")
        val time = if (ifMutants) totalEndTime.toFloat / nano else 0
        println(s"Total time to test all mutants: $time seconds")
    }

    private def testMutantsAndUpdateProgress(acc: (Int, Int, Double, Long, Int), mut: (Symbol.DefnSym, List[MutatedDef]), testKit: TestKit, f: DateTimeFormatter) = {
        mut._2.foldLeft(acc)((acc2, mDef) => {
            val (survivorCount, unknownCount, time, accTemp, mAmount) = acc2
            val mutationAmount = mAmount + 1
            val start = System.nanoTime()
            val testResults = compileAndTestMutant(mDef.df, mut, testKit)
            val nano = 1_000_000_000
            val newTime = time + (System.nanoTime() - start).toDouble / nano
            val newSurvivorCount = if (testResults.equals(TestRes.MutantSurvived))  survivorCount + 1 else survivorCount
            val newUnknownCount = if (testResults.equals(TestRes.Unknown))  unknownCount + 1 else unknownCount
            if (testResults.equals(TestRes.MutantSurvived)) {
              // println(MutationReporter.reportNonKilledMutation(mDef.exp))
              println(testKit.flix.getFormatter.code(mDef.df.exp.loc, printMutation(mDef.mutType)))

              nonKilledStrList = testKit.flix.getFormatter.code(mDef.df.exp.loc, printMutation(mDef.mutType)) :: nonKilledStrList
            }

          val now = LocalDateTime.now()
            val message = s"[${f.format(now)}] Mutants: $mutationAmount, Killed: ${mutationAmount - survivorCount - unknownCount}, Survived: $survivorCount, Unknown: $unknownCount"
            val newTemp = progressUpdate(message, accTemp)
            (newSurvivorCount, newUnknownCount, newTime, newTemp, mutationAmount)
            //val sym = mDef.sym.toString
            //println(s"mutation in $sym survived")
        })
    }

    private def compileAndTestMutant(df: TypedAst.Def, mut: (Symbol.DefnSym, List[MutatedDef]), testKit: TestKit): TestRes = {
        val defs = testKit.root.defs
        val n = defs + (mut._1 -> df)
        val newRoot = testKit.root.copy(defs = n)
        val cRes = testKit.flix.codeGen(newRoot).unsafeGet
        val testsFromTester = cRes.getTests.filter { case (s, _) => s.namespace.head.equals(testKit.testModule) }.toList
        runTest(testsFromTester)
    }


    /**
      * Runs all tests on the individual mutation, until a test failed or all succeeded.
      *
      * @param testsFromTester  : all tests regarding the given source code.
      * @return TestRes.MutantKilled: A Test failed and we mark the mutant as killed.
      * @return TestRes.MutantSurvived: All Test succeeded and we can mark the mutant as survived.
      * @return TestRes.Unknown: The test didn't terminate within a fixed number of iterations. The mutant is marked as
      *         unknown and isn't included in the mutation score.
      *
      */
    private def runTest(testsFromTester: List[(Symbol.DefnSym, TestFn)]): TestRes = {
        testsFromTester match {
            case x :: xs =>
                try {
                    x._2.run() match {
                        case java.lang.Boolean.TRUE => runTest(xs)
                        case java.lang.Boolean.FALSE => TestRes.MutantKilled
                        case _ => runTest(xs)
                    }
                } catch {
                    case e: Throwable =>
                      if (e.getClass.toString.equals("class dev.flix.runtime.MutationError$")) {
                        TestRes.Unknown
                      }
                      else TestRes.MutantKilled
                }
            case _ => TestRes.MutantSurvived
        }
    }
}
