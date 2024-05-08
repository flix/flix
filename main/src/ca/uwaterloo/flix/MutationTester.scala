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
import ca.uwaterloo.flix.language.ast.TypedAst.{Expr, MutationType, Root, empty}
import dev.flix.runtime.Global

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import ca.uwaterloo.flix.language.phase.jvm.BackendObjType

import java.io.{File, FileWriter}
import scala.collection.immutable.HashMap
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

        case object Equivalent extends TestRes
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
          if (toRemain > 0)
            (sym, shuffled.take(toRemain))
          else (sym, shuffled.take(1))
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
        case MutationType.RecordSelectMut(name) => s"Selects ${name.name}"
        case MutationType.ListMut() => "Changed to the empty list"
      }
    }

    def insertDecAndCheckInDef(d: TypedAst.Def): TypedAst.Def = {
      val loc = d.exp.loc
      val method = classOf[Global].getMethods.find(m => m.getName.equals("decAndCheck")).get
      val InvokeMethod = Expr.InvokeStaticMethod(method, Nil, Type.Int64, Type.IO, loc)
      val mask = Expr.UncheckedMaskingCast(InvokeMethod, Type.Int64, Type.Pure, loc)
      val statement = Expr.Stm(mask, d.exp, d.exp.tpe, d.exp.eff, d.exp.loc)
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
        val emptyList: List[(MutationType, TestRes)] = Nil
        val localAcc = (0, 0, 0, totalStartTime.toDouble, temp, 0, emptyList)
        val (totalSurvivorCount, totalUnknowns, equivalents, _, _, _, mOperatorResults) = mutatedDefs.foldLeft(localAcc)((acc, mut) => {
            val kit = TestKit(flix, root, testModule)
            println(s"testing ${mut._2.length} mutations in: ${mut._1}")
            testMutantsAndUpdateProgress(acc, mut, kit, f)
        })
      reportResults(totalStartTime, amountOfMutants, totalSurvivorCount, totalUnknowns, equivalents)
      collectData(mOperatorResults)
    }

  private def collectData(operatorResults: List[(MutationType, TestRes)]): Unit = {
   val sortedData = sortData(operatorResults)
   writeDataToFile(sortedData)
  }
  private def writeDataToFile(stringToPoints: Map[String, DataPoints]): Unit = {
    val stringToWrite = stringToPoints.foldLeft("") {
      case (acc, (operator, dPoints)) =>
        s"$acc\n$operator:${dPoints.total}:${dPoints.killed}:${dPoints.surviving}:${dPoints.unknown}:${dPoints.equivalent}"
    }

    val fileWriter = new FileWriter(new File(s"MutStats.txt"))
    fileWriter.write(stringToWrite)
    fileWriter.close()
  }

  private case class DataPoints(total: Int, killed: Int, surviving: Int, unknown: Int, equivalent: Int)
  private def sortData(tuples: List[(MutationType, TestRes)]): Map[String, DataPoints] = {
    def updateDataPoints(dataPoints: DataPoints, testRes: TestRes): DataPoints = testRes match {
      case TestRes.MutantKilled => dataPoints.copy(total = dataPoints.total + 1, killed = dataPoints.killed + 1)
      case TestRes.MutantSurvived => dataPoints.copy(total = dataPoints.total + 1, killed = dataPoints.surviving + 1)
      case TestRes.Unknown => dataPoints.copy(total = dataPoints.total + 1, killed = dataPoints.unknown + 1)
      case TestRes.Equivalent => dataPoints.copy(total = dataPoints.total + 1, killed = dataPoints.equivalent + 1)
    }
    def helper(opRes: (MutationType, TestRes), hMap: HashMap[String, DataPoints])  = opRes._1 match {
      case MutationType.CstMut(_) =>
        val dPoints = hMap.apply("CstMut")
        val newDPoints = updateDataPoints(dPoints, opRes._2)
        hMap.updated("CstMut", newDPoints)
      case MutationType.SigMut(_) =>
        val dPoints = hMap.apply("SigMut")
        val newDPoints = updateDataPoints(dPoints, opRes._2)
        hMap.updated("SigMut", newDPoints)
      case MutationType.IfMut(_) =>
        val dPoints = hMap.apply("IfMut")
        val newDPoints = updateDataPoints(dPoints, opRes._2)
        hMap.updated("IfMut", newDPoints)
      case MutationType.CompMut(_) =>
        val dPoints = hMap.apply("CompMut")
        val newDPoints = updateDataPoints(dPoints, opRes._2)
        hMap.updated("CompMut", newDPoints)
      case MutationType.CaseSwitch(_, _) =>
        val dPoints = hMap.apply("CaseSwitch")
        val newDPoints = updateDataPoints(dPoints, opRes._2)
        hMap.updated("CaseSwitch", newDPoints)
      case MutationType.VarMut(_, _) =>
        val dPoints = hMap.apply("VarMut")
        val newDPoints = updateDataPoints(dPoints, opRes._2)
        hMap.updated("VarMut", newDPoints)
      case MutationType.CaseDeletion(_) =>
        val dPoints = hMap.apply("CaseDeletion")
        val newDPoints = updateDataPoints(dPoints, opRes._2)
        hMap.updated("CaseDeletion", newDPoints)
      case MutationType.RecordSelectMut(_) =>
        val dPoints = hMap.apply("RecordSelectMut")
        val newDPoints = updateDataPoints(dPoints, opRes._2)
        hMap.updated("RecordSelectMut", newDPoints)
      case MutationType.ListMut() =>
        val dPoints = hMap.apply("ListMut")
        val newDPoints = updateDataPoints(dPoints, opRes._2)
        hMap.updated("ListMut", newDPoints)
    }
    val emptyMap: HashMap[String, DataPoints] = HashMap.empty
    val emptyDP = DataPoints(0,0,0,0,0)
    val m1 =  emptyMap.updated("CstMut", emptyDP)
    val m2 =  m1.updated("ListMut", emptyDP)
    val m3 =  m2.updated("RecordSelectMut", emptyDP)
    val m4 =  m3.updated("CaseDeletion", emptyDP)
    val m5 =  m4.updated("VarMut", emptyDP)
    val m6 =  m5.updated("CaseSwitch", emptyDP)
    val m7 =  m6.updated("CompMut", emptyDP)
    val m8 =  m7.updated("IfMut", emptyDP)
    val m9 =  m8.updated("SigMut", emptyDP)


    tuples.foldLeft(m9)((acc, t) => helper(t, acc))
  }


  private def reportResults(totalStartTime: Long, amountOfMutants: Int, totalSurvivorCount: Int, totalUnknowns: Int, equivalents: Int): Unit = {

    val totalEndTime = System.nanoTime() - totalStartTime
    println(s"mutation score: ${(amountOfMutants - totalSurvivorCount - totalUnknowns -equivalents).toFloat/(amountOfMutants - totalUnknowns - equivalents).toFloat}")
    println(s"There where $totalSurvivorCount surviving mutations, out of $amountOfMutants mutations")
    println(s"There where $equivalents equivalent mutations, out of $amountOfMutants mutations")
    println(s"There where $totalUnknowns mutations that did not finish terminating, out of $amountOfMutants mutations")
    val nano = 1_000_000_000.0
    val ifMutants = amountOfMutants > 0
    val average = if (ifMutants) (totalEndTime / nano) / amountOfMutants else 0
    println(s"Average time to test a mutant:  $average seconds")
    val time = if (ifMutants) totalEndTime.toFloat / nano else 0
    println(s"Total time to test all mutants: $time seconds")
  }

    private def testMutantsAndUpdateProgress(acc: (Int, Int, Int, Double, Long, Int, List[(MutationType, TestRes)]), mut: (Symbol.DefnSym, List[MutatedDef]), testKit: TestKit, f: DateTimeFormatter) = {
        mut._2.foldLeft(acc)((acc2, mDef) => {
            val (survivorCount, unknownCount, eQCount, time, accTemp, mAmount, mTypeResults) = acc2
            val mutationAmount = mAmount + 1
            val start = System.nanoTime()
            val testResult = compileAndTestMutant(mDef.df, mut, testKit)
            val nano = 1_000_000_000
            val newTime = time + (System.nanoTime() - start).toDouble / nano
            val newSurvivorCount = if (testResult.equals(TestRes.MutantSurvived))  survivorCount + 1 else survivorCount
            val newUnknownCount = if (testResult.equals(TestRes.Unknown))  unknownCount + 1 else unknownCount
            val newEQCount = if (testResult.equals(TestRes.Equivalent))  eQCount + 1 else eQCount
            if (testResult.equals(TestRes.MutantSurvived)) {
              // println(MutationReporter.reportNonKilledMutation(mDef.exp))
              println(testKit.flix.getFormatter.code(mDef.df.exp.loc, printMutation(mDef.mutType)))

              nonKilledStrList = testKit.flix.getFormatter.code(mDef.df.exp.loc, printMutation(mDef.mutType)) :: nonKilledStrList
            }

          val now = LocalDateTime.now()
            val message = s"[${f.format(now)}] Mutants: $mutationAmount, Killed: ${mutationAmount - survivorCount - unknownCount}, Survived: $survivorCount, Unknown: $unknownCount"
            val newTemp = progressUpdate(message, accTemp)
            (newSurvivorCount, newUnknownCount, newEQCount, newTime, newTemp, mutationAmount, (mDef.mutType, testResult) :: mTypeResults)
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
                    case e: Throwable => {
                      e.getClass.toString match {
                        case "class dev.flix.runtime.MutationError$" => TestRes.Unknown
                        case "class dev.flix.runtime.EQMutantException$" => TestRes.Equivalent
                        case _ => TestRes.MutantKilled
                      }
                    }
                }
            case _ => TestRes.MutantSurvived
        }
    }
}
