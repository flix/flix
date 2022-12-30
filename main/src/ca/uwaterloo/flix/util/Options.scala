/*
 * Copyright 2015-2016 Magnus Madsen
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

package ca.uwaterloo.flix.util

import ca.uwaterloo.flix.language.ast.Symbol

import java.io.PrintStream
import java.nio.file.Path

object Options {
  /**
    * Default options.
    */
  val Default: Options = Options(
    lib = LibLevel.All,
    debug = false,
    documentor = false,
    entryPoint = None,
    explain = false,
    incremental = true,
    json = false,
    output = None,
    progress = false,
    test = false,
    target = JvmTarget.Version18,
    threads = Runtime.getRuntime.availableProcessors(),
    loadClassFiles = true,
    xallowredundancies = false,
    xbddthreshold = None,
    xboolclassic = false,
    xnoboolcache = false,
    xnoboolspecialcases = false,
    xnobooltable = false,
    xnounittests = false,
    xstatistics = false,
    xstrictmono = false,
    xnoseteffects = false,
    xnobooleffects = false,
    xnooptimizer = false,
    xvirtualthreads = false,
    xprintast = Map.empty,
    xqmc = false,
    xflexibleregions = false,
  )

  /**
    * Default test options.
    */
  val DefaultTest: Options = Default.copy(lib = LibLevel.All, progress = false, test = true)

  /**
    * Default test options with the standard library.
    */
  val TestWithLibAll: Options = DefaultTest

  /**
    * Default test options with the minimal library.
    */
  val TestWithLibMin: Options = DefaultTest.copy(lib = LibLevel.Min)

  /**
    * Default test options without any library.
    */
  val TestWithLibNix: Options = DefaultTest.copy(lib = LibLevel.Nix)
}

/**
  * General Flix options.
  *
  * @param lib                selects the level of libraries to include.
  * @param debug              enables the emission of debugging information.
  * @param documentor         enables generation of flixdoc.
  * @param entryPoint         specifies the main entry point.
  * @param explain            enables additional explanations.
  * @param json               enable json output.
  * @param output             the optional output directory where to place JVM bytecode.
  * @param progress           print progress during compilation.
  * @param test               enables test mode.
  * @param target             the target JVM.
  * @param threads            selects the number of threads to use.
  * @param loadClassFiles     loads the generated class files into the JVM.
  * @param xallowredundancies disables the redundancy checker.
  * @param xbddthreshold      the threshold for when to use BDDs for SVE.
  * @param xnoboolcache       disable Boolean caches.
  * @param xnoboolspecialcases   disable Boolean unification shortcuts.
  * @param xnobooltable       disable Boolean minimization via tabling.
  * @param xnounittests       excludes unit tests from performance benchmarks.
  * @param xstatistics        enables statistics collection.
  * @param xqmc               enables the Quine McCluskey algorihm when using BDDs.
  * @param xstrictmono        enables strict monomorphization.
  * @param xprintast          prints the chosen AST to a given path.
  */
case class Options(lib: LibLevel,
                   debug: Boolean,
                   documentor: Boolean,
                   entryPoint: Option[Symbol.DefnSym],
                   explain: Boolean,
                   incremental: Boolean,
                   json: Boolean,
                   progress: Boolean,
                   output: Option[Path],
                   target: JvmTarget,
                   test: Boolean,
                   threads: Int,
                   loadClassFiles: Boolean,
                   xallowredundancies: Boolean,
                   xbddthreshold: Option[Int],
                   xboolclassic: Boolean,
                   xnoboolcache: Boolean,
                   xnoboolspecialcases: Boolean,
                   xnobooltable: Boolean,
                   xnounittests: Boolean,
                   xstatistics: Boolean,
                   xstrictmono: Boolean,
                   xnoseteffects: Boolean,
                   xnobooleffects: Boolean,
                   xnooptimizer: Boolean,
                   xvirtualthreads: Boolean,
                   xprintast: Map[Phase, Either[PrintStream, String]],
                   xqmc: Boolean,
                   xflexibleregions: Boolean,
                  )

/**
  * An option to control the version of emitted JVM bytecode.
  */
sealed trait JvmTarget

object JvmTarget {

  /**
    * Emit bytecode for Java 1.6.
    */
  object Version16 extends JvmTarget

  /**
    * Emit bytecode for Java 1.7.
    */
  object Version17 extends JvmTarget

  /**
    * Emit bytecode for Java 1.8.
    */
  object Version18 extends JvmTarget

  /**
    * Emit bytecode for Java 1.9.
    */
  object Version19 extends JvmTarget

}

sealed trait LibLevel

object LibLevel {

  /**
    * Do not include any libraries, even those essential for basic functionality.
    */
  case object Nix extends LibLevel

  /**
    * Only include essential libraries.
    */
  case object Min extends LibLevel

  /**
    * Include the full standard library.
    */
  case object All extends LibLevel
}


sealed trait Phase {

  def index: Int = this match {
    case Phase.Parser           =>  1
    case Phase.Weeder           =>  2
    case Phase.Namer            =>  3
    case Phase.Resolver         =>  4
    case Phase.Kinder           =>  5
    case Phase.Deriver          =>  6
    case Phase.Typer            =>  7
    case Phase.EntryPoint       =>  8
    case Phase.Statistics       =>  9
    case Phase.Stratifier       => 10
    case Phase.Regions          => 11
    case Phase.Redundancy       => 12
    case Phase.Safety           => 13
    case Phase.Documentor       => 14
    case Phase.Lowering         => 15
    case Phase.EarlyTreeShaker  => 16
    case Phase.Monomoph         => 17
    case Phase.Simplifier       => 18
    case Phase.ClosureConv      => 19
    case Phase.LambdaLift       => 20
    case Phase.Tailrec          => 21
    case Phase.Optimizer        => 22
    case Phase.LateTreeShaker   => 23
    case Phase.VarNumbering     => 24
    case Phase.Finalize         => 25
    case Phase.Eraser           => 26
  }

  def name: String = this match {
    case Phase.Parser           => "Parser"
    case Phase.Weeder           => "Weeder"
    case Phase.Namer            => "Namer"
    case Phase.Resolver         => "Resolver"
    case Phase.Kinder           => "Kinder"
    case Phase.Deriver          => "Deriver"
    case Phase.Typer            => "Typer"
    case Phase.EntryPoint       => "EntryPoint"
    case Phase.Statistics       => "Statistics"
    case Phase.Stratifier       => "Stratifier"
    case Phase.Regions          => "Regions"
    case Phase.Redundancy       => "Redundancy"
    case Phase.Safety           => "Safety"
    case Phase.Documentor       => "Documentor"
    case Phase.Lowering         => "Lowering"
    case Phase.EarlyTreeShaker  => "EarlyTreeShaker"
    case Phase.Monomoph         => "Monomorph"
    case Phase.Simplifier       => "Sipmlifier"
    case Phase.ClosureConv      => "ClosureConv"
    case Phase.LambdaLift       => "LambdaLift"
    case Phase.Tailrec          => "Tailrec"
    case Phase.Optimizer        => "Optimizer"
    case Phase.LateTreeShaker   => "LateTreeShaker"
    case Phase.VarNumbering     => "VarNumbering"
    case Phase.Finalize         => "Finalize"
    case Phase.Eraser           => "Eraser"
  }

}

object Phase {

  def fromIndex(i: Int): Result[Phase, String] = i match {
    case 1  => Result.Ok(Phase.Parser)
    case 2  => Result.Ok(Phase.Weeder)
    case 3  => Result.Ok(Phase.Namer)
    case 4  => Result.Ok(Phase.Resolver)
    case 5  => Result.Ok(Phase.Kinder)
    case 6  => Result.Ok(Phase.Deriver)
    case 7  => Result.Ok(Phase.Typer)
    case 8  => Result.Ok(Phase.EntryPoint)
    case 9  => Result.Ok(Phase.Statistics)
    case 10 => Result.Ok(Phase.Stratifier)
    case 11 => Result.Ok(Phase.Regions)
    case 12 => Result.Ok(Phase.Redundancy)
    case 13 => Result.Ok(Phase.Safety)
    case 14 => Result.Ok(Phase.Documentor)
    case 15 => Result.Ok(Phase.Lowering)
    case 16 => Result.Ok(Phase.EarlyTreeShaker)
    case 17 => Result.Ok(Phase.Monomoph)
    case 18 => Result.Ok(Phase.Simplifier)
    case 19 => Result.Ok(Phase.ClosureConv)
    case 20 => Result.Ok(Phase.LambdaLift)
    case 21 => Result.Ok(Phase.Tailrec)
    case 22 => Result.Ok(Phase.Optimizer)
    case 23 => Result.Ok(Phase.LateTreeShaker)
    case 24 => Result.Ok(Phase.VarNumbering)
    case 25 => Result.Ok(Phase.Finalize)
    case 26 => Result.Ok(Phase.Eraser)
    case _  => Result.Err(s"'$i' is not a phase index (must be in the inclusive range 1 - 26).")
  }

  def fromString(s: String): Result[Phase, String] = s.toLowerCase match {
    case "parser"           => Result.Ok(Phase.Parser)
    case "weeder"           => Result.Ok(Phase.Weeder)
    case "namer"            => Result.Ok(Phase.Namer)
    case "resolver"         => Result.Ok(Phase.Resolver)
    case "kinder"           => Result.Ok(Phase.Kinder)
    case "deriver"          => Result.Ok(Phase.Deriver)
    case "typer"            => Result.Ok(Phase.Typer)
    case "entrypoint"       => Result.Ok(Phase.EntryPoint)
    case "statistics"       => Result.Ok(Phase.Statistics)
    case "stratifier"       => Result.Ok(Phase.Stratifier)
    case "regions"          => Result.Ok(Phase.Regions)
    case "redundancy"       => Result.Ok(Phase.Redundancy)
    case "safety"           => Result.Ok(Phase.Safety)
    case "documentor"       => Result.Ok(Phase.Documentor)
    case "lowering"         => Result.Ok(Phase.Lowering)
    case "earlytreeshaker"  => Result.Ok(Phase.EarlyTreeShaker)
    case "monomorph"        => Result.Ok(Phase.Monomoph)
    case "simplifier"       => Result.Ok(Phase.Simplifier)
    case "closureconv"      => Result.Ok(Phase.ClosureConv)
    case "lambdalift"       => Result.Ok(Phase.LambdaLift)
    case "tailrec"          => Result.Ok(Phase.Tailrec)
    case "optimizer"        => Result.Ok(Phase.Optimizer)
    case "latetreeshaker"   => Result.Ok(Phase.LateTreeShaker)
    case "varnumbering"     => Result.Ok(Phase.VarNumbering)
    case "finalize"         => Result.Ok(Phase.Finalize)
    case "eraser"           => Result.Ok(Phase.Eraser)
    case _                  => Result.Err(s"'$s' is not a phase name.")
  }

  // Front-end

  case object Parser extends Phase

  case object Weeder extends Phase

  case object Namer extends Phase

  case object Resolver extends Phase

  case object Kinder extends Phase

  case object Deriver extends Phase

  case object Typer extends Phase

  case object EntryPoint extends Phase

  case object Statistics extends Phase

  case object Stratifier extends Phase

  case object Regions extends Phase

  case object Redundancy extends Phase

  case object Safety extends Phase

  // Back-end

  case object Documentor extends Phase

  case object Lowering extends Phase

  case object EarlyTreeShaker extends Phase

  case object Monomoph extends Phase

  case object Simplifier extends Phase

  case object ClosureConv extends Phase

  case object LambdaLift extends Phase

  case object Tailrec extends Phase

  case object Optimizer extends Phase

  case object LateTreeShaker extends Phase

  case object VarNumbering extends Phase

  case object Finalize extends Phase

  case object Eraser extends Phase


}
