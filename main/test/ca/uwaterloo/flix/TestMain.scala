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

package ca.uwaterloo.flix

import org.scalatest.FunSuite

class TestMain extends FunSuite {

  test("init") {
    val args = Array("init")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.command == Main.Command.Init)
  }

  test("build") {
    val args = Array("build")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.command == Main.Command.Build)
  }

  test("build-jar") {
    val args = Array("build-jar")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.command == Main.Command.BuildJar)
  }

  test("build-pkg") {
    val args = Array("build-pkg")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.command == Main.Command.BuildPkg)
  }

  test("run") {
    val args = Array("run")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.command == Main.Command.Run)
  }

  test("test") {
    val args = Array("test")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.command == Main.Command.Test)
  }

  test("--args --abc --def") {
    val args = Array("--args", "--abc --def")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.args.contains("--abc --def"))
  }

  test("--benchmark") {
    val args = Array("--benchmark", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.benchmark)
  }

  test("--doc") {
    val args = Array("--doc", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.documentor)
  }

  test("--interactive") {
    val args = Array("--interactive", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.interactive)
  }

  test("--json") {
    val args = Array("--json")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.json)
  }

  test("--listen") {
    val args = Array("--listen", "8080", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.listen.nonEmpty)
  }

  test("--lsp") {
    val args = Array("--lsp", "8080", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.lsp.nonEmpty)
  }

  test("--quickchecker") {
    val args = Array("--quickchecker")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.quickchecker)
  }

  test("--release") {
    val args = Array("--release", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.release)
  }

  test("--test") {
    val args = Array("--test", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.test)
  }

  test("--threads") {
    val args = Array("--threads", "42", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.threads.contains(42))
  }

  test("--verbose") {
    val args = Array("--verbose", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.verbose)
  }

  test("--verifier") {
    val args = Array("--verifier", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.verifier)
  }

  test("--Xallow-redundancies") {
    val args = Array("--Xallow-redundancies", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.xallowredundancies)
  }

  test("--Xbenchmark-phases") {
    val args = Array("--Xbenchmark-phases", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.xbenchmarkPhases)
  }

  test("--Xbenchmark-throughput") {
    val args = Array("--Xbenchmark-throughput", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.xbenchmarkThroughput)
  }

  test("--Xdebug") {
    val args = Array("--Xdebug", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.xdebug)
  }

  test("--Xinvariants") {
    val args = Array("--Xinvariants", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.xinvariants)
  }

  test("--Xlinter") {
    val args = Array("--Xlinter", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.xlinter)
  }

  test("--Xno-bool-unification") {
    val args = Array("--Xno-bool-unification", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.xnoboolunification)
  }

  test("--Xno-stratifier") {
    val args = Array("--Xno-stratifier", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.xnostratifier)
  }

  test("--Xno-tailcalls") {
    val args = Array("--Xno-tailcalls", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.xnotailcalls)
  }

  test("--Xstatistics") {
    val args = Array("--Xstatistics", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.xstatistics)
  }

}
