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

import java.util.concurrent.TimeUnit

import org.scalatest.FunSuite

import scala.concurrent.duration.Duration

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

  test("--listen") {
    val args = Array("--listen", "8080", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.listen.nonEmpty)
  }

  test("--main foo") {
    val args = Array("--main", "foo", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assertResult(Some("foo"))(opts.main)
  }

  test("--monitor") {
    val args = Array("--monitor", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.monitor)
  }

  test("--quickchecker") {
    val args = Array("--quickchecker")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.quickchecker)
  }

  test("--pipe") {
    val args = Array("--pipe")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.pipe)
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

  test("--timeout 42ms") {
    val args = Array("--timeout", "42ms", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.timeout == Duration(42, TimeUnit.MILLISECONDS))
  }

  test("--timeout 42s") {
    val args = Array("--timeout", "42s", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.timeout == Duration(42, TimeUnit.SECONDS))
  }

  test("--timeout 42min") {
    val args = Array("--timeout", "42min", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.timeout == Duration(42, TimeUnit.MINUTES))
  }

  test("--timeout 42hours") {
    val args = Array("--timeout", "42hours", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.timeout == Duration(42, TimeUnit.HOURS))
  }

  test("--threads") {
    val args = Array("--threads", "42", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.threads == 42)
  }

  test("--tutorial") {
    val args = Array("--tutorial", "tut.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.tutorial != null)
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

  test("--Xcore") {
    val args = Array("--Xcore", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.xcore)
  }

  test("--Xdebug") {
    val args = Array("--Xdebug", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.xdebug)
  }

  test("--Xinterpreter") {
    val args = Array("--Xinterpreter", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.xinterpreter)
  }

  test("--Xinvariants") {
    val args = Array("--Xinvariants", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.xinvariants)
  }

  test("--Xno-fusion") {
    val args = Array("--Xno-fusion", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.xnofusion)
  }

  test("--Xno-inline") {
    val args = Array("--Xno-inline", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.xnoinline)
  }

  test("--Xno-single-case") {
    val args = Array("--Xno-single-case", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.xnosinglecase)
  }

  test("--Xno-tailcalls") {
    val args = Array("--Xno-tailcalls", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.xnotailcalls)
  }

}
