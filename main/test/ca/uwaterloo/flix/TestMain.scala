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

import ca.uwaterloo.flix.util.LibLevel
import org.scalatest.funsuite.AnyFunSuite

class TestMain extends AnyFunSuite {

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

  test("release") {
    val args = Array("release")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.command == Main.Command.Release)
  }

  test("outdated") {
    val args = Array("outdated")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.command == Main.Command.Outdated)
  }

  test("doc") {
    val args = Array("doc")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.command == Main.Command.Doc)
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

  test("repl") {
    val args = Array("repl")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.command == Main.Command.Repl)
  }

  test("--args --abc --def") {
    val args = Array("--args", "--abc --def")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.args.contains("--abc --def"))
  }

  test("--explain foo") {
    val args = Array("--explain", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.explain)
  }

  test("--entrypoint foo") {
    val args = Array("--entrypoint", "foo", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.entryPoint.nonEmpty)
  }

  test("--json") {
    val args = Array("--json")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.json)
  }

  test("--no-install") {
    val args = Array("--no-install")
    val opts = Main.parseCmdOpts(args).get
    assert(!opts.installDeps)
  }

  test("--listen") {
    val args = Array("--listen", "8080", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.listen.nonEmpty)
  }

  test("--threads") {
    val args = Array("--threads", "42", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.threads.contains(42))
  }

  test("--yes") {
    val args = Array("--yes")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.assumeYes)
  }

  test("--Xbenchmark-code-size") {
    val args = Array("--Xbenchmark-code-size", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.xbenchmarkCodeSize)
  }

  test("--Xbenchmark-phases") {
    val args = Array("--Xbenchmark-phases", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.xbenchmarkPhases)
  }

  test("--Xbenchmark-frontend") {
    val args = Array("--Xbenchmark-frontend", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.xbenchmarkFrontend)
  }

  test("--Xbenchmark-throughput") {
    val args = Array("--Xbenchmark-throughput", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.xbenchmarkThroughput)
  }

  test("--Xlib nix") {
    val args = Array("--Xlib", "nix", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.xlib == LibLevel.Nix)
  }

  test("--Xlib min") {
    val args = Array("--Xlib", "min", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.xlib == LibLevel.Min)
  }

  test("--Xlib all") {
    val args = Array("--Xlib", "all", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.xlib == LibLevel.All)
  }

  test("--Xno-bool-cache") {
    val args = Array("--Xno-bool-cache")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.xnoboolcache)
  }

  test("--Xno-bool-specialcases") {
    val args = Array("--Xno-bool-specialcases")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.xnoboolspecialcases)
  }

  test("--Xno-bool-unif") {
    val args = Array("--Xno-bool-unif")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.xnoboolunif)
  }

  test("--explain") {
    val args = Array("--explain")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.explain)
  }

  test("--Xno-deprecated") {
    val args = Array("--Xno-deprecated")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.xnodeprecated)
  }

  test("--Xsummary") {
    val args = Array("--Xsummary")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.xsummary)
  }

}
