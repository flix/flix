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

  test("p.flix") {
    val args = Array("p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.files.exists(_.getName == "p.flix"))
  }

  test("p.flix.zip") {
    val args = Array("p.flix.zip")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.files.exists(_.getName == "p.flix.zip"))
  }

  test("a.flix b.flix c.flix") {
    val args = Array("a.flix", "b.flix", "c.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.files.exists(_.getName == "a.flix"))
    assert(opts.files.exists(_.getName == "b.flix"))
    assert(opts.files.exists(_.getName == "c.flix"))
  }

  test("-m") {
    val args = Array("-m", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.monitor)
  }

  test("--monitor") {
    val args = Array("--monitor", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.monitor)
  }

  test("-o") {
    val args = Array("-o", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.optimize)
  }

  test("--optimize") {
    val args = Array("--optimize", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.optimize)
  }

  test("-t") {
    val args = Array("-t", "42", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.threads == 42)
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

  test("-v") {
    val args = Array("-v", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.verbose)
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

  test("-p foo") {
    val args = Array("-p", "foo", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.print.contains("foo"))
  }

  test("-p foo,bar") {
    val args = Array("-p", "foo,bar", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.print.contains("foo"))
    assert(opts.print.contains("bar"))
  }

  test("--print foo") {
    val args = Array("--print", "foo", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.print.contains("foo"))
  }

  test("--print foo,bar") {
    val args = Array("--print", "foo,bar", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.print.contains("foo"))
    assert(opts.print.contains("bar"))
  }

  test("--Xdebug") {
    val args = Array("--Xdebug", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.debug)
  }

  test("--Xinterpreter") {
    val args = Array("--Xinterpreter", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.interpreter)
  }

}
