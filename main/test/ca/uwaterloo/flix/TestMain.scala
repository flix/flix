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

  test("--delta") {
    val args = Array("--delta", "delta.flix", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.delta.nonEmpty)
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

  test("--optimize") {
    val args = Array("--optimize", "p.flix")
    val opts = Main.parseCmdOpts(args).get
    assert(opts.optimize)
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

}
