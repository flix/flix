/*
 *  Copyright 2016 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package ca.uwaterloo.flix.util

import ca.uwaterloo.flix.api.Flix
import org.scalatest.FunSuite

class FlixTest(name: String, path: String) extends FunSuite {

  override def suiteName: String = name

  val Library: List[String] = List(
    "main/src/library/Option.flix",
    "main/src/library/Result.flix"
  )

  {
    val opts = Options.DefaultTest.copy(evaluation = Evaluation.Interpreted)
    val flix = new Flix().setOptions(opts)

    flix.addPath(path)
    for (path <- Library)
      flix.addPath(path)


    val model = flix.solve().get

    for ((sym, fn) <- model.getTests.toList.sortBy(_._1.toString)) {
      test(sym.name) {
        withClue(sym.loc.format) {
          val result = fn()
          assertResult(true)(result)
        }
      }
    }
  }


}
