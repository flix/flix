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

package ca.uwaterloo.flix.util.perf

import scala.util.Random

/**
 * Reference:
 *
 * https://en.wikipedia.org/wiki/Erd%C5%91s%E2%80%93R%C3%A9nyi_model
 */

object RandomGraph {

  def main(args: Array[String]): Unit = {

    // If np = 1, then a graph in G(n, p) will almost surely have a largest component whose size is of order n2/3.
    val N = args(0).toInt
    val P = 1.0 / N.toDouble

    val r = new Random()

    println(s"namespace Graph_N$N {")
    println()
    println(s"    // N = $N, P = $P")
    println()
    println(s"    rel Edge(x: Int, y: Int);")
    println(s"    rel Reach(x: Int, y: Int);")

    println(s"    index Edge({x}, {x, y});")
    println(s"    index Reach({y}, {x, y});")

    println(s"    Reach(x, y) :- Edge(x, y).")
    println(s"    Reach(x, y) :- Edge(y, x).")
    println(s"    Reach(x, z) :- Reach(x, y), Edge(y, z).")
    println()

    for (i <- 0 until N) {
      for (j <- 0 until N) {
        if (r.nextInt(Int.MaxValue).toDouble < (P * Int.MaxValue.toDouble))
          println(s"    Edge($i, $j).")
      }
    }

    println()
    println("}")
    println()

  }


  def connectedness(N: Double, P: Double): String = {
    if (P < (((1.0 - math.E) * Math.log(N)) / N))
      "Likely unconnected."
    else if (P > (((1.0 + math.E) * Math.log(N)) / N))
      "Likely connected."
    else
      "Unknown"
  }

}
