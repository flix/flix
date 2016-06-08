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

import scala.collection.mutable
import scala.util.Random

/**
 * Constructs a random graph with N vertices and M edges.
 */

object RandomGraph2 {

  def main(args: Array[String]): Unit = {

    val N = args(0).toInt
    val M = args(1).toInt

    val r = new Random()

    println(s"namespace Graph_N$N {")
    println()
    println(s"    // N = $N, M = $M")
    println()
    println(s"    rel Edge(x: Int, y: Int);")
    println(s"    rel Reach(x: Int, y: Int);")
    println()
    println(s"    index Edge({x}, {x, y});")
    println(s"    index Reach({y}, {x, y});")
    println()
    println(s"    Reach(x, y) :- Edge(x, y).")
    println(s"    Reach(x, y) :- Edge(y, x).")
    println(s"    Reach(x, z) :- Reach(x, y), Edge(y, z).")
    println()

    val result = mutable.Set.empty[(Int, Int)]
    while(result.size < M) {
      val src = r.nextInt(N)
      val dst = r.nextInt(N)
      result += (src -> dst)
    }

    for ((src, dst) <- result) {
      println(s"    Edge($src, $dst).")
    }

    println()
    println("}")
    println()

  }

}
