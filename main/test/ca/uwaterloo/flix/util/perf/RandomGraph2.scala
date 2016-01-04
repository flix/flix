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
