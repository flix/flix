package util.output

import impl.runtime.Solver
import syntax.Programs._
import syntax.Symbols._
import syntax.Values._

object Solution {

  def print(s: Solver): Unit = {
    println("*** Relations ***")
    for ((p, vs) <- s.relation1; v1 <- vs) {
      println(p.fmt + "(" + v1.fmt + ").")
    }
    for ((p, vs) <- s.relation2; (v1, v2) <- vs) {
      println(p.fmt + "(" + v1.fmt + ", " + v2.fmt + ").")
    }
    for ((p, vs) <- s.relation3; (v1, v2, v3) <- vs) {
      println(p.fmt + "(" + v1.fmt + ", " + v2.fmt + ", " + v3.fmt + ").")
    }
    for ((p, vs) <- s.relation4; (v1, v2, v3, v4) <- vs) {
      println(p.fmt + "(" + v1.fmt + ", " + v2.fmt + ", " + v3.fmt + ", " + v4.fmt + ").")
    }
    for ((p, vs) <- s.relation5; (v1, v2, v3, v4, v5) <- vs) {
      println(p.fmt + "(" + v1.fmt + ", " + v2.fmt + ", " + v3.fmt + ", " + v4.fmt + ", " + v5.fmt + ").")
    }
    println()
    println()

    println("*** Lattices ***")
    for ((p, v1) <- s.map1) {
      println(p.fmt + "(" + v1.fmt + ").")
    }
    for ((p, (v1, v2)) <- s.map2) {
      println(p.fmt + "(" + v1.fmt + ", " + v2.fmt + ").")
    }
    for ((p, (v1, v2, v3)) <- s.map3) {
      println(p.fmt + "(" + v1.fmt + ", " + v2.fmt + ", " + v3.fmt + ").")
    }
    for ((p, (v1, v2, v3, v4)) <- s.map4) {
      println(p.fmt + "(" + v1.fmt + ", " + v2.fmt + ", " + v3.fmt + ", " + v4.fmt + ").")
    }
    for ((p, (v1, v2, v3, v4, v5)) <- s.map5) {
      println(p.fmt + "(" + v1.fmt + ", " + v2.fmt + ", " + v3.fmt + ", " + v4.fmt + ", " + v5.fmt + ").")
    }
  }
}
