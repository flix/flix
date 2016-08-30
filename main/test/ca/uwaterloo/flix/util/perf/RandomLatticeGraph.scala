/*
 * Copyright 2016 Luqman Aden
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

object RandomLatticeGraph {

  val lattices = Map(
    "Belnap" -> Lattice(
      "Belnap",
      Array("Top", "True", "False", "Bot"),
      Array("not"),
      Array("lub", "glb", "and", "or", "xor", "implies", "bicondition", "nand")
    ),
    "Constant" -> Lattice(
      "Constant",
      Array("Top", "Cst(%n)", "Bot"),
      Array("inc", "dec", "negate"),
      Array("lub", "glb", "plus", "minus", "times", "divide", "modulo", "and", "or", "xor", "leftShift", "rightShift")
    ),
    "ConstantParity" -> Lattice(
      "ConstParity",
      Array("Top", "Odd", "Even", "Cst(%n)", "Bot"),
      Array("inc", "Dec"),
      Array("lub", "glb", "plus", "minus", "times", "divide", "modulo")
    ),
    "ConstantSign" -> Lattice(
      "ConstSign",
      Array("Top", "Neg", "Pos", "Cst(%n)", "Bot"),
      Array("inc", "dec"),
      Array("lub", "glb", "plus", "minus", "times", "divide", "modulo")
    ),
    "Interval" -> Lattice(
      "Interval",
      Array("Top", "Range(%n, %n)"),
      Array("norm"),
      Array("lub", "glb")
    ),
    "IntervalAlt" -> Lattice(
      "Interval",
      Array("Top", "Range(%n, %n)", "Bot"),
      Array(),
      Array("lub", "glb", "plus", "minus", "times", "divide")
    ),
    "Mod3" -> Lattice(
      "Mod3",
      Array("Top", "Zer", "One", "Two", "Bot"),
      Array("inc", "dec"),
      Array("lub", "glb", "plus", "minus", "times", "divide", "modulo")
    ),
    "Parity" -> Lattice(
      "Parity",
      Array("Top", "Odd", "Even", "Bot"),
      Array("inc", "dec", "negate"),
      Array("lub", "glb", "plus", "minus", "times", "divide", "modulo", "and", "or", "xor", "leftShift", "rightShift")
    ),
    "ParitySign" -> Lattice(
      "ParitySign",
      Array("Top", "ENeg", "EPos", "ONeg", "OPos", "Zer", "Bot"),
      Array("inc", "dec"),
      Array("lub", "glb", "plus", "minus")
    ),
    "PrefixSuffix" -> Lattice(
      "PS",
      Array("Top", "Pre(%n)", "Suf(%n)", "PreSuf(%n, %n)", "Bot"),
      Array(),
      Array("lub", "glb", "concatenate")
    ),
    "Sign" -> Lattice(
      "Sign",
      Array("Top", "Neg", "Pos", "Zer", "Bot"),
      Array("inc", "dec", "negate"),
      Array("lub", "glb", "plus", "minus", "times", "divide", "modulo", "and", "or", "xor", "leftShift", "rightShift")
    ),
    "StrictSign" -> Lattice(
      "Sign",
      Array("Top", "Neg", "Pos", "Zer", "Bot"),
      Array("inc", "dec", "negate"),
      Array("lub", "glb", "plus", "minus", "times", "divide", "modulo", "and", "or", "xor", "leftShift", "rightShift")
    ),
    "Type" -> Lattice(
      "Type",
      Array("Err", "Bool", "Int", "Real", "Bot"),
      Array(),
      Array("lub", "glb", "sum")
    )
  )

  case class Lattice(name: String, els: Array[String], unOps: Array[String], biOps: Array[String])

  def generate(lattice: String, N: Int): Option[String] = generate(lattice, N, Random.nextLong())

  def generate(which: String, N: Int, seed: Long): Option[String] = {
    val lattice = lattices get which match {
      case Some(l) => l
      case None => return None
    }
    val rng = new Random(seed)
    val out = new StringBuilder()

    def appendln(s: String): Unit = {
      out.append(s)
      out.append(System.lineSeparator())
    }

    appendln(s"// N: $N")
    appendln(s"// seed: $seed")

    // Create a list of N values, each assigned a random from our lattice
    val values: Array[String] = Seq.fill(N) {
      lattice.els(rng.nextInt(lattice.els.length))
    }.toArray

    appendln(s"namespace Random${which}Graph {")
    appendln("")
    appendln(s"    lat Value(x: Int, xv: $which/${lattice.name});")
    appendln(s"    rel UnOp(y: Int, x: Int, op: Str);")
    appendln(s"    rel BiOp(z: Int, x: Int, y: Int, op: Str);")
    appendln("")

    // Print out the UnOp rules
    for (unOp <- lattice.unOps) {
      appendln(s"""    Value(y, $which/$unOp(xv)) :- UnOp(y, x, "$unOp"), Value(x, xv).""")
    }

    appendln("")

    // and the BiOp rules
    for (biOp <- lattice.biOps) {
      appendln(s"""    Value(z, $which/$biOp(xv, yv)) :- BiOp(z, x, y, "$biOp"), Value(x, xv), Value(y, yv).""")
    }

    appendln("")

    // Print out the random Values
    for ((value, i) <- values.zipWithIndex) {
      // Replace any %n in the value with a random Int
      val v = "%n".r.replaceAllIn(value, _ => rng.nextInt(100000).toString)
      appendln(s"    Value($i, $which/${lattice.name}.$v).")
    }

    appendln("")

    if (lattice.unOps.length > 0) {
      // Generate and print out random UnOps
      for (i <- 0 until 3*N) {

        // Choose some random values
        val x = rng.nextInt(values.length)
        val y = rng.nextInt(values.length)

        // and a random operation to perform on them
        val op = lattice.unOps(rng.nextInt(lattice.unOps.length))

        appendln(s"""    UnOp($y, $x, "$op").""")
      }
    }

    if (lattice.biOps.length > 0) {
      // Generate and print out random BiOps
      for (i <- 0 until 7*N) {

        // Choose some random values
        val x = rng.nextInt(values.length)
        val y = rng.nextInt(values.length)
        val z = rng.nextInt(values.length)

        // and a random operation to perform on them
        val op = lattice.biOps(rng.nextInt(lattice.biOps.length))

        appendln(s"""    BiOp($z, $x, $y, "$op").""")

      }
    }

    appendln("")
    appendln("}")

    Some(out.toString())
  }

  def main(args: Array[String]): Unit = {

    if (args.length < 2) {
      println("Usage: lattice N [seed]")
      println()
      println(s"\tlattice \t- one of: ${lattices.keys.mkString(", ")}")
      println("\t N \t\t- how many random values to generate")
      println("\tseed \t\t- value to seed RNG with")
      return
    }

    val which = args(0).toString
    val N = args(1).toInt

    if (!lattices.contains(which)) {
      println(s"Invalid lattice - $which")
      return
    }

    val out = if (args.length == 3) {
      generate(which, N, args(2).toLong)
    } else {
      generate(which, N)
    }

    println(out.get)
  }

}
