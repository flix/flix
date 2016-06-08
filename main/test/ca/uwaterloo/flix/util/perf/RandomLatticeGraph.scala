package ca.uwaterloo.flix.util.perf

import scala.collection.immutable.HashMap
import scala.util.matching.Regex
import scala.util.Random

object RandomLatticeGraph {

  case class Lattice(name: String, els: Array[String], unOps: Array[String], biOps: Array[String])

  def main(args: Array[String]): Unit = {

    val lattices = HashMap(
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
    val rng = if (args.length == 3) {
      new Random(args(2).toLong)
    } else {
      new Random()
    }

    val lattice = lattices get which match {
      case Some(l) => l
      case None => {
        println(s"Invalid lattice - $which")
        return
      }
    }

    // Create a list of N values, each assigned a random from our lattice
    val values: Array[String] = Seq.fill(N) {
      lattice.els(rng.nextInt(lattice.els.length))
    }.toArray

    println(s"namespace Random${which}Graph {")
    println()
    println(s"    lat Value(x: Int, xv: $which/${lattice.name});")
    println(s"    rel UnOp(y: Int, x: Int, op: Str);")
    println(s"    rel BiOp(z: Int, x: Int, y: Int, op: Str);")
    println()

    // Print out the UnOp rules
    for (unOp <- lattice.unOps) {
      println(s"""    Value(y, $which/$unOp(xv)) :- UnOp(y, x, "$unOp"), Value(x, xv).""")
    }

    println()

    // and the BiOp rules
    for (biOp <- lattice.biOps) {
      println(s"""    Value(z, $which/$biOp(xv, yv)) :- BiOp(z, x, y, "$biOp"), Value(x, xv), Value(y, yv).""")
    }

    println()

    // Print out the random Values
    for ((value, i) <- values.zipWithIndex) {
      // Replace any %n in the value with a random Int
      val v = "%n".r.replaceAllIn(value, _ => rng.nextInt(100000).toString)
      println(s"    Value($i, $which/${lattice.name}.$v).")
    }

    println()

    if (lattice.unOps.length > 0) {
      // Generate and print out random UnOps
      for (i <- 0 until 3*N) {

        // Choose some random values
        val x = rng.nextInt(values.length)
        val y = rng.nextInt(values.length)

        // and a random operation to perform on them
        val op = lattice.unOps(rng.nextInt(lattice.unOps.length))

        println(s"""    UnOp($y, $x, "$op").""")
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

        println(s"""    BiOp($z, $x, $y, "$op").""")

      }
    }

    println()
    println("}")

  }

}
