package ca.uwaterloo.flix.runtime.solver.api

import java.io.{PrintWriter, StringWriter}

import ca.uwaterloo.flix.runtime.solver.LatticeOps
import ca.uwaterloo.flix.runtime.solver.datastore.IndexedLattice
import ca.uwaterloo.flix.util.{AsciiTable, BitOps}

/**
  * Represents a lattice value.
  */
class Lattice(name: String, keys: Array[Attribute], value: Attribute, ops: LatticeOps) extends Table {

  /**
    * Returns the name of the lattice.
    */
  def getName(): String = name

  /**
    * Returns the key attributes of the lattice.
    */
  def getKeys(): Array[Attribute] = keys

  /**
    * Returns the value attribute of the lattice.
    */
  def getValue(): Attribute = value

  private val indexedLattice = {
    // NB: Just an index on the first attribute and on all the keys.
    val idx = Set(Seq(0), keys.indices)
    val indexes = idx map {
      case columns => BitOps.setBits(vec = 0, bits = columns)
    }
    new IndexedLattice(keys, value, indexes, ops)
  }

  def getIndexedLattice(): IndexedLattice = indexedLattice

  /**
    * Returns a human readable string representation of the lattice.
    */
  override def toString: String = {
    val sb = new StringBuilder

    sb.append(s"$name(${indexedLattice.getSize})")
    sb.append("\n")

    // Construct an ASCII table with a column for each attribute.
    val attributes = keys.toList ::: value :: Nil
    val columns = attributes.map(_.getName())
    val table = new AsciiTable().withCols(columns: _*)

    // Add each row to the ASCII table.
    for ((key, value) <- indexedLattice.scan) {
      val row = key.toArray.toList ::: value :: Nil
      table.mkRow(row)
    }

    // Write the ASCII table to the string buffer.
    val sw = new StringWriter()
    table.write(new PrintWriter(sw))
    sb.append(sw.toString)

    sb.toString()
  }

}
