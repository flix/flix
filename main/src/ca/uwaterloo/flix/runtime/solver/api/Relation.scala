package ca.uwaterloo.flix.runtime.solver.api

import java.io.{PrintWriter, StringWriter}

import ca.uwaterloo.flix.runtime.solver.datastore.IndexedRelation
import ca.uwaterloo.flix.util.{AsciiTable, BitOps}

/**
  * Represents a relation value.
  */
class Relation(name: String, attributes: Array[Attribute]) extends Table {

  /**
    * Returns the name of the relation.
    */
  def getName(): String = name

  /**
    * Returns the attributes of the relation.
    */
  def getAttributes(): Array[Attribute] = attributes

  private val indexedRelation = {
    // NB: Just an index on the first attribute.
    val idx = Set(Seq(0))
    val indexes = idx map {
      case columns => BitOps.setBits(vec = 0, bits = columns)
    }
    new IndexedRelation(attributes, indexes, indexes.head)
  }

  def getIndexedRelation(): IndexedRelation = indexedRelation

  /**
    * Returns a human readable string representation of the relation.
    */
  override def toString: String = {
    val sb = new StringBuilder

    // Construct an ASCII table with a column for each attribute.
    val columns = attributes.map(_.getName())
    val table = new AsciiTable().withTitle(name).withCols(columns: _*)

    // Add each row to the ASCII table.
    for (row <- indexedRelation.scan) {
      table.mkRow(row.toList)
    }

    // Write the ASCII table to the string buffer.
    val sw = new StringWriter()
    table.write(new PrintWriter(sw))
    sb.append(sw.toString)

    sb.toString()
  }

}
