package ca.uwaterloo.flix.util.collection

import scala.collection.mutable

/** A mutable growable map with string keys that is step-wise searchable by prefixes. */
object MutPrefixTree {

  class Node[T]() {

    /** Contains the value of the tree if present. */
    private var value: Option[T] = None

    /** The list of children. */
    private val children: mutable.HashMap[Char, Node[T]] = new mutable.HashMap()

    /** Returns the value of `this`. */
    def getValue: Option[T] =
      value

    /** Assigns `x` as the value of `this`. */
    def setValue(x: T): Unit =
      this.value = Some(x)

    /** Inserts `c` with value `v`, overriding any existing value. */
    def put(c: Char, v: T): Unit =
      getNodeOrCreateIfAbsent(c).setValue(v)

    /**
      * Inserts `s` with value `v`, overriding any existing value.
      *
      * N.B.: `s` must be non-null and non-empty.
      */
    def put(s: String, v: T): Unit = {
      assert(s != null)
      assert(s.nonEmpty)
      // Expand the tree over all chars except the last.
      var curr = this
      var i = 0
      while (i < s.length - 1) {
        curr = curr.getNodeOrCreateIfAbsent(s.charAt(i))
        i = i + 1
      }
      // Insert the last char.
      curr.put(s.charAt(i), v)
    }

    /** Returns child `c`, creating a new node if necessary. */
    private def getNodeOrCreateIfAbsent(c: Char): Node[T] = {
      children.get(c) match {
        case Some(node) => node
        case None =>
          val node = new Node[T]()
          children.put(c, node)
          node
      }
    }

    /** Returns the `c` child if present. */
    def getNode(c: Char): Option[Node[T]] =
      children.get(c)

    /**
      * Returns the child with prefix `s` if present.
      *
      * N.B.: `s` must be non-null and non-empty.
      */
    def getNode(s: String): Option[Node[T]] = {
      assert(s != null)
      assert(s.nonEmpty)
      var curr = this
      var i = 0
      while (i < s.length) {
        curr.getNode(s.charAt(i)) match {
          case Some(node) =>
            curr = node
            i = i + 1
          case None =>
            return None
        }
      }
      Some(curr)
    }

    /** Returns the value of `s` if present. */
    def get(s: String): Option[T] =
      getNode(s).flatMap(_.getValue)

  }

}

