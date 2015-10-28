package ca.uwaterloo.flix.runtime.datastore

import ca.uwaterloo.flix.runtime.Value

trait IndexedCollection {

  def lookup(pat: Array[Value]): Iterator[Array[Value]]

}
