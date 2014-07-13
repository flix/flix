package util

import java.io.File

object TestRelation {

  def main(args: Array[String]): Unit = {
    val r5 = new Relation5[String, File]
    val foo = new File("foo.txt")
    val bar = new File("bar.txt")

    r5.put("a", "b", "c", "d", "e", foo)
    r5.put("x", "b", "y", "d", "z", bar)


    val query = List(None, Some("b"), Some("y"), Some("d"), None)

    var r: Relation[String, File] = r5
    var i = 0
    for (q <- query) {
      q match {
        case None => i += 1 // nop
        case Some(x) => r = r.get(i, x);
      }
    }
    println(r.tuples)

  }

}
