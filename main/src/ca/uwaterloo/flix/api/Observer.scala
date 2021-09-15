package ca.uwaterloo.flix.api

object Observer {

  var count = 0

  def observe(phase: String, o: AnyRef): Unit = {
    val a = Array("|", "/", "-", "\\")

    count = (count + 1) % a.length

    val icon = a(count)
    val s = s"$icon [$phase] $o"

    val r = s + " " * (80 - s.length)

    System.out.print(s"$r\r")
    System.out.flush()
    Thread.sleep(1)
  }

}
