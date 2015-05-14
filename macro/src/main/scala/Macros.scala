import scala.language.experimental.macros
import scala.reflect.macros.whitebox

object Macros {

  def demo(n: Int): Int => Int = macro demoImpl

  def demoImpl(c: whitebox.Context)(n: c.Expr[Int]): c.Expr[Int => Int] = {
    import c.universe._

    println("Hello from macro implementation.")

    c.Expr[Int => Int] {
      q"""
          (m: Int) => m + $n
       """
    }
  }
}
