import java.io.File

import impl.ast.{Compiler, Parser}
import impl.runtime.{Simplify, Propagation, Options, Solver}
import impl.verifier.{Verifier, Typer}
import impl.lmsbackend

object Main {
  val useLmsBackend = false

  def main(args: Array[String]): Unit = {
    if (args.length != 1)
      throw new RuntimeException(s"Expected one argument. Got: ${args.mkString}")

    // input file
    val file = new File(args(0))

    // abstract syntax tree
    val ast = Parser.parse(file)

    // logic program
    val program = (new Compiler).compile(ast)

    // verify
    Typer.typecheck(program)
    //Verifier.verify(program)

    if(!useLmsBackend) {
      // options
      val options = Options(propagation = Propagation.Full, simplify = Simplify.Enable)

      // compute fixpoint
      val solver = new Solver(program, options)
      solver.solve()

      // print solution
      solver.print()
    } else {
      val solver = new lmsbackend.Solver(lmsbackend.Input.convert(program))
      solver()

      solver.print()
    }
  }
}
