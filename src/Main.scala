import java.io.File

import impl.ast.{Compiler, Parser}
import impl.runtime.Solver
import impl.verifier.{Typer, Verifier}

object Main {
  def main(args: Array[String]): Unit = {
    if (args.length != 1)
      throw new RuntimeException(s"Expected one argument. Got: ${args.mkString}")

    // input file
    val file = new File(args(0))

    // abstract syntax tree
    val ast = Parser.parse(file)

    // logic program
    val program = Compiler.compile(ast)

    // verify
    Typer.typecheck(program)
    Verifier.verify(program)

    // compute fixpoint
    val solver = new Solver(program)
    solver.solve()

    // print solution
    solver.print()
  }
}
