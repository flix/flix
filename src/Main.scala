import java.io.File

import impl.ast.{Compiler, Parser}
import impl.runtime.{Simplify, Propagation, Options, Solver}
import impl.verifier.{Verifier, Typer}

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

    // options
    val options = Options(propagation = Propagation.Diff, simplify = Simplify.Enable)

    // compute fixpoint
    val solver = new Solver(program, options)
    solver.solve()

    // print solution
    solver.print()
  }
}
