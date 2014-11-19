package impl.ast2

import impl.logic.Type

class Compiler(ast: Ast.Root) {

  def run(): Unit = {
    //println(typenv(ast))
  }

  // environments
  // Typ Env
  // ValEnv
  // FunEnv
  // etc. etc.

  /**
   * Returns a map from (fully qualified) names to types.
   */
  def typenv(root: Ast.Root): Map[Ast.Name, Ast.Type] = {


    def visit(name: Ast.Name, node: Ast.Node): Map[Ast.Name, Ast.Type] = ???

    ???
  }

  // TODO: Deal with ambiquity


  // linking

  def link(ast: Ast.Type, typenv: Map[Ast.Name, Ast.Type]): Ast.Type = ast match {
    case Ast.Type.Bool => Ast.Type.Bool
    // ...
    case Ast.Type.NameRef(name) => typenv.getOrElse(name, ???)
  }

  // TODO: Need map.
  // TODO: Need fold.
  def visit[A](f: Ast.Node => A): A = ???


  /**
   * Compiles an AST type node to an internal type.
   */
  private def compile(typ: Ast.Type): Type = typ match {
    case Ast.Type.Unit => Type.Unit
    case Ast.Type.Bool => Type.Bool
    case Ast.Type.Int => Type.Int
    case Ast.Type.Str => Type.Str
    // case Ast.Type.Tag(name) => Type.Tag(), TODO
    case Ast.Type.Set(typ1) => Type.Set(compile(typ1))
    case Ast.Type.Map(keys, values) => throw new UnsupportedOperationException()
    case Ast.Type.Function(elms) => {
      def visit(xs: Seq[Ast.Type]): Type = xs match {
        case y :: Nil => compile(y)
        case y :: ys => Type.Function(compile(y), visit(ys))
      }
      visit(elms)
    }
    case Ast.Type.NameRef(name) => throw new RuntimeException()
  }

}
