package impl.ast2

class Compiler {

  // environments
  // Typ Env
  // ValEnv
  // FunEnv
  // etc. etc.
  def typenv(root: Ast.Root): Map[String, Ast.Declaration] = visit {
    case Ast.TypeDeclaration(name, typ) => ???
  }


  // linking

  def link(ast: Ast.Type, typenv: Map[Ast.Name, Ast.Type]): Ast.Type = ast match {
    case Ast.Type.Bool => Ast.Type.Bool
    // ...
    case Ast.Type.NameRef(name) => typenv.getOrElse(name, ???)
  }

  // TODO: Need map.
  // TODO: Need fold.
  def visit[A](f: Ast.Node => A): A = ???

}
