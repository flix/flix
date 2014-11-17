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

  // TODO: Need map.
  // TODO: Need fold.
  def visit[A](f: Ast.Node => A): A = ???

}
