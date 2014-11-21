package impl.ast2

import impl.logic.{Pattern, Type}

class Compiler(ast: Ast.Root) {

  def run(): Unit = {
    //println(typenv(ast))
    // phases: 1 desugar
    // phase2: environments
    // phase3: linking
    // phase4: translate
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


    def visit(name: Ast.Name, node: Ast): Map[Ast.Name, Ast.Type] = ???

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
  def visit[A](f: Ast => A): A = ???


  object TranslationPhase {

    private def compile(pattern: Ast.Pattern): Pattern = pattern match {
      case Ast.Pattern.Wildcard => Pattern.Wildcard
    }

    /**
     * Compiles an ast type to a core type.
     */
    private def compile(typ: Ast.Type): Type = typ match {
      case Ast.Type.Unit => Type.Unit
      case Ast.Type.Bool => Type.Bool
      case Ast.Type.Int => Type.Int
      case Ast.Type.Str => Type.Str
      // case Ast.Type.Tag(name) => Type.Tag(), TODO
      case Ast.Type.Set(typ1) => Type.Set(compile(typ1))
      case Ast.Type.Map(keys, values) => throw CompilerException("Map types are currently not supported.")
      case Ast.Type.NameRef(name) => throw CompilerException(s"Unresolved named type: $name.")
    }
  }


  case class CompilerException(msg: String) extends RuntimeException(msg)

}
