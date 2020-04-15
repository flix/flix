package ca.uwaterloo.flix.language.debug

import ca.uwaterloo.flix.language.ast.Type
import ca.uwaterloo.flix.language.ast.Type.ShowInstance
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.tc.Show._

object FormatType {

  /**
    * Returns a human readable representation of the given type `tpe`.
    */
  def format(tpe: Type): String = tpe match {
    case Type.SchemaEmpty => "#{}"
    case _: Type.SchemaExtend =>
      val middlePart = getSchemaTypes(tpe).map(format).mkString(", ")
      "#{ " + middlePart + " }"

    case _ => tpe.show
  }

  /**
    * Returns the types of the predicate symbols in a schema type.
    */
  private def getSchemaTypes(tpe: Type): List[Type] = tpe match {
    case Type.Var(_, _, _) => Nil
    case Type.SchemaEmpty => Nil
    case Type.SchemaExtend(sym, tpe, rest) => tpe :: getSchemaTypes(rest)
    case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'.")
  }

}
