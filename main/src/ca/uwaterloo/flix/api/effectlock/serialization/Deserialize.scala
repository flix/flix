package ca.uwaterloo.flix.api.effectlock.serialization

import ca.uwaterloo.flix.language.ast.{Scheme, Symbol, TypedAst}

object Deserialize {

  def deserializeDef(defn0: SDef): (Symbol.DefnSym, Scheme) = defn0 match {
    case SDef(namespace, text, scheme, source) => // source unused?
      val sym = Symbol.mkDefnSym(namespace.mkString("", ".", s".$text"))
      val sc = deserializeScheme(scheme)
      sym -> sc
  }

  private def deserializeScheme(sc0: SScheme): Scheme = ???

}
