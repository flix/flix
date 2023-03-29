package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.language.ast.{ErasedAst, MonoType, SourceLocation}

case class AnonClassInfo(name: String, clazz: java.lang.Class[_], tpe: MonoType, methods: List[ErasedAst.JvmMethod], loc: SourceLocation)
