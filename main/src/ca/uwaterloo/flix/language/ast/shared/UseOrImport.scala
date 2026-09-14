/*
 * Copyright 2024 Holger Dal Mogensen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast.shared

import ca.uwaterloo.flix.language.ast.jvm.JavaClass
import ca.uwaterloo.flix.language.ast.{Name, SourceLocation, Sourceable, Symbol}

/**
  * A use of a Flix symbol or import of a Java class.
  */
sealed trait UseOrImport extends Sourceable

object UseOrImport {

  /**
    * A use of a Flix declaration symbol.
    */
  case class Use(sym: Symbol, alias: Name.Ident, loc: SourceLocation) extends UseOrImport {
    val src: Source = loc.source
  }

  /**
    * An import of a Java class.
    */
  case class Import(clazz: JavaClass, alias: Name.Ident, loc: SourceLocation) extends UseOrImport {
    val src: Source = loc.source
  }

}
