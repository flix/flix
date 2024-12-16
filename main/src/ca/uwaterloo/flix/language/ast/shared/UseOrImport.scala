/*
 * Copyright 2024 Holger Dal Mogensen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package ca.uwaterloo.flix.language.ast.shared

import ca.uwaterloo.flix.language.ast.{Name, SourceLocation, Symbol}

/**
  * A use of a Flix symbol or import of a Java class.
  */
sealed trait UseOrImport

object UseOrImport {

  /**
    * A use of a Flix declaration symbol.
    */
  case class Use(sym: Symbol, alias: Name.Ident, loc: SourceLocation) extends UseOrImport

  /**
    * An import of a Java class.
    */
  case class Import(clazz: Class[?], alias: Name.Ident, loc: SourceLocation) extends UseOrImport
  
}
