/*
 * Copyright 2022 Matthew Lutze
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.ast.shared.{AvailableClasses, Source}

case object ReadAst {
  case class Root(sources: Map[Source, Unit], availableClasses: AvailableClasses)
}
