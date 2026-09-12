/*
 * Copyright 2022 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.language.ast.Symbol

/**
  * Represents a unit test.
  *
  * @param sym  the Flix def symbol.
  * @param skip true if the test case is marked @Skip.
  * @param run  the function code.
  */
case class TestFn(sym: Symbol.DefnSym, skip: Boolean, run: () => AnyRef)
