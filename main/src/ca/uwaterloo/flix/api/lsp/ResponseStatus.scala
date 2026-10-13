/*
 * Copyright 2023 Holger Dal Mogensen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api.lsp

object ResponseStatus {

  /**
    * Request was successfully processed.
    */
  val Success = "success"

  /**
    * Request was invalid and could not be processed.
    */
  val InvalidRequest = "invalid_request"

  /**
    * The compiler has crashed.
    */
  val CompilerError = "compiler_error"

}
