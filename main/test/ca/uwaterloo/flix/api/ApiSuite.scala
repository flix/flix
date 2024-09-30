package ca.uwaterloo.flix.api

import ca.uwaterloo.flix.api.lsp.LspSuite
import org.scalatest.Suites

class ApiSuite extends Suites(
  new LspSuite
)
