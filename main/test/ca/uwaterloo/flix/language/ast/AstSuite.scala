package ca.uwaterloo.flix.language.ast

import org.scalatest.Suites

class AstSuite extends Suites(
  new SourceLocationSuite,
  new TestChangeSet,
  new TestSymbolSet,
  new TestSecurityContext
)

