/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api.lsp

import ca.uwaterloo.flix.api.lsp.provider.{FindReferencesProvider, HighlightProvider, RenameProvider}
import ca.uwaterloo.flix.api.{CompilerConstants, Flix}
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.ast.shared.{SecurityContext, SourceName}
import ca.uwaterloo.flix.util.{LibLevel, Options}
import org.scalatest.funsuite.AnyFunSuite

class TestTypeVarOccurrences extends AnyFunSuite {

  private val Flix: Flix = new Flix().setOptions(Options.Default.copy(lib = LibLevel.Nix))

  private val Name = SourceName.PathName(CompilerConstants.VirtualTestFile)

  private def compile(program: String): Root = {
    implicit val sctx: SecurityContext = SecurityContext.Unrestricted
    Flix.addSource(CompilerConstants.VirtualTestFile, program, sctx)
    Flix.check() match {
      case (Some(root), Nil) => root
      case (_, errors) => fail(s"Compilation failed: ${errors.map(_.summary).mkString(", ")}.")
    }
  }

  private def at(line: Int, col: Int): Range = Range(Position(line, col), Position(line, col + 1))

  private def references(pos: Position)(implicit root: Root): Set[Range] =
    FindReferencesProvider.findRefs(Name, pos).map(_.range)

  private def highlights(pos: Position)(implicit root: Root): Set[Range] =
    HighlightProvider.processHighlight(Name, pos).map(_.range)

  private def renames(pos: Position)(implicit root: Root): Set[Range] =
    RenameProvider.processRename("z", Name, pos).toList.flatMap(_.changes.values.flatten).map(_.range).toSet

  private def assertOccurrences(program: String, expected: Set[Range]): Unit =
    assertOccurrencesFrom(program, expected, expected)

  private def assertOccurrencesFrom(program: String, expected: Set[Range], cursors: Set[Range]): Unit = {
    implicit val root: Root = compile(program)
    for (range <- cursors) {
      val pos = range.start
      assertResult(expected, s"find references at $pos")(references(pos))
      assertResult(expected, s"highlight at $pos")(highlights(pos))
      assertResult(expected, s"rename at $pos")(renames(pos))
    }
  }

  test("TypeVar.Def.Explicit") {
    val program =
      """def f[a: Type](x: a): a = x
        |""".stripMargin
    assertOccurrences(program, Set(at(1, 7), at(1, 19), at(1, 23)))
  }

  test("TypeVar.Let") {
    val program =
      """def f[a: Type](x: a): a =
        |    let y: a = x;
        |    y
        |""".stripMargin
    assertOccurrencesFrom(program, Set(at(1, 7), at(1, 19), at(1, 23), at(2, 12)), Set(at(1, 7), at(1, 19), at(1, 23)))
  }

  test("TypeVar.Def.Implicit") {
    val program =
      """def f(x: b): b = x
        |""".stripMargin
    assertOccurrences(program, Set(at(1, 10), at(1, 14)))
  }

  test("TypeVar.Def.Shadow") {
    val program =
      """def f[a: Type](x: a): a = x
        |
        |def g[a: Type](x: a): a = x
        |""".stripMargin
    assertOccurrences(program, Set(at(1, 7), at(1, 19), at(1, 23)))
    assertOccurrences(program, Set(at(3, 7), at(3, 19), at(3, 23)))
  }

  test("TypeVar.Enum") {
    val program =
      """enum Tree[a] {
        |    case Leaf,
        |    case Node(Tree[a], a, Tree[a])
        |}
        |""".stripMargin
    assertOccurrences(program, Set(at(1, 11), at(3, 20), at(3, 24), at(3, 32)))
  }

  test("TypeVar.TypeAlias") {
    val program =
      """type alias Pair[a] = (a, a)
        |""".stripMargin
    assertOccurrences(program, Set(at(1, 17), at(1, 23), at(1, 26)))
  }

  test("TypeVar.Trait") {
    val program =
      """trait Unwrap[a] {
        |    pub def unwrap(x: a): Int32
        |}
        |""".stripMargin
    assertOccurrences(program, Set(at(1, 14), at(2, 23)))
  }

  test("TypeVar.Instance.Header") {
    val program =
      """enum Box[t] {
        |    case Box(t)
        |}
        |
        |trait Unwrap[a] {
        |    pub def unwrap(x: a): Int32
        |}
        |
        |instance Unwrap[Box[b]] {
        |    pub def unwrap(_x: Box[b]): Int32 = 0
        |}
        |""".stripMargin
    assertOccurrences(program, Set(at(9, 21), at(10, 28)))
  }

  test("TypeVar.Ascribe") {
    val program =
      """def f[a: Type](x: a): a = (x: a)
        |""".stripMargin
    assertOccurrences(program, Set(at(1, 7), at(1, 19), at(1, 23), at(1, 31)))
  }

  test("TypeVar.Instance.AssocTypeDef") {
    val program =
      """enum Box[t] {
        |    case Box(t)
        |}
        |
        |trait Coll[a] {
        |    type Elm[a]: Type
        |    pub def head(c: a): Coll.Elm[a]
        |}
        |
        |instance Coll[Box[b]] {
        |    type Elm[Box[b]] = b
        |    pub def head(c: Box[b]): b = match c {
        |        case Box.Box(x) => x
        |    }
        |}
        |""".stripMargin
    assertOccurrences(program, Set(at(10, 19), at(11, 18), at(11, 24), at(12, 25), at(12, 30)))
  }

  test("TypeVar.Highlight.Kinds") {
    val program =
      """def f[a: Type](x: a): a = x
        |""".stripMargin
    implicit val root: Root = compile(program)
    val expected = Set(
      DocumentHighlight(at(1, 7), DocumentHighlightKind.Write),
      DocumentHighlight(at(1, 19), DocumentHighlightKind.Read),
      DocumentHighlight(at(1, 23), DocumentHighlightKind.Read)
    )
    assertResult(expected)(HighlightProvider.processHighlight(Name, Position(1, 19)))
  }

  test("TypeVar.Rename.NewText") {
    val program =
      """def f[a: Type](x: a): a = x
        |""".stripMargin
    implicit val root: Root = compile(program)
    val edits = RenameProvider.processRename("elm", Name, Position(1, 19)).toList.flatMap(_.changes.values.flatten)
    assertResult(Set("elm"))(edits.map(_.newText).toSet)
    assertResult(3)(edits.length)
  }

}
