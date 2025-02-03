package ca.uwaterloo.flix.api

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.api.effectlock.EffectLock
import ca.uwaterloo.flix.api.effectlock.serialization.Serialization
import ca.uwaterloo.flix.language.ast.{Scheme, Symbol, Type, TypedAst}
import ca.uwaterloo.flix.language.ast.shared.{Input, SecurityContext}
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.Path

class EffectLockSuite extends AnyFunSuite with TestUtils {

  test("IsSafe.01") {
    val input =
      """
        |pub def f(): Unit = ???
        |
        |pub def g(): Unit = ???
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(checkIsSafe("f", "g", result))
  }

  test("IsSafe.02") {
    val input =
      """
        |pub def f(): Bool -> Unit = ???
        |
        |pub def g(): Bool -> Unit = ???
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(checkIsSafe("f", "g", result))
  }

  test("IsSafe.03") {
    val input =
      """
        |pub def f(): Bool -> Unit \ IO = ???
        |
        |pub def g(): Bool -> Unit = ???
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(checkIsSafe("f", "g", result))
  }

  test("IsSafe.04") {
    val input =
      """
        |pub def f(): Bool -> Unit = ???
        |
        |pub def g(): Bool -> Unit \ IO = ???
        |
        |""".stripMargin
    val (result, _) = check(input, Options.TestWithLibNix)
    assert(!checkIsSafe("f", "g", result))
  }

  test("Reachable.01") {
    val input =
      """
        |def main(): Unit =
        |    g(f)
        |
        |def f(): Unit = ()
        |
        |def g(h: Unit -> Unit): Unit = h()
        |
        |def a(): Unit = ()
        |
      """.stripMargin
    val result = reachableDefs(checkWithReachability(input, Options.TestWithLibNix))
    val expected = Set("main", "f", "g")
    assert(result == expected)
  }

  test("Reachable.02") {
    val input =
      """
        |use A.{q => f}
        |
        |mod A {
        |    pub def q(): Unit = ()
        |}
        |
        |def main(): Unit =
        |    g(f)
        |
        |def g(h: Unit -> Unit): Unit = h()
        |
      """.stripMargin
    val result = reachableDefs(checkWithReachability(input, Options.TestWithLibNix))
    val expected = Set("main", "q", "g")
    assert(result == expected)
  }

  test("Reachable.03") {
    val input =
      """
        |use A.{q => f}
        |
        |mod A {
        |    pub def q(): Unit = ()
        |}
        |
        |def main(): Unit =
        |    g(f)
        |
        |def g(h: Unit -> Unit): Unit = h()
        |
        |def a(): Unit = ()
        |
      """.stripMargin
    val result = reachableDefs(checkWithReachability(input, Options.TestWithLibNix))
    val expected = Set("main", "q", "g")
    assert(result == expected)
  }

  test("Serialization.01") {
    val input =
      """
        |pub def f(): Unit = ???
        |""".stripMargin
    val (tpe, ser) = checkSerializationType(input, "f")
    assert(Serialization.deserializeTpe(ser).get == tpe)
  }

  test("Serialization.02") {
    val input =
      """
        |pub def g(): a = ???
        |""".stripMargin
    val (tpe, ser) = checkSerializationType(input, "g")
    assert(Serialization.deserializeTpe(ser).get == tpe)
  }

  ignore("Serialization.03") {
    val input =
      """
        |def main(): Bool = f()
        |
        |pub def f(): Bool = g()
        |
        |pub def g(): Bool = h(false) == 1
        |
        |pub def h(_: Bool): Int32 = 1
        |
        |""".stripMargin
    implicit val sctx: SecurityContext = SecurityContext.AllPermissions
    implicit val flix: Flix = new Flix().setOptions(Options.TestWithLibMin).addSourceCode("<test>", input)
    val (optRoot, _) = flix.check()
    val root = optRoot.get
    val funcs = List("f", "g", "h")
    val defs = root.defs.map {
      case (sym, defn) if funcs.contains(sym.text) =>
        val input = Input.FileInPackage(Path.of("."), "", "testPkg", sctx)
        val source = sym.loc.sp1.source.copy(input = input)
        val sp1 = sym.loc.sp1.copy(source)
        val sp2 = sym.loc.sp2.copy(source)
        val loc = sym.loc.copy(sp1 = sp1, sp2 = sp2)
        val sym1 = new Symbol.DefnSym(sym.id, sym.namespace, sym.text, loc)
        (sym1, defn.copy(sym = sym1))
      case sd => sd
    }
    val root1 = root.copy(defs = defs)
    val reachableDefs = flix.reachableLibraryFunctions(root1)
    val ser = Serialization.serialize(reachableDefs)
    val Some(actual) = Serialization.deserialize(ser)
    val expected = root1.defs.foldLeft(Map.empty[Serialization.Library, List[Serialization.NamedTypeScheme]]) {
      case (acc, (sym, defn)) => sym.loc.sp1.source.input match {
        case Input.FileInPackage(_, _, text, _) =>
          val defs = acc.getOrElse(text, List.empty)
          val nts = (defn.sym, defn.spec.declaredScheme)
          acc + (text -> (nts :: defs))
        case _ => acc
      }
    }
    assert(expected == actual)
  }

  private def checkSerializationType(input: String, funSym: String): (Type, String) = {
    implicit val sctx: SecurityContext = SecurityContext.AllPermissions
    implicit val flix: Flix = new Flix().setOptions(Options.TestWithLibNix).addSourceCode("<test>", input)
    val (optRoot, _) = flix.check()
    val root = optRoot.get
    val f = root.defs.filter(kv => kv._1.text == funSym).toList.head
    val tpe = f._2.spec.retTpe
    val ser = Serialization.serialize(tpe)
    (tpe, ser)
  }

  private def checkWithReachability(input: String, opts: Options): Option[TypedAst.Root] = {
    implicit val sctx: SecurityContext = SecurityContext.AllPermissions
    implicit val flix: Flix = new Flix().setOptions(opts).addSourceCode("<test>", input)
    val result = flix.effectLockReachable()
    result
  }

  private def reachableDefs(root: Option[TypedAst.Root]): Set[String] = root match {
    case None => Set.empty
    case Some(r) => r.defs.keys.map(_.text).toSet
  }

  private def checkIsSafe(defn1: String, defn2: String, optRoot: Option[TypedAst.Root]): Boolean = {
    optRoot match {
      case Some(root) =>
        val optDefn1 = root.defs.find {
          case (sym, _) => sym.text == defn1
        }
        if (optDefn1.isEmpty) {
          fail(s"unable to find $defn1")
        }
        val optDefn2 = root.defs.find {
          case (sym, _) => sym.text == defn2
        }
        if (optDefn2.isEmpty) {
          fail(s"unable to find $defn2")
        }

        val sc1 = optDefn1.get._2.spec.declaredScheme
        val sc2 = optDefn2.get._2.spec.declaredScheme
        EffectLock.isSafe(sc1, sc2)

      case None => fail("program does not compile")
    }
  }
}
