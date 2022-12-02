/*
 * Copyright 2022 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language.dbg.PrettierPrettyPrinting.Simple

object TreeExample {

  import Doc._

  private sealed trait Tree

  private case class Node(s: String, ts: List[Tree]) extends Tree

  private def showTree(t: Tree): Doc = t match {
    case Node(s, ts) => text(s) <> nest(s.length, showBracket(ts))
  }

  private def showBracket(ts: List[Tree]): Doc = ts match {
    case Nil => nil
    case ts => text("[") <> nest(1, showTrees(ts)) <> text("]")
  }

  private def showTrees(ts: List[Tree]): Doc = ts match {
    case Nil => nil
    case t :: Nil => showTree(t)
    case t :: ts => showTree(t) <> text(",") <> line <> showTrees(ts)
  }

  private def showTree1(t: Tree): Doc = t match {
    case Node(s, ts) => text(s) <> showBracket1(ts)
  }

  private def showBracket1(ts: List[Tree]): Doc = ts match {
    case Nil => nil
    case ts => text("[") <> nest(2, line <> showTrees1(ts)) <> line <> text("]")
  }

  private def showTrees1(ts: List[Tree]): Doc = ts match {
    case Nil => nil
    case t :: Nil => showTree1(t)
    case t :: ts => showTree1(t) <> text(",") <> line <> showTrees1(ts)
  }

  private def exampleTree: Tree = {
    Node("aaa", List(
      Node("bbbbb", List(
        Node("ccc", Nil),
        Node("dd", Nil)
      )),
      Node("eee", Nil),
      Node("ffff", List(
        Node("gg", Nil),
        Node("hhh", Nil),
        Node("ii", Nil)
      ))
    ))
  }

  def main(args: Array[String]): Unit = {
    println("Layout 1:")
    println(layout(showTree(exampleTree)))
    println("Layout 2")
    println(layout(showTree1(exampleTree)))
  }
}
