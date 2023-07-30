/*
 * Copyright 2023 Magnus Madsen
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
package ca.uwaterloo.flix.language.ast

object UnstructuredTree {

  sealed trait TreeKind

  object TreeKind {

    case object Def extends TreeKind

    // TODO: PARSER2

    case object ErrorTree extends TreeKind

  }

  case class Tree(kind: TreeKind, children: Array[Child]) {
    override def toString: String = s"Tree($kind, [${children.mkString(", ")}])"
  }

  sealed trait Child

  object Child {
    case class TokenChild(token: Token) extends Child

    case class TreeChild(tree: Tree) extends Child
  }

}
