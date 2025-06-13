/*
 * Copyright 2022 Nicola Dardanis
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

package ca.uwaterloo.flix.api.lsp.provider
import ca.uwaterloo.flix.api.lsp.acceptors.{FileAcceptor}
import ca.uwaterloo.flix.language.ast.shared.SymUse
import ca.uwaterloo.flix.language.ast.TypedAst.{Expr, Root}
import ca.uwaterloo.flix.api.lsp.{Consumer, InlayHint, InlayHintKind, Position, Range}
import ca.uwaterloo.flix.api.lsp.Visitor

object InlayHintProvider {
  private val EnableEffectHints: Boolean = false

  def getInlayHints(uri: String, range: Range)(implicit root: Root): List[InlayHint] = {
    var inlayHints: List[InlayHint] = Nil
    if(EnableEffectHints) {
      var effects: Set[(String, Int, Int)] = (getOpSymUses(uri) ++ getDefSymUses(uri)).filter {
        case (eff, _, _) =>
          eff != "Pure"
      }
      var maxColWidth: Int = effects.map { case (_, _, col) => col }.maxOption.getOrElse(0)
      var lineToEffectsMap: Map[Int, Set[String]] = Map.empty
      effects.foreach { case (eff, line, col) =>
        lineToEffectsMap = lineToEffectsMap.updated(line, lineToEffectsMap.getOrElse(line, Set.empty) + eff)
      }
      lineToEffectsMap.foreach { case (line, effects) =>
        inlayHints ::= mkHintFromEffects(line, effects, maxColWidth + 1)
      }
    }
    inlayHints
  }

  def getOpSymUses(uri: String)(implicit root: Root): Set[(String, Int, Int)] = {
    var opSymUses: Set[(String, Int, Int)] = Set.empty
    object opSymUseConsumer extends Consumer {
      override def consumeExpr(expr: Expr): Unit = {
        expr match {
          case Expr.Do(opSymUse, _, _, _, loc) =>
            opSymUses += ((opSymUse.sym.eff.name, loc.endLine, loc.endCol))
          case _ => ()
        }
      }
    }
    Visitor.visitRoot(root, opSymUseConsumer, FileAcceptor(uri))
    opSymUses
  }

  def getDefSymUses(uri: String)(implicit root: Root): Set[(String, Int, Int)] = {
    var defSymUses: Set[(String, Int, Int)] = Set.empty
    object defSymUseConsumer extends Consumer {
      override def consumeExpr(expr: Expr): Unit = {
        expr match {
          case Expr.ApplyDef(_, _, _, _, eff, loc) =>
            defSymUses += ((eff.toString, loc.endLine, loc.endCol))
          case _ => ()
        }
      }
    }
    Visitor.visitRoot(root, defSymUseConsumer, FileAcceptor(uri))
    defSymUses
  }

  def mkHintFromEffects(line: Int, effects: Set[String], col: Int): InlayHint = {
    var effectString: String = effects.mkString(" + ")
    InlayHint(
      position = Position(line, col),
      label = s"{ $effectString }",
      kind = Some(InlayHintKind.Type),
      textEdits = List.empty,
      tooltip = s"{ $effectString }",
      paddingLeft = true,
      paddingRight = true
    )
  }
}
