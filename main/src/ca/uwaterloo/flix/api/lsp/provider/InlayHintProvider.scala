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
import ca.uwaterloo.flix.language.ast.Type
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.api.lsp.{Consumer, InlayHint, InlayHintKind, Position, Range}
import ca.uwaterloo.flix.api.lsp.Visitor
import scala.collection.immutable.Map

object InlayHintProvider {
  private val EnableEffectHints: Boolean = false

  def getInlayHints(uri: String, range: Range)(implicit root: Root): List[InlayHint] = {
    var inlayHints: List[InlayHint] = Nil
    if(EnableEffectHints) {
      val effects: Set[(Type, SourceLocation)] = (getOpSymUses(uri) ++ getDefSymUses(uri)).filter {
        case (eff, loc) => eff match {
          case Type.Pure => false
          case _ => true
        }
      }
      var locationToEffectsMap: Map[SourceLocation, Set[Type]] = Map.empty
      effects.foreach { case (eff, loc) =>
        locationToEffectsMap = locationToEffectsMap.updated(loc, locationToEffectsMap.getOrElse(loc, Set.empty) + eff)
      }
      inlayHints = mkHintsFromEffects(locationToEffectsMap)
    }
    inlayHints
  }

  private def getOpSymUses(uri: String)(implicit root: Root): Set[(Type, SourceLocation)] = {
    var opSymUses: Set[(Type, SourceLocation)] = Set.empty
    object opSymUseConsumer extends Consumer {
      override def consumeExpr(expr: Expr): Unit = {
        expr match {
          case Expr.Do(_, _, _, eff, loc) =>
            opSymUses += ((eff, loc))
          case _ => ()
        }
      }
    }
    Visitor.visitRoot(root, opSymUseConsumer, FileAcceptor(uri))
    opSymUses
  }

  private def getDefSymUses(uri: String)(implicit root: Root): Set[(Type, SourceLocation)] = {
    var defSymUses: Set[(Type, SourceLocation)] = Set.empty
    object defSymUseConsumer extends Consumer {
      override def consumeExpr(expr: Expr): Unit = {
        expr match {
          case Expr.ApplyDef(_, _, _, _, eff, loc) =>
            defSymUses += ((eff, loc))
          case _ => ()
        }
      }
    }
    Visitor.visitRoot(root, defSymUseConsumer, FileAcceptor(uri))
    defSymUses
  }

  private def mkHintsFromEffects(locationToEffectsMap: Map[SourceLocation, Set[Type]]): List[InlayHint] = {
    locationToEffectsMap.map {
      case (loc, effs) =>
        mkHint(effs, loc)
    }.toList
  }

  // Creates an inlay hint combining all effects for a given line.
  // For example, if the effects are ef1 and ef2, the hint will be { ef1 + ef2 }.
  private def mkHint(effs: Set[Type], loc: SourceLocation): InlayHint = {
    val effectString: String = effs.mkString(" + ")
    InlayHint(
      position = Position(loc.endLine, loc.endCol),
      label = s"{ $effectString }",
      kind = Some(InlayHintKind.Type),
      textEdits = List.empty,
      tooltip = s"{ $effectString }",
      paddingLeft = true,
      paddingRight = true
    )
  }
}
