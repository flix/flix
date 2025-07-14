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
import ca.uwaterloo.flix.language.ast.{Symbol, Type}
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.api.lsp.{Consumer, InlayHint, InlayHintKind, Position, Range}
import ca.uwaterloo.flix.api.lsp.Visitor
import scala.collection.immutable.Map

/**
  * Provides inlay hints for effects in the Flix language.
  */
object InlayHintProvider {

  /**
    * Whether to enable effect hints.
    */
  private val EnableEffectHints: Boolean = false

  /**
    * Returns a list of inlay hints for the given URI and range.
    *
    * @param uri   The URI of the file.
    * @param range The range within the file to get inlay hints for.
    * @param root  The root of the typed AST.
    * @return A list of inlay hints.
    */
  def getInlayHints(uri: String, range: Range)(implicit root: Root): List[InlayHint] = {
    if (EnableEffectHints) {
      val opSymUses: List[(SymUse.OpSymUse, SourceLocation)] = getOpSymUses(uri)
      val opEffSyms: List[(Symbol.EffSym, SourceLocation)] = opSymUses.map {
        case (opSymUse, loc) =>
          (opSymUse.sym.eff, loc)
      }
      val defSymUses: List[(SymUse.DefSymUse, SourceLocation)] = getDefSymUses(uri)
      val defEffSyms: List[(Symbol.EffSym, SourceLocation)] = defSymUses.flatMap {
        case (defSymUse, loc) =>
          (root.defs(defSymUse.sym).spec.eff.effects.zip(loc ::Nil))
      }
      val effSyms = opEffSyms ++ defEffSyms

      /**
        * Map from position to effects.
        * The position is the end of the effect expression, and the effects are all the effects that appear on that line.
        * For example, if there are two effects on the same line, they will be combined into a single hint.
        */
      val positionToEffectsMap: Map[Position, Set[Symbol.EffSym]] = effSyms.foldLeft(Map.empty[Position, Set[Symbol.EffSym]]) {
        case (acc, (eff, loc)) =>
          val position = Position(loc.endLine, loc.source.getLine(loc.endLine).length + 2)
          acc.updated(position, acc.getOrElse(position, Set.empty[Symbol.EffSym]) + eff)
      }
      mkHintsFromEffects(positionToEffectsMap)
    } else {
      List.empty[InlayHint]
    }
  }

  /**
    * Returns a list of operation symbol uses.
    */
  private def getOpSymUses(uri: String)(implicit root: Root): List[(SymUse.OpSymUse, SourceLocation)] = {
    var opSymUses: List[(SymUse.OpSymUse, SourceLocation)] = List.empty
    object opSymUseConsumer extends Consumer {
      override def consumeExpr(expr: Expr): Unit = {
        expr match {
          case Expr.ApplyOp(opSymUse, _, _, eff, loc) =>
            opSymUses = ((opSymUse, loc)) :: opSymUses
          case _ => ()
        }
      }
    }
    Visitor.visitRoot(root, opSymUseConsumer, FileAcceptor(uri))
    opSymUses
  }

  /**
    * Returns a list of definition symbol uses.
    */
  private def getDefSymUses(uri: String)(implicit root: Root): List[(SymUse.DefSymUse, SourceLocation)] = {
    var defSymUses: List[(SymUse.DefSymUse, SourceLocation)] = List.empty
    object defSymUseConsumer extends Consumer {
      override def consumeExpr(expr: Expr): Unit = {
        expr match {
          case Expr.ApplyDef(defSymUse, _, _, _, _, loc) =>
            defSymUses = ((defSymUse, loc)) :: defSymUses
          case _ => ()
        }
      }
    }
    Visitor.visitRoot(root, defSymUseConsumer, FileAcceptor(uri))
    defSymUses
  }


  /**
    * Creates a list of inlay hints from the effects mapped to their positions.
    */
  private def mkHintsFromEffects(positionToEffectsMap: Map[Position, Set[Symbol.EffSym]]): List[InlayHint] = {
    positionToEffectsMap.map {
      case (pos, effs) =>
        mkHint(effs, pos)
    }.toList
  }

  /**
    * Creates an inlay hint combining all effects for a given line.
    * For example, if the effects are ef1 and ef2, the hint will be { ef1 + ef2 }.
    */
  private def mkHint(effs: Set[Symbol.EffSym], pos: Position): InlayHint = {
    val effectString: String = effs.mkString(" + ")
    InlayHint(
      position = pos,
      label = s"{ $effectString }",
      kind = Some(InlayHintKind.Type),
      textEdits = List.empty,
      tooltip = s"{ $effectString }",
      paddingLeft = true,
      paddingRight = true
    )
  }
}
