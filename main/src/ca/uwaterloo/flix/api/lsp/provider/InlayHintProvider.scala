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

import ca.uwaterloo.flix.api.lsp.acceptors.FileAcceptor
import ca.uwaterloo.flix.api.lsp.{Consumer, InlayHint, InlayHintKind, Position, Range, TextEdit, Visitor}
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.TypedAst.{Binder, Def, Expr, FormalParam, Root}
import ca.uwaterloo.flix.language.ast.shared.{Annotation, ExpPosition, SymUse}
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol}
import ca.uwaterloo.flix.language.errors.TypeError

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
  def getInlayHints(uri: String, range: Range, errors: List[CompilationMessage])(implicit root: Root): List[InlayHint] = {
    if (EnableEffectHints) {
      val opSymUses: List[(SymUse.OpSymUse, SourceLocation)] = getOpSymUses(uri)
      val opEffSyms: List[(Symbol.EffSym, SourceLocation)] = opSymUses.map {
        case (opSymUse, loc) =>
          (opSymUse.sym.eff, loc)
      }
      val defSymUses: List[(SymUse.DefSymUse, SourceLocation)] = getDefSymUses(uri)
      val defEffSyms: List[(Symbol.EffSym, SourceLocation)] = defSymUses.flatMap {
        case (defSymUse, loc) =>
          root.defs(defSymUse.sym).spec.eff.effects.zip(loc :: Nil)
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
      mkHintsFromEffects(positionToEffectsMap) ::: getInlayHintsFromErrors(errors) ::: getDecreasingParamHints(uri) ::: getTerminatesHints(uri) ::: getTailRecHints(uri)
    } else {
      List.empty[InlayHint] ::: getInlayHintsFromErrors(errors) ::: getDecreasingParamHints(uri) ::: getTerminatesHints(uri) ::: getTailRecHints(uri)
    }
  }

  /**
    * Returns a list of inlay hints from a given list of CompilationMessage(s),
    * specifically containing explicitly and implicitly pure functions using IO, errors.
    */
  private def getInlayHintsFromErrors(errors: List[CompilationMessage]): List[InlayHint] = {
    errors.foldLeft(List(): List[InlayHint]) {
      case (acc, TypeError.ExplicitlyPureFunctionUsesIO(loc, _)) => mkIOHint(Position.from(loc.start), "IO", "IO", Range.from(loc)) :: acc
      case (acc, TypeError.ImplicitlyPureFunctionUsesIO(loc, _)) => mkIOHint(Position.from(loc.end), " \\ IO ", " \\ IO ", Range.from(loc)) :: acc
      case (acc, _) => acc
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
          case Expr.ApplyOp(opSymUse, _, _, _, _, loc) =>
            opSymUses = (opSymUse, loc) :: opSymUses
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
          case Expr.ApplyDef(defSymUse, _, _, _, _, _, _, loc) =>
            defSymUses = (defSymUse, loc) :: defSymUses
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
    )
  }

  /**
    * Creates an inlay hint for explicitly and implicitly pure functions using IO.
    */
  private def mkIOHint(pos: Position, lbl: String, ttp: String, rng: Range): InlayHint = {
      InlayHint(
        position = pos,
        label = lbl,
        kind = Some(InlayHintKind.Type),
        textEdits = List(TextEdit(rng, lbl)),
        tooltip = ttp,
      )
  }

  /**
    * Returns a list of inlay hints for structurally decreasing parameters.
    */
  private def getDecreasingParamHints(uri: String)(implicit root: Root): List[InlayHint] = {
    var hints: List[InlayHint] = List.empty
    object decreasingConsumer extends Consumer {
      override def consumeFormalParam(fparam: FormalParam): Unit = {
        if (fparam.isDecreasing) {
          hints = InlayHint(
            position = Position.fromBegin(fparam.loc),
            label = "\u2193",
            kind = Some(InlayHintKind.Parameter),
            textEdits = List.empty,
            tooltip = "Structurally decreasing",
            paddingLeft = false,
            paddingRight = true
          ) :: hints
        }
      }
    }
    Visitor.visitRoot(root, decreasingConsumer, FileAcceptor(uri))
    hints
  }

  /**
    * Returns a list of inlay hints for `@Terminates` annotations summarizing decreasing parameters.
    */
  private def getTerminatesHints(uri: String)(implicit root: Root): List[InlayHint] = {
    var hints: List[InlayHint] = List.empty
    object terminatesConsumer extends Consumer {
      override def consumeDef(defn: Def): Unit = {
        if (!defn.spec.ann.isTerminates) return
        val decreasingNames = defn.spec.fparams.collect {
          case fp if fp.isDecreasing => fp.bnd.sym.text
        }
        defn.spec.ann.annotations.collectFirst {
          case t: Annotation.Terminates =>
            if (decreasingNames.nonEmpty)
              hints = mkTerminatesHint(t, decreasingNames) :: hints
            else
              hints = mkNoRecursiveCallsHint(t) :: hints
        }
      }
    }
    Visitor.visitRoot(root, terminatesConsumer, FileAcceptor(uri))
    hints
  }

  /**
    * Creates an inlay hint for a `@Terminates` annotation when no recursive calls are detected.
    */
  private def mkNoRecursiveCallsHint(t: Annotation.Terminates): InlayHint = {
    InlayHint(
      position = Position.fromEnd(t.loc),
      label = "(no recursive calls)",
      kind = Some(InlayHintKind.Parameter),
      textEdits = List.empty,
      tooltip = "No recursive calls detected",
      paddingLeft = true,
      paddingRight = false
    )
  }

  /**
    * Creates an inlay hint for a `@Terminates` annotation showing the decreasing parameters.
    */
  private def mkTerminatesHint(t: Annotation.Terminates, decreasingNames: List[String]): InlayHint = {
    InlayHint(
      position = Position.fromEnd(t.loc),
      label = s"(decreases on ${decreasingNames.mkString(", ")})",
      kind = Some(InlayHintKind.Parameter),
      textEdits = List.empty,
      tooltip = "Structurally decreasing parameters",
      paddingLeft = true,
      paddingRight = false
    )
  }

  /**
    * Returns a list of inlay hints for tail-recursive self-calls in `@TailRec` functions.
    */
  private def getTailRecHints(uri: String)(implicit root: Root): List[InlayHint] = {
    var hints: List[InlayHint] = List.empty
    var currentDefSym: Option[Symbol.DefnSym] = None
    object tailRecConsumer extends Consumer {
      override def consumeDef(defn: Def): Unit = {
        if (defn.spec.ann.isTailRecursive)
          currentDefSym = Some(defn.sym)
        else
          currentDefSym = None
      }
      override def consumeExpr(expr: Expr): Unit = {
        currentDefSym.foreach { sym =>
          expr match {
            case Expr.ApplyDef(symUse, _, _, _, _, _, pos, loc)
              if symUse.sym == sym && pos == ExpPosition.Tail =>
              hints = InlayHint(
                position = Position.fromBegin(loc),
                label = "\u21ba",
                kind = Some(InlayHintKind.Parameter),
                textEdits = List.empty,
                tooltip = "Tail-recursive self-call",
                paddingLeft = false,
                paddingRight = false
              ) :: hints
            case _ => ()
          }
        }
      }
    }
    Visitor.visitRoot(root, tailRecConsumer, FileAcceptor(uri))
    hints
  }

}
