/*
 * Copyright 2024 Jakob Schneider Villumsen
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
package ca.uwaterloo.flix.language

import ca.uwaterloo.flix.language.ast.Ast.SyntacticContext

sealed trait CompilationMessageKind {
  override def toString: String = this match {
    case CompilationMessageKind.DerivationError => "Derivation Error"
    case CompilationMessageKind.EntryPointError => "Entry Point Error"
    case CompilationMessageKind.InstanceError => "Instance Error"
    case CompilationMessageKind.KindError => "Kind Error"
    case CompilationMessageKind.LexerError => "Lexer Error"
    case CompilationMessageKind.NameError => "Name Error"
    case CompilationMessageKind.ParseError(sctx) => s"Parse Error ($sctx)"
    case CompilationMessageKind.PatternMatchError => "Pattern Match Error"
    case CompilationMessageKind.RedundancyError => "Redundancy Error"
    case CompilationMessageKind.ResolutionError => "Resolution Error"
    case CompilationMessageKind.SafetyError => "Safety Error"
    case CompilationMessageKind.StratificationError => "Stratification Error"
    case CompilationMessageKind.TestError => "Test Error"
    case CompilationMessageKind.TypeError => "Type Error"
    case CompilationMessageKind.WeederError => "Syntax Error"
  }
}

object CompilationMessageKind {

  case object DerivationError extends CompilationMessageKind

  case object EntryPointError extends CompilationMessageKind

  case object InstanceError extends CompilationMessageKind

  case object KindError extends CompilationMessageKind

  case object LexerError extends CompilationMessageKind

  case object NameError extends CompilationMessageKind

  case class ParseError(sctx: SyntacticContext) extends CompilationMessageKind

  case object PatternMatchError extends CompilationMessageKind

  case object RedundancyError extends CompilationMessageKind

  case object ResolutionError extends CompilationMessageKind

  case object SafetyError extends CompilationMessageKind

  case object StratificationError extends CompilationMessageKind

  case object TestError extends CompilationMessageKind

  case object TypeError extends CompilationMessageKind

  case object WeederError extends CompilationMessageKind

}
