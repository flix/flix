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

sealed trait CompilationMessageKind

object CompilationMessageKind {

  case object DerivationError extends CompilationMessageKind {
    override def toString: String = "Derivation Error"
  }

  case object EntryPointError extends CompilationMessageKind {
    override def toString: String = "Entry Point Error"
  }

  case object InstanceError extends CompilationMessageKind {
    override def toString: String = "Instance Error"
  }

  case object KindError extends CompilationMessageKind {
    override def toString: String = "Kind Error"
  }

  case object LexerError extends CompilationMessageKind {
    override def toString: String = "Lexer Error"
  }

  case object NameError extends CompilationMessageKind {
    override def toString: String = "Name Error"
  }

  case class ParseError(sctx: SyntacticContext) extends CompilationMessageKind {
    override def toString: String = s"Parse Error ($sctx)"
  }

  case object PatternMatchError extends CompilationMessageKind {
    override def toString: String = "Pattern Match"
  }

  case object RedundancyError extends CompilationMessageKind {
    override def toString: String = "Redundancy Error"
  }

  case object ResolutionError extends CompilationMessageKind {
    override def toString: String = "Resolution Error"
  }

  case object SafetyError extends CompilationMessageKind {
    override def toString: String = "Safety Error"
  }

  case object StratificationError extends CompilationMessageKind {
    override def toString: String = "Stratification Error"
  }

  case object TestError extends CompilationMessageKind {
    override def toString: String = "Test Error"
  }

  case object TypeError extends CompilationMessageKind {
    override def toString: String = "Type Error"
  }

  case object WeederError extends CompilationMessageKind {
    override def toString: String = "Syntax Error"
  }

}
