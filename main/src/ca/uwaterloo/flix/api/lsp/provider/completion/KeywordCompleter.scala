/*
 * Copyright 2024 Alexander Dybdahl Troelsen
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
package ca.uwaterloo.flix.api.lsp.provider.completion

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.Index
import ca.uwaterloo.flix.language.ast.TypedAst

object KeywordCompleter {
    object Other extends Completer {
        def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root): Iterable[Completion] =
            List(
            ("with", CompletionPriority.highest),
            ("law", CompletionPriority.higher),
            ("@Test", CompletionPriority.high),
            ("where", CompletionPriority.low),
            ("fix", CompletionPriority.low),
            ("@Deprecated", CompletionPriority.lowest),
            ("@Parallel", CompletionPriority.lowest),
            ("@ParallelWhenPure", CompletionPriority.lowest),
            ("@Lazy", CompletionPriority.lowest),
            ("@LazyWhenPure", CompletionPriority.lowest),
            ("Record", CompletionPriority.lowest),
            ("redef", CompletionPriority.lowest),
            ("Schema", CompletionPriority.lowest),
            ) map { case (name, priority) => Completion.KeywordCompletion(name, priority(name)) }
    }

    object Decl extends Completer {
        def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root): Iterable[Completion] = 
            List(      
            ("def", CompletionPriority.highest),
            ("pub", CompletionPriority.higher),
            ("enum", CompletionPriority.high),
            ("type", CompletionPriority.high),
            ("instance", CompletionPriority.high),
            ("mod", CompletionPriority.low),
            ("eff", CompletionPriority.lower),
            ("struct", CompletionPriority.lower),
            ("sealed", CompletionPriority.lowest),
            ("trait", CompletionPriority.lowest),
            ("import", CompletionPriority.lowest),
        ) map {case (name, priority) => Completion.KeywordCompletion(name, priority(name)) }
    }

    object Enum extends Completer { 
        def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root): Iterable[Completion] = 
            List(Completion.KeywordCompletion("case", CompletionPriority.low("case")))
    }

    object Expr extends Completer {
        def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root): Iterable[Completion] =
            List(
            "and",
            "as",
            "def",
            "discard",
            "do",
            "else",
            "false",
            "forA",
            "forM",
            "force",
            "foreach",
            "from",
            "if",
            "inject",
            "into",
            "lazy",
            "let",
            "match",
            "new",
            "not",
            "or",
            "par",
            "query",
            "region",
            "select",
            "solve",
            "spawn",
            "struct",
            "true",
            "try",
            "typematch",
            "unsafe",
            "use",
            "without",
            "yield"
            ) map (name => Completion.KeywordCompletion(name, CompletionPriority.lower(name)))

    }
}
