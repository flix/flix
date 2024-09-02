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

/**
  * A collection of keyword completers, including
  * 
  * - `Decl`: completer for declaraiton keywords
  * - `Enum`: completer for declaration keywords
  * - `Expr`: completer for expressoin keywords
  * - `Other`: completer for miscellaneous keywords 
  * 
  */
object KeywordCompleters {

    /**
      * A completer for miscellaneous keywords.
      */ 
    object Other extends Completer {
        def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root): Iterable[Completion] =
            List(
            ("with", Priority.highest),
            ("law", Priority.higher),
            ("@Test", Priority.high),
            ("where", Priority.low),
            ("fix", Priority.low),
            ("@Deprecated", Priority.lowest),
            ("@Parallel", Priority.lowest),
            ("@ParallelWhenPure", Priority.lowest),
            ("@Lazy", Priority.lowest),
            ("@LazyWhenPure", Priority.lowest),
            ("Record", Priority.lowest),
            ("redef", Priority.lowest),
            ("Schema", Priority.lowest),
            ) map { case (name, priority) => Completion.KeywordCompletion(name, priority(name)) }
    }

    /**
      * A completer for trait declaration keywords. These are keywords that occur within a trait declaration.
      */
    object Trait extends Completer {
      def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root): Iterable[Completion] = {
        if (context.previousWord == "pub") {
          Completion.KeywordCompletion("def", Priority.lower("def")) :: Nil
        } else {
          Completion.KeywordCompletion("pub", Priority.lower("pub")) :: Nil
        }
      }
    }

    val def_key = ("def", Priority.highest)
    val pub_key = ("pub", Priority.higher)
    val enum_key = ("enum", Priority.high)
    val type_key = ("type", Priority.high)
    val instance_key = ("instance", Priority.high)
    val mod_key = ("mod", Priority.low)
    val eff_key = ("eff", Priority.lower)
    val struct_key = ("struct", Priority.lower)
    val sealed_key = ("sealed", Priority.lowest)
    val trait_key = ("trait", Priority.lowest)
    val import_key = ("import", Priority.lowest)

    val decl_keys = List(
      def_key,
      pub_key,
      enum_key,
      type_key,
      instance_key,
      mod_key,
      eff_key,
      struct_key,
      sealed_key,
      trait_key,
      import_key,
    )

    object SealedDecl extends Completer {
      def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root): Iterable[Completion] = 
        List(
          trait_key,
        ) map { case (name, priority) => Completion.KeywordCompletion(name, priority(name) )}
    }

    object PubDecl extends Completer {
      def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root): Iterable[Completion] = 
        List(
          def_key,
          enum_key,
          type_key,
          eff_key,
          struct_key,
        ) map { case (name, priority) => Completion.KeywordCompletion(name, priority(name)) }
    }

    object PriDecl extends Completer {
      def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root): Iterable[Completion] = 
        decl_keys map { case (name, priority) => Completion.KeywordCompletion(name, priority(name)) }
    }

    /**
      * A completer for declaration keywords. These are keywords that denote a declaration.
      */
    object Decl extends Completer {
        def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root): Iterable[Completion] = {
          context.previousWord match {
            case "pub" => PubDecl.getCompletions(context)
            case "sealed" => SealedDecl.getCompletions(context)
            case _ => PriDecl.getCompletions(context)
          }
        }
    }

    /**
      * A completer for enum keywords. These are keywords that can appear within the declaration of an enum.
      */ 
    object Enum extends Completer { 
        def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root): Iterable[Completion] = 
            List(Completion.KeywordCompletion("case", Priority.low("case")))
    }

    /**
      * A completer for expression keywords. These are keywords that can appear within expressions (fx within the body of a function).
      */ 
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
            ) map (name => Completion.KeywordCompletion(name, Priority.lower(name)))

    }
}
