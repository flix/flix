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


    /**
      * A completer for declaration keywords. These are keywords that denote a declaration.
      */
    object Decl extends Completer {
      val DEF       = "def"
      val PUB       = "pub"
      val ENUM      = "enum"
      val TYPE      = "type"
      val INSTANCE  = "instance"
      val MOD       = "mod"
      val EFF       = "eff"
      val STRUCT    = "struct"
      val SEALED    = "sealed"
      val TRAIT     = "trait"
      val IMPORT    = "import"

      val DEF_ENTRY       = (DEF      , Priority.highest)
      val PUB_ENTRY       = (PUB      , Priority.higher)
      val ENUM_ENTRY      = (ENUM     , Priority.high)
      val TYPE_ENTRY      = (TYPE     , Priority.high)
      val INSTANCE_ENTRY  = (INSTANCE , Priority.high)
      val MOD_ENTRY       = (MOD      , Priority.low)
      val EFF_ENTRY       = (EFF      , Priority.lower)
      val STRUCT_ENTRY    = (STRUCT   , Priority.lower)
      val SEALED_ENTRY    = (SEALED   , Priority.lowest)
      val TRAIT_ENTRY     = (TRAIT    , Priority.lowest)
      val IMPORT_ENTRY    = (IMPORT   , Priority.lowest)

      def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root): Iterable[Completion] = {
        context.previousWord match {
          case PUB => PubDecl.getCompletions(context)
          case SEALED => SealedDecl.getCompletions(context)
          case _ => PriDecl.getCompletions(context) ++ DeclMod.getCompletions(context)
        }
      }

      object DeclMod extends Completer {
        def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root): Iterable[Completion] = 
          List(
            PUB_ENTRY,
            SEALED_ENTRY,
          ) map { case (name, priority) => Completion.KeywordCompletion(name, priority(name)) }
      }

      object SealedDecl extends Completer {
        def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root): Iterable[Completion] = 
          List(
            TRAIT_ENTRY,
          ) map { case (name, priority) => Completion.KeywordCompletion(name, priority(name)) }
      }

      object PubDecl extends Completer {
        def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root): Iterable[Completion] = 
          List(
            DEF_ENTRY,
            ENUM_ENTRY,
            TYPE_ENTRY,
            EFF_ENTRY,
            STRUCT_ENTRY,
          ) map { case (name, priority) => Completion.KeywordCompletion(name, priority(name)) }
      }

      object PriDecl extends Completer {
        def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root): Iterable[Completion] = 
          List(
            DEF_ENTRY,
            ENUM_ENTRY,
            TYPE_ENTRY,
            INSTANCE_ENTRY,
            MOD_ENTRY,
            EFF_ENTRY,
            STRUCT_ENTRY,
            TRAIT_ENTRY,
            IMPORT_ENTRY,
          ) map { case (name, priority) => Completion.KeywordCompletion(name, priority(name)) }
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
