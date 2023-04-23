/*
 * Copyright 2022 Paul Butcher, Lukas Rønn
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
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.UseCompletion
import ca.uwaterloo.flix.api.lsp.{CompletionItemKind, Index}
import ca.uwaterloo.flix.language.ast.TypedAst

import scala.annotation.tailrec

object UseCompleter extends Completer {
  /**
    * Returns a List of Completion for completer.
    */
  override def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root, delta: DeltaContext): Iterable[UseCompletion] = {
    val regex = raw"\s*use\s+(.*)".r

    context.prefix match {
      case regex(ns) =>
        // Six cases
        // 0: We have nothing i.e.
        // 1: a path i.e. A.B
        // 2: an item i.e. Foo (enum/class/def/type alias)
        // 3: path and item i.e. A.B.Foo
        // 4: item and tag/sig i.e Foo.Bar
        // 5: path, item and tag/sig i.e A.B.Foo.Bar

        val segments = ns.split('.')
        segments.toList match {
          // case 0
          case Nil => nsCompletionsAfterPrefix(Nil) ++ getItemUseCompletions(Nil)
          // case 1/2
          case x :: Nil =>
            // We might be done typing the namespace or not. We need to try both cases
            val prefix1 = x.split('.').toList
            val prefix2 = x.split('.').dropRight(1).toList
            // case 1
            nsCompletionsAfterPrefix(prefix1) ++
              nsCompletionsAfterPrefix(prefix2) ++
              getItemUseCompletions(prefix1) ++
              getItemUseCompletions(prefix2) ++
              // case 2
              getEnumTagCompletions(Nil, x) ++
              getClassSigCompletions(Nil, x)
          // case 3/4
          case x :: y :: Nil =>
            val ns = x.split('.').toList
            // case 3
            getItemUseCompletions(ns) ++
              getEnumTagCompletions(ns, y) ++
              getClassSigCompletions(ns, y) ++
              // case 4
              getEnumTagCompletions(Nil, x) ++
              getClassSigCompletions(Nil, x)
          // case 5
          case x :: y :: _ :: Nil =>
            val ns = x.split('.').toList
            getEnumTagCompletions(ns, y) ++
              getClassSigCompletions(ns, y)
          case _ => Nil
        }
      case _ => Nil
    }
  }

  /**
    * Gets completions for a sub namespace of a prefix namespace
    * I.e if you have namespace A.B.C.D, then if prefix is A.B it will return a completion for A.B.C
    */
  private def nsCompletionsAfterPrefix(prefix: List[String])(implicit root: TypedAst.Root): Iterable[UseCompletion] = {
    val nss = root.defs.keySet.map(_.namespace) ++
      root.enums.keySet.map(_.namespace) ++
      root.classes.keySet.map(_.namespace) ++
      root.typeAliases.keySet.map(_.namespace)

    nss.flatMap(ns => getFirstAfterGivenPrefix(ns, prefix))
      .map(nextNs => {
        val name = prefix.appended(nextNs).mkString(".")
        UseCompletion(name, CompletionItemKind.Module)
      })
  }

  /**
    * Returns the first namespace after a given prefix if the prefix is a prefix and there is a next
    */
  @tailrec
  private def getFirstAfterGivenPrefix(ns: List[String], prefix: List[String]): Option[String] = {
    (ns, prefix) match {
      case (x :: _, Nil) => Some(x)
      case (x :: xs, y :: ys) if x == y => getFirstAfterGivenPrefix(xs, ys)
      case _ => None
    }
  }

  /**
    * Gets completions for all items in a namespace
    */
  private def getItemUseCompletions(ns: List[String])(implicit root: TypedAst.Root): Iterable[UseCompletion] = {
    getEnumUseCompletions(ns) ++
      getClassUseCompletions(ns) ++
      getDefUseCompletions(ns) ++
      getTypeUseCompletions(ns)
  }

  /**
    * Gets completions for enums in a given namespace
    */
  private def getEnumUseCompletions(ns: List[String])(implicit root: TypedAst.Root): Iterable[UseCompletion] = {
    root.enums.filter { case (_, emn) => emn.mod.isPublic }
      .keySet.filter(_.namespace == ns)
      .map(sym => UseCompletion(s"${nsToStringDot(ns)}${sym.name}", CompletionItemKind.Enum))
  }

  /**
    * Gets completions for classes in a given namespace
    */
  private def getClassUseCompletions(ns: List[String])(implicit root: TypedAst.Root): Iterable[UseCompletion] = {
    root.classes.filter { case (_, clazz) => clazz.mod.isPublic }
      .keySet.filter(_.namespace == ns)
      .map(sym => UseCompletion(s"${nsToStringDot(ns)}${sym.name}", CompletionItemKind.Interface))
  }

  /**
    * Gets completions for functions in a given namespace
    */
  private def getDefUseCompletions(ns: List[String])(implicit root: TypedAst.Root): Iterable[UseCompletion] = {
    root.defs.filter { case (_, df) => df.spec.mod.isPublic }
      .keySet.filter(_.namespace == ns)
      .map(sym => UseCompletion(s"${nsToStringDot(ns)}${sym.name}", CompletionItemKind.Function))
  }

  /**
    * Gets completion for type aliases in a given namespace
    */
  private def getTypeUseCompletions(ns: List[String])(implicit root: TypedAst.Root): Iterable[UseCompletion] = {
    root.typeAliases.filter { case (_, tpe) => tpe.mod.isPublic }
      .keySet.filter(_.namespace == ns)
      .map(sym => UseCompletion(s"${nsToStringDot(ns)}${sym.name}", CompletionItemKind.Struct))
  }

  /**
    * Gets completions for enum tags
    */
  private def getEnumTagCompletions(ns: List[String], enmName: String)(implicit root: TypedAst.Root): Iterable[UseCompletion] = {
    root.enums.filter { case (sym, _) => sym.name == enmName && sym.namespace == ns }
      .flatMap { case (sym, emn) => emn.cases.map {
        case (casSym, _) => UseCompletion(s"${nsToStringDot(ns)}${sym.name}.${casSym.name}", CompletionItemKind.EnumMember)
      }
      }
  }

  /**
    * Gets completions for class sigs
    */
  private def getClassSigCompletions(ns: List[String], className: String)(implicit root: TypedAst.Root): Iterable[UseCompletion] = {
    root.classes.filter { case (sym, _) => sym.name == className && sym.namespace == ns }
      .flatMap { case (sym, clazz) => clazz.signatures
        .map(sig => UseCompletion(s"${nsToStringDot(ns)}${sym.name}.${sig.sym.name}", CompletionItemKind.EnumMember))
      }
  }

  /**
    * Converts a namespace into a .-seperated string with a dot at the end unless it is the root namespace
    */
  private def nsToStringDot(ns: List[String]): String = {
    ns match {
      case Nil => ""
      case _ => s"${ns.mkString(".")}."
    }
  }
}
