/*
 * Copyright 2021 Magnus Madsen
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

import ca.uwaterloo.flix.api.lsp.{CompletionItem, CompletionItemKind, Index, InsertTextFormat, Position}
import ca.uwaterloo.flix.language.ast.{Scheme, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.debug.{Audience, FormatScheme, FormatType}

object CompleteProvider {

  private implicit val audience: Audience = Audience.External

  /**
    * Returns a list of auto-complete suggestions.
    */
  def autoComplete(uri: String, pos: Position, prefix: Option[String])(implicit index: Index, root: TypedAst.Root): Iterable[CompletionItem] = {
    getKeywordCompletionItems() ++ getSnippetCompletionItems() ++ getSuggestions(uri, pos, prefix)
  }

  /**
    * Returns a list of keyword completion items.
    */
  private def getKeywordCompletionItems(): List[CompletionItem] = List(
    // TODO: Manoj: Add more.
    // NB: Please keep the list alphabetically sorted.

    // Keywords:
    CompletionItem("as", "as", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("alias", "alias", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("and", "and", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("case", "case", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.Snippet, Nil),
    CompletionItem("chan", "chan", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("choose", "choose", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("class", "class", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("def", "def", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("deref", "deref", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("else", "else", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("enum", "enum", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("false", "false", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("forall", "forall", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("force", "force", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("from", "from", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("get", "get", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("if", "if", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("import", "import", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("instance", "instance", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("inline", "inline", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("into", "into", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("lat", "lat", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("law", "law", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("let", "let", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("lawless", "lawless", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("lazy", "lazy", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("main", "main", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("match", "match", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("namespace", "namespace", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("null", "null", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("not", "not", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("new", "new", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("opaque", "opaque", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("or", "or", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("override", "override", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("project", "project", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("pub", "pub", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("Pure", "Pure", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("query", "query", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("Record", "Record", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("ref", "ref", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("rel", "rel", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("Schema", "Schema", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("sealed", "sealed", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("select", "select ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("set", "set ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("solve", "solve", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("spawn", "spawn", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("true", "true", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("type", "type", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("unlawful", "unlawful", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("use", "use", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("with", "with", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("where", "where", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),

    CompletionItem("BigInt", "BigInt", None, Some("type"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("Bool", "Bool", None, Some("type"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("Char", "Char", None, Some("type"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("Float32", "Float32", None, Some("type"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("Float64", "Float64", None, Some("type"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("Int8", "Int8", None, Some("type"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("Int32", "Int32", None, Some("type"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("Int64", "Int64", None, Some("type"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("String", "String", None, Some("type"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("Unit", "Unit", None, Some("type"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil)
  )

  /**
    * Returns a list of snippet completion items.
    */
  private def getSnippetCompletionItems(): List[CompletionItem] = List(
    // TODO: Manoj: Add more.
    // NB: Please keep the list alphabetically sorted.

    // Declaration-based:

    // Expressed-based:
    CompletionItem("def", "def ${1:name}(${2:arg}:${3:type}): ${4:type} & Impure = \n\t", None, Some("snippet to define Impure function"), CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
    CompletionItem("def", "def ${1:name}(${2:arg}:${3:type}): ${4:type} & Pure = \n\t", None, Some("snippet to define Pure function"), CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
    CompletionItem("enum", "enum ${1:Name} {\n\tcase ${2:name}\n}", None, Some("snippet for enum"), CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
    CompletionItem("if", "if ${1:cond} ${2:exp} else ${3:exp}", None, Some("snippet for if ()"), CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
    CompletionItem("import", "import ${1:method}", None, Some("import snippet for method call"), CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
    CompletionItem("import", "import new ${1:object} as ${2:name}", None, Some("import snippet for new object"), CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
    CompletionItem("import", "import get ${1:field} as ${2:name}", None, Some("import snippet to get field"), CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
    CompletionItem("import", "import set ${1:field} as ${2:name}", None, Some("import snippet to set field"), CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
    CompletionItem("instance", "instance ${1:name}[Option[${2:a}]] with $1[$2] {\n    ${3:/* code */}\n}", None, Some("snippet for instance"), CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
    CompletionItem("let", "let ${1:name}:${2:type} = ${3:exp};", None, Some("snippet for let"), CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
    CompletionItem("let*", "let* ${1:name}:${2:type} = ${3:exp};", None, Some("snippet for let*"), CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
    CompletionItem("main", "def main(args: Array[String]) : Int32 & Impure = \n    ${1:/* code */} \n    0", None, Some("snippet for main ()"), CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
    CompletionItem("Hello World", "def main(_args: Array[String]) : Int32 & Impure = \n    println(\"Hello World!\");\n    0", None, Some("snippet for Hello World Program"), CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
    CompletionItem("match", "match ${1:exp} {\n    case ${2:pat} => ${3:exp}\n}", None, Some("snippet for pattern match"), CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
    CompletionItem("namespace", "namespace ${1:name} {\n    ${2:/* code */} \n}", None, Some("snippet to create namespace"), CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
    CompletionItem("opaque", "opaque type ${1:name} = ${2:type}", None, Some("snippet for opaque type"), CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
    CompletionItem("query", "query ${1:db} select ${2:cols} from ${3:preds} ${4:where ${5:cond}}", None, Some("snippet for query"), CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
    CompletionItem("rel", "rel ${1:name}(${2:x}: ${3:type}, ${4:y}: ${5:type})", None, Some("snippet to declare predicate symbol"), CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
    CompletionItem("select", "select {\n\tcase ${1:x} <- ${2:c} =>${3:exp}\n}", None, Some("snippet for select"), CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
    CompletionItem("type alias", "type alias ${1:name} = ${2:type}", None, Some("snippet for type alias"), CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
    CompletionItem("use", "use ${1:module};", None, Some("snippet for use"), CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
    CompletionItem("law", "law ${1:name}: forall(${2:params}).${3:exp}", None, Some("snippet for law"), CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
    CompletionItem("class", "pub class ${1:name}[${2:clause}] {\n    $3\n}", None, Some("snippet for class"), CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
    CompletionItem("project", "project ${1:exp} into ${2:fixPoint}", None, Some("snippet for project"), CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil)

  )

  /**
    * Returns a list of other completion items.
    */
  private def getSuggestions(uri: String, pos: Position, prefix: Option[String])(implicit index: Index, root: TypedAst.Root): List[CompletionItem] = {
    ///
    /// Return immediately if there is no AST.
    ///
    if (root == null) {
      return Nil
    }

    // TODO: Add support for classes and enums?
    // TODO: Use the current position to determine what to suggest.

    val defSuggestions = root.defs.values.filter(matchesDef(_, prefix, uri)).map(getDefCompletionItem)
    val sigSuggestions = root.sigs.values.filter(matchesSig(_, prefix, uri)).map(getSigCompletionItem)
    (defSuggestions ++ sigSuggestions).toList.sortBy(_.label)
  }

  /**
    * Returns `true` if the given definition `decl` should be included in the suggestions.
    */
  private def matchesDef(decl: TypedAst.Def, prefix: Option[String], uri: String): Boolean = {
    val isPublic = decl.spec.mod.isPublic
    val isMatch = prefix match {
      case None => true
      case Some(s) =>
        val isNamespace = s.nonEmpty && s.head.isUpper
        if (isNamespace)
          decl.sym.toString.startsWith(s)
        else
          decl.sym.text.startsWith(s)
    }
    val isInFile = decl.spec.loc.source.name == uri

    isMatch && (isPublic || isInFile)
  }

  /**
    * Returns `true` if the given signature `sign` matches the given `prefix`.
    */
  private def matchesSig(sign: TypedAst.Sig, prefix: Option[String], uri: String): Boolean = {
    val isPublic = sign.spec.mod.isPublic
    val isMatch = prefix match {
      case None => true
      case Some(s) =>
        val isNamespace = s.nonEmpty && s.head.isUpper
        if (isNamespace)
          sign.sym.toString.startsWith(s)
        else
          sign.sym.name.startsWith(s)
    }
    val isInFile = sign.spec.loc.source.name == uri

    isMatch && (isPublic || isInFile)
  }

  /**
    * Returns a completion item for the given definition `decl`.
    */
  private def getDefCompletionItem(decl: TypedAst.Def): CompletionItem = {
    val name = decl.sym.toString
    val label = getDefLabel(decl)
    val insertText = getApplySnippet(name, decl.spec.fparams)
    val detail = Some(FormatScheme.formatScheme(decl.spec.declaredScheme))
    val documentation = Some(decl.spec.doc.text)
    val completionKind = CompletionItemKind.Function
    val textFormat = InsertTextFormat.Snippet
    val commitCharacters = Nil
    CompletionItem(label, insertText, detail, documentation, completionKind, textFormat, commitCharacters)
  }

  /**
    * Returns a completion item for the given signature `decl`.
    */
  private def getSigCompletionItem(decl: TypedAst.Sig): CompletionItem = {
    val name = decl.sym.toString
    val label = getSigLabel(decl)
    val insertText = getApplySnippet(name, decl.spec.fparams)
    val detail = Some(FormatScheme.formatScheme(decl.spec.declaredScheme))
    val documentation = Some(decl.spec.doc.text)
    val completionKind = CompletionItemKind.Interface
    val textFormat = InsertTextFormat.Snippet
    val commitCharacters = Nil
    CompletionItem(label, insertText, detail, documentation, completionKind, textFormat, commitCharacters)
  }

  /**
    * Returns the label for the given definition `decl`.
    */
  private def getDefLabel(decl: TypedAst.Def): String =
    getLabel(decl.sym.toString, decl.spec.fparams, decl.spec.declaredScheme)

  /**
    * Returns the label for the given signature `decl`.
    */
  private def getSigLabel(decl: TypedAst.Sig): String =
    getLabel(decl.sym.toString, decl.spec.fparams, decl.spec.declaredScheme)

  /**
    * Returns the label for the given `name`, formal parameters `fparams`, and scheme `sc`.
    */
  private def getLabel(name: String, fparams: List[TypedAst.FormalParam], sc: Scheme): String = {
    val args = fparams.map {
      case fparam => s"${fparam.sym.text}: ${FormatType.formatType(fparam.tpe)}"
    }

    val base = sc.base
    val tpe = FormatType.formatType(getResultType(base))
    val eff = base.typeConstructor match {
      case Some(TypeConstructor.Arrow(_)) => base.typeArguments.head match {
        case Type.Cst(TypeConstructor.True, _) => ""
        case Type.Cst(TypeConstructor.False, _) => " & Impure"
        case e => " & " + FormatType.formatType(e)
      }
      case _ => ""
    }

    s"$name(${args.mkString(", ")}): $tpe$eff"
  }

  /**
    * Returns a snippet for a function with the given `name` and `fparams`.
    *
    * For example, `name(${0:arg1}, ${1:arg2})`.
    */
  private def getApplySnippet(name: String, fparams: List[TypedAst.FormalParam]): String = {
    val args = fparams.zipWithIndex.map {
      case (fparam, idx) => "$" + s"{${idx + 1}:${fparam.sym.text}}"
    }
    s"$name(${args.mkString(", ")})"
  }

  /**
    * Returns the return type of the given function type `tpe0`.
    */
  private def getResultType(tpe0: Type): Type = tpe0.typeArguments.last

}
