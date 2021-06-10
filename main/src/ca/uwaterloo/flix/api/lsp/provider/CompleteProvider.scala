package ca.uwaterloo.flix.api.lsp.provider

import ca.uwaterloo.flix.api.lsp.{CompletionItem, CompletionItemKind, InsertTextFormat, Position}
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.debug.{Audience, FormatScheme, FormatType}

object CompleteProvider {

  /**
    * Returns a list of auto-complete suggestions.
    */
  def autoComplete(uri: String, pos: Position, prefix: String, root: TypedAst.Root): List[CompletionItem] = {
    getKeywordCompletionItems() ::: getSnippetCompletionItems() ::: getSuggestions(root)
  }

  /**
    * Returns a list of keyword completion items.
    */
  private def getKeywordCompletionItems(): List[CompletionItem] = List(
    // TODO: Manoj: Add more.
    // NB: Please keep the list alphabetically sorted.

    // Keywords:
    CompletionItem("as", "as", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("and", "and", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("BigInt", "BigInt", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("case", "case", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.Snippet, Nil),
    CompletionItem("chan", "chan", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("choose", "choose", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("deref", "deref", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("from", "from", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("false", "false", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("Float32", "Float32", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("Float64", "Float64", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("Impure", "Impure", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("inline", "inline", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("into", "into", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("lat", "lat", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("lawless", "lawless", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("lazy", "lazy", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("mut", "mut", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("namespace", "namespace", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("null", "null", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("not", "not", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("new", "new", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("or", "or", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("override", "override", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("pub", "pub", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("Pure", "Pure", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, List("=")),
    CompletionItem("Record", "Record", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("ref", "ref", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("rel", "rel", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("Schema", "Schema", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("sealed", "sealed", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("select", "select ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, List("{")),
    CompletionItem("solve", "solve", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("spawn", "spawn", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("true", "true", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, List(";")),
    CompletionItem("type", "type", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("unlawful", "unlawful", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("with", "with", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),

    CompletionItem("Bool", "Bool", None, Some("data type"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("Char", "Char", None, Some("data type"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("Int8", "Int8", None, Some("data type"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("Int32", "Int32", None, Some("data type"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("Int64", "Int64", None, Some("data type"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("String", "String", None, Some("data type"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("Unit", "Unit", None, Some("data type"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil)
  )

  /**
    * Returns a list of snippet completion items.
    */
  private def getSnippetCompletionItems(): List[CompletionItem] = List(
    // TODO: Manoj: Add more.
    // NB: Please keep the list alphabetically sorted.

    // Declaration-based:

    // Expressed-based:
    CompletionItem("def", "def ${1:function_name}(${2:arg}:${3:arg_type}): ${4:return_type} = \n", None, Some("code snippet to define a function"), CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
    CompletionItem("elif", "else if (${1:/* condition */}) {\n    ${2:/* code */}\n}", None, Some("code snippet for else if ()"), CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
    CompletionItem("else", "else {\n    ${1:/* code */}\n}", None, Some("code snippet for else"), CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
    CompletionItem("enum", "enum ${1:Sample} {\n    case ${2:DataType1}\n    ${4:/* add more algebric data types separated by comma (,) */}\n}", None, Some("code snippet for enum"), CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
    CompletionItem("if", "if (${1:/* condition */}) {\n    ${2:/* code */}\n}", None, Some("code snippet for if ()"), CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
    CompletionItem("import", "import ${1:module}", None, Some("preprocessor keyword"), CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
    CompletionItem("instance", "instance ${1:Sample}[Option[${2:a}]] with $1[$2] {\n    ${3:/* code */}\n}", None, Some("code snippet for instance"), CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
    CompletionItem("let", "let ${1:name} = ${2:exp};\n", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("let*", "let* ${1:name} = ${2:exp};\n", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
    CompletionItem("main", "def main(args: Array[String]) : Int32 & Impure = \n    ${1:/* code */} \n    0", None, Some("code snippet for main ()"), CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
    CompletionItem("main Hello World", "def main(_args: Array[String]) : Int32 & Impure = \n    println(\"Hello World!\");\n    0", None, Some("code snippet for Hello World Program"), CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
    CompletionItem("match", "match ${1:exp} {\n    case ${2:pat} => ${3:exp}\n}", None, Some("code snippet for pattern match"), CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
    CompletionItem("namespace", "namespace ${1:sample} {\n    ${2:/* code */} \n}", None, Some("code snippet to create namespace"), CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
    CompletionItem("opaque", "opaque type ${1:name} = ${2:type}", None, Some("code snippet for opaque type"), CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
    CompletionItem("pub def", "pub def ${1:function_name}(${2:arg}:${3:arg_type}): ${4:return_type} = \n", None, Some("code snippet to define a public function"), CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
    CompletionItem("query", "query ${1:db} select ${2:cols} from ${3:preds} ${4:where ${5:cond}}", None, Some("code snippet for query"), CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
    CompletionItem("rel", "rel ${1:Sample}(${2:x}: ${3:type}, ${4:y}: ${5:type})", None, Some("code snippet to declare predicate symbol"), CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
    CompletionItem("type alias", "type alias ${1:name} = ${2:type}", None, Some("code snippet for type alias"), CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
    CompletionItem("use", "use ${1:module};", None, Some("code snippet for use"), CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
    CompletionItem("law", "law ${1:name}: forall(${2:params}).${3:exp}", None, Some("code snippet for law"), CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
    CompletionItem("class", "pub class ${1:Sample}[${2:clause}] {\n    $3\n}", None, Some("code snippet for class"), CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
    CompletionItem("force", "force ${1:t}", None, Some("code snippet for force"), CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
    CompletionItem("project", "project ${1:exp} into ${2:fixPoint}", None, Some("code snippet for project"), CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil)

  )

  /**
    * Returns a list of other completion items.
    */
  private def getSuggestions(root: TypedAst.Root): List[CompletionItem] = {
    // TODO: Magnus

    val result1 = if (root == null) Nil else {
      // TODO: Cleanup
      val listDefs = root.defs.filter(kv => kv._1.namespace == List("List") && kv._2.spec.mod.isPublic)
      listDefs.map {
        case (_, defn) =>
          implicit val audience = Audience.External
          val label = reconstructSignature(defn)
          val insertText = defInsertText(defn)
          val detail = Some(FormatScheme.formatScheme(defn.spec.declaredScheme))
          val documentation = Some(defn.spec.doc.text)
          CompletionItem(label, insertText, detail, documentation, CompletionItemKind.Function, InsertTextFormat.Snippet, List("(", ")"))
      }
    }.toList
    result1
  }

  // TODO: Magnus
  private def reconstructSignature(defn: TypedAst.Def): String = {
    implicit val audience = Audience.External

    val prefix = defn.sym.toString
    val args = defn.spec.fparams.map {
      case fparam => s"${fparam.sym.text}: ${FormatType.formatType(fparam.tpe)}"
    }
    val eff = FormatType.formatType(defn.spec.eff)
    s"${prefix}(${args.mkString(", ")}): ??? & "
  }

  // TODO: Magnus
  private def defInsertText(defn: TypedAst.Def): String = {
    val prefix = defn.sym.toString
    val args = defn.spec.fparams.zipWithIndex.map {
      case (fparam, idx) => "$" + s"{${idx + 1}:${fparam.sym.text}}"
    }
    s"${prefix}(${args.mkString(", ")})"
  }

}
