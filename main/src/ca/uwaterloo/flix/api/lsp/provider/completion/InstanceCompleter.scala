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
import ca.uwaterloo.flix.api.lsp.Index
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.InstanceCompletion
import ca.uwaterloo.flix.language.ast.{Ast, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.fmt.FormatType

object InstanceCompleter extends Completer {
  /**
    * Returns a List of Completion based on type classes.
    */
  override def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root, delta: DeltaContext): Iterable[InstanceCompletion] = {
    if (context.previousWord != "instance") {
      return Nil
    }

    /**
      * Replaces the text in the given variable symbol `sym` everywhere in the type `tpe`
      * with an equivalent variable symbol with the given `newText`.
      */
    def replaceText(tvar: Symbol.KindedTypeVarSym, tpe: Type, newText: String): Type = tpe match {
      case Type.Var(sym, loc) if tvar == sym => Type.Var(sym.withText(Ast.VarText.SourceText(newText)), loc)
      case Type.Var(_, _) => tpe
      case Type.Cst(_, _) => tpe

      case Type.Apply(tpe1, tpe2, loc) =>
        val t1 = replaceText(tvar, tpe1, newText)
        val t2 = replaceText(tvar, tpe2, newText)
        Type.Apply(t1, t2, loc)

      case Type.Alias(sym, args0, tpe0, loc) =>
        val args = args0.map(replaceText(tvar, _, newText))
        val t = replaceText(tvar, tpe0, newText)
        Type.Alias(sym, args, t, loc)

      case Type.AssocType(sym, args0, kind, loc) =>
        val args = args0.map(replaceText(tvar, _, newText))
        Type.AssocType(sym, args, kind, loc)
    }

    /**
      * Formats the given type `tpe`.
      */
    def fmtType(clazz: TypedAst.Class, tpe: Type, hole: String)(implicit flix: Flix): String =
      FormatType.formatType(replaceText(clazz.tparam.sym, tpe, hole))

    /**
      * Formats the given formal parameters in `spec`.
      */
    def fmtFormalParams(clazz: TypedAst.Class, spec: TypedAst.Spec, hole: String)(implicit flix: Flix): String =
      spec.fparams.map(fparam => s"${fparam.sym.text}: ${fmtType(clazz, fparam.tpe, hole)}").mkString(", ")

    /**
      * Formats the given signature `sig`.
      */
    def fmtSignature(clazz: TypedAst.Class, sig: TypedAst.Sig, hole: String)(implicit flix: Flix): String = {
      val fparams = fmtFormalParams(clazz, sig.spec, hole)
      val retTpe = fmtType(clazz, sig.spec.retTpe, hole)
      val eff = sig.spec.eff match {
        case Type.Cst(TypeConstructor.Pure, _) => ""
        case Type.Cst(TypeConstructor.EffUniv, _) => raw" \ IO"
        case e => raw" \ " + FormatType.formatType(e)
      }
      s"    pub def ${sig.sym.name}($fparams): $retTpe$eff = ???"
    }

    root.classes.map {
      case (_, clazz) =>
        val hole = "${1:t}"
        val classSym = clazz.sym
        val signatures = clazz.signatures.filter(_.exp.isEmpty)
        val body = signatures.map(s => fmtSignature(clazz, s, hole)).mkString("\n\n")
        val completion = s"$classSym[$hole] {\n\n$body\n\n}\n"

        InstanceCompletion(clazz, completion)
    }.toList
  }

  /**
    * Formats the given class `clazz`.
    */
  def fmtClass(clazz: TypedAst.Class): String = {
    s"class ${clazz.sym.name}[${clazz.tparam.name.name}]"
  }
}
