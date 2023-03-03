/*
 * Copyright 2022 Paul Butcher, Lukas Rønn, Magnus Madsen
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
import ca.uwaterloo.flix.api.lsp.TextEdit
import ca.uwaterloo.flix.api.lsp.provider.CompletionProvider.{Priority}
import ca.uwaterloo.flix.language.ast.{Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.fmt.FormatType

import java.lang.reflect.{Constructor, Executable, Method}

object CompletionUtils {

  /**
    * returns a triple from a java executable (method/constructor) instance, providing information the make the specific completion.
    * clazz is the clazz in string form used for the completion.
    * aliasSuggestion is used to suggest an alias for the function if applicable.
    */
  def getExecutableCompletionInfo(exec: Executable, clazz: String, aliasSuggestion: Option[String], context: CompletionContext): (String, String, TextEdit) = {
    val typesString = exec.getParameters.map(param => convertJavaClassToFlixType(param.getType)).mkString("(", ", ", ")")
    val finalAliasSuggestion = aliasSuggestion match {
      case Some(aliasSuggestion) => s" as $${0:$aliasSuggestion}"
      case None => ""
    }
    // Get the name of the function if it is not a constructor.
    val name = if (exec.isInstanceOf[Constructor[_ <: Object]]) "" else s".${exec.getName}"
    // So for constructors we do not have a return type method but we know it is the declaring class.
    val returnType = exec match {
      case method: Method => method.getReturnType
      case _ => exec.getDeclaringClass
    }
    val label = s"$clazz$name$typesString"
    val replace = s"$clazz$name$typesString: ${convertJavaClassToFlixType(returnType)} \\ IO$finalAliasSuggestion;"
    (label, Priority.high(s"${exec.getParameterCount}$label"), TextEdit(context.range, replace))
  }


  private def isUnitType(tpe: Type): Boolean = tpe == Type.Unit

  private def isUnitFunction(fparams: List[TypedAst.FormalParam]): Boolean = fparams.length == 1 && isUnitType(fparams(0).tpe)

  def getLabelForNameAndSpec(name: String, spec: TypedAst.Spec)(implicit flix: Flix): String = spec match {
    case TypedAst.Spec(_, _, _, _, fparams, _, retTpe0, pur0, eff0, _, _) =>
      val args = if (isUnitFunction(fparams))
        Nil
      else
        fparams.map {
          fparam => s"${fparam.sym.text}: ${FormatType.formatType(fparam.tpe)}"
        }

      val retTpe = FormatType.formatType(retTpe0)

      // don't show purity if bool effects are turned off
      val pur = if (flix.options.xnobooleffects) {
        ""
      } else {
        pur0 match {
          case Type.Cst(TypeConstructor.True, _) => ""
          case Type.Cst(TypeConstructor.False, _) => " & Impure"
          case p => " & " + FormatType.formatType(p)
        }
      }

      // don't show effect if set effects are turned off
      val eff = if (flix.options.xnoseteffects) {
        ""
      } else {
        eff0 match {
          case Type.Cst(TypeConstructor.Empty, _) => ""
          case e => " \\ " + FormatType.formatType(e)
        }
      }

      s"$name(${args.mkString(", ")}): $retTpe$pur$eff"
  }

  /**
    * Generate a snippet which represents calling a function.
    * Drops the last one or two arguments in the event that the function is in a pipeline
    * (i.e. is preceeded by `|>`, `!>`, or `||>`)
    */
  def getApplySnippet(name: String, fparams: List[TypedAst.FormalParam])(implicit context: CompletionContext): String = {
    val functionIsUnit = isUnitFunction(fparams)

    val args = fparams.dropRight(paramsToDrop).zipWithIndex.map {
      case (fparam, idx) => "$" + s"{${idx + 1}:?${fparam.sym.text}}"
    }
    if (functionIsUnit)
      s"$name()"
    else if (args.nonEmpty)
      s"$name(${args.mkString(", ")})"
    else
      name
  }

  /**
    * Helper function for deciding if a snippet can be generated.
    * Returns false if there are too few arguments.
    */
  def canApplySnippet(fparams: List[TypedAst.FormalParam])(implicit context: CompletionContext): Boolean = {
    val functionIsUnit = isUnitFunction(fparams)

    if (paramsToDrop > fparams.length || (functionIsUnit && paramsToDrop > 0)) false else true
  }

  /**
    * Calculates how many params to drops in the event that the function is in a pipeline
    * (i.e. is preceeded by `|>`, `!>`, or `||>`)
    */
  private def paramsToDrop(implicit context: CompletionContext): Int = {
    context.previousWord match {
      case "||>" => 2
      case "|>" | "!>" => 1
      case _ => 0
    }
  }

  /**
    * Under some circumstances, even though we set `isIncomplete`, which is supposed to opt-out
    * of this behaviour, VSCode filters returned completions when the user types more text
    * without calling the language server again (so it has no chance to return different
    * completions).
    *
    * If we use `label` as filter text (which is the default), this can result in many false
    * positives, e.g. if the user types "MyList[t", the "t" will result in many potential Def
    * and Sig completions. If the user then types "]" VSCode will filter this list using the
    * "word" "t]" which will match many of these completions (e.g. "Nec.tail(c: Nec[a]): ...").
    *
    * To avoid this behaviour, we set `filterText` for Def and Sig completions to be just the
    * name. The "(" is there so that they still see completions if they enter the opening
    * bracket of a function call (but not if they start filling in the argument list).
    */
  def getFilterTextForName(name: String): String = {
    s"$name("
  }

  /**
    * Converts a namespace into a .-seperated string with a / at the end unless it is the root namespace
    */
  private def nsToStringSlash(ns: List[String]): String = {
    ns match {
      case Nil => ""
      case _ => s"${ns.mkString(".")}/"
    }
  }

  /**
    * Returns a class object if the string is a class or removing the last "part" makes it a class
    */
  def classFromDotSeperatedString(clazz: String): Option[(Class[_], String)] = {
    // If the last charachter is . then this drops that
    // I.e if we have java.lang.String. this converts to java.lang.String
    // while if it does not end with . it is unchanged
    val clazz1 = clazz.split('.').mkString(".")
    // If we are typing the method/field to import we drop that
    val clazz2 = clazz.split('.').dropRight(1).mkString(".")
    classFromString(clazz1).orElse(classFromString(clazz2))
  }

  /**
    * Return a class object if the class exists
    */
  def classFromString(clazz: String): Option[(Class[_], String)] = {
    try {
      Some((java.lang.Class.forName(clazz), clazz))
    }
    catch {
      case _: ClassNotFoundException => None
    }
  }

  /**
    * Converts a Java Class Object into a string representing the type in flix syntax.
    * I.e. java.lang.String => String, byte => Int8, java.lang.Object[] => Array[##java.lang.Object, false].
    */
  def convertJavaClassToFlixType(clazz: Class[_]): String = {
    if (clazz.isArray()) {
      s"Array[${convertJavaClassToFlixType(clazz.getComponentType())}, Static]"
    }
    else {
      clazz.getName() match {
        case "byte" => "Int8"
        case "short" => "Int16"
        case "int" => "Int32"
        case "long" => "Int64"
        case "float" => "Float32"
        case "double" => "Float64"
        case "boolean" => "Bool"
        case "char" => "Char"
        case "java.lang.String" => "String"
        case "java.math.BigInteger" => "BigInt"
        case "java.math.BigDecimal" => "BigDecimal"
        case "java.util.function.IntFunction" => "Int32 => ##java.lang.Object"
        case "java.util.function.IntUnaryOperator" => "Int32 => Int32"
        case "void" => "Unit"
        case other => s"##$other"
      }
    }
  }

}
