/*
 * Copyright 2016 Magnus Madsen
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

package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.util.vt.VirtualString._
import ca.uwaterloo.flix.util.vt.VirtualTerminal

/**
  * A common super-type for naming errors.
  */
sealed trait NameError extends CompilationError {
  val kind = "Name Error"
}

object NameError {

  /**
    * An error raised to indicate that the given `name` is ambiguous.
    *
    * @param name the ambiguous name.
    * @param loc  the location of the ambiguous name.
    * @param loc1 the location of the var.
    * @param loc2 the location of the use.
    */
  case class AmbiguousVarOrUse(name: String, loc: SourceLocation, loc1: SourceLocation, loc2: SourceLocation) extends NameError {
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Ambiguous name '" << Red(name) << "'. The name may refer to both a variable and a use." << NewLine
      vt << NewLine
      vt << Code(loc, "ambiguous name.") << NewLine
      vt << NewLine
      vt << "The relevant declarations are:" << NewLine
      vt << NewLine
      vt << Code(loc1, "the var was declared here.") << NewLine
      vt << NewLine
      vt << Code(loc2, "the use was declared here.") << NewLine
    }
  }

  /**
    * An error raised to indicate that the given def `name` is defined multiple times.
    *
    * @param name the name.
    * @param loc1 the location of the first definition.
    * @param loc2 the location of the second definition.
    */
  case class DuplicateDef(name: String, loc1: SourceLocation, loc2: SourceLocation) extends NameError {
    val loc: SourceLocation = loc1 min loc2
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Duplicate definition '" << Red(name) << "'." << NewLine
      vt << NewLine
      vt << Code(loc1, "the first occurrence was here.") << NewLine
      vt << NewLine
      vt << Code(loc2, "the second occurrence was here.") << NewLine
      vt << NewLine
      vt << Underline("Tip:") << " Remove or rename one of the occurrences." << NewLine
    }
  }

  /**
    * An error raised to indicate that the given def `name` is used twice.
    *
    * @param name the clashing name.
    * @param loc1 the location of the first use.
    * @param loc2 the location of the second use.
    */
  case class DuplicateUseDef(name: String, loc1: SourceLocation, loc2: SourceLocation) extends NameError {
    val loc: SourceLocation = loc1 min loc2
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Duplicate use of the def '" << Red(name) << "'." << NewLine
      vt << NewLine
      vt << Code(loc1, "the first use was here.") << NewLine
      vt << NewLine
      vt << Code(loc2, "the second use was here.") << NewLine
    }
  }

  /**
    * An error raised to indicate that the given type `name` is used twice.
    *
    * @param name the clashing name.
    * @param loc1 the location of the first use.
    * @param loc2 the location of the second use.
    */
  case class DuplicateUseTyp(name: String, loc1: SourceLocation, loc2: SourceLocation) extends NameError {
    val loc: SourceLocation = loc1 min loc2
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Duplicate use of the type '" << Red(name) << "'." << NewLine
      vt << NewLine
      vt << Code(loc1, "the first use was here.") << NewLine
      vt << NewLine
      vt << Code(loc2, "the second use was here.") << NewLine
    }
  }

  /**
    * An error raised to indicate that the given `tag` is used twice.
    *
    * @param name the clashing name.
    * @param loc1 the location of the first use.
    * @param loc2 the location of the second use.
    */
  case class DuplicateUseTag(name: String, loc1: SourceLocation, loc2: SourceLocation) extends NameError {
    val loc: SourceLocation = loc1 min loc2
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Duplicate use of the tag '" << Red(name) << "'." << NewLine
      vt << NewLine
      vt << Code(loc1, "the first use was here.") << NewLine
      vt << NewLine
      vt << Code(loc2, "the second use was here.") << NewLine
    }
  }

  /**
    * An error raised to indicate that the given type alias `name` is defined multiple times.
    *
    * @param name the name.
    * @param loc1 the location of the first definition.
    * @param loc2 the location of the second definition.
    */
  case class DuplicateTypeAlias(name: String, loc1: SourceLocation, loc2: SourceLocation) extends NameError {
    val loc: SourceLocation = loc1 min loc2
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Duplicate type alias '" << Red(name) << "'." << NewLine
      vt << NewLine
      vt << Code(loc1, "the first occurrence was here.") << NewLine
      vt << NewLine
      vt << Code(loc2, "the second occurrence was here.") << NewLine
      vt << NewLine
      vt << Underline("Tip:") << " Remove or rename one of the occurrences." << NewLine
    }
  }

  /**
    * An error raised to indicate a suspicious type variable name.
    *
    * @param name the name of the type variable.
    * @param loc  the location of the suspicious type variable.
    */
  case class SuspiciousTypeVarName(name: String, loc: SourceLocation) extends NameError {
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Suspicious type variable '" << Red(name) << s"'. Did you mean: '" << Cyan(name.capitalize) << "'?" << NewLine
      vt << NewLine
      vt << Code(loc, "Suspicious type variable.") << NewLine
      vt << NewLine
      vt << Underline("Tip:") << " Type variables are always lowercase. Named types are uppercase." << NewLine
    }
  }

  /**
    * An error raised to indicate that the class name was not found.
    *
    * @param name the class name.
    * @param loc  the location of the class name.
    */
  case class UndefinedNativeClass(name: String, loc: SourceLocation) extends NameError {
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Undefined class '" << Red(name) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "undefined class.") << NewLine
    }
  }

  /**
    * An error raised to indicate that the local variable was not found.
    *
    * @param name the name of the variable.
    * @param loc  the location of the undefined variable.
    */
  case class UndefinedVar(name: String, loc: SourceLocation) extends NameError {
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Undefined variable '" << Red(name) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "undefined variable.") << NewLine
    }
  }

  /**
    * An error raised to indicate that the type variable was not found.
    *
    * @param name the name of the type variable.
    * @param loc  the location of the undefined type variable.
    */
  case class UndefinedTypeVar(name: String, loc: SourceLocation) extends NameError {
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Undefined type variable '" << Red(name) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "undefined type variable.") << NewLine
    }
  }

}
