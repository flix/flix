package ca.uwaterloo.flix.language.phase.njvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.{Def, Root}
import ca.uwaterloo.flix.language.ast.{FinalAst, MonoType, Symbol}
import ca.uwaterloo.flix.language.phase.jvm.JvmName
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.MnemonicsTypes._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.{MnemonicsGenerator, _}
import ca.uwaterloo.flix.language.phase.njvm.NJvmType._
import ca.uwaterloo.flix.language.phase.njvm.classes.Main

object GenMainClass extends MnemonicsGenerator{
  /**
    * Method should receive a Map of all the generated classes so far. It should generate all the new classes
    * and return an updated map with the new generated classes.
    *
    * @param map of all the generated classes so far.
    * @param ts  set of Monotypes this will be used to generate certain classes such as Enum.
    * @return update map with new generated classes
    */
  def gen(map: Map[JvmName, Mnemonics.MnemonicsClass], ts: Set[MonoType])(implicit root: FinalAst.Root, flix: Flix): Map[JvmName, Mnemonics.MnemonicsClass] =
    getMain(root) match{
        case None => map
        case Some(defn) =>
          map + (getErasedJvmType(defn.tpe) match {
            case PrimBool => new Main[MBool](map).getClassMapping
            case PrimChar =>  new Main[MChar](map).getClassMapping
            case PrimByte =>  new Main[MByte](map).getClassMapping
            case PrimShort =>  new Main[MShort](map).getClassMapping
            case PrimInt =>  new Main[MInt](map).getClassMapping
            case PrimLong =>  new Main[MLong](map).getClassMapping
            case PrimFloat =>  new Main[MFloat](map).getClassMapping
            case PrimDouble =>  new Main[MDouble](map).getClassMapping
            case Reference(_) => new Main[Ref[MObject]](map).getClassMapping
            case _ => ???
          })
    }

  /**
    * Optionally returns the main definition in the given AST `root`.
    */
  private def getMain(root: Root): Option[Def] = {
    // The main function must be called `main` and occur in the root namespace.
    val sym = Symbol.mkDefnSym("main")

    // Check if the main function exists.
    root.defs.get(sym) flatMap {
      case defn =>
        // The main function must take zero arguments.
        Some(defn)
    }
  }
}
