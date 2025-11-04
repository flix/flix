/*
 * Copyright 2021 Magnus Madsen
 *                Casper Dalgaard Nielsen
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

package ca.uwaterloo.flix.language.phase.monomorph

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{LoweredAst, MonoAst, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.monomorph.Specialization.Context
import ca.uwaterloo.flix.language.phase.monomorph.Symbols.{Defs, Types}
import ca.uwaterloo.flix.util.InternalCompilerException

/**
  * This phase translates AST expressions related to the Datalog subset of the
  * language into `Fixpoint.Ast.Datalog` values (which are ordinary Flix values).
  * This allows the Datalog engine to be implemented as an ordinary Flix program.
  *
  * In addition to translating expressions, types must also be translated from
  * Schema types to enum types.
  *
  * Finally, values must be boxed using the Boxable.
  *
  * It also translates channels to the lowered types.
  */
object Lowering {

  /**
    * Lowers the type `t`. Currently implemented as a no-op.
    *
    * When implemented:
    * Replaces schema types with the Datalog enum type and channel-related types with the channel enum type.
    *
    * @param tpe0 the type to be lowered.
    * @return
    */
   def lowerType(tpe0: Type): Type = tpe0 match {
     case Type.Cst(_, _) => tpe0 // Performance: Reuse tpe0.

     case Type.Var(_, _) => tpe0

     // Rewrite Sender[t] to Concurrent.Channel.Mpmc[t, IO]
     case Type.Apply(Type.Cst(TypeConstructor.Sender, loc), tpe, _) =>
       val t = lowerType(tpe)
       mkChannelTpe(t, loc)

     // Rewrite Receiver[t] to Concurrent.Channel.Mpmc[t, IO]
     case Type.Apply(Type.Cst(TypeConstructor.Receiver, loc), tpe, _) =>
       val t = lowerType(tpe)
       mkChannelTpe(t, loc)

     case Type.Apply(tpe1, tpe2, loc) =>
       val t1 = lowerType(tpe1)
       val t2 = lowerType(tpe2)
       // Performance: Reuse tpe0, if possible.
       if ((t1 eq tpe1) && (t2 eq tpe2)) {
         tpe0
       } else {
         Type.Apply(t1, t2, loc)
       }

     case Type.Alias(sym, args, t, loc) =>
       Type.Alias(sym, args.map(lowerType), lowerType(t), loc)

     case Type.AssocType(cst, args, kind, loc) =>
       // TODO: It appears that substitution (`reduceAssocType`) removes `AssocTypes`, so this should be an `InternalCompilerError`, right?
       Type.AssocType(cst, args.map(lowerType), kind, loc)

     case Type.JvmToType(_, loc) => throw InternalCompilerException("unexpected JVM type", loc)

     case Type.JvmToEff(_, loc) => throw InternalCompilerException("unexpected JVM eff", loc)

     case Type.UnresolvedJvmType(_, loc) => throw InternalCompilerException("unexpected JVM type", loc)

   }

  /**
    * Returns the definition associated with the given symbol `sym`.
    *
    * @param tpe must be subst. Can be visited, if the underlying function can handle that
    */
  private def lookup(sym: Symbol.DefnSym, tpe: Type)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): Symbol.DefnSym =
    Specialization.specializeDefnSym(sym, tpe)

  /**
    * Make a new channel tuple (sender, receiver) expression
    *
    * @param tpe The specialized type of the result
    */
  def visitNewChannel(exp: MonoAst.Expr, tpe: Type, eff: Type, loc: SourceLocation)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): MonoAst.Expr = {
    val itpe = Type.mkIoArrow(exp.tpe, tpe, loc)
    val defnSym = lookup(Defs.ChannelNewTuple, itpe)
    MonoAst.Expr.ApplyDef(defnSym, exp :: Nil, lowerType(itpe), lowerType(tpe), eff, loc)
  }

  /**
    * The type of a channel which can transmit variables of type `tpe`.
    */
  private def mkChannelTpe(tpe: Type, loc: SourceLocation): Type = {
    mkChannelTpe(tpe, Type.IO, loc)
  }

  /**
    * The type of a channel which can transmit variables of type `tpe1` in region `tpe2`.
    */
  private def mkChannelTpe(tpe1: Type, tpe2: Type, loc: SourceLocation): Type = {
    Type.Apply(Type.Apply(Types.ChannelMpmc, tpe1, loc), tpe2, loc)
  }

}
