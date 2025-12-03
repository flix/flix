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

import ca.uwaterloo.flix.language.ast.{Kind, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.Lowering.Enums

object Symbols {
  protected[monomorph] object Defs {

    lazy val ChannelGet: Symbol.DefnSym = Symbol.mkDefnSym("Concurrent.Channel.get")
    lazy val ChannelPut: Symbol.DefnSym = Symbol.mkDefnSym("Concurrent.Channel.put")
    lazy val ChannelMpmcAdmin: Symbol.DefnSym = Symbol.mkDefnSym("Concurrent.Channel.mpmcAdmin")
    lazy val ChannelNewTuple: Symbol.DefnSym = Symbol.mkDefnSym("Concurrent.Channel.newChannelTuple")
    lazy val ChannelSelectFrom: Symbol.DefnSym = Symbol.mkDefnSym("Concurrent.Channel.selectFrom")
    lazy val ChannelUnsafeGetAndUnlock: Symbol.DefnSym = Symbol.mkDefnSym("Concurrent.Channel.unsafeGetAndUnlock")

  }

  protected[monomorph] object Enums {
    lazy val ChannelMpmc: Symbol.EnumSym = Symbol.mkEnumSym("Concurrent.Channel.Mpmc")
    lazy val ChannelMpmcAdmin: Symbol.EnumSym = Symbol.mkEnumSym("Concurrent.Channel.MpmcAdmin")
    lazy val ConcurrentReentrantLock: Symbol.EnumSym = Symbol.mkEnumSym("Concurrent.ReentrantLock")
    lazy val FList: Symbol.EnumSym = Symbol.mkEnumSym("List")
  }

  protected[monomorph] object Types {
    lazy val ChannelMpmc: Type = Type.Cst(TypeConstructor.Enum(Enums.ChannelMpmc, Kind.Star ->: Kind.Eff ->: Kind.Star), SourceLocation.Unknown)
    lazy val ChannelMpmcAdmin: Type = Type.mkEnum(Enums.ChannelMpmcAdmin, Nil, SourceLocation.Unknown)
    lazy val ConcurrentReentrantLock: Type = Type.mkEnum(Enums.ConcurrentReentrantLock, Nil, SourceLocation.Unknown)

    def mkList(t: Type, loc: SourceLocation): Type = Type.mkEnum(Enums.FList, List(t), loc)

  }

}
