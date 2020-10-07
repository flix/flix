/*
 * Copyright 2020 Magnus Madsen
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
namespace Choice {

    ///
    /// Returns the value in `c` which must be `Present`.
    ///
    pub def unbox[a, isAbsent :# Bool](c: Choice[a, false, isAbsent]): a = choose c {
        case Present(v) => v
    }

    ///
    /// Returns the given choice `c` as an `Option`.
    ///
//    pub def toOption[a, isAbsent :# Bool, isPresent :# Bool](c: Choice[a, isAbsent, isPresent]): a = choose c {
//        case Absent     => None
//        case Present(v) => Some(v)
//    }

    // TODO:

    // - def pick2(c1: Choice[a, b1, b2], c2: Choice[a, b3, b4]): a
    // - def chooseLeft2(c1: Choice[a, ?, ?], c2: Choice[a, ?, ?]): a
    // - def zip(c1: ..., c2: ...): Option[(a, b)]

    // - def map(f: a -> b, c: Choice[a, b1, b2]): Choice[b, b1, b2])

//    pub def map[a, b, isAbsent :# Bool, isPresent :# Bool](f: a -> b, c: Choice[a, isAbsent, isPresent]): Choice[b, isAbsent, isPresent] =
//        choose c {
//            case Absent     => Absent               // TODO: Loss of knowledge about absence/presence.
//            case Present(v) => Present(f(v))        // TODO: Loss of knowledge about absence/presence.
//        }

//    pub def mapBorked[a, b, isAbsent :# Bool, isPresent :# Bool](f: a -> b, c: Choice[a, isAbsent, isPresent]): Choice[b, true, true] =
//        choose c {
//            case Absent     => Absent
//            case Present(v) => Present(f(v))
//        }

   pub def mapPresent[a, b](f: a -> b, c: Choice[a, false, true]): Choice[b, false, true] =
       choose c {
           case Present(v) => Present(f(v))
       }


    // - def Option.toChoice(o: Option[a]): Choice[a, T, T]

    // TODO: Add tests for all of these.

}
