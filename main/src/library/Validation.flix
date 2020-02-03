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

///
/// The Validation type.
///
pub enum Validation[t, e] {
    case Success(t),
    case Failure(Nel[e])
}

namespace Validation {

    // TODO: app
    // TODO: combine
    // TODO: ensure?

    ///
    /// Returns `t` if `v` is `Success(t).` Otherwise returns `d`.
    ///
    pub def getWithDefault(v: Validation[t, e], d: t): t = match v {
        case Success(t) => t
        case Failure(_) => d
    }

    ///
    /// Returns `true` if and only if `v` is `Success(t)` and `f(t)` is true.
    ///
    /// Returns `false` if `v` is `Failure`.
    ///
    pub def exists(f: t -> Bool, v: Validation[t, e]): Bool = match v {
        case Success(t) => f(t)
        case Failure(_) => false
    }

    ///
    /// Returns `true` if `v` is `Success(t)` and `f(t)` is true or if `v` is `Failure`.
    ///
    pub def forall(f: t -> Bool, v: Validation[t, e]): Bool = match v {
        case Success(t) => f(t)
        case Failure(_) => true
    }

    // TODO: foldLeft, foldRight
    // TODO: getOrElse
    // TODO: leftMap

    ///
    /// Returns `Success(f(v))` if `o` is `Success(v)`. Otherwise returns `v`.
    ///
    pub def map(f: t -> u, v: Validation[t, e]): Validation[u, e] = match v {
        case Success(t) => Success(f(t))
        case Failure(e) => Failure(e)
    }

    // TODO: orElse

    // TODO: traverse


    ///
    /// Swaps the success and failure values.
    ///
    /// Returns `Success(e)` if `v` is `Failure(e)`.
    /// Returns `Failure(t)` if `v` is `Success(t)`.
    ///
    pub def swap(v: Validation[t, e]): Validation[Nel[e], v] = match v {
        case Success(t) => Failure(Nel(t, Nil))
        case Failure(e) => Success(e)
    }

    ///
    /// Converts a Validation to an Option.
    ///
    /// Returns `Some(t)` if `v` is `Success(t)`.
    /// Returns `None` otherwise.
    ///
    pub def toOption(v: Validation[t, e]): Option[t] = match v {
        case Success(t) => Some(t)
        case Failure(_) => None
    }

    ///
    /// Converts a Validation to a Result.
    ///
    /// Returns `Ok(t)` if `v` is `Success(t)`.
    /// Returns `Err(e)` if `v` is `Failure(e)`.
    ///
    pub def toResult(v: Validation[t, e]): Result[t, Nel[e]] = match v {
        case Success(t) => Ok(t)
        case Failure(e) => Err(e)
    }

    ///
    /// Converts a Validation to a List.
    ///
    /// Returns `t :: Nil` if `v` is `Success(v)`.
    /// Returns `Nil` if `v` is `Failure(e)`.
    ///
    pub def toList(v: Validation[t, e]): List[t] = match v {
        case Success(t) => t :: Nil
        case Failure(_) => Nil
    }

}

