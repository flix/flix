/*
 * Copyright 2026 Magnus Madsen
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
/// A designated portable exception value.
///
/// Portable compilation targets (LLVM native/wasm) represent throwing as returning a `Result.Exception`
/// that carries an `Exn` value (payload + trace).
///
/// The `kindId` is used for deterministic catch dispatch without relying on JVM `Class` objects.
///
pub enum Exn {
    case Exn(Int32, AnyType, AnyType)
}

///
/// A designated cancellation payload.
///
/// The LLVM runtime throws `Exn.mk(Cancelled(...))` (or an equivalent runtime-constructed `Exn`)
/// when a task observes cooperative cancellation.
///
/// The `cause` indicates why cancellation was requested (typically a child or parent exception).
///
/// Note: Flix currently requires structs to have at least one type parameter, so we define a
/// payload struct and expose a parameterless alias for ergonomic catching: `case exn: Cancelled => ...`.
///
pub struct CancelledPayload[_: Eff] {
    cause: Exn
}

pub type alias Cancelled = CancelledPayload[Static]

///
/// A designated arithmetic payload for portable exceptions.
///
/// This is used for runtime arithmetic failures that do not have a natural total return value,
/// e.g. non-terminating exact `BigDecimal` division.
///
pub mod Exn {

    ///
    /// A portable, runtime-defined stack trace representation.
    ///
    /// v0: this is intentionally opaque; the runtime will eventually provide a concrete shape
    /// and formatting support.
    ///
    pub type alias Trace = AnyType

    ///
    /// Constructs an exception from the given `payload`.
    ///
    /// The portable backends attach a (portable) logical stack trace when the exception is thrown.
    ///
    pub def mk(payload: a): Exn =
        Exn.Exn(%%EXN_KIND_ID%%(payload), unchecked_cast(payload as AnyType), emptyTrace())

    ///
    /// Returns the kind id of `exn`.
    ///
    pub def kindId(exn: Exn): Int32 = match exn {
        case Exn.Exn(k, _, _) => k
    }

    ///
    /// Returns the payload of `exn`.
    ///
    pub def payload(exn: Exn): AnyType = match exn {
        case Exn.Exn(_, p, _) => p
    }

    ///
    /// Returns the trace of `exn`.
    ///
    pub def trace(exn: Exn): Trace = match exn {
        case Exn.Exn(_, _, t) => unchecked_cast(t as Trace)
    }

    ///
    /// Returns the payload of `exn` cast to `a`.
    ///
    /// This is intended to be used under a matching catch rule (where the kind id has already
    /// been checked by the runtime/compiler).
    ///
    pub def payloadAs[a: Type](exn: Exn): a =
        unchecked_cast(payload(exn) as a)

    ///
    /// Returns an empty trace value.
    ///
    /// v0: the runtime populates the trace at the first throw site.
    ///
    pub def emptyTrace(): Trace =
        unchecked_cast(null as Trace)

}
