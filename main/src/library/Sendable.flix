/*
 * Copyright 2022 Paul Butcher
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
/// A trait for types that can be passed to `Channel.send`.
///
pub trait Sendable[a]

instance Sendable[Unit]

instance Sendable[Bool]

instance Sendable[Char]

instance Sendable[Float32]

instance Sendable[Float64]

instance Sendable[BigDecimal]

instance Sendable[Int8]

instance Sendable[Int16]

instance Sendable[Int32]

instance Sendable[Int64]

instance Sendable[BigInt]

instance Sendable[String]

instance Sendable[(a1, a2)] with Sendable[a1], Sendable[a2]

instance Sendable[(a1, a2, a3)] with Sendable[a1], Sendable[a2], Sendable[a3]

instance Sendable[(a1, a2, a3, a4)] with Sendable[a1], Sendable[a2], Sendable[a3], Sendable[a4]

instance Sendable[(a1, a2, a3, a4, a5)] with Sendable[a1], Sendable[a2], Sendable[a3], Sendable[a4], Sendable[a5]

instance Sendable[(a1, a2, a3, a4, a5, a6)] with Sendable[a1], Sendable[a2], Sendable[a3], Sendable[a4], Sendable[a5], Sendable[a6]

instance Sendable[(a1, a2, a3, a4, a5, a6, a7)] with Sendable[a1], Sendable[a2], Sendable[a3], Sendable[a4], Sendable[a5], Sendable[a6], Sendable[a7]

instance Sendable[(a1, a2, a3, a4, a5, a6, a7, a8)] with Sendable[a1], Sendable[a2], Sendable[a3], Sendable[a4], Sendable[a5], Sendable[a6], Sendable[a7], Sendable[a8]

instance Sendable[(a1, a2, a3, a4, a5, a6, a7, a8, a9)] with Sendable[a1], Sendable[a2], Sendable[a3], Sendable[a4], Sendable[a5], Sendable[a6], Sendable[a7], Sendable[a8], Sendable[a9]

instance Sendable[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] with Sendable[a1], Sendable[a2], Sendable[a3], Sendable[a4], Sendable[a5], Sendable[a6], Sendable[a7], Sendable[a8], Sendable[a9], Sendable[a10]

instance Sendable[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)] with Sendable[a1], Sendable[a2], Sendable[a3], Sendable[a4], Sendable[a5], Sendable[a6], Sendable[a7], Sendable[a8], Sendable[a9], Sendable[a10], Sendable[a11]

instance Sendable[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)] with Sendable[a1], Sendable[a2], Sendable[a3], Sendable[a4], Sendable[a5], Sendable[a6], Sendable[a7], Sendable[a8], Sendable[a9], Sendable[a10], Sendable[a11], Sendable[a12]

instance Sendable[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)] with Sendable[a1], Sendable[a2], Sendable[a3], Sendable[a4], Sendable[a5], Sendable[a6], Sendable[a7], Sendable[a8], Sendable[a9], Sendable[a10], Sendable[a11], Sendable[a12], Sendable[a13]

instance Sendable[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)] with Sendable[a1], Sendable[a2], Sendable[a3], Sendable[a4], Sendable[a5], Sendable[a6], Sendable[a7], Sendable[a8], Sendable[a9], Sendable[a10], Sendable[a11], Sendable[a12], Sendable[a13], Sendable[a14]

instance Sendable[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)] with Sendable[a1], Sendable[a2], Sendable[a3], Sendable[a4], Sendable[a5], Sendable[a6], Sendable[a7], Sendable[a8], Sendable[a9], Sendable[a10], Sendable[a11], Sendable[a12], Sendable[a13], Sendable[a14], Sendable[a15]
