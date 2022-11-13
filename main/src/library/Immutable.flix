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
/// A type class for types that are immutable.
///
pub class Immutable[a]

instance Immutable[Unit]

instance Immutable[Bool]

instance Immutable[Char]

instance Immutable[Float32]

instance Immutable[Float64]

instance Immutable[BigDecimal]

instance Immutable[Int8]

instance Immutable[Int16]

instance Immutable[Int32]

instance Immutable[Int64]

instance Immutable[BigInt]

instance Immutable[String]

instance Immutable[(a1, a2)] with Immutable[a1], Immutable[a2]

instance Immutable[(a1, a2, a3)] with Immutable[a1], Immutable[a2], Immutable[a3]

instance Immutable[(a1, a2, a3, a4)] with Immutable[a1], Immutable[a2], Immutable[a3], Immutable[a4]

instance Immutable[(a1, a2, a3, a4, a5)] with Immutable[a1], Immutable[a2], Immutable[a3], Immutable[a4], Immutable[a5]

instance Immutable[(a1, a2, a3, a4, a5, a6)] with Immutable[a1], Immutable[a2], Immutable[a3], Immutable[a4], Immutable[a5], Immutable[a6]

instance Immutable[(a1, a2, a3, a4, a5, a6, a7)] with Immutable[a1], Immutable[a2], Immutable[a3], Immutable[a4], Immutable[a5], Immutable[a6], Immutable[a7]

instance Immutable[(a1, a2, a3, a4, a5, a6, a7, a8)] with Immutable[a1], Immutable[a2], Immutable[a3], Immutable[a4], Immutable[a5], Immutable[a6], Immutable[a7], Immutable[a8]

instance Immutable[(a1, a2, a3, a4, a5, a6, a7, a8, a9)] with Immutable[a1], Immutable[a2], Immutable[a3], Immutable[a4], Immutable[a5], Immutable[a6], Immutable[a7], Immutable[a8], Immutable[a9]

instance Immutable[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] with Immutable[a1], Immutable[a2], Immutable[a3], Immutable[a4], Immutable[a5], Immutable[a6], Immutable[a7], Immutable[a8], Immutable[a9], Immutable[a10]

instance Immutable[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)] with Immutable[a1], Immutable[a2], Immutable[a3], Immutable[a4], Immutable[a5], Immutable[a6], Immutable[a7], Immutable[a8], Immutable[a9], Immutable[a10], Immutable[a11]

instance Immutable[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)] with Immutable[a1], Immutable[a2], Immutable[a3], Immutable[a4], Immutable[a5], Immutable[a6], Immutable[a7], Immutable[a8], Immutable[a9], Immutable[a10], Immutable[a11], Immutable[a12]

instance Immutable[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)] with Immutable[a1], Immutable[a2], Immutable[a3], Immutable[a4], Immutable[a5], Immutable[a6], Immutable[a7], Immutable[a8], Immutable[a9], Immutable[a10], Immutable[a11], Immutable[a12], Immutable[a13]

instance Immutable[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)] with Immutable[a1], Immutable[a2], Immutable[a3], Immutable[a4], Immutable[a5], Immutable[a6], Immutable[a7], Immutable[a8], Immutable[a9], Immutable[a10], Immutable[a11], Immutable[a12], Immutable[a13], Immutable[a14]

instance Immutable[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)] with Immutable[a1], Immutable[a2], Immutable[a3], Immutable[a4], Immutable[a5], Immutable[a6], Immutable[a7], Immutable[a8], Immutable[a9], Immutable[a10], Immutable[a11], Immutable[a12], Immutable[a13], Immutable[a14], Immutable[a15]
