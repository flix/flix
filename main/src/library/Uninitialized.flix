/*
 *  Copyright 2021 Esben Bjerre
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

///
/// Types with a default value.
///
pub class Uninitialized[a] {
    ///
    /// Returns a default value for the type `a`.
    ///
    pub def uninitialized(): a
}

instance Uninitialized[Unit] {
    pub def uninitialized(): Unit = ()
}

instance Uninitialized[Bool] {
    pub def uninitialized(): Bool = false
}

instance Uninitialized[Char] {
    pub def uninitialized(): Char = '\u0000'
}

instance Uninitialized[Float32] {
    pub def uninitialized(): Float32 = 0.0f32
}

instance Uninitialized[Float64] {
    pub def uninitialized(): Float64 = 0.0f64
}

instance Uninitialized[Int8] {
    pub def uninitialized(): Int8 = 0i8
}

instance Uninitialized[Int16] {
    pub def uninitialized(): Int16 = 0i16
}

instance Uninitialized[Int32] {
    pub def uninitialized(): Int32 = 0
}

instance Uninitialized[Int64] {
    pub def uninitialized(): Int64 = 0i64
}

instance Uninitialized[BigInt] {
    pub def uninitialized(): BigInt = null as BigInt
}

instance Uninitialized[String] {
    pub def uninitialized(): String = null as String
}

instance Uninitialized[(a1, a2)] with [a1 : Uninitialized, a2 : Uninitialized] {
    def uninitialized(): (a1, a2) = null as (a1, a2)
}

instance Uninitialized[(a1, a2, a3)] with [a1 : Uninitialized, a2 : Uninitialized, a3 : Uninitialized] {
    def uninitialized(): (a1, a2, a3) = null as (a1, a2, a3)
}

instance Uninitialized[(a1, a2, a3, a4)] with [a1 : Uninitialized, a2 : Uninitialized, a3 : Uninitialized, a4 : Uninitialized] {
    def uninitialized(): (a1, a2, a3, a4) = null as (a1, a2, a3, a4)
}

instance Uninitialized[(a1, a2, a3, a4, a5)] with [a1 : Uninitialized, a2 : Uninitialized, a3 : Uninitialized, a4 : Uninitialized, a5 : Uninitialized] {
    def uninitialized(): (a1, a2, a3, a4, a5) = null as (a1, a2, a3, a4, a5)
}

instance Uninitialized[(a1, a2, a3, a4, a5, a6)] with [a1 : Uninitialized, a2 : Uninitialized, a3 : Uninitialized, a4 : Uninitialized, a5 : Uninitialized, a6 : Uninitialized] {
    def uninitialized(): (a1, a2, a3, a4, a5, a6) = null as (a1, a2, a3, a4, a5, a6)
}

instance Uninitialized[(a1, a2, a3, a4, a5, a6, a7)] with [a1 : Uninitialized, a2 : Uninitialized, a3 : Uninitialized, a4 : Uninitialized, a5 : Uninitialized, a6 : Uninitialized, a7 : Uninitialized] {
    def uninitialized(): (a1, a2, a3, a4, a5, a6, a7) = null as (a1, a2, a3, a4, a5, a6, a7)
}

instance Uninitialized[(a1, a2, a3, a4, a5, a6, a7, a8)] with [a1 : Uninitialized, a2 : Uninitialized, a3 : Uninitialized, a4 : Uninitialized, a5 : Uninitialized, a6 : Uninitialized, a7 : Uninitialized, a8 : Uninitialized] {
    def uninitialized(): (a1, a2, a3, a4, a5, a6, a7, a8) = null as (a1, a2, a3, a4, a5, a6, a7, a8)
}

instance Uninitialized[(a1, a2, a3, a4, a5, a6, a7, a8, a9)] with [a1 : Uninitialized, a2 : Uninitialized, a3 : Uninitialized, a4 : Uninitialized, a5 : Uninitialized, a6 : Uninitialized, a7 : Uninitialized, a8 : Uninitialized, a9 : Uninitialized] {
    def uninitialized(): (a1, a2, a3, a4, a5, a6, a7, a8, a9) = null as (a1, a2, a3, a4, a5, a6, a7, a8, a9)
}

instance Uninitialized[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] with [a1 : Uninitialized, a2 : Uninitialized, a3 : Uninitialized, a4 : Uninitialized, a5 : Uninitialized, a6 : Uninitialized, a7 : Uninitialized, a8 : Uninitialized, a9 : Uninitialized, a10 : Uninitialized] {
    def uninitialized(): (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = null as (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
}