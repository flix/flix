/*
 *  Copyright 2020 Magnus Madsen
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
/// A trait for partially ordered types that have a lower bound.
///
pub trait LowerBound[a] {
    ///
    /// Returns the smallest value of `a`.
    ///
    pub def minValue(): a
}

instance LowerBound[(a1, a2)] with LowerBound[a1], LowerBound[a2] {
    pub def minValue(): (a1, a2) = (LowerBound.minValue(), LowerBound.minValue())
}

instance LowerBound[(a1, a2, a3)] with LowerBound[a1], LowerBound[a2], LowerBound[a3] {
    pub def minValue(): (a1, a2, a3) = (LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue())
}

instance LowerBound[(a1, a2, a3, a4)] with LowerBound[a1], LowerBound[a2], LowerBound[a3], LowerBound[a4] {
    pub def minValue(): (a1, a2, a3, a4) = (LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue())
}

instance LowerBound[(a1, a2, a3, a4, a5)] with LowerBound[a1], LowerBound[a2], LowerBound[a3], LowerBound[a4], LowerBound[a5] {
    pub def minValue(): (a1, a2, a3, a4, a5) = (LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue())
}

instance LowerBound[(a1, a2, a3, a4, a5, a6)] with LowerBound[a1], LowerBound[a2], LowerBound[a3], LowerBound[a4], LowerBound[a5], LowerBound[a6] {
    pub def minValue(): (a1, a2, a3, a4, a5, a6) = (LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue())
}

instance LowerBound[(a1, a2, a3, a4, a5, a6, a7)] with LowerBound[a1], LowerBound[a2], LowerBound[a3], LowerBound[a4], LowerBound[a5], LowerBound[a6], LowerBound[a7] {
    pub def minValue(): (a1, a2, a3, a4, a5, a6, a7) = (LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue())
}

instance LowerBound[(a1, a2, a3, a4, a5, a6, a7, a8)] with LowerBound[a1], LowerBound[a2], LowerBound[a3], LowerBound[a4], LowerBound[a5], LowerBound[a6], LowerBound[a7], LowerBound[a8] {
    pub def minValue(): (a1, a2, a3, a4, a5, a6, a7, a8) = (LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue())
}

instance LowerBound[(a1, a2, a3, a4, a5, a6, a7, a8, a9)] with LowerBound[a1], LowerBound[a2], LowerBound[a3], LowerBound[a4], LowerBound[a5], LowerBound[a6], LowerBound[a7], LowerBound[a8], LowerBound[a9] {
    pub def minValue(): (a1, a2, a3, a4, a5, a6, a7, a8, a9) = (LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue())
}

instance LowerBound[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] with LowerBound[a1], LowerBound[a2], LowerBound[a3], LowerBound[a4], LowerBound[a5], LowerBound[a6], LowerBound[a7], LowerBound[a8], LowerBound[a9], LowerBound[a10] {
    pub def minValue(): (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = (LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue(), LowerBound.minValue())
}

instance LowerBound[Unit] {
    pub def minValue(): Unit = ()
}






