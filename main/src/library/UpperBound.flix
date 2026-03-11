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
/// A trait for partially ordered types that have an upper bound.
///
pub trait UpperBound[a] {
    ///
    /// Returns the largest value of `a`.
    ///
    pub def maxValue(): a
}

instance UpperBound[(a1, a2)] with UpperBound[a1], UpperBound[a2] {
    pub def maxValue(): (a1, a2) = (UpperBound.maxValue(), UpperBound.maxValue())
}

instance UpperBound[(a1, a2, a3)] with UpperBound[a1], UpperBound[a2], UpperBound[a3] {
    pub def maxValue(): (a1, a2, a3) = (UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue())
}

instance UpperBound[(a1, a2, a3, a4)] with UpperBound[a1], UpperBound[a2], UpperBound[a3], UpperBound[a4] {
    pub def maxValue(): (a1, a2, a3, a4) = (UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue())
}

instance UpperBound[(a1, a2, a3, a4, a5)] with UpperBound[a1], UpperBound[a2], UpperBound[a3], UpperBound[a4], UpperBound[a5] {
    pub def maxValue(): (a1, a2, a3, a4, a5) = (UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue())
}

instance UpperBound[(a1, a2, a3, a4, a5, a6)] with UpperBound[a1], UpperBound[a2], UpperBound[a3], UpperBound[a4], UpperBound[a5], UpperBound[a6] {
    pub def maxValue(): (a1, a2, a3, a4, a5, a6) = (UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue())
}

instance UpperBound[(a1, a2, a3, a4, a5, a6, a7)] with UpperBound[a1], UpperBound[a2], UpperBound[a3], UpperBound[a4], UpperBound[a5], UpperBound[a6], UpperBound[a7] {
    pub def maxValue(): (a1, a2, a3, a4, a5, a6, a7) = (UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue())
}

instance UpperBound[(a1, a2, a3, a4, a5, a6, a7, a8)] with UpperBound[a1], UpperBound[a2], UpperBound[a3], UpperBound[a4], UpperBound[a5], UpperBound[a6], UpperBound[a7], UpperBound[a8] {
    pub def maxValue(): (a1, a2, a3, a4, a5, a6, a7, a8) = (UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue())
}

instance UpperBound[(a1, a2, a3, a4, a5, a6, a7, a8, a9)] with UpperBound[a1], UpperBound[a2], UpperBound[a3], UpperBound[a4], UpperBound[a5], UpperBound[a6], UpperBound[a7], UpperBound[a8], UpperBound[a9] {
    pub def maxValue(): (a1, a2, a3, a4, a5, a6, a7, a8, a9) = (UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue())
}

instance UpperBound[(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)] with UpperBound[a1], UpperBound[a2], UpperBound[a3], UpperBound[a4], UpperBound[a5], UpperBound[a6], UpperBound[a7], UpperBound[a8], UpperBound[a9], UpperBound[a10] {
    pub def maxValue(): (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = (UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue(), UpperBound.maxValue())
}

instance UpperBound[Unit] {
    pub def maxValue(): Unit = ()
}
