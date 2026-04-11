/*
 * Copyright 2017 Liam Palmer, Esben Bjerre
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

import java.util.{Set => JSet}

///
/// JVM-only extensions for `Set`.
///

instance ToJava[Set[a]] with Order[a] {
    type Out = JSet
    type Aef = IO
    pub def toJava(s: Set[a]): JSet \ IO = Adaptor.toSet(s)
}

instance ToFlix[Set[a]] with Order[a] {
    type In = JSet
    pub def toFlix(s: JSet): Set[a] = Adaptor.fromSet(s)
}
