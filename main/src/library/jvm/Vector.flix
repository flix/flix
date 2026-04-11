/*
 *  Copyright 2023 Magnus Madsen, Stephen Tetley
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

import java.util.{List => JList}
import java.util.{Iterator => JIterator}

///
/// JVM-only extensions for `Vector`.
///

instance ToJava[Vector[a]] {
    type Out = JList
    type Aef = IO
    pub def toJava(v: Vector[a]): JList \ IO = Adaptor.toList(v)
}

instance ToFlix[Vector[a]] {
    type In = JList
    pub def toFlix(l: JList): Vector[a] = region rc {
        let iter = unsafe IO { l.iterator() };
        iter |> Adaptor.fromIterator(rc, (Proxy.Proxy: Proxy[a])) |> Iterator.toVector
    }
}
