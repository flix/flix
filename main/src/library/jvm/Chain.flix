/*
 * Copyright 2021 Stephen Tetley
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

import java.util.{Iterator => JIterator}
import java.util.{List => JList}

///
/// JVM-only extensions for `Chain`.
///

instance ToJava[Chain[a]] {
    type Out = JList
    type Aef = IO
    pub def toJava(c: Chain[a]): JList \ IO = Adaptor.toList(c)
}

instance ToFlix[Chain[a]] {
    type In = JList
    pub def toFlix(l: JList): Chain[a] = region rc {
        let it = unsafe IO { l.iterator() };
        it |> Adaptor.fromIterator(rc, (Proxy.Proxy: Proxy[a])) |> Iterator.toChain
    }
}
