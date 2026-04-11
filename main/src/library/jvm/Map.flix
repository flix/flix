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

import java.util.{Map => JMap}

///
/// JVM-only extensions for `Map`.
///

instance ToJava[Map[k, v]] with Order[k] {
    type Out = JMap
    type Aef = IO
    pub def toJava(m: Map[k, v]): JMap \ IO = Adaptor.toMap(m)
}

instance ToFlix[Map[k, v]] with Order[k] {
    type In = JMap
    pub def toFlix(m: JMap): Map[k, v] = Adaptor.fromMap(m)
}
