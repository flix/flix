/*
 * Copyright 2024 Magnus Madsen
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
mod Parallel {

    @Experimental
    pub def race(f: a -> b \ ef, l: Nel[a]): b \ {ef, NonDet} with Sendable[b] = try {
            region rc {
                let (tx, rx) = Channel.buffered(rc, Nel.length(l));
                foreach (x <- l) {
                    spawn (Channel.send(f(x), tx)) @ rc
                };
                Channel.recv(rx)
            }
        } with Blocking {}

}
