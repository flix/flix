/*
 *  Copyright 2023 Jakob Schneider Villumsen
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

mod Ref {

    pub def new(rh: Region[r], x: a): Ref[a, r] \ Write(r) =
        ref x @ rh

    pub def get(rf: Ref[a, r]): a \ Read(r) =
        deref rf

    pub def put(x: a, rf: Ref[a, r]): Unit \ Write(r) =
        rf := x

    pub def transform(f: a -> a \ ef, rf: Ref[a, r]): Unit \ { Read(r), Write(r), ef } =
        rf := f(deref rf)

}
