/*
 *  Copyright 2021 Magnus Madsen
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
namespace Unsafe {

    ///
    /// Converts `x` to a string and unsafely prints it to standard out.
    ///
    pub def print[a has ToString](x: a): a =
        let _ = (x |> ToString.toString |> Console.print) as & Pure;
        x

    ///
    /// Converts `x` to a string and unsafely prints it to standard out followed by a new line.
    ///
    pub def println[a has ToString](x: a): a =
        let _ = (x |> ToString.toString |> Console.printLine) as & Pure;
        x

}
