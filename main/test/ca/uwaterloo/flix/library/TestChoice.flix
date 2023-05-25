/*
 * Copyright 2020 Magnus Madsen
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
mod TestChoice {

    use Choice.{assertAbsent, assertPresent, isAbsent, isPresent, unbox, getWithDefault,
                withDefault, filter, flatten, map, flatMap, invert}

    def assertAbsent01(): Bool = assertAbsent(Absent)

    def assertPresent01(): Bool = assertPresent(Present(123))

    def isAbsent01(): Bool = isAbsent(Absent)

    def isAbsent02(): Bool = not isAbsent(Present(123))

    def isPresent01(): Bool = isPresent(Present(123))

    def isPresent02(): Bool = not isPresent(Absent)

    def unbox01(): Bool = unbox(Present(123)) == 123

    def unbox02(): Bool = unbox(Present("Hello World")) == "Hello World"

    def getWithDefault01(): Bool = getWithDefault(42, Absent) == 42

    def getWithDefault02(): Bool = getWithDefault(42, Present(21)) == 21

    def withDefault01(): Bool = unbox(withDefault(default = Present(456), Present(123))) == 123

    def withDefault02(): Bool = unbox(withDefault(default = Present(456), Absent)) == 456

    def withDefault03(): Bool = unbox(withDefault(default = Present(456), Present(123))) == 123

    def filter01(): Bool = assertAbsent(filter(_x -> true, Absent))

    def filter02(): Bool = getWithDefault(42, filter(_ -> true, Present(21))) == 21

    def filter03(): Bool = getWithDefault(42, filter(_ -> false, Present(21))) == 42

    def filter04(): Bool = getWithDefault(42, filter(_ -> true, if (true) Absent else Present(123))) == 42

    def filter05(): Bool = getWithDefault(42, filter(_ -> true, if (true) Present(123) else Absent)) == 123

    def flatten01(): Bool = assertAbsent(flatten(Absent))

    def flatten02(): Bool = assertAbsent(flatten(Present(Absent)))

    def flatten03(): Bool = unbox(flatten(Present(Present(42)))) == 42

    def flatten04(): Bool = getWithDefault(42, if (true) Absent else Present(123)) == 42

    def flatten05(): Bool = getWithDefault(42, if (true) Present(123) else Absent) == 123

    def map01(): Bool = isAbsent(map(x -> x, Absent))

    def map02(): Bool = isPresent(map(x -> x, Present(123)))

    def map03(): Bool = unbox(map(x -> x * 2, Present(42))) == 84

    def flatMap01(): Bool = assertAbsent(flatMap(_x -> Absent, Absent))

    def flatMap02(): Bool = assertAbsent(flatMap(_x -> Absent, Present(42)))

    def flatMap03(): Bool = assertAbsent(flatMap(_x -> Present(42), Absent))

    def flatMap04(): Bool = unbox(flatMap(x -> Present(x + 21), Present(21))) == 42

    def flatMap05(): Bool = getWithDefault(42, flatMap(x -> if (true) Absent else Present(x), Present(21))) == 42

    def flatMap06(): Bool = getWithDefault(42, flatMap(x -> if (true) Present(x) else Absent, Present(21))) == 21

    def flatMap07(): Bool = getWithDefault(42, flatMap(x -> Present(x), if (true) Absent else Present(21))) == 42

    def flatMap08(): Bool = getWithDefault(42, flatMap(x -> Present(x), if (true) Present(21) else Absent)) == 21

    def invert01(): Bool = unbox(invert(Absent, 42)) == 42

    def invert02(): Bool = isAbsent(invert(Present(42), 84))

    def invert03(): Bool = isAbsent(invert(invert(Absent, 42), 84))

    def invert04(): Bool = isPresent(invert(invert(invert(Absent, 21), 42), 84))

}
