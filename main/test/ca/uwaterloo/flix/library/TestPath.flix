/*
 * Copyright 2019 Magnus Madsen
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

@test
def new01(): Path = Path.new("hello.txt")

@test
def new02(): Path = Path.new("hello/world.txt")

@test
def new03(): Path = Path.new("/tmp/hello.txt")

@test
def exists01(): Bool = Path.exists(Path.new("hello.txt")) |> Result.isOk

@test
def exists02(): Bool = Path.exists(Path.new("./doc/")) |> Result.isOk

@test
def exists03(): Bool = Path.exists(Path.new("./doc/foo")) |> Result.isOk

@test
def isDirectory01(): Bool = Path.isDirectory(Path.new("hello.txt")) |> Result.isOk

@test
def isDirectory02(): Bool = Path.isDirectory(Path.new("./doc/")) |> Result.isOk

@test
def isDirectory03(): Bool = Path.isDirectory(Path.new("./doc/foo")) |> Result.isOk

@test
def isExecutable01(): Bool = Path.isDirectory(Path.new("hello.txt")) |> Result.isOk

@test
def isExecutable02(): Bool = Path.isDirectory(Path.new("./doc/")) |> Result.isOk

@test
def isExecutable03(): Bool = Path.isDirectory(Path.new("./doc/foo")) |> Result.isOk

@test
def testReadLines01(): Bool = Path.readLines(Path.new("README.md")) |> Result.isOk
