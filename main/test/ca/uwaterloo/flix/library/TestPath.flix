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
def new01(): Path & Impure = Path.new("hello.txt")

@test
def new02(): Path & Impure = Path.new("hello/world.txt")

@test
def new03(): Path & Impure = Path.new("/tmp/hello.txt")

@test
def exists01(): Bool & Impure = Path.exists(Path.new("hello.txt")) |> Result.isOk

@test
def exists02(): Bool & Impure = Path.exists(Path.new("./doc/")) |> Result.isOk

@test
def exists03(): Bool & Impure = Path.exists(Path.new("./doc/foo")) |> Result.isOk

@test
def getName01(): Bool & Impure = Path.getName(Path.new("hello.txt")) == Ok("hello.txt")

@test
def getName02(): Bool & Impure = Path.getName(Path.new("./doc/")) == Ok("doc")

@test
def getName03(): Bool & Impure = Path.getName(Path.new("./doc/foo")) == Ok("foo")

@test
def getParent01(): Bool & Impure = Path.getParent(Path.new("hello.txt")) |> Result.isOk

@test
def getParent02(): Bool & Impure = Path.getParent(Path.new("./doc/")) |> Result.isOk

@test
def getParent03(): Bool & Impure = Path.getParent(Path.new("./doc/foo")) |> Result.isOk

@test
def getChildren01(): Bool & Impure = Path.getChildren(Path.new("hello.txt")) |> Result.isOk

@test
def getChildren02(): Bool & Impure = Path.getChildren(Path.new("./doc/")) |> Result.isOk

@test
def getChildren03(): Bool & Impure = Path.getChildren(Path.new("./doc/foo")) |> Result.isOk

@test
def isDirectory01(): Bool & Impure = Path.isDirectory(Path.new("hello.txt")) |> Result.isOk

@test
def isDirectory02(): Bool & Impure = Path.isDirectory(Path.new("./doc/")) |> Result.isOk

@test
def isDirectory03(): Bool & Impure = Path.isDirectory(Path.new("./doc/foo")) |> Result.isOk

@test
def isExecutable01(): Bool & Impure = Path.isDirectory(Path.new("hello.txt")) |> Result.isOk

@test
def isExecutable02(): Bool & Impure = Path.isDirectory(Path.new("./doc/")) |> Result.isOk

@test
def isExecutable03(): Bool & Impure = Path.isDirectory(Path.new("./doc/foo")) |> Result.isOk

@test
def isReadable01(): Bool & Impure = Path.isReadable(Path.new("hello.txt")) |> Result.isOk

@test
def isReadable02(): Bool & Impure = Path.isReadable(Path.new("./doc/")) |> Result.isOk

@test
def isReadable03(): Bool & Impure = Path.isReadable(Path.new("./doc/foo")) |> Result.isOk

@test
def isFile01(): Bool & Impure = Path.isFile(Path.new("hello.txt")) |> Result.isOk

@test
def isFile02(): Bool & Impure = Path.isFile(Path.new("./doc/")) |> Result.isOk

@test
def isFile03(): Bool & Impure = Path.isFile(Path.new("./doc/foo")) |> Result.isOk

@test
def isSymbolicLink01(): Bool & Impure = Path.isSymbolicLink(Path.new("hello.txt")) |> Result.isOk

@test
def isSymbolicLink02(): Bool & Impure = Path.isSymbolicLink(Path.new("./doc/")) |> Result.isOk

@test
def isSymbolicLink03(): Bool & Impure = Path.isSymbolicLink(Path.new("./doc/foo")) |> Result.isOk

@test
def isWritable01(): Bool & Impure = Path.isWritable(Path.new("hello.txt")) |> Result.isOk

@test
def isWritable02(): Bool & Impure = Path.isWritable(Path.new("./doc/")) |> Result.isOk

@test
def isWritable03(): Bool & Impure = Path.isWritable(Path.new("./doc/foo")) |> Result.isOk

@test
def testReadLines01(): Bool & Impure = Path.readLines(Path.new("README.md")) |> Result.isOk
