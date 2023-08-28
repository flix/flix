/*
 * Copyright 2023 Stephen Tetley
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

mod TestFile {

    use File.Mode.{ReadOnly, ReadWrite}
    use IOError.Generic

    def withReadOnlyFile(path: String, fn: File -> Result[IOError, a] \IO): Result[IOError, a] \ IO =
        match File.open(path, ReadOnly) {
            case Ok(f)    => match fn(f) {
                case Ok(ans)  => Functor.replace(File.close!(f), ans)
                case Err(err) => Err(err)
            }
            case Err(err) => Err(err)
    }


    /////////////////////////////////////////////////////////////////////////////
    // open / close!                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def openClose01(): Bool \ IO =
        match File.open("README.md", ReadOnly) {
            case Ok(f) => match File.close!(f) {
                case Ok() => true
                case _    => false
            }
            case _     => false
        }

    @test
    def openClose02(): Bool \ IO =
        match File.open("./main/test/ca/uwaterloo/flix/library/testdata/ABCD.bin", ReadOnly) {
            case Ok(f) => match File.close!(f) {
                case Ok() => true
                case _    => false
            }
            case _     => false
        }

    /////////////////////////////////////////////////////////////////////////////
    // length                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def length01(): Bool \ IO =
        withReadOnlyFile("README.md",
                    f -> Result.flatMap(len -> if (len > 1i64) Ok() else Err(Generic("empty length")), File.length(f)))
            |> Result.isOk

    /////////////////////////////////////////////////////////////////////////////
    // readChar                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def readChar01(): Bool \ IO =
        withReadOnlyFile("./main/test/ca/uwaterloo/flix/library/testdata/ABCD.bin",
                    f -> Result.flatMap(ch -> if (ch == 'A') Ok() else Err(Generic("not char A")), File.readChar!(f)))
            |> Result.isOk

}
