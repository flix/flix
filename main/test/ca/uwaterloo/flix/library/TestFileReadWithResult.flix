/*
 * Copyright 2024 Holger Dal Mogensen
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

mod TestFileReadWithResultWithResult {

    use IoError.IoError
    use IoError.ErrorKind
    use Monad.{>>=}

    import java.io.IOException
    import java.nio.file.Path


    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def exists01(): Bool \ {Sys, IO} = {
        Result.flatten(
            FileWrite.handle(() -> {
                let dir = FileWrite.mkTempDir("flix");
                let path = "${dir}/flix-tmpfile-68975";
                FileWrite.write({ str = "content" }, path);
                FileReadWithResult.handle(() -> FileReadWithResult.exists(path))()
            })()
        ) == Ok(true)
    }

    @Test
    def exists02(): Bool \ IO = {
        FileReadWithResult.handle(() -> FileReadWithResult.exists("SomeFileThatDoesNotExist"))() == Ok(false)
    }

    @Test
    def exists03(): Bool \ IO = {
        match FileReadWithResult.handle(() -> FileReadWithResult.exists("Some\\Invalid:{\u0000Path}/"))() {
            case Ok(_) => false
            case Err(IoError(kind, _)) => kind == ErrorKind.InvalidPath
        }
    }

}
