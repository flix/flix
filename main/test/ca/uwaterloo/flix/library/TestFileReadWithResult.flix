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

mod TestFileReadWithResult {

    use IoError.IoError
    use IoError.ErrorKind


    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def exists01(): Bool \ IO =
      FileReadWithResult.handle(() -> FileReadWithResult.exists("./LICENSE.md"))() == Ok(true)

    @Test
    def exists02(): Bool \ IO =
      FileReadWithResult.handle(() -> FileReadWithResult.exists("./SomeFileThatDoesNotExist.txt"))() == Ok(false)

    @Test
    def exists03(): Bool \ IO =
      match FileReadWithResult.handle(() -> FileReadWithResult.exists("Some\\Invalid:{Path}/"))() {
        case Ok(_) => false
        case Err(IoError(kind, _)) => kind == ErrorKind.InvalidPath
      }

}
