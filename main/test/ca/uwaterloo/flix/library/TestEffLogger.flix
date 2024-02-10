/*
 *  Copyright 2024 Stephen Tetley
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
mod TestEffLogger {

    use Eff.Logger

    pub def infoExample(): Int32 \ Logger = {
        Eff.Logger.info("A1");
        Eff.Logger.warn("A2");
        1000
    }

    @Test
    pub def testTEMP01(): (List[String],Int32) =
        Eff.Logger.toListHandler(() -> infoExample())

    @Test
    pub def testTEMP02(): Int32 \ IO =
        Eff.Logger.ioHandlerWithSeverity(Eff.Logger.Severity.Debug, () -> infoExample())

}
