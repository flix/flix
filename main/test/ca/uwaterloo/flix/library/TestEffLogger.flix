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


    @Test
    pub def orderGreater01(): Bool =
        Eff.Logger.Severity.Fatal > Eff.Logger.Severity.Trace

    @Test
    pub def orderGreater02(): Bool =
        Eff.Logger.Severity.Info > Eff.Logger.Severity.Debug


    pub def sampleInfoWarn(): Int32 \ Logger = {
        Eff.Logger.info("A1");
        Eff.Logger.warn("A2");
        1000
    }

    @Test
    pub def toListHandler01(): Bool =
        let (ss, ans) = Eff.Logger.toListHandler(() -> sampleInfoWarn());
        ans == 1000 and ss == List#{"[Info] - A1", "[Warn] - A2"}

    @Test
    pub def ioHandlerWithSeverity01(): Bool \ IO =
        let ans = Eff.Logger.ioHandlerWithSeverity(Eff.Logger.Severity.Debug, () -> sampleInfoWarn());
        ans == 1000

    @Test
    pub def ioHandlerWithSeverity02(): Bool \ IO =
        let ans = Eff.Logger.ioHandlerWithSeverity(Eff.Logger.Severity.Warn, () -> sampleInfoWarn());
        ans == 1000

}
