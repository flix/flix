/*
 * Copyright 2022 Paul Butcher
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

mod Concurrent {
  ///
  /// A wrapper around a Java CyclicBarrier
  ///
  @internal
  pub enum CyclicBarrier(##java.util.concurrent.CyclicBarrier)
}

mod Concurrent.CyclicBarrier {

    // TODO NS-REFACTOR can remove these
    use Concurrent.CyclicBarrier
    use Concurrent.CyclicBarrier.CyclicBarrier

  ///
  /// Create a new CyclicBarrier which will wait for `parties` threads
  ///
  @internal
  pub def newCyclicBarrier(parties: Int32): CyclicBarrier \ IO =
    import new java.util.concurrent.CyclicBarrier(Int32): ##java.util.concurrent.CyclicBarrier \ IO as newBarrier;
    CyclicBarrier(newBarrier(parties))

  ///
  /// Waits until all parties have invoked `await` on this barrier
  ///
  @internal
  pub def awaitBarrier(barrier: CyclicBarrier): Int32 \ IO =
    import java.util.concurrent.CyclicBarrier.await(): Int32 \ IO;
    let CyclicBarrier(b) = barrier;
    await(b)
}
