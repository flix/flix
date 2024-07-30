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
    import java.util.concurrent.{CyclicBarrier => JCyclicBarrier}
  ///
  /// A wrapper around a Java CyclicBarrier
  ///
  @Internal
  pub enum CyclicBarrier(JCyclicBarrier)
}

mod Concurrent.CyclicBarrier {
    // TODO NS-REFACTOR can remove these
    use Concurrent.CyclicBarrier
    use Concurrent.CyclicBarrier.CyclicBarrier
    import java.util.concurrent.{CyclicBarrier => JCyclicBarrier}

  ///
  /// Create a new CyclicBarrier which will wait for `parties` threads
  ///
  @Internal
  pub def newCyclicBarrier(parties: Int32): CyclicBarrier \ IO =
    CyclicBarrier(new JCyclicBarrier(parties))

  ///
  /// Waits until all parties have invoked `await` on this barrier
  ///
  @Internal
  pub def awaitBarrier(barrier: CyclicBarrier): Int32 \ IO =
    let CyclicBarrier(b) = barrier;
    b.await()
}
