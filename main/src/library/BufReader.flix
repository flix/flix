/*
 *  Copyright 2025 Ry Wiese
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

///
/// A `Readable` implementation that wraps an underlying `Readable` instance
/// and stores the items read from the underlying instance in an internal buffer.
///
/// A call to `read` will read directly from the buffer if items are available, and
/// will trigger a `read` from the underlying `reader` when the buffer is empty.
///
/// This has the effect of reducing the overall number of `read`s to the underlying
/// `Readable`.
///
/// A call to `read` might read more than the requested number of items from the
/// underlying reader, in order to fill the entire buffer.
///
/// A call to `read` might not read from the underlying `reader` at all, if there are
/// enough items in the buffer.
///
/// `buffer`: the underlying buffer.
/// `cursor`: the index in `buffer` of the first element logically considered to be in the buffer.
/// `size`: the number of elements (starting at `cursor`) logically considered to be in the buffer.
/// `reader`: the underlying `Readable` being read from.
///
pub struct BufReader[a: Type, t: Type, r: Region] {
    buffer: Array[a, r],
    mut cursor: Int32,
    mut size: Int32,
    reader: t
}

instance Readable[BufReader[a, t, rr]] with Readable[t] where Readable.Elm[t] ~ a {

    type Elm = a

    type Aef = rr + Readable.Aef[t]

    pub def read(buffer: Array[a, rb], bufReader: BufReader[a, t, rr]): Result[IoError, Int32] \ rb + rr + Readable.Aef[t] =
        BufReader.read(buffer, bufReader)

}

instance Peekable[BufReader[a, t, r]] with Readable[t] where Readable.Elm[t] ~ a {

    pub def peek(bufReader: BufReader[a, t, r]): Result[IoError, Option[Readable.Elm[t]]] \ Readable.Aef[t] + r = BufReader.peek(bufReader)

    pub def skip(n: Int32, bufReader: BufReader[a, t, r]): Result[IoError, Int32] \ Readable.Aef[t] + r = BufReader.skip(n, bufReader)

    pub def readWhile(p: Readable.Elm[t] -> Bool, bufReader: BufReader[a, t, r]): Result[IoError, Vector[Readable.Elm[t]]] \ Readable.Aef[t] + r = BufReader.readWhile(p, bufReader)

}

pub mod BufReader {

    ///
    /// Constructs a `BufReader` wrapping `reader` with a default capacity of 4096 items.
    ///
    pub def withDefaultCapacity(rc: Region[r], reader: t): BufReader[a,t, r] \ r with Readable[t] =
        withCapacity(rc, defaultBufferCapacity(), reader)

    ///
    /// Constructs a `BufReader` wrapping `reader` with the specified `capacity` in items.
    ///
    /// If `capacity <= 0`, the `BufReader` will delegate directly to `reader` without buffering.
    ///
    pub def withCapacity(rc: Region[r], capacity: Int32, reader: t): BufReader[a, t, r] \ r with Readable[t] =
        new BufReader @ rc {
            buffer = Array.empty(rc, capacity),
            cursor = 0,
            size = 0,
            reader = reader
        }

    ///
    /// Reads `k` items from the underlying `Readable` into `dest`, storing items read into
    /// the internal buffer.
    ///
    /// Returns `Ok(k)` to signify that `k` items were successfully read and written to `dest`.
    ///
    /// Guarantees that `0 <= k <= length(dest)`.
    ///
    /// Returns `Err(e)` if some underlying error occurs.
    ///
    pub def read(dest: Array[Readable.Elm[t], rb], bufReader: BufReader[a, t, rr]): Result[IoError, Int32] \ rb + rr + Readable.Aef[t] with Readable[t] where Readable.Elm[t] ~ a =
        if (Array.size(bufReader->buffer) > 0) {
            readFromCursor(0, dest, bufReader)
        } else {
            Readable.read(dest, bufReader->reader)
        }

    ///
    /// Returns `Ok(Some(x))` where x is the next value to be read, or `Ok(None)` if the `bufReader` is empty.
    /// Does not advance the `bufReader`.
    ///
    /// Returns `Err(err)` if there is an IO error.
    ///
    pub def peek(bufReader: BufReader[a, t, r]): Result[IoError, Option[Readable.Elm[t]]] \ Readable.Aef[t] + r with Readable[t] where Readable.Elm[t] ~ a =
        if (bufReader->size != 0) {
            Ok(Some(Array.get(bufReader->cursor, bufReader->buffer)))
        } else {
            forM(
                itemsFilled <- fillBuffer(bufReader)
            ) yield {
                if (itemsFilled != 0) {
                    Some(Array.get(bufReader->cursor, bufReader->buffer))
                } else {
                    None
                }
            }
        }

    ///
    /// Advances the reader by `n`, skipping over the elements without reading them.
    ///
    /// Returns `Ok(k)` where `k` is the number of items skipped.
    ///
    /// Guarantees that `0 <= k <= max(n, 0)`. If `k < n`, then EOF has been reached.
    ///
    /// Returns `Err(err)` if there is an IO error.
    ///
    pub def skip(n: Int32, bufReader: BufReader[a, t, r]): Result[IoError, Int32] \ Readable.Aef[t] + r with Readable[t] where Readable.Elm[t] ~ a =
        if (n <= 0) {
            // Done
            Ok(0)
        } else if (bufReader->size != 0) {
            // Nonempty Buffer: process items
            let itemsSkipped = skipFromBuffer(n, bufReader);

            // recurse into Done or Empty Buffer branch
            forM(
                rest <- skip(n - itemsSkipped, bufReader)
            ) yield itemsSkipped + rest
        } else {
            // Empty Buffer: refill it
            forM(
                itemsFilled <- fillBuffer(bufReader);
                skipped <- if (itemsFilled == 0) {
                    // Done: Eof
                    Ok(0)
                } else {
                    // Nonempty Buffer: continue processing
                    skip(n, bufReader)
                }
            ) yield skipped
        }

    ///
    /// Advances the reader by `n` or the size of the buffer, whichever is smaller, skipping over the elements without reading them.
    ///
    /// If the buffer runs out, does NOT trigger a read to the underlying reader.
    ///
    /// Guarantees that `0 <= k <= min(n, size(buffer))`.
    ///
    /// Assumes `n > 0`.
    ///
    def skipFromBuffer(n: Int32, bufReader: BufReader[a, t, r]): Int32 \ r with Readable[t] where Readable.Elm[t] ~ a =
        let itemsToSkip = Int32.min(n, bufReader->size);
        bufReader->cursor = bufReader->cursor + itemsToSkip;
        bufReader->size = bufReader->size - itemsToSkip;
        itemsToSkip

    ///
    /// Reads from `reader` until an element is reached that does not satisfy predicate `p`.
    ///
    /// Returns `Ok(vec)` where `vec` is a vector of the first `k` elements where `k` is the index of the first element that does not satisfy `p`.
    /// This element is not included in the vector, and will be read on the next read.
    ///
    /// All elements of `vec` are guaranteed to satisfy `p`.
    ///
    /// Returns `Err(err)` if there is an IO error.
    ///
    pub def readWhile(p: a -> Bool, bufReader: BufReader[a, t, r]): Result[IoError, Vector[a]] \ Readable.Aef[t] + r with Readable[t] where Readable.Elm[t] ~ a =
        region rc {
            let mutList = MutList.empty(rc);
            forM(
                _ <- readWhileInto(rc, p, mutList, bufReader)
            ) yield mutList |> MutList.toVector
        }

    ///
    /// Like `readWhile`, but appends results into `results` and returns `Ok(())` on success.
    ///
    def readWhileInto(rc: Region[rl], p: a -> Bool, results: MutList[a, rl], bufReader: BufReader[a, t, rb]): Result[IoError, Unit] \ Readable.Aef[t] + rb + rl with Readable[t] where Readable.Elm[t] ~ a =
        if (bufReader->size != 0) {
            ///
            /// Finds the index of the first element in the buffer that does not match predicate `p`.
            /// If all elements in the buffer match, returns `i + 1` where `i = bufReader->cursor + bufReader->size`
            /// is the last element in the logical buffer.
            ///
            def findIndex(i) = {
                if (i < bufReader->cursor + bufReader->size) {
                    let x = Array.get(i, bufReader->buffer);
                    if (p(x)) findIndex(i + 1) else i
                } else {
                    i
                }
            };
            let i = findIndex(bufReader->cursor);
            if (i == bufReader->cursor) {
                Ok(())
            } else {
                let vec = Array.slice(rc, start = bufReader->cursor, end = i, bufReader->buffer) |> Array.toVector;
                MutList.append(vec, results);
                let itemsRead = Vector.length(vec);
                bufReader->cursor = bufReader->cursor + itemsRead;
                bufReader->size = bufReader->size - itemsRead;
                readWhileInto(rc, p, results, bufReader)
            }
        } else {
            forM(
                itemsFilled <- fillBuffer(bufReader);
                unit <- if (itemsFilled == 0) Ok(()) else readWhileInto(rc, p, results, bufReader)
            ) yield unit
        }

    ///
    /// Like `read`, but starting at a `cursor` which indicates the next index in `dest` to be written.
    ///
    /// Reads until `dest` is full or until there is nothing left to read from the underlying resource.
    ///
    /// Returns `Ok(c)` where `c` is a cursor indicating the next index in `dest` that can be written. If `dest` is full,
    /// `c == length(dest)`. This is equivalent to the total number of items written since `cursor` was 0.
    ///
    /// Guarantees that `0 <= c <= length(dest)`.
    ///
    /// Assumes that `cursor <= length(dest)`.
    ///
    def readFromCursor(cursor: Int32, dest: Array[a, rb], bufReader: BufReader[a, t, rr]): Result[IoError, Int32] \ rb + rr + Readable.Aef[t] with Readable[t] where Readable.Elm[t] ~ a =
        if (cursor == Array.length(dest)) {
            Ok(cursor)
        } else if (bufReader->size != 0) {
            let itemsCopied = copyFromBuffer(cursor, dest, bufReader);
            readFromCursor(cursor + itemsCopied, dest, bufReader)
        } else {
            forM(
                itemsFilled <- fillBuffer(bufReader);
                newCursor <- if (itemsFilled == 0) {
                    Ok(cursor)
                } else {
                    readFromCursor(cursor, dest, bufReader)
                }
            ) yield newCursor
        }

    ///
    /// Copies from the buffer into the destination, removing copied items from the buffer.
    ///
    /// Does not read from the underlying reader.
    ///
    /// Returns the number of items copied.
    ///
    /// Assumes that `cursor < length(dest)`.
    ///
    def copyFromBuffer(cursor: Int32, dest: Array[a, rb], bufReader: BufReader[a, t, rr]): Int32 \ rb + rr with Readable[t] =
        let itemsToRead = Int32.min(Array.length(dest) - cursor, bufReader->size);
        def copy(i) = {
            if (i >= itemsToRead) {
                ()
            } else {
                let item = Array.get(bufReader->cursor + i, bufReader->buffer);
                Array.put(item, cursor + i, dest);
                copy(i + 1)
            }
        };
        copy(0);
        bufReader->cursor = bufReader->cursor + itemsToRead;
        bufReader->size = bufReader->size - itemsToRead;
        itemsToRead

    ///
    /// Reads from the underlying reader into the buffer, updating the size
    /// and resetting the buffer's cursor back to 0.
    ///
    /// Returns `Ok(k)` where `k` is the number of items read.
    ///
    /// Assumes that `bufReader->size == 0`.
    ///
    def fillBuffer(bufReader: BufReader[a, t, rr]): Result[IoError, Int32] \ rr + Readable.Aef[t] with Readable[t] where Readable.Elm[t] ~ a =
        forM(
            itemsRead <- Readable.read(bufReader->buffer, bufReader->reader)
        ) yield {
            bufReader->cursor = 0;
            bufReader->size = itemsRead;
            itemsRead
        }

    def defaultBufferCapacity(): Int32 = 4096

}
