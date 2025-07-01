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
/// A `Reader` implementation that wraps an underlying `Reader` instance
/// and caches the bytes read from the underlying instance in an internal buffer.
///
/// A call to `read` will read directly from the buffer if bytes are available, and
/// will trigger a `read` from the underlying `reader` when the buffer is empty.
///
/// This has the effect of reducing the overall number of `read`s to the underlying
/// `Reader`.
///
/// A call to `read` might read more than the requested number of bytes from the
/// underlying reader, in order to fill the entire buffer.
///
/// A call to `read` might not read from the underlying `reader` at all, if there are
/// enough bytes in the buffer.
///
/// `buffer`: the underlying buffer.
/// `cursor`: the index in `buffer` of the first element logically considered to be in the buffer.
/// `size`: the number of elements (starting at `cursor`) logically considered to be in the buffer.
/// `reader`: the underlying `Reader` being read from.
///
pub struct BufReader[t: Type, r: Region] {
    buffer: Array[Int8, r],
    mut cursor: Int32,
    mut size: Int32,
    reader: t
}

instance Reader[BufReader[t, rr]] with Reader[t] {

    type Aef = rr + Reader.Aef[t]

    pub def read(buffer: Array[Int8, rb], bufReader: BufReader[t, rr]): Result[IoError, Int32] \ rb + rr + Reader.Aef[t] =
        BufReader.read(buffer, bufReader)

}

mod BufReader {

    ///
    /// Constructs a `BufReader` wrapping `reader` with a default capacity of 4096 bytes.
    ///
    pub def withDefaultCapacity(rc: Region[r], reader: t): BufReader[t, r] \ r =
        withCapacity(rc, defaultBufferCapacity(), reader)

    ///
    /// Constructs a `BufReader` wrapping `reader` with the specified `capacity` in bytes.
    ///
    /// If `capacity <= 0`, the `BufReader` will delegate directly to `reader` without buffering.
    ///
    pub def withCapacity(rc: Region[r], capacity: Int32, reader: t): BufReader[t, r] \ r =
        new BufReader @ rc {
            buffer = Array.repeat(rc, capacity, 0i8),
            cursor = 0,
            size = 0,
            reader = reader
        }

    ///
    /// Reads `k` bytes from the underlying `Reader` into `dest`, caching bytes read into
    /// the internal buffer.
    ///
    /// Returns `Ok(k)` to signify that `k` bytes were successfully read and written to `dest`.
    ///
    /// Guarantees that `0 <= k <= length(dest)`.
    ///
    /// Returns `Err(e)` if some underlying error occurs.
    ///
    pub def read(dest: Array[Int8, rb], bufReader: BufReader[t, rr]): Result[IoError, Int32] \ rb + rr + Reader.Aef[t] with Reader[t] =
        if (Array.size(bufReader->buffer) > 0) {
            readFromCursor(0, dest, bufReader)
        } else {
            Reader.read(dest, bufReader->reader)
        }

    ///
    /// Like `read`, but starting at a `cursor` which indicates the next index in `dest` to be written.
    ///
    /// Reads until `dest` is full or until there is nothing left to read from the underlying resource.
    ///
    /// Returns `Ok(c)` where `c` is a cursor indicating the next index in `dest` that can be written. If `dest` is full,
    /// `c == length(dest)`. This is equivalent to the total number of bytes written since `cursor` was 0.
    ///
    /// Guarantees that `0 <= c <= length(dest)`.
    ///
    /// Assumes that `cursor <= length(dest)`.
    ///
    def readFromCursor(cursor: Int32, dest: Array[Int8, rb], bufReader: BufReader[t, rr]): Result[IoError, Int32] \ rb + rr + Reader.Aef[t] with Reader[t] =
        if (cursor == Array.length(dest)) {
            Ok(cursor)
        } else if (bufReader->size != 0) {
            let bytesCopied = copyFromBuffer(cursor, dest, bufReader);
            readFromCursor(cursor + bytesCopied, dest, bufReader)
        } else {
            forM(
                bytesFilled <- fillBuffer(bufReader);
                newCursor <- if (bytesFilled == 0) {
                    Ok(cursor)
                } else {
                    readFromCursor(cursor, dest, bufReader)
                }
            ) yield newCursor
        }

    ///
    /// Copies from the buffer into the destination, removing copied bytes from the buffer.
    ///
    /// Does not read from the underlying reader.
    ///
    /// Returns the number of bytes copied.
    ///
    /// Assumes that `cursor < length(dest)`.
    ///
    def copyFromBuffer(cursor: Int32, dest: Array[Int8, rb], bufReader: BufReader[t, rr]): Int32 \ rb + rr with Reader[t] =
        let bytesToRead = Int32.min(Array.length(dest) - cursor, bufReader->size);
        def copy(i) = {
            if (i >= bytesToRead) {
                ()
            } else {
                let byte = Array.get(bufReader->cursor + i, bufReader->buffer);
                Array.put(byte, cursor + i, dest);
                copy(i + 1)
            }
        };
        copy(0);
        bufReader->cursor = bufReader->cursor + bytesToRead;
        bufReader->size = bufReader->size - bytesToRead;
        bytesToRead

    ///
    /// Reads from the underlying reader into the buffer, updating the size
    /// and resetting the buffer's cursor back to 0.
    ///
    /// Returns `Ok(k)` where `k` is the number of bytes read.
    ///
    /// Assumes that `bufReader->size == 0`.
    ///
    def fillBuffer(bufReader: BufReader[t, rr]): Result[IoError, Int32] \ rr + Reader.Aef[t] with Reader[t] =
        forM(
            bytesRead <- Reader.read(bufReader->buffer, bufReader->reader)
        ) yield {
            bufReader->cursor = 0;
            bufReader->size = bytesRead;
            bytesRead
        }

    def defaultBufferCapacity(): Int32 = 4096

}
