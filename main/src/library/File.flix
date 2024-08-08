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

///
/// Represents a RandomAccessFile - a read-only or read-write file with random access.
///
/// File does not properly support Unicode and uses Java / JVM conventions for representing
/// numbers so it is inadequate for serializing data portably.
///
enum File(##java.io.RandomAccessFile)

instance Reader[File] {
    pub def read(b: Array[Int8, r], r: File): Result[IOError, Int32] \ {IO, r} =
        File.read!(b, r)
}

mod File {

    use IOError.{Generic}
    use File.Mode.{ReadOnly, ReadWrite, ReadWriteS, ReadWriteD}
    import java.io.RandomAccessFile;

    ///
    /// Represents a mode for opening files used by the function `open`.
    ///
    /// `ReadOnly` opens the file in read-only mode, attempting to use any of the write
    /// functions on this file will result in an error.
    ///
    /// `ReadWrite` opens the file in read-write mode, if the file does not exist the
    /// `open` function will try to create it.
    ///
    /// `ReadWriteS` opens the file in read-write mode, and the files content or metadata
    /// will be written synchronously to the underlying storage device with each update.
    ///
    /// `ReadWriteD` opens the file in read-write mode, and the files content will be
    /// written synchronously to the underlying storage device with each update.
    ///
    pub enum Mode with Eq, ToString {
        case ReadOnly
        case ReadWrite
        case ReadWriteS
        case ReadWriteD
    }

    ///
    /// Helper function to marshal `Mode`.
    ///
    def modeString(m: Mode): String = match m {
        case ReadOnly   => "r"
        case ReadWrite  => "rw"
        case ReadWriteS => "rws"
        case ReadWriteD => "rwd"
    }

    ///
    /// Opens a file. `path` should represent a valid path string. See the `Mode`
    /// enum for an explanation of the available access modes.
    /// If the file does not exist, but it `open` is acalled with a write mode the
    /// system will try to make a new file (the attempt may fail due to permissions, IO
    /// errors, etc.).
    ///
    pub def open(path: String, mode: Mode): Result[IOError, File] \ IO =
        IOError.tryCatch(_ -> {
            let mode1 = modeString(mode);
            File(new RandomAccessFile(path, mode1))
        })

    ///
    /// Close a file.
    ///
    pub def close!(f: File): Result[IOError, Unit] \ IO =
        IOError.tryCatch(_ -> {
            let File(f1) = f;
            f1.close()
        })

    ///
    /// Get the length of an open file.
    ///
    pub def length(f: File): Result[IOError, Int64] \ IO =
        IOError.tryCatch(_ -> {
            let File(f1) = f;
            f1.length()
        })

    ///
    /// Move the file pointer position to `pos` measured in bytes from the start of the file.
    ///
    pub def seek!(pos: Int64, f: File): Result[IOError, Unit] \ IO =
        IOError.tryCatch(_ -> {
            let File(f1) = f;
            f1.seek(pos)
        })

    ///
    /// Skip over `n` bytes in a file.
    /// The number returned may be smaller than `n` if the end of file is reached.
    /// If `n` is negative no bytes are skipped.
    ///
    pub def skipBytes!(n: Int32, f: File): Result[IOError, Int32] \ IO =
        IOError.tryCatch(_ -> {
            let File(f1) = f;
            f1.skipBytes(n)
        })

    ///
    /// Set the length of the file `f` to `len`.
    /// If `len` is smaller than the length of the file then the file will be truncated.
    /// If `len` is greater than the length of the file then the file will be extended.
    /// In the case of the file being extended, how the new section of the file is filled
    /// is not defined and is dependent on the implementation.
    ///
    pub def setLength!(len: Int64, f: File): Result[IOError, Unit] \ IO =
        IOError.tryCatch(_ -> {
            let File(f1) = f;
            f1.setLength(len)
        })

    ///
    /// Read up to `Array.length(buf)` bytes from the file `f`.
    /// This function blocks until at least one byte of data is available.
    ///
    pub def read!(buf: Array[Int8, r], f: File): Result[IOError, Int32] \ {IO, r} =
        IOError.tryCatch(_ -> {
            let File(f1) = f;
            unchecked_cast(f1.read(buf) as _ \ IO + r)
        })

    ///
    /// Read up to `length#length` bytes from the file `f`, the bytes are written to the array `buf` at the offset `destOffset`.
    /// This function blocks until at least one byte of data is available.
    ///
    pub def readWith!(destOffset: {destOffset = Int32}, length: {length = Int32}, buf: Array[Int8, r], f: File): Result[IOError, Int32] \ {IO, r} =
        IOError.tryCatch(_ -> {
            let File(f1) = f;
            unchecked_cast(f1.read(buf, destOffset#destOffset, length#length) as _ \ IO + r)
        })

    ///
    /// Reads `Array.length(buf)` bytes from the file `f`.
    /// This function blocks until the requested length of data is available.
    ///
    pub def readFully!(buf: Array[Int8, r], f: File): Result[IOError, Unit] \ {IO, r} =
        IOError.tryCatch(_ -> {
            let File(f1) = f;
            unchecked_cast(f1.readFully(buf) as _ \ IO + r)
        })

    ///
    /// Reads `length#length` bytes from the file `f`, the bytes are written to the array `buf` at the offset `destOffset`.
    /// This function blocks until the requested length of data is available.
    ///
    pub def readFullyWith!(destOffset: {destOffset = Int32}, length: {length = Int32}, buf: Array[Int8, r], f: File): Result[IOError, Unit] \ {IO, r} =
        IOError.tryCatch(_ -> {
            let File(f1) = f;
            unchecked_cast(f1.readFully(buf, destOffset#destOffset, length#length) as _ \ IO + r)
        })

    ///
    /// Read an unsigned single byte from the file `f`.
    ///
    /// The byte is represented as an Int32, the value returned will be in the range:
    /// 0x00 => 0
    /// ...
    /// 0xFF => 255
    ///
    pub def readByte!(f: File): Result[IOError, Int32] \ IO =
        IOError.tryCatch(_ -> {
            let File(f1) = f;
            f1.read()
        })

    ///
    /// Read a Bool from the file `f`. The Bool is represented as a single byte.
    ///
    /// Reading byte value of 0x00 produces `false`. Reading any other value produces `true`.
    ///
    pub def readBool!(f: File): Result[IOError, Bool] \ IO =
        IOError.tryCatch(_ -> {
            let File(f1) = f;
            f1.readBoolean()
        })


    ///
    /// Read a Char value from file `f` stored in Java's 2-byte Char representation.
    ///
    /// This function blocks until the two bytes are read or an exception otherwise occurs.
    ///
    pub def readChar!(f: File): Result[IOError, Char] \ IO =
        IOError.tryCatch(_ -> {
            let File(f1) = f;
            f1.readChar()
        })

    ///
    /// Read a signed Int8 value from file `f` stored as a single byte.
    ///
    pub def readInt8!(f: File): Result[IOError, Int8] \ IO =
        IOError.tryCatch(_ -> {
            let File(f1) = f;
            f1.readByte()
        })

    ///
    /// Read a Int16 value from file `f` stored in Java's big-endian 2-byte Short representation.
    ///
    /// This function blocks until the two bytes are read or an exception otherwise occurs.
    ///
    pub def readInt16!(f: File): Result[IOError, Int16] \ IO =
        IOError.tryCatch(_ -> {
            let File(f1) = f;
            f1.readShort()
        })

    ///
    /// Read a Int32 value from file `f` stored in Java's big-endian 4-byte Int representation.
    ///
    /// This function blocks until the four bytes are read or an exception otherwise occurs.
    ///
    pub def readInt32!(f: File): Result[IOError, Int32] \ IO =
        IOError.tryCatch(_ -> {
            let File(f1) = f;
            f1.readInt()
        })

    ///
    /// Read a Int64 value from file `f` stored in Java's big-endian 8-byte Long representation.
    ///
    /// This function blocks until the eight bytes are read or an exception otherwise occurs.
    ///
    pub def readInt64!(f: File): Result[IOError, Int64] \ IO =
        IOError.tryCatch(_ -> {
            let File(f1) = f;
            f1.readLong()
        })

    ///
    /// Read a Float32 value from file `f` stored in Java's big-endian 4-byte Float representation.
    ///
    /// Internally `d` is read as an 4-byte Int32 then converted to a Float32.
    ///
    pub def readFloat32!(f: File): Result[IOError, Float32] \ IO =
        IOError.tryCatch(_ -> {
            let File(f1) = f;
            f1.readFloat()
        })

    ///
    /// Read a floating point value from file `f` stored in Java's big-endian 8-byte Float representation.
    ///
    /// Internally `d` is read as an 8-byte Int64 then converted to a Float64.
    ///
    pub def readFloat64!(f: File): Result[IOError, Float64] \ IO =
        IOError.tryCatch(_ -> {
            let File(f1) = f;
            f1.readDouble()
        })

    ///
    /// Read an unsigned byte value from file `f` stored as a single byte.
    ///
    /// The answer is widened to an Int32 because so it can contain values
    /// in the range 128..255.
    ///
    pub def readUnsignedByte!(f: File): Result[IOError, Int32] \ IO =
        IOError.tryCatch(_ -> {
            let File(f1) = f;
            f1.readUnsignedByte()
        })

    ///
    /// Read a unsigned two byte (short) value from file `f`.
    ///
    /// This function blocks until the two bytes are read or an exception otherwise occurs.
    ///
    pub def readUnsignedShort!(f: File): Result[IOError, Int32] \ IO =
        IOError.tryCatch(_ -> {
            let File(f1) = f;
            f1.readUnsignedShort()
        })

    ///
    /// Read a line of text from the file `f`.
    ///
    /// This function does not support the full Unicode character set.
    ///
    pub def readLine!(f: File): Result[IOError, String] \ IO =
        IOError.tryCatch(_ -> {
            let File(f1) = f;
            f1.readLine()
        })


    ///
    /// Read a length prefixed, "modified" UTF8 string from the file `f`.
    /// The length is derived from the first two bytes which are treated as
    /// an unsigned short.
    ///
    /// The modifications are:
    /// Null ('\u0000') is written as two bytes rather than one.
    /// Character ranges are represented by 1, 2, or 3 byte formats.
    /// Supplementary characters are represented as surrogate pairs.
    ///
    pub def readModifiedUTF!(f: File): Result[IOError, String] \ IO =
        IOError.tryCatch(_ -> {
            let File(f1) = f;
            f1.readUTF()
        })

    ///
    /// Write to `Array.length(buf)` bytes to the file `f` starting at the
    /// current position of the file pointer.
    ///
    pub def write!(buf: Array[Int8, r], f: File): Result[IOError, Unit] \ {IO, r} =
        IOError.tryCatch(_ -> {
            let File(f1) = f;
            unchecked_cast(f1.write(buf) as _ \ IO + r)
        })

    ///
    /// Write to `length#length` bytes to the file `f`, the bytes are read from the
    /// array `buf` starting at `sourceOffset#sourceOffset`.
    ///
    pub def writeWith!(sourceOffset: {sourceOffset = Int32}, length: {length = Int32}, buf: Array[Int8, r], f: File): Result[IOError, Unit] \ {IO, r} =
        IOError.tryCatch(_ -> {
            let File(f1) = f;
            unchecked_cast(f1.write(buf, sourceOffset#sourceOffset, length#length) as _ \ IO + r)
        })

    ///
    /// Write the Bool value `b` to the file `f` at the current file pointer position.
    /// The Bool is represented as a single byte.
    ///
    /// `0x00` is written for `false`.
    /// `0x01` is written for `true`.
    ///
    pub def writeBool!(b: Bool, f: File): Result[IOError, Unit] \ IO =
        IOError.tryCatch(_ -> {
            let File(f1) = f;
            f1.writeBoolean(b)
        })

    ///
    /// Write the char value `c` (represented as an Int32) to the file `f` at the current file pointer position.
    /// The char is written as two bytes with the high byte first (big endian).
    ///
    pub def writeAsChar!(c: Int32, f: File): Result[IOError, Unit] \ IO =
        IOError.tryCatch(_ -> {
            let File(f1) = f;
            f1.writeChar(c)
        })

    ///
    /// Write the byte value `i` (represented as an Int32) to the file `f` at the current file pointer position.
    /// `i` is written as a single byte.
    ///
    pub def writeAsByte!(i: Int32, f: File): Result[IOError, Unit] \ IO =
        IOError.tryCatch(_ -> {
            let File(f1) = f;
            f1.writeByte(i)
        })

    ///
    /// Write the short value `i` (represented as an Int32) to the file `f` at the current file pointer position.
    /// The short is written as two bytes with the high byte first (big endian).
    ///
    pub def writeAsShort!(i: Int32, f: File): Result[IOError, Unit] \ IO =
        IOError.tryCatch(_ -> {
            let File(f1) = f;
            f1.writeShort(i)
        })

    ///
    /// Write the Int32 value `i` to the file `f` at the current file pointer position.
    /// The Int32 is written as four bytes with the high byte first (big endian).
    ///
    pub def writeInt32!(i: Int32, f: File): Result[IOError, Unit] \ IO =
        IOError.tryCatch(_ -> {
            let File(f1) = f;
            f1.writeInt(i)
        })

    ///
    /// Write the Int64 value `i` to the file `f` at the current file pointer position.
    /// The Int64 is written as eight bytes with the high byte first (big endian).
    ///
    pub def writeInt64!(i: Int64, f: File): Result[IOError, Unit] \ IO =
        IOError.tryCatch(_ -> {
            let File(f1) = f;
            f1.writeLong(i)
        })

    ///
    /// Write the floating point value `d` to file `f` using Java's 4-byte Float representation.
    ///
    /// Internally `d` is first converted to a Int32 than written to file with the high byte first.
    ///
    pub def writeFloat32!(d: Float32, f: File): Result[IOError, Unit] \ IO =
        IOError.tryCatch(_ -> {
            let File(f1) = f;
            f1.writeFloat(d)
        })


    ///
    /// Write the floating point value `d` to file `f` using Java's 8-byte Double representation.
    ///
    /// Internally `d` is first converted to a Int64 than written to file with the high byte first.
    ///
    pub def writeFloat64!(d: Float64, f: File): Result[IOError, Unit] \ IO =
        IOError.tryCatch(_ -> {
            let File(f1) = f;
            f1.writeDouble(d)
        })

    ///
    /// Read a length prefixed, "modified" UTF8 string.
    /// The length is written to the first two bytes using the same encoding as an unsigned short.
    ///
    /// The modifications follow the rules used by `readString`.
    ///
    pub def writeModifiedUTF!(s: String, f: File): Result[IOError, Unit] \ IO =
        IOError.tryCatch(_ -> {
            let File(f1) = f;
            f1.writeUTF(s)
        })

}
