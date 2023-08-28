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
/// Represents a RandomAccessFile - a read-write file with random access.
///
enum File(##java.io.RandomAccessFile)

// instance Reader[File] {
//     pub def read(b: Array[Int8, r], r: File): Result[IOError, Int32] \ {r, IO} =
//         File.read!(b, r)
// }

mod File {

    use IOError.{Generic}
    use File.Mode.{ReadOnly, ReadWrite, ReadWriteS, ReadWriteD}

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
            import new java.io.RandomAccessFile(String, String): ##java.io.RandomAccessFile \ IO as newRandomAccessFile;
            let mode1 = modeString(mode);
            File(newRandomAccessFile(path, mode1))
        })

    ///
    /// Close a file.
    ///
    pub def close!(f: File): Result[IOError, Unit] \ IO =
        IOError.tryCatch(_ -> {
            import java.io.RandomAccessFile.close(): Unit \ IO;
            let File(f1) = f;
            close(f1)
        })

    ///
    /// Get the length of an open file.
    ///
    pub def length(f: File): Result[IOError, Int64] \ IO =
        IOError.tryCatch(_ -> {
            import java.io.RandomAccessFile.length(): Int64 \ IO;
            let File(f1) = f;
            length(f1)
        })

    ///
    /// Move the file pointer position to `pos`.
    ///
    pub def seek!(pos: Int64, f: File): Result[IOError, Unit] \ IO =
        IOError.tryCatch(_ -> {
            import java.io.RandomAccessFile.seek(Int64): Unit \ IO;
            let File(f1) = f;
            seek(f1, pos)
        })

    pub def skipBytes!(n: Int32, f: File): Result[IOError, Int32] \ IO =
        IOError.tryCatch(_ -> {
            import java.io.RandomAccessFile.skipBytes(Int32): Int32 \ IO;
            let File(f1) = f;
            skipBytes(f1, n)
        })


    pub def setLength!(len: Int64, f: File): Result[IOError, Unit] \ IO =
        IOError.tryCatch(_ -> {
            import java.io.RandomAccessFile.setLength(Int64): Unit \ IO;
            let File(f1) = f;
            setLength(f1, len)
        })

    pub def read!(buf: Array[Int8, r], f: File): Result[IOError, Int32] \ {IO, r} =
        IOError.tryCatch(_ -> {
            import java.io.RandomAccessFile.read(Array[Int8, r]): Int32 \ {IO, r};
            let File(f1) = f;
            read(f1, buf)
        })

    pub def readWith!(offset: {offset = Int32}, length: {length = Int32}, buf: Array[Int8, r], f: File): Result[IOError, Int32] \ {IO, r} =
        IOError.tryCatch(_ -> {
            import java.io.RandomAccessFile.read(Array[Int8, r], Int32, Int32): Int32 \ {IO, r};
            let File(f1) = f;
            read(f1, buf, offset.offset, length.length)
        })

    pub def readFully!(buf: Array[Int8, r], f: File): Result[IOError, Unit] \ {IO, r} =
        IOError.tryCatch(_ -> {
            import java.io.RandomAccessFile.readFully(Array[Int8, r]): Unit \ {IO, r};
            let File(f1) = f;
            readFully(f1, buf)
        })

    pub def readFullyWith!(offset: {offset = Int32}, length: {length = Int32}, buf: Array[Int8, r], f: File): Result[IOError, Unit] \ {IO, r} =
        IOError.tryCatch(_ -> {
            import java.io.RandomAccessFile.readFully(Array[Int8, r], Int32, Int32): Unit \ {IO, r};
            let File(f1) = f;
            readFully(f1, buf, offset.offset, length.length)
        })

    /// Read an unsigned byte
    /// 00 => 0
    /// FF => 255
    pub def readByte!(f: File): Result[IOError, Int32] \ IO =
        IOError.tryCatch(_ -> {
            import java.io.RandomAccessFile.read(): Int32 \ IO;
            let File(f1) = f;
            read(f1)
        })

    /// zero is false any other value is true
    pub def readBool!(f: File): Result[IOError, Bool] \ IO =
        IOError.tryCatch(_ -> {
            import java.io.RandomAccessFile.readBoolean(): Bool \ IO;
            let File(f1) = f;
            readBoolean(f1)
        })

    pub def readChar!(f: File): Result[IOError, Char] \ IO =
        IOError.tryCatch(_ -> {
            import java.io.RandomAccessFile.readChar(): Char \ IO;
            let File(f1) = f;
            readChar(f1)
        })

    /// Reads a signed byte i.e. an Int8
    ///
    pub def readInt8!(f: File): Result[IOError, Int8] \ IO =
        IOError.tryCatch(_ -> {
            import java.io.RandomAccessFile.readByte(): Int8 \ IO;
            let File(f1) = f;
            readByte(f1)
        })

    pub def readInt16!(f: File): Result[IOError, Int16] \ IO =
        IOError.tryCatch(_ -> {
            import java.io.RandomAccessFile.readShort(): Int16 \ IO;
            let File(f1) = f;
            readShort(f1)
        })

    pub def readInt32!(f: File): Result[IOError, Int32] \ IO =
        IOError.tryCatch(_ -> {
            import java.io.RandomAccessFile.read(): Int32 \ IO;
            let File(f1) = f;
            read(f1)
        })

    pub def readInt64!(f: File): Result[IOError, Int64] \ IO =
        IOError.tryCatch(_ -> {
            import java.io.RandomAccessFile.readLong(): Int64 \ IO;
            let File(f1) = f;
            readLong(f1)
        })

    pub def readFloat32!(f: File): Result[IOError, Float32] \ IO =
        IOError.tryCatch(_ -> {
            import java.io.RandomAccessFile.readFloat(): Float32 \ IO;
            let File(f1) = f;
            readFloat(f1)
        })

    pub def readFloat64!(f: File): Result[IOError, Float64] \ IO =
        IOError.tryCatch(_ -> {
            import java.io.RandomAccessFile.readDouble(): Float64 \ IO;
            let File(f1) = f;
            readDouble(f1)
        })

    pub def readUnsignedByte!(f: File): Result[IOError, Int32] \ IO =
        IOError.tryCatch(_ -> {
            import java.io.RandomAccessFile.readUnsignedByte(): Int32 \ IO;
            let File(f1) = f;
            readUnsignedByte(f1)
        })

    pub def readUnsignedShort!(f: File): Result[IOError, Int32] \ IO =
        IOError.tryCatch(_ -> {
            import java.io.RandomAccessFile.readUnsignedShort(): Int32 \ IO;
            let File(f1) = f;
            readUnsignedShort(f1)
        })

    pub def readLine!(f: File): Result[IOError, String] \ IO =
        IOError.tryCatch(_ -> {
            import java.io.RandomAccessFile.readLine(): String \ IO;
            let File(f1) = f;
            readLine(f1)
        })

    pub def readUTF!(f: File): Result[IOError, String] \ IO =
        IOError.tryCatch(_ -> {
            import java.io.RandomAccessFile.readUTF(): String \ IO;
            let File(f1) = f;
            readUTF(f1)
        })

    pub def write!(buf: Array[Int8, r], f: File): Result[IOError, Unit] \ {IO, r} =
        IOError.tryCatch(_ -> {
            import java.io.RandomAccessFile.write(Array[Int8, r]): Unit \ {IO, r};
            let File(f1) = f;
            write(f1, buf)
        })

    pub def writeWith!(offset: {offset = Int32}, length: {length = Int32}, buf: Array[Int8, r], f: File): Result[IOError, Unit] \ {IO, r} =
        IOError.tryCatch(_ -> {
            import java.io.RandomAccessFile.write(Array[Int8, r], Int32, Int32): Unit \ {IO, r};
            let File(f1) = f;
            write(f1, buf, offset.offset, length.length)
        })


    /// Writes `0x00` for `false`.
    /// Writes `0x01` for `true`.
    pub def writeBool!(b: Bool, f: File): Result[IOError, Unit] \ IO =
        IOError.tryCatch(_ -> {
            import java.io.RandomAccessFile.writeBoolean(Bool): Unit \ IO;
            let File(f1) = f;
            writeBoolean(f1, b)
        })

    pub def writeAsChar!(c: Int32, f: File): Result[IOError, Unit] \ IO =
        IOError.tryCatch(_ -> {
            import java.io.RandomAccessFile.writeChar(Int32): Unit \ IO;
            let File(f1) = f;
            writeChar(f1, c)
        })

    pub def writeAsByte!(i: Int32, f: File): Result[IOError, Unit] \ IO =
        IOError.tryCatch(_ -> {
            import java.io.RandomAccessFile.writeByte(Int32): Unit \ IO;
            let File(f1) = f;
            writeByte(f1, i)
        })

    pub def writeAsShort!(i: Int32, f: File): Result[IOError, Unit] \ IO =
        IOError.tryCatch(_ -> {
            import java.io.RandomAccessFile.writeShort(Int32): Unit \ IO;
            let File(f1) = f;
            writeShort(f1, i)
        })

    pub def writeInt32!(i: Int32, f: File): Result[IOError, Unit] \ IO =
        IOError.tryCatch(_ -> {
            import java.io.RandomAccessFile.writeInt(Int32): Unit \ IO;
            let File(f1) = f;
            writeInt(f1, i)
        })

    pub def writeInt64!(i: Int64, f: File): Result[IOError, Unit] \ IO =
        IOError.tryCatch(_ -> {
            import java.io.RandomAccessFile.writeLong(Int64): Unit \ IO;
            let File(f1) = f;
            writeLong(f1, i)
        })

    pub def writeFloat32!(d: Float32, f: File): Result[IOError, Unit] \ IO =
        IOError.tryCatch(_ -> {
            import java.io.RandomAccessFile.writeFloat(Float32): Unit \ IO;
            let File(f1) = f;
            writeFloat(f1, d)
        })

    pub def writeFloat64!(d: Float64, f: File): Result[IOError, Unit] \ IO =
        IOError.tryCatch(_ -> {
            import java.io.RandomAccessFile.writeDouble(Float64): Unit \ IO;
            let File(f1) = f;
            writeDouble(f1, d)
        })

    pub def writeUTF!(s: String, f: File): Result[IOError, Unit] \ IO =
        IOError.tryCatch(_ -> {
            import java.io.RandomAccessFile.writeUTF(String): Unit \ IO;
            let File(f1) = f;
            writeUTF(f1, s)
        })

}
