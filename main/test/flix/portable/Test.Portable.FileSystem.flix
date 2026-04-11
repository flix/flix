mod Test.Portable.FileSystem {

    def assertTrue(cond: Bool, msg: String): Unit =
        if (cond) () else bug!(msg)

    def expectOk(res: Result[IoError, a]): a = match res {
        case Ok(v) => v
        case Err(e) => bug!("expected Ok but got Err: ${ToString.toString(e)}")
    }

    def expectOkUnit(res: Result[IoError, Unit]): Unit = match res {
        case Ok(_) => ()
        case Err(e) => bug!("expected Ok but got Err: ${ToString.toString(e)}")
    }

    def expectErrKind(res: Result[IoError, a], expected: IoError.ErrorKind): Unit = match res {
        case Err(IoError.IoError(k, _)) if (k == expected) => ()
        case Err(e) => bug!("expected Err(${ToString.toString(expected)}) but got: ${ToString.toString(e)}")
        case Ok(_) => bug!("expected Err but got Ok.")
    }

    @Test
    def basic01(): Unit \ IO =
        FileWriteWithResult.runWithIO(() ->
            FileReadWithResult.runWithIO(() -> {
                // Temp directory.
                let dir = expectOk(FileWriteWithResult.mkTempDir("flix-test"));

                // mkdir + mkdirs.
                let d1 = dir / "d1";
                expectOkUnit(FileWriteWithResult.mkDir(d1));
                expectErrKind(FileWriteWithResult.mkDir(d1), IoError.ErrorKind.AlreadyExists);

                let nested = dir / "a" / "b" / "c";
                expectOkUnit(FileWriteWithResult.mkDirs(nested));

                let isNestedDir = expectOk(FileReadWithResult.isDirectory(nested));
                let _ = assertTrue(isNestedDir, "expected directory to exist after mkdirs.");

                // Write/read.
                let f1 = dir / "hello.txt";
                expectOkUnit(FileWriteWithResult.write(str = "hello", f1));

                let exists1 = expectOk(FileReadWithResult.exists(f1));
                let _ = assertTrue(exists1, "expected file to exist after write.");

                let s1 = expectOk(FileReadWithResult.read(f1));
                let _ = assertTrue(s1 == "hello", "unexpected file contents.");

                // readLines.
                let f2 = dir / "lines.txt";
                expectOkUnit(FileWriteWithResult.write(str = "a\nb\n", f2));
                let lines = expectOk(FileReadWithResult.readLines(f2));
                let _ = assertTrue(lines == "a" :: "b" :: Nil, "unexpected readLines result.");

                // readBytes + writeBytes + truncate.
                let b1 = expectOk(FileReadWithResult.readBytes(f1));
                let _ = assertTrue(Vector.length(b1) == 5i32, "expected 5 bytes for 'hello'.");

                let f3 = dir / "bytes.bin";
                expectOkUnit(FileWriteWithResult.writeBytes(b1, f3));
                let b2 = expectOk(FileReadWithResult.readBytes(f3));
                let _ = assertTrue(Vector.length(b2) == Vector.length(b1), "unexpected byte length after writeBytes.");

                expectOkUnit(FileWriteWithResult.truncate(f3));
                let b3 = expectOk(FileReadWithResult.readBytes(f3));
                let _ = assertTrue(Vector.isEmpty(b3), "expected file to be empty after truncate.");

                // list.
                let names = expectOk(FileReadWithResult.list(dir));
                let _ = assertTrue(List.exists(n -> n == "hello.txt", names), "expected hello.txt to appear in directory listing.");

                // Non-existent file: exists returns ok + false.
                let missing = dir / "missing.txt";
                let missingExists = expectOk(FileReadWithResult.exists(missing));
                let _ = assertTrue(not missingExists, "expected missing file to not exist.");

                ()
            })
        )

    pub def runAll(): Unit \ IO = {
        let _ = basic01();
        ()
    }

}
