mod TestFileReadWithResult {

    use IoError.IoError;
    use IoError.ErrorKind;
    use Assert.{assertEq, fail};

    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def exists01(): Unit \ {Assert, IO} = {
        let result = FileWriteWithResult.handle(() -> {
            match FileWriteWithResult.mkTempDir("flix") {
                case Ok(dir) =>
                    let path = "${dir}/flix-tmpfile-68975";
                    match FileWriteWithResult.write({ str = "content" }, path) {
                        case Ok(_) =>
                            FileReadWithResult.handle(() -> FileReadWithResult.exists(path))()
                        case Err(e) => Err(e)
                    }
                case Err(e) => Err(e)
            }
        })();
        assertEq(expected = Ok(true), result)
    }

    @Test
    def exists02(): Unit \ {Assert, IO} = {
        let result = FileReadWithResult.handle(() -> FileReadWithResult.exists("SomeFileThatDoesNotExist"))();
        assertEq(expected = Ok(false), result)
    }

    @Test
    def exists03(): Unit \ {Assert, IO} = {
        match FileReadWithResult.handle(() -> FileReadWithResult.exists("Some\\Invalid:{\u0000Path}/"))() {
            case Ok(_) => fail("Expected error for invalid path")
            case Err(IoError(kind, _)) => assertEq(expected = ErrorKind.InvalidPath, kind)
        }
    }

}
