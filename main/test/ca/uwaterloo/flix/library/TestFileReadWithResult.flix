mod TestFileReadWithResult {

    use IoError.IoError;
    use IoError.ErrorKind;
    use Monad.{>>=};
    use Assert.{assertEq, fail};

    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def exists01(): Unit \ {Assert, IO} = {
        let result = Result.flatten(
            FileWrite.handle(() -> {
                let dir = FileWrite.mkTempDir("flix");
                let path = "${dir}/flix-tmpfile-68975";
                FileWrite.write({ str = "content" }, path);
                FileReadWithResult.handle(() -> FileReadWithResult.exists(path))()
            })()
        );
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