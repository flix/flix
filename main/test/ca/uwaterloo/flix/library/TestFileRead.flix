mod TestFileRead {

    use Assert.{assertEq, fail};
    use IoError.ErrorKind;
    use IoError.IoError;
    use Monad.{>>=};

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
                FileRead.handle(() -> FileRead.exists(path))()
            })()
        );
        assertEq(expected = Ok(true), result)
    }

    @Test
    def exists02(): Unit \ {Assert, IO} = {
        let result = FileRead.handle(() -> FileRead.exists("SomeFileThatDoesNotExist"))();
        assertEq(expected = Ok(false), result)
    }

    @Test
    def exists03(): Unit \ {Assert, IO} = {
        match FileRead.handle(() -> FileRead.exists("Some\\Invalid:{\u0000Path}/"))() {
            case Ok(_) => fail("Expected error for invalid path")
            case Err(IoError(kind, _)) => assertEq(expected = ErrorKind.InvalidPath, kind)
        }
    }

}