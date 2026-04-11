mod Test.Portable.RegionEffects {

    def assertTrue(cond: Bool, msg: String): Unit =
        if (cond) () else bug!(msg)

    def expectOk(r: Result[IoError, a], msg: String): a = match r {
        case Ok(v) => v
        case Err(e) => bug!("${msg}: ${e}")
    }

    @Test
    def fileReadWriteInsideRegion01(): Unit \ IO = region rc {
        let sep = %%FILE_SEPARATOR%%(());

        // Ensure we actually allocate in the region, not just enter/exit it.
        let arr: Array[Int32, rc] = Array#{1, 2, 3} @ rc;
        let _ = assertTrue(Array.length(arr) == 3, "array length mismatch");

        let dir = expectOk(
            FileWriteWithResult.runWithIO(() -> FileWriteWithResult.mkTempDir("flix-region-effects-")),
            "mkTempDir failed"
        );
        let file = dir + sep + "hello.txt";

        expectOk(
            FileWriteWithResult.runWithIO(() -> FileWriteWithResult.write({str = "abc"}, file)),
            "write failed"
        );

        let s = expectOk(
            FileReadWithResult.runWithIO(() -> FileReadWithResult.read(file)),
            "read failed"
        );
        let _ = assertTrue(s == "abc", "read/write mismatch");
        ()
    }

    pub def runAll(): Unit \ IO = {
        let _ = fileReadWriteInsideRegion01();
        ()
    }

}
