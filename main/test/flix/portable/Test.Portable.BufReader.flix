mod Test.Portable.BufReaderSuite {

    def assertTrue(cond: Bool, msg: String): Unit =
        if (cond) () else bug!(msg)

    pub struct SeqReader[r] {
        data: Array[Int32, r],
        mut pos: Int32
    }

    mod SeqReader {
        pub def readInto(buffer: Array[Int32, rb], reader: SeqReader[r]): Result[IoError, Int32] \ rb + r =
            let available = Array.length(reader->data) - reader->pos;
            if (available <= 0) {
                Ok(0)
            } else {
                let toRead = Int32.min(Array.length(buffer), available);
                def loop(i: Int32): Unit \ {rb, r} =
                    if (i < toRead) {
                        let x = Array.get(reader->pos + i, reader->data);
                        Array.put(x, i, buffer);
                        loop(i + 1)
                    } else {
                        ()
                    };
                loop(0);
                reader->pos = reader->pos + toRead;
                Ok(toRead)
            }
    }

    instance Readable[SeqReader[r]] {
        type Elm = Int32
        type Aef = r

        pub def read(buffer: Array[Int32, rb], reader: SeqReader[r]): Result[IoError, Int32] \ rb + r =
            SeqReader.readInto(buffer, reader)
    }

    @Test
    def readBuffered01(): Unit = region rc {
        let data = Array.empty(rc, 4);
        Array.put(10i32, 0, data);
        Array.put(20i32, 1, data);
        Array.put(30i32, 2, data);
        Array.put(40i32, 3, data);

        let reader = new SeqReader @ rc { data = data, pos = 0 };
        let br = BufReader.withCapacity(rc, 2, reader);

        let buf1 = Array.empty(rc, 1);
        match BufReader.read(buf1, br) {
            case Ok(k) => assertTrue(k == 1i32 and Array.get(0, buf1) == 10i32, "readBuffered01 failed: expected first read to return 10.")
            case Err(_) => bug!("readBuffered01 failed: unexpected Err.")
        };

        match BufReader.read(buf1, br) {
            case Ok(k) => assertTrue(k == 1i32 and Array.get(0, buf1) == 20i32, "readBuffered01 failed: expected second read to return 20.")
            case Err(_) => bug!("readBuffered01 failed: unexpected Err.")
        };

        let buf2 = Array.empty(rc, 2);
        match BufReader.read(buf2, br) {
            case Ok(k) => assertTrue(k == 2i32 and Array.get(0, buf2) == 30i32 and Array.get(1, buf2) == 40i32, "readBuffered01 failed: expected third read to return 30,40.")
            case Err(_) => bug!("readBuffered01 failed: unexpected Err.")
        };

        match BufReader.read(buf1, br) {
            case Ok(k) => assertTrue(k == 0i32, "readBuffered01 failed: expected EOF (0).")
            case Err(_) => bug!("readBuffered01 failed: unexpected Err.")
        };
        ()
    }

    @Test
    def peekSkipReadWhile01(): Unit = region rc {
        let data = Array.empty(rc, 5);
        Array.put(1i32, 0, data);
        Array.put(1i32, 1, data);
        Array.put(1i32, 2, data);
        Array.put(0i32, 3, data);
        Array.put(1i32, 4, data);

        let reader = new SeqReader @ rc { data = data, pos = 0 };
        let br = BufReader.withCapacity(rc, 2, reader);

        match BufReader.peek(br) {
            case Ok(Some(x)) => assertTrue(x == 1i32, "peekSkipReadWhile01 failed: expected peek == Some(1).")
            case Ok(None) => bug!("peekSkipReadWhile01 failed: expected Some.")
            case Err(_) => bug!("peekSkipReadWhile01 failed: unexpected Err.")
        };

        match BufReader.readWhile(x -> x == 1i32, br) {
            case Ok(v) => assertTrue(v |> Vector.toList == 1i32 :: 1i32 :: 1i32 :: Nil, "peekSkipReadWhile01 failed: expected readWhile to return three 1s.")
            case Err(_) => bug!("peekSkipReadWhile01 failed: unexpected Err.")
        };

        match BufReader.peek(br) {
            case Ok(Some(x)) => assertTrue(x == 0i32, "peekSkipReadWhile01 failed: expected next element to be 0 after readWhile.")
            case _ => bug!("peekSkipReadWhile01 failed: expected Ok(Some(0)).")
        };

        match BufReader.skip(1, br) {
            case Ok(k) => assertTrue(k == 1i32, "peekSkipReadWhile01 failed: expected skip(1) == 1.")
            case Err(_) => bug!("peekSkipReadWhile01 failed: unexpected Err.")
        };

        match BufReader.peek(br) {
            case Ok(Some(x)) => assertTrue(x == 1i32, "peekSkipReadWhile01 failed: expected peek == Some(1) after skipping 0.")
            case _ => bug!("peekSkipReadWhile01 failed: expected Ok(Some(1)).")
        };
        ()
    }

    pub def runAll(): Unit = {
        let _ = readBuffered01();
        let _ = peekSkipReadWhile01();
        ()
    }

}
