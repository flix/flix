mod TestCodec {

    use Assert.{assertEq, assertErr, assertOk};

    /////////////////////////////////////////////////////////////////////////////
    // encodeBase64                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def encodeBase6401(): Unit \ Assert =
        assertEq(expected = "", Codec.encodeBase64(String.toBytes("")))

    @Test
    def encodeBase6402(): Unit \ Assert =
        assertEq(expected = "Zg==", Codec.encodeBase64(String.toBytes("f")))

    @Test
    def encodeBase6403(): Unit \ Assert =
        assertEq(expected = "Zm8=", Codec.encodeBase64(String.toBytes("fo")))

    @Test
    def encodeBase6404(): Unit \ Assert =
        assertEq(expected = "Zm9v", Codec.encodeBase64(String.toBytes("foo")))

    @Test
    def encodeBase6405(): Unit \ Assert =
        assertEq(expected = "Zm9vYg==", Codec.encodeBase64(String.toBytes("foob")))

    @Test
    def encodeBase6406(): Unit \ Assert =
        assertEq(expected = "Zm9vYmFy", Codec.encodeBase64(String.toBytes("foobar")))

    @Test
    def encodeBase6407(): Unit \ Assert =
        assertEq(expected = "YQ==", Codec.encodeBase64(String.toBytes("a")))

    @Test
    def encodeBase6408(): Unit \ Assert =
        assertEq(expected = "YWI=", Codec.encodeBase64(String.toBytes("ab")))

    @Test
    def encodeBase6409(): Unit \ Assert =
        assertEq(expected = "YWJj", Codec.encodeBase64(String.toBytes("abc")))

    @Test
    def encodeBase6410(): Unit \ Assert =
        assertEq(expected = "QUJDREVGR0hJSktMTU5PUFFSU1RVVldYWVo=",
            Codec.encodeBase64(String.toBytes("ABCDEFGHIJKLMNOPQRSTUVWXYZ")))

    @Test
    def encodeBase6411(): Unit \ Assert =
        assertEq(expected = "MDEyMzQ1Njc4OQ==",
            Codec.encodeBase64(String.toBytes("0123456789")))

    @Test
    def encodeBase6412(): Unit \ Assert =
        assertEq(expected = "Kys=", Codec.encodeBase64(String.toBytes("++")))

    /////////////////////////////////////////////////////////////////////////////
    // decodeBase64                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def decodeBase6401(): Unit \ Assert =
        assertEq(expected = Ok(String.toBytes("")), Codec.decodeBase64(""))

    @Test
    def decodeBase6402(): Unit \ Assert =
        assertEq(expected = Ok(String.toBytes("f")), Codec.decodeBase64("Zg=="))

    @Test
    def decodeBase6403(): Unit \ Assert =
        assertEq(expected = Ok(String.toBytes("fo")), Codec.decodeBase64("Zm8="))

    @Test
    def decodeBase6404(): Unit \ Assert =
        assertEq(expected = Ok(String.toBytes("foo")), Codec.decodeBase64("Zm9v"))

    @Test
    def decodeBase6405(): Unit \ Assert =
        assertEq(expected = Ok(String.toBytes("foobar")), Codec.decodeBase64("Zm9vYmFy"))

    @Test
    def decodeBase6406(): Unit \ Assert =
        assertErr(Codec.decodeBase64("!!!invalid!!!"))

    @Test
    def decodeBase6407(): Unit \ Assert =
        assertEq(expected = Ok(String.toBytes("a")), Codec.decodeBase64("YQ=="))

    @Test
    def decodeBase6408(): Unit \ Assert =
        assertEq(expected = Ok(String.toBytes("ab")), Codec.decodeBase64("YWI="))

    @Test
    def decodeBase6409(): Unit \ Assert =
        assertEq(expected = Ok(String.toBytes("abc")), Codec.decodeBase64("YWJj"))

    @Test
    def decodeBase6410(): Unit \ Assert =
        assertErr(Codec.decodeBase64("A"))

    @Test
    def decodeBase6411(): Unit \ Assert =
        assertErr(Codec.decodeBase64("AB"))

    @Test
    def decodeBase6412(): Unit \ Assert =
        assertErr(Codec.decodeBase64("ABCDE"))

    @Test
    def decodeBase6413(): Unit \ Assert =
        assertErr(Codec.decodeBase64("===="))

    /////////////////////////////////////////////////////////////////////////////
    // encodeBase64String                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def encodeBase64String01(): Unit \ Assert =
        assertEq(expected = "", Codec.encodeBase64String(""))

    @Test
    def encodeBase64String02(): Unit \ Assert =
        assertEq(expected = "SGVsbG8sIFdvcmxkIQ==", Codec.encodeBase64String("Hello, World!"))

    @Test
    def encodeBase64String03(): Unit \ Assert =
        assertEq(expected = "RmxpeA==", Codec.encodeBase64String("Flix"))

    @Test
    def encodeBase64String04(): Unit \ Assert =
        assertEq(expected = "ICAgIA==", Codec.encodeBase64String("    "))

    @Test
    def encodeBase64String05(): Unit \ Assert =
        assertEq(expected = "8J+YgA==", Codec.encodeBase64String("😀"))

    @Test
    def encodeBase64String06(): Unit \ Assert =
        assertEq(expected = "w6XDpMO2", Codec.encodeBase64String("åäö"))

    /////////////////////////////////////////////////////////////////////////////
    // decodeBase64String                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def decodeBase64String01(): Unit \ Assert =
        assertEq(expected = Ok(""), Codec.decodeBase64String(""))

    @Test
    def decodeBase64String02(): Unit \ Assert =
        assertEq(expected = Ok("Hello, World!"), Codec.decodeBase64String("SGVsbG8sIFdvcmxkIQ=="))

    @Test
    def decodeBase64String03(): Unit \ Assert =
        assertErr(Codec.decodeBase64String("!!!invalid!!!"))

    @Test
    def decodeBase64String04(): Unit \ Assert =
        assertEq(expected = Ok("    "), Codec.decodeBase64String("ICAgIA=="))

    @Test
    def decodeBase64String05(): Unit \ Assert =
        assertEq(expected = Ok("😀"), Codec.decodeBase64String("8J+YgA=="))

    @Test
    def decodeBase64String06(): Unit \ Assert =
        assertEq(expected = Ok("åäö"), Codec.decodeBase64String("w6XDpMO2"))

    /////////////////////////////////////////////////////////////////////////////
    // roundtrip                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def roundtrip01(): Unit \ Assert =
        assertEq(expected = Ok(""), Codec.decodeBase64String(Codec.encodeBase64String("")))

    @Test
    def roundtrip02(): Unit \ Assert =
        assertEq(expected = Ok("The quick brown fox jumps over the lazy dog"),
            Codec.decodeBase64String(Codec.encodeBase64String("The quick brown fox jumps over the lazy dog")))

    @Test
    def roundtrip03(): Unit \ Assert =
        assertEq(expected = Ok(String.toBytes("foobar")),
            Codec.decodeBase64(Codec.encodeBase64(String.toBytes("foobar"))))

    @Test
    def roundtrip04(): Unit \ Assert =
        assertEq(expected = Ok("x"), Codec.decodeBase64String(Codec.encodeBase64String("x")))

    @Test
    def roundtrip05(): Unit \ Assert =
        assertEq(expected = Ok("xy"), Codec.decodeBase64String(Codec.encodeBase64String("xy")))

    @Test
    def roundtrip06(): Unit \ Assert =
        assertEq(expected = Ok("xyz"), Codec.decodeBase64String(Codec.encodeBase64String("xyz")))

    @Test
    def roundtrip07(): Unit \ Assert =
        assertEq(expected = Ok("😀"),
            Codec.decodeBase64String(Codec.encodeBase64String("😀")))

    @Test
    def roundtrip08(): Unit \ Assert =
        assertEq(expected = Ok("ABCDEFGHIJKLMNOPQRSTUVWXYZ"),
            Codec.decodeBase64String(Codec.encodeBase64String("ABCDEFGHIJKLMNOPQRSTUVWXYZ")))

}
