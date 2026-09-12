mod TestCodec {

    use Assert.{assertEq, assertErr};
    use Util.Codec;

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

    /////////////////////////////////////////////////////////////////////////////
    // encodeHex                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def encodeHex01(): Unit \ Assert =
        assertEq(expected = "", Codec.encodeHex(String.toBytes("")))

    @Test
    def encodeHex02(): Unit \ Assert =
        assertEq(expected = "66", Codec.encodeHex(String.toBytes("f")))

    @Test
    def encodeHex03(): Unit \ Assert =
        assertEq(expected = "666f6f", Codec.encodeHex(String.toBytes("foo")))

    @Test
    def encodeHex04(): Unit \ Assert =
        assertEq(expected = "666f6f626172", Codec.encodeHex(String.toBytes("foobar")))

    /////////////////////////////////////////////////////////////////////////////
    // decodeHex                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def decodeHex01(): Unit \ Assert =
        assertEq(expected = Ok(String.toBytes("")), Codec.decodeHex(""))

    @Test
    def decodeHex02(): Unit \ Assert =
        assertEq(expected = Ok(String.toBytes("f")), Codec.decodeHex("66"))

    @Test
    def decodeHex03(): Unit \ Assert =
        assertEq(expected = Ok(String.toBytes("foobar")), Codec.decodeHex("666f6f626172"))

    @Test
    def decodeHex04(): Unit \ Assert =
        assertErr(Codec.decodeHex("zz"))

    @Test
    def decodeHex05(): Unit \ Assert =
        assertErr(Codec.decodeHex("abc"))

    /////////////////////////////////////////////////////////////////////////////
    // encodeHexString                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def encodeHexString01(): Unit \ Assert =
        assertEq(expected = "", Codec.encodeHexString(""))

    @Test
    def encodeHexString02(): Unit \ Assert =
        assertEq(expected = "466c6978", Codec.encodeHexString("Flix"))

    @Test
    def encodeHexString03(): Unit \ Assert =
        assertEq(expected = "48656c6c6f2c20576f726c6421", Codec.encodeHexString("Hello, World!"))

    /////////////////////////////////////////////////////////////////////////////
    // decodeHexString                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def decodeHexString01(): Unit \ Assert =
        assertEq(expected = Ok(""), Codec.decodeHexString(""))

    @Test
    def decodeHexString02(): Unit \ Assert =
        assertEq(expected = Ok("Flix"), Codec.decodeHexString("466c6978"))

    @Test
    def decodeHexString03(): Unit \ Assert =
        assertEq(expected = Ok("Hello, World!"), Codec.decodeHexString("48656c6c6f2c20576f726c6421"))

    @Test
    def decodeHexString04(): Unit \ Assert =
        assertErr(Codec.decodeHexString("zz"))

    /////////////////////////////////////////////////////////////////////////////
    // hex roundtrip                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def hexRoundtrip01(): Unit \ Assert =
        assertEq(expected = Ok(""), Codec.decodeHexString(Codec.encodeHexString("")))

    @Test
    def hexRoundtrip02(): Unit \ Assert =
        assertEq(expected = Ok("The quick brown fox jumps over the lazy dog"),
            Codec.decodeHexString(Codec.encodeHexString("The quick brown fox jumps over the lazy dog")))

    @Test
    def hexRoundtrip03(): Unit \ Assert =
        assertEq(expected = Ok(String.toBytes("foobar")),
            Codec.decodeHex(Codec.encodeHex(String.toBytes("foobar"))))

}
